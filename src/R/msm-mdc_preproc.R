rm(list=ls()); gc()
pacman::p_load(
  tidyverse,
  magrittr,
  broom
)

deltat<-60
t_seq<-seq(0,5*365.25,by=deltat)
tn<-length(t_seq)

##==== cohort selection ====
tbl2<-readRDS("./data/tbl1_cov_endpt.rds") %>%
  filter(
    COMPLT_FLAG == 'complete' & 
    CASE_ASSERT == 'confirmed'
  ) 

# exclude post-censor and post-death period
excld<-readRDS("./data/als_endpts_endpt_sub_60.rda") %>%
  semi_join(tbl2,by="PATID") %>%
  mutate(val = 1) %>%
  spread(ENDPT_SUB,val) %>%
  filter(!is.na(coalesce(death,censor))) %>%
  mutate(excld_ind = coalesce(death, censor)) %>%
  select(-death, -censor) %>%
  group_by(PATID) %>%
  filter(T_DAYS > min(T_DAYS)) %>%
  ungroup

##==== training-testing partition mapping ====
tr_ts<-tbl2 %>% select(PATID) %>% unique %>%
  mutate(hdout82=sample(c(0,1),nrow(.),replace=T,prob=c(0.8,0.2)),
         hdout91=sample(c(0,1),nrow(.),replace=T,prob=c(0.9,0.1)),
         cv5=sample(1:5,nrow(.),replace=T,prob=rep(0.2,5)),
         cv10=sample(1:10,nrow(.),replace=T,prob=rep(0.1,10)))
saveRDS(tr_ts,file="./data/part_idx.rda")
rm(tr_ts);gc()

##==== prepare full analytic dataset =====
cnt_spec<-function(v){
  return(
    ifelse(
      any(grepl('neurolog',v)),
      length(unique(v[!v %in% c('neurology','pcp')])),
      0
    ))
}
# propensity targets and overall outcomes
dt<-data.frame(
  PATID = as.character(),
  var = as.character(),
  val = as.numeric(),
  T_DAYS = as.numeric(),
  stringsAsFactors = F
) %>%
  # death and censor
  bind_rows(
    readRDS("./data/als_endpts_endpt_sub_60.rda") %>%
      mutate(val = 1) %>%
      rename(var = ENDPT_SUB) %>%
      mutate(var = paste0("TX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # any use of FDA-approved drug or AOT
  bind_rows(
    readRDS("./data/als_all_rx_in_fda_aot_60.rda") %>%
      mutate(val = 1) %>%
      rename(var = FDA_AOT) %>%
      mutate(var = paste0("TX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # any use of PEG, NIV, and wheelchairs
  bind_rows(
    readRDS("./data/als_sel_device_device_60.rda") %>%
      mutate(val = 1) %>%
      rename(var = DEVICE) %>%
      mutate(var = paste0("TX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # providers - individual specialty
  bind_rows(
    readRDS("./data/als_px_prvdr_specialty_group_60.rda") %>%
      mutate(val = 1) %>%
      rename(var = SPECIALTY_GROUP) %>%
      mutate(var = paste0("PRVDR_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # providers - specialty care team
  bind_rows(
    readRDS("./data/als_px_prvdr_specialty_group_60.rda") %>%
      filter(!SPECIALTY_GROUP %in% c('other','eye','surgery','home-health','icu','palliative')) %>%
      group_by(PATID,T_DAYS) %>%
      summarise(spec_cnt=cnt_spec(SPECIALTY_GROUP),.groups="drop") %>%
      filter(spec_cnt>=4) %>%
      mutate(val = 1,var='PRVDR_NEURO_w4up') %>% 
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # mdc px code
  bind_rows(
    readRDS("./data/als_all_px_ccs_ccs_pxgrpcd_60.rda") %>%
      filter(CCS_PXGRPCD=='99999') %>%
      mutate(val = 1,var = 'PRVDR_mdc') %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # attach overall outcomes
  bind_rows(
    readRDS("./data/als_endpts_endpt_sub_60.rda") %>%
      filter(ENDPT_SUB %in% c("death","censor")) %>%
      mutate(val=1) %>%
      rename(var = ENDPT_SUB) %>%
      mutate(var = paste0("OC_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # filter feasible outcomes
  inner_join(
    tbl2 %>% select(PATID,time_death_censor),
    by="PATID"
  ) %>%
  filter(T_DAYS < time_death_censor+deltat) %>%
  select(-time_death_censor)

# low-frequency filter
N_pat<-length(unique(dt$PATID))
drop_rt<-0.01
dt %<>%
  semi_join(
    dt %>% 
      group_by(var) %>%
      summarise(pt_cnt = length(unique(PATID)),.groups="drop") %>%
      filter(pt_cnt > N_pat*drop_rt),
    by = "var"
  )

# fill gap period with 0
dt2<-tbl2 %>% select(PATID) %>% unique %>%
  dplyr::slice(rep(1:n(),each = tn)) %>%
  mutate(T_DAYS = rep(t_seq,nrow(tbl2))) %>%
  left_join(
    dt %>% pivot_wider(
      names_from = "var",
      values_from = "val",
      values_fill = 0
    ),
    by=c("PATID","T_DAYS"),
    multiple = "all"
  ) %>%
  replace(is.na(.),0) %>%
  pivot_longer(
    !PATID:T_DAYS,
    names_to = "var",
    values_to = "val"
  ) %>%
  # attach cohort filter
  anti_join(excld,by=c("PATID","T_DAYS")) %>%
  # filter out time-zero
  filter(T_DAYS > 0)

saveRDS(
  dt2 %>% semi_join(readRDS("./data/part_idx.rda") %>% filter(hdout82 == 0),by="PATID"),
  file="./data/trainY_82.rda"
);gc()

saveRDS(
  dt2 %>% semi_join(readRDS("./data/part_idx.rda") %>% filter(hdout82 == 1),by="PATID"),
  file="./data/testY_82.rda"
);gc()


##==== gather time-invariant variables ====
rm(dt,dt2);gc()
tbl2_demo_expand<-tbl2 %>%
  # mutate(AGEGRP_AT_INDEX = case_when(
  #   AGE_AT_INDEX< 50 ~ 'agegrp1',
  #   AGE_AT_INDEX>= 50 & AGE_AT_INDEX<55 ~ 'agegrp2',
  #   AGE_AT_INDEX>= 55 & AGE_AT_INDEX<60 ~ 'agegrp3',
  #   AGE_AT_INDEX>= 60 & AGE_AT_INDEX<65 ~ 'agegrp4',
  #   AGE_AT_INDEX>= 65 & AGE_AT_INDEX<70 ~ 'agegrp5',
  #   AGE_AT_INDEX>= 70 & AGE_AT_INDEX<75~ 'agegrp6',
  #   AGE_AT_INDEX>= 75 & AGE_AT_INDEX<80 ~ 'agegrp7',
  #   AGE_AT_INDEX>= 80 & AGE_AT_INDEX<85  ~ 'agegrp8',
  #   TRUE ~ 'agegrp9'
  # )) %>%
select(PATID,SEX,RACE,HISPANIC,INDEX_EVENT,INDEX_SRC) %>%
  pivot_longer(!PATID, names_to = "var", values_to = "val") %>%
  mutate(var = paste0(var,":",val),val=1) %>%
  bind_rows(
    tbl2 %>%
      select(PATID,AGE_AT_INDEX) %>%
      rename(val = AGE_AT_INDEX) %>%
      mutate(var = "AGE_AT_INDEX")
  )
N<-nrow(tbl2_demo_expand)

#==== gather time-varying variables ====
dt<-data.frame(
  PATID = as.character(),
  var = as.character(),
  val = as.numeric(),
  T_DAYS = as.numeric(),
  stringsAsFactors = F
) %>%
  # lagged features
  #-- phecodes
  bind_rows(
    readRDS("./data/als_all_phecd_phecd_dxgrpcd_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      mutate(val = 1) %>%
      rename(var = PHECD_DXGRPCD) %>%
      mutate(var = paste0("PHECD_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- meds
  bind_rows(
    readRDS("./data/als_all_rx_in_ingredient_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      mutate(val = 1) %>%
      rename(var = INGREDIENT) %>%
      mutate(var = paste0("RX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- procedures
  # bind_rows(
  #   readRDS("./data/als_all_px_ccs_ccs_pxgrpcd_60.rda") %>%
  #     inner_join(
  #       tbl2 %>% select(PATID,time_death_censor),
  #       by="PATID"
  #     ) %>%
  #     filter(T_DAYS < time_death_censor) %>%
  #     mutate(val = 1) %>%
  #     rename(var = CCS_PXGRPCD) %>%
  #     mutate(var = paste0("PXCCS_",var)) %>%
  #     select(PATID,var,val,T_DAYS) %>% unique
  # ) %>%
  #-- sdoh
  bind_rows(
    readRDS("./data/als_sel_sdoh_obscomm_code_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(grepl("^(ADI)+",OBSCOMM_CODE)) %>%
      mutate(val = as.numeric(OBSCOMM_RESULT)) %>%
      rename(var = OBSCOMM_CODE) %>%
      mutate(var = paste0("PAST_SDH_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  bind_rows(
    readRDS("./data/als_sel_sdoh_obscomm_code_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(!grepl("^(ADI)+",OBSCOMM_CODE)) %>%
      mutate(val = 1) %>%
      mutate(var = paste0(OBSCOMM_CODE,":",OBSCOMM_RESULT)) %>%
      select(-OBSCOMM_CODE,-OBSCOMM_RESULT) %>%
      mutate(var = paste0("PAST_SDH_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- use of riluzole, endarovone, or AOT
  bind_rows(
    readRDS("./data/als_all_rx_in_fda_aot_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      mutate(val = 1) %>%
      rename(var = FDA_AOT) %>%
      mutate(var = paste0("TX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- use of NIV, PEG, Wheelchair
  bind_rows(
    readRDS("./data/als_sel_device_device_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      mutate(val = 1) %>%
      rename(var = DEVICE) %>%
      mutate(var = paste0("TX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- providers - individual specialty
  bind_rows(
    readRDS("./data/als_px_prvdr_specialty_group_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      mutate(val = 1) %>%
      rename(var = SPECIALTY_GROUP)  %>%
      mutate(var = paste0("PRVDR_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # providers - specialty care team
  bind_rows(
    readRDS("./data/als_px_prvdr_specialty_group_60.rda") %>%
      filter(!SPECIALTY_GROUP %in% c('other','eye','surgery','home-health','icu','palliative')) %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      group_by(PATID,T_DAYS) %>%
      summarise(spec_cnt=cnt_spec(SPECIALTY_GROUP),.groups="drop") %>%
      filter(spec_cnt>=4) %>%
      mutate(val = 1,var='PRVDR_NEURO_w4up') %>% 
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # providers - mdc px code
  bind_rows(
    readRDS("./data/als_all_px_ccs_ccs_pxgrpcd_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(CCS_PXGRPCD=='99999') %>%
      mutate(val = 1,var = 'PRVDR_mdc') %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- labs: indicator
  bind_rows(
    readRDS("./data/als_sel_obs_obs_name_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      mutate(val = 1) %>%
      rename(var = OBS_NAME)  %>%
      mutate(var = paste0("LAB_",var,"_IND")) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- labs: numerical
  bind_rows(
    readRDS("./data/als_sel_obs_obs_name_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor & !is.na(OBS_NUM)) %>%
      mutate(var = paste0("LAB_",OBS_NAME)) %>%
      rename(val=OBS_NUM)  %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  #-- labs: categorical
  bind_rows(
    readRDS("./data/als_sel_obs_obs_name_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(
        T_DAYS < time_death_censor & 
        !is.na(OBS_QUAL) & !OBS_QUAL %in% c('NI','UN','OT') &
        OBS_QUAL %in% c("SMOKING","TOBACCO")
      ) %>%
      unite("var",c("OBS_NAME","OBS_QUAL"),sep="_") %>%
      mutate(var=paste0("LAB_",var),val = 1) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # lagging by 1 delta t
  mutate(T_DAYS = T_DAYS + deltat) %>%
  # attach concurrent features
  # - ADI, RUCA, LIS_DUAL, PART_C, PART_D
  bind_rows(
    readRDS("./data/als_sel_sdoh_obscomm_code_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(grepl("^(ADI)+",OBSCOMM_CODE)) %>%
      mutate(val = as.numeric(OBSCOMM_RESULT)) %>%
      rename(var = OBSCOMM_CODE) %>%
      mutate(var = paste0("CURR_SDH_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  bind_rows(
    readRDS("./data/als_sel_sdoh_obscomm_code_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(!grepl("^(ADI)+",OBSCOMM_CODE)) %>%
      mutate(val = 1) %>%
      mutate(var = paste0(OBSCOMM_CODE,":",OBSCOMM_RESULT)) %>%
      select(-OBSCOMM_CODE,-OBSCOMM_RESULT) %>%
      mutate(var = paste0("CURR_SDH_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # attach time-invariant features, broadcast to each time point
  bind_rows(
    tbl2_demo_expand %>%
      select(PATID,var,val) %>% unique %>%
      dplyr::slice(rep(1:n(),each = tn)) %>% 
      mutate(
        T_DAYS = rep(t_seq,N),
        var = paste0('DEMO_',var)
      )
  ) %>%
  # attach time index
  bind_rows(
    tbl2 %>% select(PATID) %>% unique %>%
      dplyr::slice(rep(1:n(),each = tn)) %>% 
      mutate(
        T_DAYS = rep(t_seq,nrow(tbl2)),
        val = T_DAYS,
        var = "T_DAYS"
      )
  ) %>%
  # filter out post-outcome data points
  anti_join(excld,by=c("PATID","T_DAYS")) %>%
  # filter out time-zero
  filter(T_DAYS > 0) %>%
  mutate(var2=paste0("v",dense_rank(var)))

# low-frequency filter
N_pat<-length(unique(dt$PATID))
drop_rt<-0.01
dt %<>%
  semi_join(
    dt %>% 
      group_by(var) %>%
      summarise(pt_cnt = length(unique(PATID)),.groups="drop") %>%
      filter(pt_cnt > N_pat*drop_rt),
    by = "var"
  )

# get var mapping and some auxiliary stats
var_map<-dt %>% 
  group_by(var,var2) %>% 
  mutate(
    val_lev = length(unique(val))
  ) %>% ungroup 

var_map2<-var_map %>% filter(val_lev > 1) %>%
    group_by(var,var2,val_lev,T_DAYS) %>%
    summarise(
      pt_cnt = length(unique(PATID)),
      pt_prop = round(pt_cnt/N_pat,3),
      val_m = mean(val,na.rm=T),
      val_sd = sd(val,na.rm=T),
      .groups = 'drop'
    ) %>%
  bind_rows(
    var_map %>% filter(val_lev == 1) %>%
      group_by(T_DAYS) %>%
      mutate(pt_cnt = length(unique(PATID))) %>%
      ungroup %>%
      group_by(var,var2,val_lev,T_DAYS,pt_cnt) %>%
      summarise(
        val_sum = sum(val),
        .groups = 'drop'
      ) %>%
      mutate(
        val_m = val_sum/pt_cnt,
        val_sd = sqrt(val_m*(1-val_m)/pt_cnt),
        pt_prop = round(pt_cnt/N_pat,3)
      ) %>%
      select(-val_sum)
  )

saveRDS(var_map2,file = "./data/var_encoder.rda");gc()

dt %<>% select(-var)
saveRDS(
  dt %>% semi_join(readRDS("./data/part_idx.rda") %>% filter(hdout82 == 0),by="PATID"),
  file="./data/trainX_82.rda"
);gc()

saveRDS(
  dt %>% semi_join(readRDS("./data/part_idx.rda") %>% filter(hdout82 == 1),by="PATID"),
  file="./data/testX_82.rda"
);gc()


