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
    COMPLT_IND == 1 & 
    CASE_ASSERT == 'confirmed' &
    !INDEX_EVENT %in% c('PRX','DRX')
  ) 

##==== training-testing partition mapping ====
# tr_ts<-tbl2 %>% select(PATID) %>% unique %>%
#   mutate(hdout82=sample(c(0,1),nrow(.),replace=T,prob=c(0.8,0.2)),
#          hdout91=sample(c(0,1),nrow(.),replace=T,prob=c(0.9,0.1)),
#          cv5=sample(1:5,nrow(.),replace=T,prob=rep(0.2,5)),
#          cv10=sample(1:10,nrow(.),replace=T,prob=rep(0.1,10)))
# saveRDS(tr_ts,file="./data/part_idx.rda")
# rm(tr_ts);gc()

##==== prepare full analytic dataset =====
# propensity and overall outcomes
dt<- readRDS("./data/als_all_rx_in_fda_aot_60.rda") %>%
  mutate(val = 1) %>%
  rename(var = FDA_AOT) %>%
  mutate(var = paste0("TX_",var)) %>%
  select(PATID,var,val,T_DAYS) %>% unique %>%
  bind_rows(
    readRDS("./data/als_endpts_endpt_sub_60.rda") %>%
      filter(!ENDPT_SUB %in% c("death","censor")) %>%
      mutate(val = 1) %>%
      rename(var = ENDPT_SUB) %>%
      mutate(var = paste0("TX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  bind_rows(
    readRDS("./data/als_office_prvdr_specialty_group_60.rda") %>%
      mutate(val = 1) %>%
      rename(var = SPECIALTY_GROUP) %>%
      mutate(var = paste0("PRVDR_",var)) %>%
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
  # align time index with covariates
  mutate(T_DAYS = T_DAYS - deltat) %>%
  filter(T_DAYS >= 0) %>%
  select(-time_death_censor)

# exclude post-censor and post-death period
excld<-readRDS("./data/als_endpts_endpt_sub_60.rda") %>%
  filter(ENDPT_SUB %in% c("death","censor")) %>%
  semi_join(tbl2,by="PATID") %>%
  mutate(val = 1) %>%
  spread(ENDPT_SUB,val) %>%
  filter(!is.na(coalesce(death,censor))) %>%
  mutate(excld_ind = coalesce(death, censor)) %>%
  select(-death, -censor) %>%
  group_by(PATID) %>%
  filter(T_DAYS > min(T_DAYS)) %>%
  ungroup

dt2<-tbl2 %>% select(PATID) %>% unique %>%
  dplyr::slice(rep(1:n(),each = tn)) %>%
  mutate(T_DAYS = rep(t_seq,nrow(tbl2))) %>%
  left_join(
    dt %>% pivot_wider(
      names_from = "var",
      values_from = "val",
      values_fill = 0
    ),
    by=c("PATID","T_DAYS")
  ) %>%
  replace(is.na(.),0) %>%
  pivot_longer(
    !PATID:T_DAYS,
    names_to = "var",
    values_to = "val"
  ) %>%
  # attach cohort filter
  anti_join(excld,by=c("PATID","T_DAYS"))

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
select(PATID,SEX,RACE,HISPANIC) %>%
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
dt<-
  # lagged features
  readRDS("./data/als_all_phecd_60.rda") %>%
    inner_join(
      tbl2 %>% select(PATID,time_death_censor),
      by="PATID"
    ) %>%
    filter(T_DAYS < time_death_censor) %>%
    mutate(val = 1) %>%
    rename(var = PHECD_DXGRPCD) %>%
    mutate(var = paste0("PHECD_",var)) %>%
    select(PATID,var,val,T_DAYS) %>% unique %>%
  bind_rows(
    readRDS("./data/als_all_rx_in_60.rda") %>%
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
  bind_rows(
    readRDS("./data/als_endpts_endpt_sub_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(!ENDPT_SUB %in% c("death","censor")) %>%
      mutate(val = 1) %>%
      rename(var = ENDPT_SUB) %>%
      mutate(var = paste0("TX_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  bind_rows(
    readRDS("./data/als_office_prvdr_specialty_group_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      mutate(val = 1) %>%
      rename(var = SPECIALTY_GROUP)  %>%
      mutate(var = paste0("PRvDR_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # attach concurrent features
  # - ADI, RUCA, LIS_DUAL, PART_C, PART_D
  bind_rows(
    readRDS("./data/als_sel_sdoh_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(grepl("^(ADI)+",OBSCOMM_CODE)) %>%
      mutate(val = as.numeric(OBSCOMM_RESULT),
             T_DAYS = T_DAYS + deltat) %>%
      rename(var = OBSCOMM_CODE) %>%
      mutate(var = paste0("SDH_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  bind_rows(
    readRDS("./data/als_sel_sdoh_60.rda") %>%
      inner_join(
        tbl2 %>% select(PATID,time_death_censor),
        by="PATID"
      ) %>%
      filter(T_DAYS < time_death_censor) %>%
      filter(!grepl("^(ADI)+",OBSCOMM_CODE)) %>%
      mutate(val = 1,T_DAYS = T_DAYS + deltat) %>%
      mutate(var = paste0(OBSCOMM_CODE,":",OBSCOMM_RESULT)) %>%
      select(-OBSCOMM_CODE,-OBSCOMM_RESULT) %>%
      mutate(var = paste0("SDH_",var)) %>%
      select(PATID,var,val,T_DAYS) %>% unique
  ) %>%
  # attach time-invariant features
  bind_rows(
    tbl2_demo_expand %>%
      select(PATID,var,val) %>% unique %>%
      dplyr::slice(rep(1:n(),each = tn)) %>% 
      mutate(
        T_DAYS = rep(t_seq,N)
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
  anti_join(excld,by=c("PATID","T_DAYS")) %>%
  mutate(var2=paste0("v",dense_rank(var))) 

saveRDS(
  dt %>% select(var,var2) %>% unique,
  file = "./data/var_encoder.rda"
);gc()

dt %<>% select(-var)
saveRDS(
  dt %>% semi_join(readRDS("./data/part_idx.rda") %>% filter(hdout82 == 0),by="PATID"),
  file="./data/trainX_82.rda"
);gc()

saveRDS(
  dt %>% semi_join(readRDS("./data/part_idx.rda") %>% filter(hdout82 == 1),by="PATID"),
  file="./data/testX_82.rda"
);gc()


