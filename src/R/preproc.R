rm(list=ls()); gc()
setwd("C:/repo/cdc_als4m")

# install.packages("pacman")
pacman::p_load(
  tidyverse,
  magrittr,
  dbplyr,
  devtools,
  stringdist,
  RANN, 
  data.table,
  caret,
  mice,
  lubridate
)

# source utility function
source_url("https://raw.githubusercontent.com/sxinger/utils/master/extract_util.R")

#==== time-invariant feature table =====
tbl1<-readRDS("C:/repo/cdc_als4m/data/als_tbl1.rds")
sdoh_ruca<-readRDS("./data/als_sdoh.rds") %>%
  filter(grepl("(RUCA\\|PRIMARY)+",OBSCOMM_CODE)) %>%
  group_by(PATID,OBSCOMM_CODE) %>% 
  arrange(abs(DAYS_SINCE_INDEX)) %>% 
  slice(1:1) %>%  ungroup %>%
  filter(!is.na(OBSCOMM_RESULT_TEXT)) %>%
  mutate(RUCA = OBSCOMM_RESULT_TEXT) %>%
  select(PATID,RUCA) %>%
  {. ->> sdoh_ruca_temp} %>% # save intermediate table which can be called later
  bind_rows(
    tbl1 %>% select(PATID) %>%
      anti_join(sdoh_ruca_temp,by="PATID") %>%
      mutate(RUCA = "0")
  ) %>%
  mutate(RUCA_col = paste0("RUCA_",RUCA),
         ind=1) %>%
  select(PATID,RUCA,RUCA_col,ind) %>%
  spread(RUCA_col,ind,fill=0) %>%
  mutate(RUCAregrp = case_when(RUCA %in% c("1","2","3") ~ 'RUCA_metro',
                               RUCA %in% c("4","5","6") ~ 'RUCA_micro',
                               RUCA %in% c("7","8","9",'10') ~ 'RUCA_small',
                               TRUE ~ 'RUCA_unknown'),
         RUCAregrp_col = paste0("RUCAregrp_",RUCAregrp),ind=1) %>%
  spread(RUCAregrp_col,ind,fill=0)

sdoh_cov<-readRDS("./data/als_sdoh.rds") %>%
  group_by(PATID,OBSCOMM_CODE) %>% 
  arrange(abs(DAYS_SINCE_INDEX)) %>% 
  slice(1:1) %>%  ungroup %>%
  filter(!is.na(OBSCOMM_RESULT_NUM)) %>%
  mutate(sdh_var=paste0("SDH_",OBSCOMM_CODE)) %>%
  select(PATID,sdh_var,OBSCOMM_RESULT_NUM) %>%
  spread(sdh_var,OBSCOMM_RESULT_NUM) %>%
  replace_na(list(
    SDH_LIS_DUAL = 0,
    SDH_PART_C = 0,
    SDH_PART_D = 0
  ))
# remove invariant metrics
sdoh_nzv<- nearZeroVar(sdoh_cov, saveMetrics = TRUE)
sdoh_cov<-sdoh_cov[,row.names(sdoh_nzv)[!sdoh_nzv$zeroVar]]
# # quick imputation
# sdoh_cov_ruca<-sdoh_ruca %>%
#   select(all_of(c("PATID",paste0("RUCA_",1:10)))) %>%
#   left_join(sdoh_cov,by="PATID")
# init<-mice(sdoh_cov_ruca, maxit=0)
# predM<-init$predictorMatrix
# predM[,c("PATID")]=0
# sdoh_cov_imputed<-mice(sdoh_cov_ruca, m=1) # default: pmm
# sdoh_cov_imputed<-complete(sdoh_cov_imputed)

tbl1_sdoh<-tbl1 %>%
  left_join(sdoh_ruca,by="PATID") %>%
  left_join(sdoh_cov,by="PATID")
saveRDS(tbl1_sdoh,file="./data/tbl1_cov.rds")

# attach endpoint to table 1
endpt<-readRDS("./data/als_endpts.rds") %>%
  select(PATID,STAGE,STAGE_SINCE_INDEX) %>%
  spread(STAGE,STAGE_SINCE_INDEX) %>%
  mutate(
    time_death_censor = coalesce(death,censor),
    death = as.numeric(!is.na(death)),
    time_als1dx = als1dx,
    time_als1dx_censor = coalesce(als1dx,censor),
    als1dx = as.numeric(!is.na(als1dx)),
    time_stg1 = stg1,
    time_stg1_censor = coalesce(stg1,censor),
    stg1 = as.numeric(!is.na(stg1)),
    time_stg2 = stg2,
    time_stg2_censor = coalesce(stg2,censor),
    stg2 = as.numeric(!is.na(stg2)),
    time_stg3 = stg3,
    time_stg3_censor = coalesce(stg3,censor),
    stg3 = as.numeric(!is.na(stg3)),
    time_stg4 = stg4,
    time_stg4_censor = coalesce(stg4,censor),
    stg4 = as.numeric(!is.na(stg4)),
    time_stg5 = stg5,
    time_stg5_censor = coalesce(stg5,censor),
    stg5 = as.numeric(!is.na(stg5))
  )
tbl1_sdoh_endpt<-readRDS("./data/tbl1_cov.rds") %>%
  inner_join(endpt,by="PATID") %>%
  filter(time_death_censor > 0) # exclude pre-mature death
saveRDS(tbl1_sdoh_endpt,file="./data/tbl1_cov_endpt.rds")

# attach PCE indicators
fda<-readRDS("./data/als_fda_rx.rds") %>%
  right_join(readRDS("./data/tbl1_cov_endpt.rds") %>% 
               select(PATID),by="PATID") %>%
  replace(is.na(.),0)

aot<-readRDS("./data/als_aot_rx.rds") %>%
  right_join(
    readRDS("./data/tbl1_cov_endpt.rds") %>% 
      select(PATID),by="PATID"
  ) %>%
  replace(is.na(.),0)

tbl1_sdoh_endpt_pce<-readRDS("./data/tbl1_cov_endpt.rds") %>%
  inner_join(fda,by="PATID") %>%
  inner_join(aot,by="PATID")
saveRDS(tbl1_sdoh_endpt_pce,file="./data/tbl1_cov_endpt_pce.rds")

# attach provider specialty indicator
prvdr<-readRDS("./data/als_mdc_prvdr.rds") %>%
  right_join(readRDS("./data/tbl1_cov_endpt.rds") %>% 
               select(PATID),by="PATID") %>%
  replace(is.na(.),0)

cnt_spec<-function(v){
  return(
    ifelse(
      any(grepl('neurolog',v)),
      length(unique(v[!v %in% c('neurology','pcp')])),
      0
  ))
}
prvdrgrp<-prvdr %>%
  pivot_longer(
    cols = !PATID,
    names_to = "prvdr",
    values_to = "ind"
  ) %>%
  filter(ind==1) %>%
  group_by(PATID) %>%
  summarise(prvdr_spec_cnt = cnt_spec(prvdr),.groups = 'drop') %>% 
  mutate(
    PRVDR_w_1up = as.numeric(prvdr_spec_cnt>=1),
    PRVDR_w_2up = as.numeric(prvdr_spec_cnt>=2),
    PRVDR_w_3up = as.numeric(prvdr_spec_cnt>=3),
    PRVDR_w_4up = as.numeric(prvdr_spec_cnt>=4),
    PRVDR_w_5up = as.numeric(prvdr_spec_cnt>=5),
    PRVDR_w_6up = as.numeric(prvdr_spec_cnt>=6),
    PRVDR_w_7up = as.numeric(prvdr_spec_cnt>=7),
    PRVDR_w_8up = as.numeric(prvdr_spec_cnt>=8)
  ) %>%
  right_join(readRDS("./data/tbl1_cov_endpt.rds") %>% 
               select(PATID),by="PATID") %>%
  replace(is.na(.),0)

mdc<-readRDS("./data/als_mdc_px.rds") %>%
  right_join(readRDS("./data/tbl1_cov_endpt.rds") %>% 
               select(PATID),by="PATID") %>%
  replace(is.na(.),0)

tbl1_sdoh_endpt_pce_prvdr<-readRDS("./data/tbl1_cov_endpt_pce.rds") %>%
  inner_join(prvdr,by="PATID") %>%
  inner_join(prvdrgrp,by="PATID") %>%
  inner_join(mdc,by="PATID")

saveRDS(tbl1_sdoh_endpt_pce_prvdr,file="./data/tbl1_sdoh_endpt_pce_prvdr.rds")


