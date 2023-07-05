#################################################################
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: preproc.R
# Description: data preprocess to form final analytic set with
#              one patient per row
#################################################################
rm(list=ls()); gc()
setwd("C:/repo/GPC-Analytics-ALS-Cohort")

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

##==== Data Preprocessing ==============================================
# code-label mapping
# st_label<-load_mapping("resdac","SSA_STATE")

# exposure - riluzole use indicator - anytime
exposure_rx<-readRDS("./data/als_post_rx.rds") %>%
  filter(grepl("RILUZ",RAW_RX_MED_NAME)) %>%
  select(PATID, DAYS_SINCE_ALS1DX) %>%
  bind_rows(readRDS("./data/als_pre_rx.rds") %>%
              filter(grepl("RILUZ",RAW_RX_MED_NAME)) %>%
              select(PATID, DAYS_SINCE_ALS1DX) %>%
              mutate(DAYS_SINCE_ALS1DX = - DAYS_SINCE_ALS1DX)) %>%
  group_by(PATID) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(RILUZOLE_FLAG = 1)

# exposure - niv - anytime
niv<-load_valueset(vs_template = "ncbo",
                   vs_url="https://raw.githubusercontent.com/RWD2E/phecdm/main/res/valueset_autogen/als-tx_output.json",
                   vs_name_str="continuous positive airway pressure") %>%
  bind_rows(
    load_valueset(vs_template = "ncbo",
                  vs_url="https://raw.githubusercontent.com/RWD2E/phecdm/main/res/valueset_autogen/als-tx_output.json",
                  vs_name_str="ventilator")
  )

exposure_niv<-readRDS("./data/als_post_px.rds") %>%
  semi_join(niv,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
  select(PATID, DAYS_SINCE_ALS1DX) %>%
  bind_rows(readRDS("./data/als_pre_px.rds") %>%
              semi_join(niv,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
              select(PATID, DAYS_SINCE_ALS1DX) %>%
              mutate(DAYS_SINCE_ALS1DX = - DAYS_SINCE_ALS1DX)) %>%
  group_by(PATID) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(NIV_FLAG = 1)

# exposure - wheelchair - anytime
wheelchair<-load_valueset(vs_template = "ncbo",
                          vs_url="https://raw.githubusercontent.com/RWD2E/phecdm/main/res/valueset_autogen/als-tx_output.json",
                          vs_name_str="power wheelchair")
exposure_pw<-readRDS("./data/als_post_px.rds") %>%
  semi_join(wheelchair,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
  select(PATID, DAYS_SINCE_ALS1DX) %>%
  bind_rows(readRDS("./data/als_pre_px.rds") %>%
              semi_join(wheelchair,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
              select(PATID, DAYS_SINCE_ALS1DX) %>%
              mutate(DAYS_SINCE_ALS1DX = - DAYS_SINCE_ALS1DX)) %>%
  group_by(PATID) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(WHEELCHAIR_FLAG = 1)

# exposure - gastrostomy - anytime
gastrostomy<-load_valueset(vs_template = "ncbo",
                           vs_url="https://raw.githubusercontent.com/RWD2E/phecdm/main/res/valueset_autogen/als-tx_output.json",
                           vs_name_str="gastrostomy")
exposure_gas<-readRDS("./data/als_post_px.rds") %>%
  semi_join(gastrostomy,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
  select(PATID, DAYS_SINCE_ALS1DX) %>%
  bind_rows(readRDS("./data/als_pre_px.rds") %>%
              semi_join(gastrostomy,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
              select(PATID, DAYS_SINCE_ALS1DX) %>%
              mutate(DAYS_SINCE_ALS1DX = - DAYS_SINCE_ALS1DX)) %>%
  group_by(PATID) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(GASTROSTOMY_FLAG = 1)

# exposure - tracheostomy - anytime
tracheostomy<-load_valueset(vs_template = "ncbo",
                            vs_url="https://raw.githubusercontent.com/RWD2E/phecdm/main/res/valueset_autogen/als-tx_output.json",
                            vs_name_str="tracheostomy")
exposure_trach<-readRDS("./data/als_post_px.rds") %>%
  semi_join(tracheostomy,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
  select(PATID, DAYS_SINCE_ALS1DX) %>%
  bind_rows(readRDS("./data/als_pre_px.rds") %>%
              semi_join(tracheostomy,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
              select(PATID, DAYS_SINCE_ALS1DX) %>%
              mutate(DAYS_SINCE_ALS1DX = - DAYS_SINCE_ALS1DX)) %>%
  group_by(PATID) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(TRACHEOSTOMY_FLAG = 1)

# exposure - physical therapy - anytime
pt<-load_valueset(vs_template = "ncbo",
                  vs_url="https://raw.githubusercontent.com/RWD2E/phecdm/main/res/valueset_autogen/als-tx_output.json",
                  vs_name_str="physical therapy")
exposure_pt<-readRDS("./data/als_post_px.rds") %>%
  semi_join(tracheostomy,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
  select(PATID, DAYS_SINCE_ALS1DX) %>%
  bind_rows(readRDS("./data/als_pre_px.rds") %>%
              semi_join(tracheostomy,by=c("PX_TYPE"="CODE_TYPE_CDM","PX"="CODE")) %>%
              select(PATID, DAYS_SINCE_ALS1DX) %>%
              mutate(DAYS_SINCE_ALS1DX = - DAYS_SINCE_ALS1DX)) %>%
  group_by(PATID) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(PT_FLAG = 1)

# pivot pre-existing phecode table
n<-nrow(readRDS("./data/als_tbl1.rds"))
n0<-floor(n*0.05)
n1<-ceiling(n*0.95)
cov_phecd<-readRDS("./data/als_pre_dx_phecd.rds") %>%
  mutate(PHECODE = gsub("(\\.)+.*","",PHECD_DXGRPCD)) %>% # only retain integer-level phecode
  group_by(PATID,PHECODE) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(phecode_var=paste0("PHECD_",PHECODE),ind=1) %>%
  select(PATID,phecode_var,ind) %>%
  group_by(phecode_var) %>% mutate(support=sum(ind)) %>% ungroup %>%
  # drop invariant phecodes
  filter(support >= n0) %>%
  filter(support <= n1) %>%
  select(-support) %>%
  spread(phecode_var,ind, fill=0)

# pivot baseline medication table
cov_med<-readRDS("./data/als_pre_rx.rds") %>%
  mutate(MED_CD = paste0("MED",as.numeric(as.factor(RAW_RX_MED_NAME)))) %>%
  group_by(PATID,MED_CD) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(ind=1) %>%
  select(PATID,MED_CD,ind) %>%
  group_by(MED_CD) %>% mutate(support=sum(ind)) %>% ungroup %>%
  # drop invariant phecodes
  filter(support >= n0) %>%
  filter(support <= n1) %>%
  select(-support) %>%
  spread(MED_CD,ind,fill=0)

# pivot baseline procedure table
cov_px<-readRDS("./data/als_pre_px.rds") %>%
  mutate(PX_GRPCD = paste0("CCSPX_",PX_GRPCD)) %>%
  group_by(PATID,PX_GRPCD) %>% arrange(DAYS_SINCE_ALS1DX) %>% slice(1:1) %>% ungroup %>%
  mutate(ind=1) %>%
  select(PATID,PX_GRPCD,ind) %>%
  group_by(PX_GRPCD) %>% mutate(support=sum(ind)) %>% ungroup %>%
  # drop invariant phecodes
  filter(support >= n0) %>%
  filter(support <= n1) %>%
  select(-support) %>%
  spread(PX_GRPCD,ind,fill=0)

# create analytical dataset
aset<-readRDS("./data/als_tbl1.rds") %>%
  # left_join(st_label,by=c("ADDRESS_STATE"="CODE")) %>%
  # rename("ST_LABEL" = "LABEL") %>%
  # # group uncovered non-GPC states into 'OT'
  # group_by(ADDRESS_STATE) %>%
  # mutate(pat_prev = length(unique(PATID))) %>% ungroup %>%
  # mutate(ST_LABEL = case_when(pat_prev < 11 ~ 'OT',
  #                             TRUE ~ ST_LABEL)) %>%
  # select(-pat_prev,-ADDRESS_STATE) %>%
  mutate(
    # get calendar year
    ALS1DX_CALYR = year(ymd(ALS1DX_DATE)),
    # all-cause DEATH survival obj
    DEATH_time = coalesce(DAYS_ALS1DX_TO_DEATH,DAYS_ALS1DX_TO_CENSOR),
    DEATH_status = as.numeric(!is.na(DAYS_ALS1DX_TO_DEATH)),
    # label demographic fields with regrouping
    AGEGRP = case_when(AGE_AT_ALS1DX < 65 ~ 'AGEGRP1',
                       AGE_AT_ALS1DX >= 65 & AGE_AT_ALS1DX < 70 ~ 'AGEGRP2',
                       AGE_AT_ALS1DX >= 70 & AGE_AT_ALS1DX < 75  ~ 'AGEGRP3',
                       AGE_AT_ALS1DX >= 75 & AGE_AT_ALS1DX < 80 ~ 'AGEGRP4',
                       AGE_AT_ALS1DX >= 80 & AGE_AT_ALS1DX < 85 ~ 'AGEGRP5',
                       TRUE ~ 'AGEGRP6',),
    AGEGRP_fac = relevel(as.factor(AGEGRP),ref="AGEGRP2"),
    RACE_LABEL = case_when(RACE=='05' ~ 'White',
                           RACE=='03' ~ 'AA',
                           RACE=='02' ~ 'Other',
                           RACE=='01' ~ 'Other',
                           RACE=='OT' & HISPANIC=='Y' ~ 'Other',
                           RACE=='NI' ~ 'Unknown',
                           RACE=='UN' ~ 'Unknown',
                           TRUE ~ 'Other'),
    RACE_LABEL_fac = paste0("RACE_",relevel(as.factor(RACE_LABEL),ref="White")),
    SEXF = as.numeric(SEX=='F'),
    # OREC_fac = paste0("OREC_",relevel(as.factor(OREC),ref="0")), orec_ind = 1
    # ST_LABEL_fac = paste0("ST_",relevel(as.factor(ST_LABEL),ref="OT")), state_ind = 1
    agegrp_ind = 1,race_ind = 1
  ) %>%
  # spread(OREC_fac,orec_ind,fill=0) %>%
  spread(AGEGRP_fac,agegrp_ind,fill=0) %>%
  spread(RACE_LABEL_fac,race_ind,fill=0) %>%
  # spread(ST_LABEL_fac,state_ind,fill=0) %>%
  select(
    PATID, SEXF, DEATH_time, DEATH_status,
    AGE_AT_ALS1DX, AGEGRP, AGEGRP1, AGEGRP2, AGEGRP3, AGEGRP4, AGEGRP5, AGEGRP6,
    RACE_LABEL, RACE_White, RACE_AA, RACE_Other, RACE_Unknown,
    # OREC,OREC_0,OREC_1,OREC_2,OREC_3,
    # ST_LABEL,ST_Indiana,ST_Iowa,ST_Kansas,ST_Minnesota,ST_Missouri,ST_Nebraska,ST_OT,ST_Texas,ST_Wisconsin,
    PARTD_IND, LIS_DUAL_IND
  ) %>%
  # attach exposure indicators - riluzole
  left_join(exposure_rx %>% select(PATID, RILUZOLE_FLAG),
            by="PATID") %>%
  # attach exposure indicators - niv
  left_join(exposure_niv %>% select(PATID, NIV_FLAG),
            by="PATID") %>%
  # attach exposure indicators - power wheelchair
  left_join(exposure_pw %>% select(PATID, WHEELCHAIR_FLAG),
            by="PATID") %>%
  # attach exposure indicators - gastrostomy
  left_join(exposure_gas %>% select(PATID, GASTROSTOMY_FLAG),
            by="PATID") %>%
  # attach exposure indicators - physical therapy
  left_join(exposure_pt %>% select(PATID, PT_FLAG),
            by="PATID") %>%
  # attach all historical phecodes
  left_join(cov_phecd,by="PATID") %>% replace(is.na(.),0) %>%
  # attach eligible historical meds
  left_join(cov_med,by="PATID") %>% replace(is.na(.),0) %>%
  # attach eligible historical procedures
  left_join(cov_px,by="PATID") %>% replace(is.na(.),0) %>%
  # attach exposure indicators - tracheostomy
  left_join(exposure_trach %>% select(PATID, DAYS_SINCE_ALS1DX, TRACHEOSTOMY_FLAG) %>%
              rename(DAYS_TO_TRACHEOSTOMY = DAYS_SINCE_ALS1DX),
            by="PATID") %>%
  replace_na(list(TRACHEOSTOMY_FLAG=0))

# pivot sdoh table
sdoh_ruca<-readRDS("./data/als_sdoh.rds") %>%
  group_by(PATID,OBSCOMM_CODE) %>% 
  arrange(abs(ADDRESS_PERIOD_START_ALS1DX_DAYS)) %>% 
  slice(1:1) %>%  ungroup %>%
  filter(!is.na(OBSCOMM_RESULT_TEXT)) %>%
  filter(grepl("(RUCA\\|PRIMARY)+",OBSCOMM_CODE)) %>%
  mutate(RUCA = OBSCOMM_RESULT_TEXT) %>%
  select(PATID,RUCA) %>%
  {. ->> sdoh_ruca_temp} %>% # save intermediate table which can be called later
  bind_rows(aset %>% select(PATID) %>%
              anti_join(sdoh_ruca_temp,by="PATID") %>%
              mutate(RUCA = "0")) %>%
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
  arrange(abs(ADDRESS_PERIOD_START_ALS1DX_DAYS)) %>% 
  slice(1:1) %>%  ungroup %>%
  filter(!is.na(OBSCOMM_RESULT_NUM)) %>%
  mutate(sdh_var=paste0("SDH_",OBSCOMM_CODE)) %>%
  select(PATID,sdh_var,OBSCOMM_RESULT_NUM) %>%
  spread(sdh_var,OBSCOMM_RESULT_NUM)
# remove invariant metrics
sdoh_nzv<- nearZeroVar(sdoh_cov, saveMetrics = TRUE)
sdoh_cov<-sdoh_cov[,row.names(sdoh_nzv)[!sdoh_nzv$zeroVar]]
# quick imputation
sdoh_cov_ruca<-sdoh_ruca %>%
  select(all_of(c("PATID",paste0("RUCA_",1:10)))) %>%
  left_join(sdoh_cov,by="PATID")
init<-mice(sdoh_cov_ruca, maxit=0) 
predM<-init$predictorMatrix
predM[,c("PATID")]=0
sdoh_cov_imputed<-mice(sdoh_cov_ruca, m=1) # default: pmm
sdoh_cov_imputed<-complete(sdoh_cov_imputed) %>%
  select(-all_of(paste0("RUCA_",1:10)))

# combine everything
aset<-aset %>%
  left_join(sdoh_ruca,by="PATID") %>%
  left_join(sdoh_cov_imputed,by="PATID") %>%
  # filter out negative event time
  filter(DEATH_time>0)

# fold index creation
set.seed(131)
sample_idx<-aset %>% 
  select(PATID) %>%
  mutate(fold10=sample.int(10,size=nrow(.),replace = T)) %>%
  mutate(fold5=sample.int(5,size=nrow(.),replace = T)) %>%
  mutate(holdout08=rbinom(nrow(.), 1, 0.8)) %>%
  mutate(holdout09=rbinom(nrow(.), 1, 0.9))

# save final analytic dataset
saveRDS(aset,file="./data/als_aset_mort.rds")
saveRDS(sample_idx,file="./data/als_aset_mort_part.rds")

##==== Data Dictionary ==============================================
phecd_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_phecode.rds")
ccspx_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_ccspx.rds")
med_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_med.rds")
acs_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_acs.rds")
var_lbl_df<-data.frame(var=c(paste0('PHECD_',phecd_dd$phecode),
                             paste0('SDH_',acs_dd$OBSCOMM_CODE),
                             med_dd$MED_CD,
                             ccspx_dd$PX_GRPCD),
                       varx=c(paste0('xxPHECD_',phecd_dd$phecode),
                              paste0('xxSDH_',acs_dd$OBSCOMM_CODE),
                              paste0('xx',med_dd$MED_CD),
                              paste0('xx',ccspx_dd$PX_GRPCD)),
                       var_lbl=c(phecd_dd$phecode_string,
                                 acs_dd$RAW_OBSCOMM_NAME,
                                 med_dd$RAW_RX_MED_NAME,
                                 ccspx_dd$PX_GRP),
                       stringsAsFactors = F) %>%
  filter(!grepl("\\.",var))
saveRDS(var_lbl_df,file="./data/als_aset_data_dict.rds")
