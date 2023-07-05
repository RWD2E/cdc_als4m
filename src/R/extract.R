# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: extract.R
# Description: pre-analytic data from snowflake to R
#################################################################

rm(list=ls()); gc()
setwd("C:/repo/GPC-Analytics-ALS-Cohort")

#=======================================================================
# install.packages("pacman")
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  magrittr,
  dbplyr
)

# make db connection
sf_conn <- DBI::dbConnect(drv = odbc::odbc(),
                          dsn = Sys.getenv("ODBC_DSN_NAME"),
                          uid = Sys.getenv("SNOWFLAKE_USER"),
                          pwd = Sys.getenv("SNOWFLAKE_PWD"))


#======= ALS cohort
# collect table1 (demographic, primary outcome)
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALS_CASE_DEMO")) %>% collect()
saveRDS(dat,file="./data/als_tbl1.rds")

# collect SDOH
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALS_CASE_SDOH")) %>% collect()
saveRDS(dat,file="./data/als_sdoh.rds")


#======= pre-ALS covariates
# collect historical diagnosis table
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_DIAGNOSIS_PHECD_BEFORE_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_pre_dx_phecd.rds")

# dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_DIAGNOSIS_CCS_BEFORE_ALS1DX")) %>% collect()
# saveRDS(dat,file="./.data/als_pre_dx_ccs.rds")

# collect diagnostic visits before ALS onset
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_ENCOUNTER_BEFORE_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_pre_vis.rds")


# collect dispensing medication info before ALS onset
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_DISPENSING_BEFORE_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_pre_rx.rds")


# collect procedures before ALS onset
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_PROCEDURES_BEFORE_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_pre_px.rds")


#======= post-ALS covariates
# collect care visits after ALS onset
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_ENCOUNTER_AFTER_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_post_vis.rds")


# collect medication info after ALS onset
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_DISPENSING_AFTER_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_post_rx.rds")


# collect diagnoses info after ALS onset
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_DIAGNOSIS_PHECD_AFTER_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_post_dx_phecd.rds")

# dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_DIAGNOSIS_CCS_AFTER_ALS1DX")) %>% collect()
# saveRDS(dat,file="./data/als_post_dx_ccs.rds")


# collect procedures after ALS onset
dat<-tbl(sf_conn,in_schema("ALS_GPC","ALL_PROCEDURES_AFTER_ALS1DX")) %>% collect()
saveRDS(dat,file="./data/als_post_px.rds")


#=======================================================================
#======= provider types
ptypes<-c(
  "NEUROLOGY",
  "NEUROPSYCH",
  "PHYSICAL_REHAB",
  "GASTROENTEROLOGY",
  "CARDIOLOGY",
  "NUTRITION"
)
for(ptype in ptypes){
  dat<-tbl(sf_conn,in_schema("ALS_GPC",paste0(ptype,"_CARE"))) %>% collect()
  saveRDS(dat,file=paste0("./data/als_care_",ptype,".rds"))
}

#=======================================================================
#======= data dictionary 
dat<-tbl(sf_conn,sql("select * from ONTOLOGY.GROUPER_VALUESETS.PHECODEX_REF")) %>% collect()
saveRDS(dat,file="./data/dd_phecode.rds")

acs_dd<-readRDS("./data/als_sdoh.rds") %>% select(OBSCOMM_CODE,RAW_OBSCOMM_NAME) %>% unique()
saveRDS(acs_dd,file="./data/dd_ace.rds")

med_dd<-readRDS("./data/als_pre_rx.rds") %>% select(RAW_RX_MED_NAME) %>%
  mutate(MED_CD= paste0("MED",as.numeric(as.factor(RAW_RX_MED_NAME)))) %>%
  bind_rows(readRDS("./data/als_post_rx.rds") %>% select(RAW_RX_MED_NAME) %>%
              mutate(MED_CD= paste0("MED",as.numeric(as.factor(RAW_RX_MED_NAME))))) %>%
  unique
saveRDS(med_dd,file="./data/dd_med.rds")

ccspx_dd<-readRDS("./data/als_pre_px.rds") %>%
  mutate(PX_GRPCD = paste0("CCSPX_",PX_GRPCD)) %>%
  select(PX_GRPCD,PX_GRP) %>% unique %>%
  bind_rows(readRDS("./data/als_post_px.rds") %>%
              mutate(PX_GRPCD = paste0("CCSPX_",PX_GRPCD)) %>%
              select(PX_GRPCD,PX_GRP) %>% unique)
saveRDS(ccspx_dd,file="./data/dd_ccspx.rds")


#==================================================================================
# disconnect
DBI::dbDisconnect(sf_conn)

