rm(list=ls()); gc()
setwd("C:/repo/cdc_als4m")

# install.packages("pacman") 
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  magrittr,
  dbplyr
)

#==== load ====
# make db connection
sf_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = Sys.getenv("ODBC_DSN_NAME"),
  uid = Sys.getenv("SNOWFLAKE_USER"),
  pwd = Sys.getenv("SNOWFLAKE_PWD")
)

# load variables from different tables
phecd_dd<-tbl(sf_conn,sql(
  "select distinct DX, DX_TYPE, PHECD_DXGRPCD, PHECD_DXGRP 
   from GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_ALL_PHECD"
)) %>% collect()
# saveRDS(phecd_dd,file="./data/dd_phecode.rds")

px_ccs_dd<-tbl(sf_conn,sql(
  "select distinct PX, PX_TYPE, PX_GRPCD, PX_GRP 
   from GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_ALL_PX_CCS"
)) %>% collect()
# saveRDS(px_ccs_dd,file="./data/dd_ccspx.rds")

obs_dd<-tbl(sf_conn,sql(
  "select distinct OBS_CODE_TYPE, OBS_CODE, OBS_NAME, OBS_SRC
   from GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_ALL_OBS
   where OBS_CODE is not null and trim(OBS_CODE) <> ''"
)) %>% collect()
# saveRDS(obs_dd,file="./data/dd_obs.rds")

acs_dd<-readRDS("./data/als_sdoh.rds") %>% 
  select(OBSCOMM_CODE,RAW_OBSCOMM_NAME) %>% 
  unique()
# saveRDS(acs_dd,file="./data/dd_ace.rds")

var_lbl_df<-data.frame(
  var=c(
     paste0('PHECD_',phecd_dd$phecode)
    ,paste0('SDH_',acs_dd$OBSCOMM_CODE)
    ,paste0()
    ,med_dd$MED_CD
    ,ccspx_dd$PX_GRPCD
  ),
  var_lbl=c(
    phecd_dd$phecode_string
    ,acs_dd$RAW_OBSCOMM_NAME
    ,med_dd$RAW_RX_MED_NAME
    ,ccspx_dd$PX_GRP
  ),
  stringsAsFactors = F
) %>%
  filter(!grepl("\\.",var))

# disconnect
DBI::dbDisconnect(sf_conn)

#==== tabulate ====
phecd_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_phecode.rds")
med_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_med.rds")
acs_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_acs.rds")
ccspx_dd<-readRDS("C:/repo/GPC-Analytics-ALS-Cohort/data/dd_ccspx.rds")
var_lbl_df<-data.frame(
  var=c(
    paste0('PHECD_',phecd_dd$phecode)
    ,paste0('SDH_',acs_dd$OBSCOMM_CODE)
    ,med_dd$MED_CD
    ,ccspx_dd$PX_GRPCD
  ),
  var_lbl=c(
    phecd_dd$phecode_string
    ,acs_dd$RAW_OBSCOMM_NAME
    ,med_dd$RAW_RX_MED_NAME
    ,ccspx_dd$PX_GRP
  ),
  stringsAsFactors = F
) %>%
  filter(!grepl("\\.",var))
saveRDS(var_lbl_df,file="./data/data_dict.rds")