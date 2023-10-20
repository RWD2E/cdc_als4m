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
)) %>% collect() %>% unique()
saveRDS(phecd_dd,file="./data/dd_phecode.rds")
rm(phecd_dd); gc()

px_ccs_dd<-tbl(sf_conn,sql(
  "select distinct PX, PX_TYPE, CCS_PXGRPCD, CCS_PXGRP 
   from GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_ALL_PX_CCS"
)) %>% collect() %>% unique()
saveRDS(px_ccs_dd,file="./data/dd_ccspx.rds")
rm(px_ccs_dd); gc()

obs_dd<-tbl(sf_conn,sql(
  "select distinct OBS_CODE_TYPE, OBS_CODE, OBS_NAME, OBS_SRC
   from GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_ALL_OBS
   where OBS_CODE is not null and trim(OBS_CODE) <> ''"
)) %>% collect() %>% unique()
saveRDS(obs_dd,file="./data/dd_obs.rds")
rm(obs_dd); gc()

sdh_dd<-tbl(sf_conn,sql(
  "select distinct OBSCOMM_CODE, RAW_OBSCOMM_NAME
   from GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_ALL_SDOH"
)) %>% collect() %>% unique()
saveRDS(sdh_dd,file="./data/dd_sdh.rds")
rm(acs_dd); gc()

# disconnect
DBI::dbDisconnect(sf_conn)

#==== tabulate ====
phecd_dd<-readRDS("./data/dd_phecode.rds")
pxccs_dd<-readRDS("./data/dd_ccspx.rds")
sdh_dd<-readRDS("./data/dd_sdh.rds")
var_lbl_df<-data.frame(
  var=c(
     paste0('PHECD_',phecd_dd$PHECD_DXGRPCD)
    ,paste0('PAST_SDH_',sdh_dd$OBSCOMM_CODE)
    ,paste0('CURR_SDH_',sdh_dd$OBSCOMM_CODE)
    ,paste0('PXCCS_',pxccs_dd$CCS_PXGRPCD)
  ),
  var_lbl=c(
     phecd_dd$PHECD_DXGRP
    ,sdh_dd$RAW_OBSCOMM_NAME
    ,sdh_dd$RAW_OBSCOMM_NAME
    ,pxccs_dd$CCS_PXGRP
  ),
  stringsAsFactors = F
) %>% unique()
saveRDS(var_lbl_df,file="./data/data_dict.rds")
