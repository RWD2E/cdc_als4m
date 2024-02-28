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

# make db connection
sf_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = Sys.getenv("ODBC_DSN_NAME"),
  uid = Sys.getenv("SNOWFLAKE_USER"),
  pwd = Sys.getenv("SNOWFLAKE_PWD")
)

#==== ALS cohort snapshot ====
# collect table1 (demographic, primary outcome)
dat<-tbl(sf_conn,in_schema("SX_ALS_GPC","ALS_TABLE1")) %>% collect()
saveRDS(dat,file="./data/als_tbl1.rds")

# collect SDOH
dat<-tbl(sf_conn,in_schema("SX_ALS_GPC","ALS_ALL_SDOH")) %>% collect()
saveRDS(dat,file="./data/als_sdoh.rds")

# collect ALS staging file
dat<-tbl(sf_conn,in_schema("SX_ALS_GPC","ALS_ENDPTS")) %>% collect()
saveRDS(dat,file="./data/als_endpts.rds")

# collect PCE 
dat<-tbl(sf_conn,
      sql("with cte_pid as (
            select PATID from SX_ALS_GPC.ALS_ALL_DRX where FDA_APPROVED is not null
            union
            select PATID from SX_ALS_GPC.ALS_ALL_PRX where FDA_APPROVED is not null
           )
           select distinct PATID from cte_pid")) %>% 
  collect() %>%
  mutate(fda = 1)
saveRDS(dat,file="./data/als_fda_rx.rds")

dat<-tbl(sf_conn,
         sql("with cte_pid as (
            select PATID, AOT_POSSIBLE from SX_ALS_GPC.ALS_ALL_DRX where AOT_POSSIBLE is not null
            union
            select PATID, AOT_POSSIBLE from SX_ALS_GPC.ALS_ALL_PRX where AOT_POSSIBLE is not null
           )
           select distinct PATID, AOT_POSSIBLE from cte_pid")) %>% 
  collect() %>%
  mutate(ind = 1) %>%
  spread(AOT_POSSIBLE, ind) %>%
  replace(is.na(.), 0) %>%
  mutate(aot = 1)
saveRDS(dat,file="./data/als_aot_rx.rds")

# collect provider specialty
dat<-tbl(sf_conn,
         sql("select distinct PATID, SPECIALTY_GROUP from SX_ALS_GPC.ALS_PX_PRVDR")) %>% 
  collect() %>%
  mutate(ind = 1) %>%
  spread(SPECIALTY_GROUP, ind) %>%
  replace(is.na(.), 0)
saveRDS(dat,file="./data/als_mdc_prvdr.rds")

# collect provider specialty
dat<-tbl(sf_conn,
         sql("select distinct PATID, 'MDC' AS MDC_PX_IND from SX_ALS_GPC.ALS_SEL_PX_MDC")) %>% 
  collect() %>%
  mutate(ind = 1) %>%
  spread(MDC_PX_IND, ind) %>%
  replace(is.na(.), 0)
saveRDS(dat,file="./data/als_mdc_px.rds")

#==== ALS time-varying dataset ====
deltat<-60
t_seq<-seq(0,5*365.25,by=deltat)
drop_rt<-0.01

#--- covariates
val_str<-function(...){
  dots<-list(...)
  return(paste0(unlist(dots),collapse = ","))
}
tbl_part_map<-tibble(
  tbl=as.character(),
  part_col=as.character(),
  val_cols=as.character(),
  cond = as.character()
  ) %>%
  add_row(
    tbl="ALS_ALL_PHECD",
    part_col="PHECD_DXGRPCD",
    cond="a.PHECD_DXGRPCD<>'00000'"
  ) %>%
  add_row(
    tbl="ALS_ALL_PX_CCS",
    part_col="CCS_PXGRPCD",
    cond="a.CCS_PXGRPCD<>'00000'"
  ) %>%
  add_row(
    tbl="ALS_ALL_RX_IN",
    part_col="INGREDIENT",
    cond="a.INGREDIENT not in ('riluzole','edaravone')"
  ) %>%
  add_row(
    tbl="ALS_SEL_SDOH",
    part_col="OBSCOMM_CODE",
    val_cols=val_str("OBSCOMM_RESULT")
  ) %>%
  add_row(
    tbl="ALS_ALL_RX_IN",
    part_col="FDA_AOT",
    val_cols=val_str("INGREDIENT"),
    cond="a.FDA_AOT <> 'ot'"
  ) %>%
  add_row(
    tbl="ALS_SEL_DEVICE",
    part_col="DEVICE"
  ) %>%
  add_row(
    tbl="ALS_PX_PRVDR",
    part_col="SPECIALTY_GROUP"
  ) %>%
  add_row(
    tbl="ALS_ENDPTS",
    part_col="ENDPT_SUB",
    cond="a.ENDPT_SUB in ('death','censor')"
  ) %>%
  add_row(
    tbl="ALS_SEL_OBS",
    part_col="OBS_NAME",
    val_cols = val_str("OBS_QUAL","OBS_NUM","OBS_UNIT","OBS_SRC","OBS_REF_LOW","OBS_REF_HIGH")
  )

k<-nrow(tbl_part_map)
cov_lst<-list()
for(i in 1:k){
  tbl_long<-c(); gc()
  if(!is.na(tbl_part_map$val_cols[i][[1]])){
    val_cols_str<-paste0(",",paste0(tbl_part_map$val_cols[i],collapse = ","))
  }else{
    val_cols_str<-""
  }
  if(!is.na(tbl_part_map$cond[i][[1]])){
    cond_str<-paste0(" and ",tbl_part_map$cond[i]," ")
  }else{
    cond_str<-""
  }
  
  for(t in t_seq){
    # extraction
    if(tbl_part_map$tbl[i]=="ALS_ENDPTS"){
      # positive carry-over
      dat_t<-tbl(
        sf_conn,
        sql(paste0(
          "with cte_ord as (
            select distinct a.*,",
          "   row_number() over (
                partition by a.patid, a.",tbl_part_map$part_col[i],
                " order by a.STAGE_SINCE_INDEX desc) as rn",
          " from SX_ALS_GPC.", tbl_part_map$tbl[i], " a",
          " where a.STAGE_SINCE_INDEX < ", t, cond_str,")",
          " select PATID, ",tbl_part_map$part_col[i],val_cols_str,",",t," as T_DAYS",
          " from cte_ord where rn = 1"
        ))
      ) %>% collect()
    
    }else if(tbl_part_map$part_col[i] %in% c("FDA_AOT","SPECIALTY_GROUP","CCS_PXGRPCD")){
      # no carry-over
      dat_t<-tbl(
        sf_conn,
        sql(paste0(
          "with cte_ord as (
            select distinct a.*,",
          "   row_number() over (
                partition by a.patid, a.",tbl_part_map$part_col[i],
          " order by DAYS_SINCE_INDEX desc) as rn",
          " from SX_ALS_GPC.", tbl_part_map$tbl[i], " a",
          " where a.DAYS_SINCE_INDEX < ", t, 
            " and a.DAYS_SINCE_INDEX >=", t-deltat, cond_str,")",
          " select PATID, ",tbl_part_map$part_col[i],val_cols_str,",",t," as T_DAYS",
          " from cte_ord where rn = 1"
        ))
      ) %>% collect()

    }else{
      # extraction
      # carry-over
      dat_t<-tbl(
        sf_conn,
        sql(paste0(
          "with cte_ord as (
            select distinct a.*,",
          "   row_number() over (
                partition by a.patid, a.",tbl_part_map$part_col[i],
          " order by DAYS_SINCE_INDEX desc) as rn",
          " from SX_ALS_GPC.", tbl_part_map$tbl[i], " a",
          " where DAYS_SINCE_INDEX < ", t, cond_str,")",
          " select PATID, ",tbl_part_map$part_col[i],val_cols_str,",",t," as T_DAYS",
          " from cte_ord where rn = 1"
        ))
      ) %>% collect()
      
      # low-frequency filter
      N<-length(unique(dat_t$PATID))
      dat_t %<>%
        semi_join(
          dat_t %>% 
            group_by_at(tbl_part_map$part_col[i]) %>%
            summarise(pt_cnt = length(unique(PATID)),.groups="drop") %>%
            filter(pt_cnt > N*drop_rt),
          by=tbl_part_map$part_col[i]
        )
    }
    # stack
    tbl_long %<>% bind_rows(dat_t)
  }
  # save table
  fn<-paste0(tolower(tbl_part_map$tbl[i]),"_",tolower(tbl_part_map$part_col[i]),"_",deltat,".rda")
  saveRDS(tbl_long,file=file.path('./data',fn))
  
  # report progress
  print(paste(
    "values from",
    tbl_part_map$tbl[i],":",tbl_part_map$part_col[i],
    "extracted."))
}

#==================================================================================
# disconnect
DBI::dbDisconnect(sf_conn)

