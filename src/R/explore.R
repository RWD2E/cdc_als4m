rm(list=ls()); gc()
pacman::p_load(
  tidyverse,
  magrittr,
  broom,
  survival,
  survminer,
  devtools,
  kableExtra
)

# webshot::install_phantomjs() # needed for save_kabel()
source_url("https://raw.githubusercontent.com/sxinger/utils/master/analysis_util.R")

# explore
tbl1<-readRDS("C:/repo/cdc_als4m/data/tbl1_cov_endpt_pce.rds") %>%
  mutate(
    surv_1yr = as.numeric((time_death_censor>=365)),
    surv_3yr = as.numeric((time_death_censor>=365*3)),
    surv_5yr = as.numeric((time_death_censor>=365*5))
  ) %>%
  filter(time_death_censor > 0) 

var_lst<-colnames(tbl1)[
  !colnames(tbl1) %in% c(
    "PATID",
    "ALS1DX_DATE",
    "BIRTH_DATE",
    "INDEX_DATE"
  )
]
numvar_lst<-var_lst[
  var_lst %in% c(
    "AGE_AT_INDEX",
    "censor"
  ) | 
  (grepl("^(SDH)+",var_lst)&!grepl("(LIS_DUAL|PART_D|PART_C)+",var_lst)) | 
  grepl("(CNT)+?",var_lst) | 
  grepl("^(time)+",var_lst)
]
facvar_lst<-var_lst[!var_lst %in% numvar_lst]
var_lbl_df<-readRDS("./data/data_dict.rds") %>%
  select(-varx)

case_ctrl<-univar_analysis_mixed(
  df = tbl1,
  id_col ="PATID",
  var_lst = var_lst,
  facvar_lst  = facvar_lst,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_all.pdf")
  )

case_ctrl<-univar_analysis_mixed(
  df = tbl1 %>% 
    filter(COMPLT_IND == 1 & CASE_ASSERT == 'confirmed'),
  id_col ="PATID",
  var_lst = var_lst,
  facvar_lst  = facvar_lst,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_xwalk.pdf")
  )

# crosswalk population & confirmed
tbl2<-tbl1 %>% 
  filter(
    COMPLT_IND == 1 & 
    CASE_ASSERT == 'confirmed' &
    !INDEX_EVENT %in% c('PRX','DRX')
  ) %>%
  mutate(INDEX_EVENT = case_when(
    INDEX_EVENT=="NEUROLOGIST" ~ 'DX', TRUE ~ INDEX_EVENT
  ))
var_lst2<-var_lst[!var_lst %in% c(
  "CASE_ASSERT",
  "COMPLT_IND",
  "als1dx"
)]
facvar_lst2<-facvar_lst[!facvar_lst %in% c(
  "CASE_ASSERT",
  "COMPLT_IND",
  "als1dx"
)]

# by index event
case_ctrl<-univar_analysis_mixed(
  df = tbl2,
  id_col ="PATID",
  grp = tbl2$SDH_PART_C,
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_xwalk_by_partc.pdf")
  )

case_ctrl<-univar_analysis_mixed(
  df = tbl2,
  id_col ="PATID",
  grp = tbl2$SDH_PART_D,
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_xwalk_by_partd.pdf")
  )

# by fda-approved rx use
case_ctrl<-univar_analysis_mixed(
  df = tbl2,
  id_col ="PATID",
  grp = tbl2$fda,
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_xwalk_by_fda.pdf")
  )

# by aot rx use
case_ctrl<-univar_analysis_mixed(
  df = tbl2,
  id_col ="PATID",
  grp = tbl2$aot,
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_xwalk_by_aot.pdf")
  )
