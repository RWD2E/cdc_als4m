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
tbl1<-readRDS("C:/repo/cdc_als4m/data/tbl1_sdoh_endpt_pce_prvdr.rds") %>%
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

var_lst2<-var_lst[!var_lst %in% c(
  "CASE_ASSERT",
  "als1dx"
)]
facvar_lst2<-facvar_lst[!facvar_lst %in% c(
  "CASE_ASSERT",
  "als1dx"
)]
case_ctrl<-univar_analysis_mixed(
  df = tbl1,
  id_col ="PATID",
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  grp = tbl1$CASE_ASSERT,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_all_by_assert.pdf")
  )

# confirmed
tbl2<-tbl1 %>% 
  filter(
    CASE_ASSERT == "confirmed"
  ) %>%
  mutate(INDEX_EVENT = case_when(
    INDEX_EVENT=="NEUROLOGIST" ~ 'DX', 
    TRUE ~ INDEX_EVENT
  ))

case_ctrl<-univar_analysis_mixed(
  df = tbl2,
  id_col ="PATID",
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_confirmed.pdf")
  )

case_ctrl<-univar_analysis_mixed(
  df = tbl2,
  id_col ="PATID",
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  grp = tbl2$COMPLT_FLAG,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_confirmed_by_xwalk.pdf")
  )

case_ctrl<-univar_analysis_mixed(
  df = tbl2,
  id_col ="PATID",
  var_lst = var_lst2,
  facvar_lst  = facvar_lst2,
  grp = tbl2$INDEX_EVENT,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_confirmed_by_index.pdf")
  )

# confirmed + crosswalk
tbl3<-tbl2 %>%
  filter(COMPLT_FLAG == 'complete')

var_lst3<-var_lst2[!var_lst2 %in% c(
  "COMPLT_FLAG"
)]
facvar_lst3<-facvar_lst2[!facvar_lst2 %in% c(
  "COMPLT_FLAG"
)]
case_ctrl<-univar_analysis_mixed(
  df = tbl3,
  id_col ="PATID",
  var_lst = var_lst3,
  facvar_lst  = facvar_lst3,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_complete.pdf")
  )

case_ctrl<-univar_analysis_mixed(
  df = tbl3,
  id_col ="PATID",
  var_lst = var_lst3,
  facvar_lst  = facvar_lst3,
  grp = tbl3$INDEX_EVENT,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_complete_by_index.pdf")
  )

case_ctrl<-univar_analysis_mixed(
  df = tbl3,
  id_col ="PATID",
  var_lst = var_lst3,
  facvar_lst  = facvar_lst3,
  grp = tbl3$fda,
  pretty = T,
  var_lbl_df=var_lbl_df
)
case_ctrl %>%
  save_kable(
    paste0("./res/als_complete_by_fda.pdf")
  )
