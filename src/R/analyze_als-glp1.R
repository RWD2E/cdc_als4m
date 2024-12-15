rm(list=ls()); gc()
setwd("C:/repo/cdc_als4m")

# install.packages("pacman")
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  broom,
  magrittr,
  dbplyr,
  devtools,
  kableExtra,
  survival,
  survminer,
  ggrepel
)

source_url("https://raw.githubusercontent.com/sxinger/utils/master/analysis_util.R")

path_to_data<-"C:/repo/cdc_als4m/data"
path_to_res<-"C:/repo/cdc_als4m/res"

#==== load data ====
# make db connection
sf_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = Sys.getenv("ODBC_DSN_NAME"),
  uid = Sys.getenv("SNOWFLAKE_USER"),
  pwd = Sys.getenv("SNOWFLAKE_PWD")
)

dat<-tbl(sf_conn,in_schema("SX_ALS_GPC","ALS_GLP1_TRT_CTRL")) %>% 
  filter(COMPLT_FLAG=="complete"&CASE_ASSERT=='confirmed') %>%
  mutate(
    GLP1_USE_IND = case_when(GLP1_USE_GRP %in% c('aft','bef')~1,TRUE~0),
    SURV_1YR = case_when(STATUS==0|DAYS_ALS1_TO_DEATH>365~1,TRUE~0),
    SURV_3YR = case_when(STATUS==0|DAYS_ALS1_TO_DEATH>365*3~1,TRUE~0),
    SURV_5YR = case_when(STATUS==0|DAYS_ALS1_TO_DEATH>365*5~1,TRUE~0),
    SURV_10YR = case_when(STATUS==0|DAYS_ALS1_TO_DEATH>365*10~1,TRUE~0)
  ) %>%
  collect()
saveRDS(dat,file=file.path(path_to_data,"als_glp1_cohort.rds"))

var_lst<-c(
  "SEX",
  "RACE",
  "HISPANIC",
  "INDEX_EVENT",
  "AGE_AT_INDEX",
  "COMPLT_FLAG",
  "CASE_ASSERT",
  "RX_CLS",
  "RX_IN",
  "RX_START_YEAR",
  "DAYS_GLP1_TO_ALS1",
  "GLP1_USE_GRP",
  "GLP1_USE_IND",
  "BMI_AT_ALS1",
  "BMIGRP_AT_ALS1",
  "DM_IND",
  "DAYS_ALS1_TO_DM",
  "DAYS_ALS1_TO_METFM_F",
  "DAYS_ALS1_TO_METFM_L",
  "METFM_IND",
  "GLUC_AT_ALS1",
  "GLUC_IND",
  "HBA1C_AT_ALS1",
  "HBA1C_IND",
  "CHOL_AT_ALS1",
  "CHOL_IND",
  "HDL_AT_ALS1",
  "HDL_IND",
  "LDL_AT_ALS1",
  "LDL_IND",
  "TRIG_AT_ALS1",
  "TRIG_IND",
  "DAYS_ALS1_TO_CENSOR",
  "DAYS_ALS1_TO_DEATH",
  "DAYS_ALS1_TO_END",
  "STATUS",
  "SURV_1YR",
  "SURV_3YR",
  "SURV_5YR",
  "SURV_10YR"
)
numvar_lst<-c(
  "AGE_AT_INDEX",
  "BMI_AT_ALS1",
  "DAYS_ALS1_TO_DM",
  "DAYS_ALS1_TO_METFM_F",
  "DAYS_ALS1_TO_METFM_L",
  "GLUC_AT_ALS1",
  "HBA1C_AT_ALS1",
  "CHOL_AT_ALS1",
  "HDL_AT_ALS1",
  "LDL_AT_ALS1",
  "TRIG_AT_ALS1",
  "DAYS_GLP1_TO_ALS1",
  "DAYS_ALS1_TO_CENSOR",
  "DAYS_ALS1_TO_DEATH",
  "DAYS_ALS1_TO_END"
)
facvar_lst<-var_lst[!var_lst %in% numvar_lst]

overview<-univar_analysis_mixed(
  df = dat,
  id_col ="PATID",
  var_lst = var_lst,
  facvar_lst  = facvar_lst,
  pretty = T
)
overview %>%
  save_kable(
    file.path(path_to_res,'als-glp1_overview.html')
  )

#==== prelim clean-up ====
dat<-readRDS(file.path(path_to_data,"als_glp1_cohort.rds"))
var_lst<-c(
  "SEX",
  "RACE",
  "HISPANIC",
  "INDEX_EVENT",
  "AGE_AT_INDEX",
  "BMI_AT_ALS1",
  "BMIGRP_AT_ALS1",
  "DM_IND",
  "DAYS_ALS1_TO_DM",
  "DAYS_ALS1_TO_METFM_F",
  "DAYS_ALS1_TO_METFM_L",
  "METFM_IND",
  "GLUC_AT_ALS1",
  "GLUC_IND",
  "HBA1C_AT_ALS1",
  "HBA1C_IND",
  "CHOL_AT_ALS1",
  "CHOL_IND",
  "HDL_AT_ALS1",
  "HDL_IND",
  "LDL_AT_ALS1",
  "LDL_IND",
  "TRIG_AT_ALS1",
  "TRIG_IND",
  "DAYS_ALS1_TO_CENSOR",
  "DAYS_ALS1_TO_DEATH",
  "DAYS_ALS1_TO_END",
  "STATUS",
  "SURV_1YR",
  "SURV_3YR",
  "SURV_5YR",
  "SURV_10YR"
)
numvar_lst<-c(
  "AGE_AT_INDEX",
  "BMI_AT_ALS1",
  "DAYS_ALS1_TO_DM",
  "DAYS_ALS1_TO_METFM_F",
  "DAYS_ALS1_TO_METFM_L",
  "GLUC_AT_ALS1",
  "HBA1C_AT_ALS1",
  "CHOL_AT_ALS1",
  "HDL_AT_ALS1",
  "LDL_AT_ALS1",
  "TRIG_AT_ALS1",
  "DAYS_ALS1_TO_CENSOR",
  "DAYS_ALS1_TO_DEATH",
  "DAYS_ALS1_TO_END"
)
facvar_lst<-var_lst[!var_lst %in% numvar_lst]
str_glp1<-univar_analysis_mixed(
  df = dat,
  id_col ="PATID",
  var_lst = var_lst,
  facvar_lst  = facvar_lst,
  grp = dat$GLP1_USE_IND,
  pretty = T
)
str_glp1 %>%
  save_kable(
    file.path(path_to_res,'als-glp1_stratified.html')
  )


dat2<-dat %>% filter(GLP1_USE_IND==1)
str_glp1_ByCls<-univar_analysis_mixed(
  df = dat2,
  id_col ="PATID",
  var_lst = var_lst,
  facvar_lst  = facvar_lst,
  grp = dat2$RX_CLS,
  pretty = T
)
str_glp1_ByCls %>%
  save_kable(
    file.path(path_to_res,'als-glp1-by-cls_stratified.html')
  )

# KM
sfit_obj<-survfit(Surv(DAYS_ALS1_TO_END,STATUS) ~ GLP1_USE_IND, data = dat)
risk_tbl<-summary(sfit_obj,times = 365*c(0:5))
km_mort_unadj<-ggsurvplot(
  fit = sfit_obj,
  pval = TRUE,
  conf.int = TRUE,
  # legend.labs=c("wo/ GLP1", "w/ GLP1"),
  # risk.table = TRUE,
  linetype = "strata",
  break.x.by = 365,
  xlim = c(0, 1825),
  xlab = "Days since Index Date", 
  ylab = "Unadjusted Survival Probability"
)
km_mort_unadj

# rapid coxph
var_lst<-c(
  "SEX",
  "RACE",
  "HISPANIC",
  "AGE_AT_INDEX",
  "BMI_AT_ALS1",
  "DM_IND",
  "METFM_IND",
  # "GLUC_AT_ALS1",
  "GLUC_IND",
  # "HBA1C_AT_ALS1",
  "HBA1C_IND",
  # "CHOL_AT_ALS1",
  "CHOL_IND",
  # "HDL_AT_ALS1",
  "HDL_IND",
  # "LDL_AT_ALS1",
  "LDL_IND",
  # "TRIG_AT_ALS1",
  "TRIG_IND"
)
fit_main<-formula(
  paste0(
    "Surv(DAYS_ALS1_TO_END,STATUS) ~ ",
    paste(c(var_lst,"GLP1_USE_IND"),collapse = "+")
  )
)
fit_cox<-coxph(
  fit_main,
  data = dat, 
  # weights = df_iptw$iptw,
  model=TRUE
)
# ggforest(fit_cox)
print(fit_cox)


# iptw-adjusted
x<-data.matrix(df_adj[,var_all])
y<-df_adj$CPAP_IND
fit_ps<-cv.glmnet(x=x,y=y,family="binomial")
# plot(fit_ps)
ps_cpap<-predict(
  fit_ps, 
  newx = x, 
  s = "lambda.min", 
  type="response"
)
wt_long<-data.frame(
  PATID=df_adj$PATID,
  time=1,
  wt_num=1,
  p=ps_cpap[,1],
  tgt=y
) %>%
  mutate(
    wt_den = tgt*p + (1-tgt)*(1-p)
  )
ipw_df<-ipw.naive(
  wt_long = wt_long,
  id_col = 'PATID',
  ot_cols = 'tgt',
  truncate = TRUE,
  truncate_lower = 0.0001,
  truncate_upper = 0.99
) %>% ungroup


