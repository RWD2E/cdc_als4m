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

# multi-state model
var_incld_msm<-c(
  'SEX'
  ,'RACE'
  ,'HISPANIC'
  ,'AGE_AT_INDEX'
  ,'RUCAregrp'
  ,"COMPLT_IND"
  ,"INDEX_EVENT"
  ,"CASE_ASSERT"
)
var_oc<-c(
  "als1dx","time_als1dx",
  "stg1","time_stg1",
  "stg2","time_stg2",
  "stg3","time_stg3",
  "stg4","time_stg4",
  "stg5","time_stg5",
  "death","time_death",
  "censor"
)
df_ind<-tbl1[,c(
  'PATID',var_incld_msm,var_oc)] %>%
  filter(CASE_ASSERT=="confirmed")

df_tmerge<-tmerge(
  data1 = df_ind,
  data2 = df_ind,
  id = PATID,
  death = event(time_death,death),
  # als1dx = event(time_als1dx),
  stg1 = event(time_stg1),
  stg2 = event(time_stg2),
  stg3 = event(time_stg3),
  stg4 = event(time_stg4),
  stg5 = event(time_stg5)
) %>%
  mutate(
    event = factor(
      # 1*als1dx + 2*stg1 + 3*stg2 + 4*stg3 + 5*stg4 + 6*death,
      # 0:6,c("censor","als1dx","stg1","stg2","stg3","stg4","death")
      1*stg1 + 2*stg2 + 3*stg3 + 4*stg4 + 5*stg4 + 6*death,
      0:6,c("censor","stg1","stg2","stg3","stg4","stg5","death")
    )
  )

df_tmerge_prvdr<-df_tmerge %>%
  inner_join(
    df_tmerge %>% select(PATID) %>%
      left_join(readRDS("./data/als_prvdr.rds"),by="PATID") %>%
      replace(is.na(.),0) %>% unique,
    by="PATID"
  )

chk<-survcheck(
  Surv(tstart,tstop,event) ~ 1,
  data = df_tmerge_prvdr,id=PATID
)
chk$transition
chk$flag

fit1<-survfit(
  Surv(tstart,tstop,event) ~ NEUROLOGY_IND,
  data = df_tmerge_prvdr,
  id = PATID
)
plot(fit1)

fit_cox<-coxph(
  Surv(tstart,tstop,event) ~ NEUROLOGY_IND + NP_IND + NEUROPSYCH_IND + CARDIO_IND + PULMONARY_IND + SEX + RACE + AGE_AT_INDEX,
  data = df_tmerge_prvdr,
  id = PATID
)
ans<-broom::tidy(fit_cox)
