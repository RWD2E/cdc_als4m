rm(list=ls()); gc()
pacman::p_load(
  tidyverse,
  magrittr,
  broom,
  survival,
  survminer,
  devtools,
  kableExtra,
  xgboost,
  Matrix,
  ParBayesianOptimization,
  plotly
)

deltat<-60

source_url("https://raw.githubusercontent.com/sxinger/utils/master/preproc_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/model_util.R")

# outcomes of interests
y_lst<-c(
  "TX_aot",
  "TX_fda",
  "TX_gastrostomy",
  "TX_non-invasive-ventilator",
  "TX_power-wheelchairs",
  "PRVDR_neurology",
  "PRVDR_nurse",
  "PRVDR_pain",
  "PRVDR_pcp",
  "PRVDR_psychiatry",
  "PRVDR_pt_ot",
  "OC_death",
  "OC_censor"
)

rslt_set<-list()
# y_str<-"OC_death"
for(y_str in y_lst){
  # load training data
  trainY<-readRDS("./data/trainY_82.rda") %>%
    mutate(PATID2 = PATID) %>%
    unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
    arrange(PATID_T) %>% 
    filter(var == y_str) %>%
    select(PATID2,PATID_T,val)

  trainX<-trainY %>% select(PATID_T) %>% # ensure alignment
    inner_join(
      readRDS("./data/trainX_82.rda") %>% 
        unite("PATID_T",c("PATID","T_DAYS"),sep="_"),
      by="PATID_T",multiple = "all"
    ) %>%
    long_to_sparse_matrix(
      .,
      id = "PATID_T",
      variable = "var2",
      value = "val"
    )
  # for dropping due to lack of X
  trainY %<>% 
    inner_join(data.frame(PATID_T = row.names(trainX)),by="PATID_T")
  
  # customize folds (so same patient remain in the same fold)
  folds<-list()
  for(fold in 1:5){
    fold_lst<-readRDS("./data/part_idx.rda") %>%
      filter(hdout82==0&cv5==fold) %>%
      select(PATID) %>%
      left_join(
        trainY %>% 
          select(PATID2) %>% rowid_to_column(),
        by = c("PATID" = "PATID2"),multiple = "all"
      ) %>%
      select(rowid)
    folds[[fold]]<-fold_lst$rowid
  }
  
  # load testing data
  testY<-readRDS("./data/testY_82.rda") %>%
    unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
    arrange(PATID_T) %>% 
    filter(var == y_str) %>%
    select(PATID_T,val)
  
  testX<-testY %>% select(PATID_T) %>% # ensure alignment
    inner_join(
      readRDS("./data/testX_82.rda") %>% 
        unite("PATID_T",c("PATID","T_DAYS"),sep="_"),
      by="PATID_T",multiple = "all"
    ) %>%
    long_to_sparse_matrix(
      .,
      id = "PATID_T",
      variable = "var2",
      value = "val"
    )
  # for dropping due to lack of X
  testY %<>% 
    inner_join(data.frame(PATID_T = row.names(testX)),by="PATID_T")
  
  # align feature sets
  shared<-colnames(trainX)[colnames(trainX) %in% colnames(testX)]
  trainX<-trainX[,shared]
  testX<-testX[,shared]
  
  # convert to DMatrix
  dtrain<-xgb.DMatrix(data = trainX,label = trainY$val)
  dtest<-xgb.DMatrix(data = testX,label = testY$val)
  
  # rapid xgb
  xgb_rslt<-prune_xgb(
    dtrain = dtrain,
    dtest = dtest,
    folds = folds,
    params=list(
      booster = "gbtree",
      max_depth = 10,
      min_child_weight = 2,
      colsample_bytree = 0.8,
      subsample = 0.7,
      eta = 0.05,
      lambda = 1,
      alpha = 0,
      gamma = 1,
      objective = "binary:logistic",
      eval_metric = "auc"
    )
  )
  
  # shap explainer
  explainer<-explain_model(
    X = trainX,
    y = trainY$val,
    xgb_rslt = xgb_rslt,
    top_k = 30,
    boots = 10,
    nns = 30,
    shap_cond = "v842" # time index
  )
  
  # result set
  rslt_set<-list(
    fit_model = xgb_rslt,
    explain_model = explainer
  )
  saveRDS(
    rslt_set,
    file=file.path("./data",paste0("tvm_",y_str,".rda"))
  )
  
  cat("finish modeling for:",y_str,".\n")
}

pr<-c(
  "PRVDR_neurology",
  "PRVDR_nurse",
  "PRVDR_pain",
  "PRVDR_pcp",
  "PRVDR_psychiatry",
  "PRVDR_pt_ot"
)

dd<-readRDS("./data/data_dict.rds")

rslt<-readRDS("./data/tvm_OC_death.rda")

explainer<-rslt$explain_model %>%
  rename(var2 = var) %>%
  inner_join(readRDS("./data/var_encoder.rda"),by="var2") %>%
  group_by(val,cond,var,var2) %>%
  summarise(eff_m = median(effect,na.rm=T),
            eff_l = quantile(effect,probs = 0.025,na.rm=T),
            eff_u = quantile(effect,probs = 0.975,na.rm=T),
            .groups = "drop") %>%
  left_join(readRDS("./data/data_dict.rds"),by="var") %>%
  mutate(exp_eff_m = exp(eff_m),
         exp_eff_l = exp(eff_l),
         exp_eff_u = exp(eff_u)) %>%
  filter(var2 != 'v842') %>%
  mutate(var_lbl = coalesce(var_lbl,var))

ggplot(
  explainer %>% 
    filter(grepl("^(TX|PRvDR|RX)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val))
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=3,scales ="free")


ggplot(
  explainer %>% 
    filter(!grepl("^(TX|PRvDR|RX|SDH|AGE|HISP)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val))
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=4,scales ="free")


ggplot(
  explainer %>% 
    filter(grepl("^(SDH|HISP)+",var) & !grepl("^(SDH_ADI)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val))
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=3,scales ="free")


ggplot(
  explainer %>% 
    filter(grepl("^(SDH_ADI)+",var) & grepl("(NAT)+",var) & cond > 0),
  aes(x=val,y=exp_eff_m,color = val)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  labs(x = "ADI|NATRANK", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ cond,ncol=6,scales ="free")


ggplot(
  explainer %>% 
    filter(grepl("^(SDH_ADI)+",var) & grepl("(STAT)+",var) & cond > 0),
  aes(x=val,y=exp_eff_m,color = val)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  labs(x = "ADI|STATERANK", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ cond,ncol=6,scales ="free")

ggplot(
  explainer %>% 
    filter(grepl("^(AGE)+",var) & cond > 0),
  aes(x=val,y=exp_eff_m,color = val)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  labs(x = "Age at Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ cond,ncol=6,scales ="free")
