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

explainer %<>%
  rename(var2 = var) %>%
  inner_join(readRDS("./data/var_encoder.rda"),by="var2") %>%
  mutate(exp_effect = exp(effect)) %>%
  filter(var2 != 'v842')

ggplot(
  explainer,
  aes(x=val,y=exp_effect,color=cond)
) + 
  geom_point(aes(fill=cond))+
  geom_smooth()+
  facet_wrap(~ var,ncol=5,scales ="free")
