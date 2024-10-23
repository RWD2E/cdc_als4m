rm(list=ls()); gc()
pacman::p_load(
  tidyverse,
  magrittr,
  broom,
  recipes,
  survival,
  survminer,
  devtools,
  kableExtra,
  xgboost,
  Matrix,
  ParBayesianOptimization
)

source_url("https://raw.githubusercontent.com/sxinger/utils/master/preproc_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/model_util.R")

var_encoder<-readRDS("./data/var_encoder.rda") %>%
  select(var,var2) %>% unique

##===== iptw-adjusted model ====
# load unadj outcome result
unadj<-readRDS("./data/unadj/tvm_OC_death.rda")

# load training data
trainY<-readRDS("./data/trainY_82.rda") %>%
  mutate(PATID2 = PATID) %>%
  unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
  arrange(PATID_T) %>% 
  # collect y
  filter(var == 'OC_death') %>%
  select(PATID2,PATID_T,val)

# ensure alignment
trainX<-trainY %>% 
  select(PATID_T) %>%
  inner_join(
    readRDS("./data/trainX_82.rda") %>% 
      unite("PATID_T",c("PATID","T_DAYS"),sep="_"),
    by="PATID_T",multiple = "all"
  )

# manual feature selection
trainX %<>%
  semi_join(
    var_encoder %>%
      filter(!grepl("^(CURR_)+",var)),
    by="var2"
  )

# transform to sparse matrix
trainX %<>%
  long_to_sparse_matrix(
    .,
    id = "PATID_T",
    variable = "var2",
    value = "val"
  )

# for dropping due to lack of X
trainY %<>% 
  inner_join(data.frame(PATID_T = row.names(trainX)),by="PATID_T")

# load testing data
testY<-readRDS("./data/testY_82.rda") %>%
  unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
  arrange(PATID_T) %>% 
  filter(var == 'OC_death') %>%
  select(PATID_T,val)

# ensure alignment
testX<-testY %>% select(PATID_T) %>% 
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

# generate iptw
ps_comm<-c(
  "TX_fda",
  "TX_aot",
  "TX_gastrostomy",
  "TX_non-invasive-ventilator",
  "TX_wheelchair"
)
ps_tgt<-c(
  "PRVDR_mdc",
  "PRVDR_NEURO_w4up",
  "PRVDR_psychiatry",
  "PRVDR_neurology",
  "PRVDR_home-health",
  "PRVDR_pcp",
  "PRVDR_eye",
  "PRVDR_surgery",
  "PRVDR_pt",
  "PRVDR_ent",
  "PRVDR_ot",
  "PRVDR_social", 
  # "PRVDR_genetic", 
  "PRVDR_pain",
  "PRVDR_dietition",
  "PRVDR_nurse",
  "PRVDR_urology",
  "PRVDR_respiratory",
  "PRVDR_palliative",
  "PRVDR_cardiology",
  "PRVDR_intv-radiology",
  "PRVDR_slp"
)

for(ps in c(ps_comm,ps_tgt)){
  # ps<-ps_comm[1] # uncomment for testing
  path_to_file<-file.path("./data/aipw",paste0("tvm_OC_death_",ps,".rda"))
  if(!file.exists(path_to_file)){
    # extract ps
    ps_df<-c()
    for(y_ps in c(ps)){
      ps_fit<-readRDS(paste0("./data/unadj/tvm_",y_ps,".rda"))
      ps_df %<>%
        bind_rows(
          ps_fit$fit_model$pred_ts %>% 
            mutate(PATID_T = id) %>%
            separate('id',c('PATID','T_DAYS'),sep='_') %>%
            mutate(T_DAYS = as.numeric(T_DAYS),tgt = y_ps)
        )
    }
    
    # estimate counterfactuals
    ps_idx<-var_encoder %>% filter(var==ps) %>% select(var2) %>% unlist()
    #intervene with 1
    testX[,ps_idx]<-1
    dtest<-xgb.DMatrix(data = testX,label = testY$val)
    y1_cf<-predict(unadj$fit_model$model,dtest,type="terms")
    #intervene with 0
    testX[,ps_idx]<-0
    dtest<-xgb.DMatrix(data = testX,label = testY$val)
    y0_cf<-predict(unadj$fit_model$model,dtest,type="terms")

    ite_df<-data.frame(
      PATID_T = row.names(testX),
      y = testY$val,
      y1_cf = y1_cf,
      y0_cf = y0_cf
    ) %>%
      left_join(
        ps_df %>%
          select(PATID_T,actual,pred,tgt) %>%
          rename(ps = pred, a = actual),
        by="PATID_T"
      ) %>%
      separate(PATID_T,c("PATID",'T_DAYS'),sep="_") %>%
      mutate(
        T_DAYS = as.numeric(T_DAYS),
        ITE = a*y/ps - (1-a)*y/(1-ps),
        ITE_aug = ((a*y)/ps - (1-a)*y/(1-ps))-(a-ps)/(ps*(1-ps))*((1-ps)*y1_cf+ps*y0_cf),
        ITE_aug_ub = quantile(ITE_aug,probs = 0.9,na.rm=T),
        ITE_aug_lb = quantile(ITE_aug,probs = 0.1,na.rm=T)
      ) %>%
      # clipping
      mutate(
        ITE_aug = pmin(pmax(ITE,ITE_aug_lb),ITE_aug_ub)
      )
    
    # result set
    rslt_set<-list(
      ite_df = ite_df
    )
    saveRDS(rslt_set,file = path_to_file)
    
    cat("finish iptw outcome modeling for intervention:",ps,".\n")
  }
}