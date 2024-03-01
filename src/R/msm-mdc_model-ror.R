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
  ParBayesianOptimization,
  plotly
)

deltat<-60

source_url("https://raw.githubusercontent.com/sxinger/utils/master/preproc_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/model_util.R")

var_encoder<-readRDS("./data/var_encoder.rda") %>%
  select(var,var2) %>% unique

##===== tmle-adjusted model ====
# load unadj outcome result
unadj<-readRDS("./data/unadj/tvm_OC_death.rda")

# load training data
trainY<-readRDS("./data/trainY_82.rda") %>%
  mutate(PATID2 = PATID,T_DAYS2 = T_DAYS) %>%
  unite("PATID_T",c("PATID2","T_DAYS2"),sep="_") %>%
  arrange(PATID_T) %>% 
  # collect y
  filter(var == 'OC_death') %>%
  select(PATID,T_DAYS,PATID_T,val) %>%
  inner_join(
    unadj$fit_model$pred_tr %>% select(id, pred),
    by=c("PATID_T"="id"),multiple = "all"
  )

# load testing data
testY<-readRDS("./data/testY_82.rda") %>%
  mutate(PATID2 = PATID,T_DAYS2 = T_DAYS) %>%
  unite("PATID_T",c("PATID2","T_DAYS2"),sep="_") %>%
  arrange(PATID_T) %>% 
  filter(var == 'OC_death') %>%
  select(PATID_T,PATID,T_DAYS,val) %>%
  inner_join(
    unadj$fit_model$pred_ts %>% select(id, pred),
    by=c("PATID_T"="id"),multiple = "all"
  )

# customize folds (so same patient remain in the same fold)
folds<-list()
for(fold in 1:5){
  fold_lst<-readRDS("./data/part_idx.rda") %>%
    filter(hdout82==0&cv5==fold) %>%
    select(PATID) %>%
    left_join(
      trainY %>% 
        select(PATID) %>% rowid_to_column(),
      by = "PATID",multiple = "all"
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
  # "PRVDR_genetic", -- <1%
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
  # ps<-ps_tgt[1] #uncomment for testing
  path_to_file<-file.path("./data/tmle",paste0("tvm_OC_death_",ps,".rda"))
  if(!file.exists(path_to_file)){
    # calculate iptw
    wt_long<-c()
    for(y_ps in c(ps)){
      ps_fit<-readRDS(paste0("./data/unadj/tvm_",y_ps,".rda"))
      wt_long %<>%
        bind_rows(
          ps_fit$fit_model$pred_tr %>% 
            separate('id',c('PATID','T_DAYS'),sep='_') %>%
            bind_rows(
              ps_fit$fit_model$pred_ts %>% 
                separate('id',c('PATID','T_DAYS'),sep='_')) %>%
            mutate(T_DAYS = as.numeric(T_DAYS),tgt = y_ps) %>%
            rename(wt_den = pred)
        )
    }
    
    # calculate wt stabilizer
    wt_long %<>%
      group_by(T_DAYS,tgt,prev) %>%
      mutate(wt_num_marginal = sum(wt_den)) %>%
      group_by(T_DAYS,tgt,actual,prev) %>%
      mutate(wt_num_joint = sum(wt_den)) %>%
      ungroup %>%
      mutate(wt_num = wt_num_joint/wt_num_marginal) %>%
      mutate(wt_den = case_when(
        actual==0 ~ 1 - wt_den,
        TRUE ~ wt_den)
      )
    
    # calculate time-varying iptw
    iptw_df<-ipw.naive(
      wt_long = wt_long, 
      id_col = 'PATID', 
      time_col = 'T_DAYS', 
      wt_den_col = 'wt_den',
      wt_num_col = 'wt_num',
      ot_cols = c(),
      truncate = TRUE,
      truncate_lower = 0.01,
      truncate_upper = 0.95
    ) %>% 
      inner_join(
        wt_long %>% filter(tgt == ps) %>%
          select(PATID,T_DAYS,actual),
        by=c("PATID","T_DAYS")
      )
    
    # create covariate matrix with the "clever covariate" and time index
    all_df<-trainY %>%
      bind_rows(testY) %>%
      left_join(
        iptw_df %>%
          unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
          select(PATID_T,iptw,actual),
        by="PATID_T") %>%
      mutate(
        res_ps = actual - 1/,
        res_oc = val - pred
      )
    
    fit_logit<-function(df){
      glm(val ~ res_ps + pred,offset=offset,family="binomial",data=df)
    }
    fit_str<-all_df %>%
      select(T_DAYS,val,h,offset) %>%
      group_by(T_DAYS) %>%
      nest() %>%
      mutate(
        fit=map(data,fit_logit),
        out=map(fit,tidy)
      ) %>%
      unnest(out) %>%
      select(T_DAYS,term,estimate) %>%
      pivot_wider(
        names_from = term,
        values_from = estimate
      ) %>%
      rename(
        b0 = `(Intercept)`,
        b1 = h
      )
    
    ite_df<-all_df %>%
      semi_join(testY,by="PATID_T") %>%
      mutate(
        h = actual/iptw - (1-actual)/(1-iptw),
        offset = log(pred/(1-pred))
      ) %>%
      left_join(fit_tr,by="T_DAYS") %>%
      mutate(
        logit_ystar = offset + b0 + b1*h,
        ystar = exp(logit_ystar)/(1+exp(logit_ystar))
      ) 
    
    # fit <- glmer(
    #   val ~ (1+h|PATID),
    #   family = binomial,
    #   data = train_df,
    #   offset = pred,
    #   control = glmerControl(optimizer = "bobyqa")
    # )
    # summary(fit)
    
    # create covariate matrix with the "clever covariate" and time index
    # trainX<-trainY %>% 
    #   select(PATID_T,PATID,T_DAYS) %>%
    #   left_join(
    #     iptw_df %>%
    #       unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
    #       select(PATID_T,iptw,actual),
    #     by="PATID_T") %>%
    #   mutate(
    #     h = actual/iptw - (1-actual)/(1-iptw)
    #   ) %>% 
    #   select(-PATID,-actual,-iptw)
    # 
    # testX<-testY %>% 
    #     select(PATID_T,PATID,T_DAYS) %>%
    #     left_join(
    #       iptw_df %>%
    #         unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
    #         select(PATID_T,iptw,actual),
    #       by="PATID_T") %>%
    #   mutate(
    #     h = actual/iptw - (1-actual)/(1-iptw)
    #   ) %>% 
    #   select(-PATID,-actual,-iptw)
    # 
    # # convert to DMatrix
    # trainX<-as.matrix(trainX %>% select(-PATID_T)); gc()
    # dtrain<-xgb.DMatrix(data = trainX,label = trainY$val)
    # attr(dtrain,'id')<-trainY$PATID_T # assume the order doesn't change
    # setinfo(dtrain,'base_margin',trainY$pred)
    # testX<-as.matrix(testX %>% select(-PATID_T,-T_DAYS)); gc()
    # dtest<-xgb.DMatrix(data = testX,label = testY$val)
    # attr(dtest,'id')<-testY$PATID_T # assume the order doesn't change
    # setinfo(dtest,'base_margin',testY$pred)
    # 
    # # rapid xgb - only tune the number of trees
    # xgb_rslt<-prune_xgb(
    #   # dtrain, dtest are required to have attr:'id'
    #   dtrain = dtrain,
    #   dtest = dtest,
    #   folds = folds,
    #   params=list(
    #     booster = "gbtree",
    #     max_depth = 10,
    #     min_child_weight = 2,
    #     colsample_bytree = 0.8,
    #     subsample = 0.7,
    #     eta = 0.05,
    #     lambda = 1,
    #     alpha = 0,
    #     gamma = 1,
    #     objective = "binary:logistic",
    #     eval_metric = "auc"
    #   )
    # )
    
    # result set
    rslt_set<-list(
      fit_model = fit_str_logit,
      ite_df = ite_df
    )
    saveRDS(rslt_set,file = path_to_file)
    
    cat("finish tmle outcome modeling for intervention:",ps,".\n")
  }
}