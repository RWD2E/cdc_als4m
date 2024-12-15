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

source_url("https://raw.githubusercontent.com/sxinger/utils/master/preproc_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/model_util.R")

deltat<-120

var_encoder<-readRDS(file.path(
  "./data",paste0("t_",deltat,"d"),
  "var_encoder.rda")
) %>%
  select(var,var2) %>% unique

##===== tmle-adjusted model ====
# load unadj outcome result
unadj<-readRDS(file.path(
  "./data",paste0("t_",deltat,"d"),
  "unadj/tvm_OC_death.rda"
))

# load training data
trainY<-readRDS(file.path(
  "./data",paste0("t_",deltat,"d"),
  "trainY_82.rda"
)) %>%
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

# ensure alignment
trainX<-trainY %>% 
  select(PATID_T) %>%
  inner_join(
    readRDS(file.path(
      "./data",paste0("t_",deltat,"d"),
      "trainX_82.rda"
    )) %>% 
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

# load testing data
testY<-readRDS(file.path(
  "./data",paste0("t_",deltat,"d"),
  "testY_82.rda"
)) %>%
  mutate(PATID2 = PATID,T_DAYS2 = T_DAYS) %>%
  unite("PATID_T",c("PATID2","T_DAYS2"),sep="_") %>%
  arrange(PATID_T) %>% 
  filter(var == 'OC_death') %>%
  select(PATID_T,PATID,T_DAYS,val) %>%
  inner_join(
    unadj$fit_model$pred_ts %>% select(id, pred),
    by=c("PATID_T"="id"),multiple = "all"
  )

# ensure alignment
testX<-testY %>% select(PATID_T) %>% 
  inner_join(
    readRDS(file.path(
      "./data",paste0("t_",deltat,"d"),
      "testX_82.rda"
    )) %>% 
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
  fold_lst<-readRDS(file.path(
    "./data",paste0("t_",deltat,"d"),
    "part_idx.rda"
  )) %>%
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
  # ps<-ps_comm[1] #uncomment for testing
  path_to_file<-file.path(
    "./data",paste0("t_",deltat,"d"),"tmle",
    paste0("tvm_OC_death_",ps,".rda")
  )
  if(!file.exists(path_to_file)){
    # extract ps
    ps_df<-c()
    for(y_ps in c(ps)){
      ps_fit<-readRDS(file.path(
        "./data",paste0("t_",deltat,"d"),"unadj",
        paste0("tvm_",y_ps,".rda")
      ))
      ps_df %<>%
        bind_rows(
          ps_fit$fit_model$pred_tr %>% 
            separate('id',c('PATID','T_DAYS'),sep='_') %>%
            bind_rows(
              ps_fit$fit_model$pred_ts %>% 
                separate('id',c('PATID','T_DAYS'),sep='_')) %>%
            mutate(T_DAYS = as.numeric(T_DAYS),tgt = y_ps) %>%
            mutate(
              h = actual/pred - (1-actual)/(1-pred),
              h1 = 1/pred,
              h0 = -1/(1-pred),
              wt_den = pred
            )
        )
    }
    
    # calculate wt stabilizer
    ps_df %<>%
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
      wt_long = ps_df, 
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
        ps_df %>% filter(tgt == ps) %>%
          select(PATID,T_DAYS,actual),
        by=c("PATID","T_DAYS")
      )
    
    ps_df %<>%
      left_join(
        iptw_df %>% select(PATID,T_DAYS,iptw),
        by = c("PATID","T_DAYS")
      )

    # estimate counterfactuals
    ps_idx<-var_encoder %>% filter(var==ps) %>% select(var2) %>% unlist()
    intx_tr<-trainX[,ps_idx]
    intx_ts<-testX[,ps_idx]
    #intervene with 1
    trainX[,ps_idx]<-1
    testX[,ps_idx]<-1
    dtrain<-xgb.DMatrix(data = trainX,label = trainY$val)
    dtest<-xgb.DMatrix(data = testX,label = testY$val)
    y1_cf<-c(
      predict(unadj$fit_model$model,dtrain),
      predict(unadj$fit_model$model,dtest)
    )
    #intervene with 0
    trainX[,ps_idx]<-0
    testX[,ps_idx]<-0
    dtrain<-xgb.DMatrix(data = trainX,label = trainY$val)
    dtest<-xgb.DMatrix(data = testX,label = testY$val)
    y0_cf<-c(
      predict(unadj$fit_model$model,dtrain),
      predict(unadj$fit_model$model,dtest)
    )

    # extract "clever covarite"
    all_df<-trainY %>%
      bind_rows(testY) %>%
      mutate(
        y1_cf = y1_cf,
        y0_cf = y0_cf
      ) %>%
      left_join(
        ps_df %>%
          unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
          select(PATID_T,h,h1,h0,actual,pred,iptw) %>%
          rename(ps = pred),
        by="PATID_T") %>%
      mutate(
        offset = log(pred/(1-pred)),
        offset1 = log(y1_cf/(1-y1_cf)),
        offset0 = log(y0_cf/(1-y0_cf))
      )
    
    # fit the adjustment model
    fit_logit<-function(df,wts){
      # https://github.com/cran/tmle/blob/a15f7203befd07bed27b7179bfb8682ca909507a/R/tmle.R#L903
      glm(val ~ -1+h,offset=offset,family="binomial",weights = iptw,data=df)
    }
    fit_str_logit<-all_df %>%
      select(T_DAYS,val,h,offset,iptw) %>%
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
        b1 = h
      )
    
    # estimate TMLE on testing set
    ite_df<-all_df %>%
      semi_join(testY,by="PATID_T") %>%
      left_join(fit_str_logit,by="T_DAYS") %>%
      mutate(
        logit_ystar = offset + b1*h,
        logit_ystar1 = offset1 + b1*h1,
        logit_ystar0 = offset0 + b1*h0,
        ystar = exp(logit_ystar)/(1+exp(logit_ystar)),
        ystar1 = exp(logit_ystar1)/(1+exp(logit_ystar1)),
        ystar0 = exp(logit_ystar0)/(1+exp(logit_ystar0))
      ) %>%
      mutate(
        ITE = ystar1/pmax(ystar0,0.025),
        # ITE = exp(logit_ystar1-logit_ystar0),
        ITE_ub = quantile(ITE,probs = 0.8,na.rm=T),
        ITE_lb = quantile(ITE,probs = 0.01,na.rm=T)
      ) %>%
      # clipping
      mutate(
        ITE = pmin(pmax(ITE,ITE_lb),ITE_ub)
      )
     
    # fit <- glmer(
    #   val ~ (1+h|PATID),
    #   family = binomial,
    #   data = train_df,
    #   offset = pred,
    #   control = glmerControl(optimizer = "bobyqa")
    # )
    # summary(fit)
    
    # result set
    rslt_set<-list(
      fit_model = fit_str_logit,
      ite_df = ite_df
    )
    saveRDS(rslt_set,file = path_to_file)
    
    cat("finish tmle outcome modeling for intervention:",ps,".\n")
  }
}
