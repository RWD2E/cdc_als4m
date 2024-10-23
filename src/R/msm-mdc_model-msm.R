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
  path_to_file<-file.path("./data/msm",paste0("tvm_OC_death_",ps,".rda"))
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
    # # iptw diagnostic
    # ggplot(iptw_df,aes(x=factor(T_DAYS),y=iptw))+
    #   geom_boxplot()+
    #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    # 
    # smd_df<-readRDS("./data/trainX_82.rda") %>%
    #   inner_join(
    #     iptw_df %>%
    #       select(PATID, T_DAYS, actual, iptw), 
    #     by = c('PATID','T_DAYS')
    #   ) %>%
    #   inner_join(
    #     readRDS("./data/var_encoder.rda"),
    #     by=c("var2","T_DAYS")
    #   ) %>%
    #   group_by(var,T_DAYS,val_sd) %>%
    #   mutate(iptw_sum = sum(iptw)) %>% 
    #   ungroup 
    # 
    # smd_df2<-smd_df %>% filter(val_lev > 1) %>%
    #   group_by(var,T_DAYS,actual,iptw_sum,val_sd) %>%
    #   summarise(
    #     val_m = mean(val,na.rm=T),
    #     val_m_wt = mean(val*iptw,na.rm=T),
    #     .groups = 'drop'
    #   ) %>%
    #   bind_rows(
    #     smd_df %>% filter(val_lev == 1) %>%
    #       group_by(var,T_DAYS,actual,pt_cnt,iptw_sum,val_sd) %>%
    #       summarise(
    #         val_sum = sum(val),
    #         val_sum_wt = sum(val*iptw),
    #         .groups = 'drop'
    #       ) %>%
    #       mutate(
    #         val_m = val_sum/pt_cnt,
    #         val_m_wt = val_sum_wt/iptw_sum,
    #       ) %>%
    #       select(-val_sum_wt)
    #   ) %>%
    #   select(var,T_DAYS,actual,val_sd,val_m,val_m_wt) %>%
    #   pivot_wider(
    #     names_from = actual,
    #     names_prefix = 'm',
    #     values_from = c(val_m,val_m_wt),
    #     id_cols = c("var",'T_DAYS','val_sd'),
    #     values_fill = 0
    #   ) %>%
    #   mutate(
    #     smd = (val_m_m1 - val_m_m0)/val_sd,
    #     smd_wt = (val_m_wt_m1 - val_m_wt_m0)/val_sd
    #   ) %>%
    #   mutate(
    #     smd_p = 2*pnorm(abs(smd),lower.tail = FALSE),
    #     smd_wt_p = 2*pnorm(abs(smd_wt),lower.tail = FALSE)
    #   ) %>%
    #   mutate(smd_delta = case_when(
    #     abs(smd_wt)<= abs(smd) ~ 'less-bias',
    #     TRUE ~ 'more-bias'
    #   ))
    # 
    # # volcano plot
    # ggplot(smd_df2 %>%
    #          select(var,T_DAYS,smd,smd_p) %>%
    #          mutate(type = 'unweighted') %>%
    #          bind_rows(
    #            smd_df2 %>%
    #              select(var,T_DAYS,smd_wt,smd_wt_p) %>%
    #              rename(smd = smd_wt, smd_p = smd_wt_p) %>%
    #              mutate(type = 'weighted')
    #          ),
    #        aes(x=smd,y=-log10(smd_p)))+
    #   geom_point(aes(color=type),alpha = 0.5)+
    #   facet_wrap(~T_DAYS, ncol = 5, scales = "free")
    
    # outcome model only based on ps target
    # var_sel<-var_encoder %>%
    #   filter(var %in% c('T_DAYS',c(ps_comm,ps))) %>%
    #   select(var2) %>% unlist()
    # trainX<-trainX[,var_sel]
    # testX<-testX[,var_sel]
    
    # convert to DMatrix
    dtrain<-xgb.DMatrix(data = trainX,label = trainY$val)
    attr(dtrain,'id')<-trainY$PATID_T # assume the order doesn't change
    dtest<-xgb.DMatrix(data = testX,label = testY$val)
    attr(dtest,'id')<-testY$PATID_T # assume the order doesn't change
    
    # reset weights in training and testing data
    iptw_align<-data.frame(
      PATID_T = row.names(trainX)
    ) %>%
      left_join(
        iptw_df %>%
          unite("PATID_T",c("PATID","T_DAYS"),sep="_"),
        by="PATID_T"
      )
    setinfo(dtrain,'weight',iptw_align$iptw)
    
    iptw_align<-data.frame(
      PATID_T = row.names(testX)
    ) %>%
      left_join(
        iptw_df %>%
          unite("PATID_T",c("PATID","T_DAYS"),sep="_"),
        by="PATID_T"
      )
    setinfo(dtest,'weight',iptw_align$iptw)
    
    # rapid xgb - only tune the number of trees
    xgb_rslt<-prune_xgb(
      # dtrain, dtest are required to have attr:'id'
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
    time_idx<-var_encoder %>%
      filter(var=="T_DAYS") %>% 
      select(var2) %>% unlist()
    var_sel<-var_encoder %>%
      filter(var %in% c(ps_comm,ps_tgt)) %>% 
      select(var2) %>% unlist()
    
    # hte by separating between late-onset and late-incld-of-early-onset
    # pt<-readRDS("C:/repo/cdc_als4m/data/als_tbl1.rds") %>%
    #   select(PATID,AGE_AT_INDEX) %>%
    #   mutate(late_onset = as.numeric(AGE_AT_INDEX>=64))
    
    explainer<-explain_model(
      X = testX,
      y = testY$val,
      xgb_rslt = xgb_rslt,
      top_k = 50,
      var_lst = var_sel,
      boots = 10,
      nns = 30,
      shap_cond = time_idx, # time index
      verb = FALSE
    )

    # result set
    rslt_set<-list(
      fit_model = xgb_rslt,
      explain_model = explainer
    )
    saveRDS(rslt_set,file = path_to_file)
    
    cat("finish iptw outcome modeling for intervention:",ps,".\n")
  }
}