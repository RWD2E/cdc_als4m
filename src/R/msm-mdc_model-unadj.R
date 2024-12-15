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

deltat<-60

var_encoder<-readRDS(file.path(
  "./data",paste0("t_",deltat,"d"),
  "var_encoder.rda")
) %>%
  select(var,var2) %>% unique

##===== unadjusted model ====
# outcomes of interests
y_lst_tx<-c(
  "TX_aot",
  "TX_fda",
  "TX_gastrostomy",
  "TX_non-invasive-ventilator",
  "TX_wheelchair"
)
y_lst_prvdr<-c(
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
y_lst_oc<-c(
  "OC_death"
)
y_lst<-c(
  y_lst_tx,y_lst_prvdr,y_lst_oc
)

for(y_str in y_lst){
  # y_str<-'PRVDR_neurology' # uncomment for line-by-line testing
  y_str<-'OC_death' # uncomment for line-by-line testing
  path_to_file<-file.path(
    "./data",paste0("t_",deltat,"d"),
    "unadj",paste0("tvm_",y_str,".rda")
  )
  if(!file.exists(path_to_file)){
    #----load testing data----
    testY<-readRDS(file.path(
      "./data",paste0("t_",deltat,"d"),
      "testY_82.rda"
    )) %>%
      unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
      arrange(PATID_T) %>% 
      filter(var == y_str) %>%
      select(PATID_T,val)
    
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
    
    #----load training data----
    trainY<-readRDS(file.path(
      "./data",paste0("t_",deltat,"d"),
      "trainY_82.rda"
    )) %>%
      mutate(PATID2 = PATID) %>%
      unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
      arrange(PATID_T) %>% 
      # collect y
      filter(var == y_str) %>%
      select(PATID2,PATID_T,val)
    
    # ensure alignment
    trainX<-trainY %>% 
      select(PATID_T) %>%
      inner_join(
        readRDS(
          file.path(
            "./data",paste0("t_",deltat,"d"),
            "trainX_82.rda"
        )) %>% 
          unite("PATID_T",c("PATID","T_DAYS"),sep="_"),
        by="PATID_T",multiple = "all"
      )
    
    # manual feature selection
    if(y_str %in% y_lst_oc){
      #-- remove reverse cause
      trainX %<>%
        semi_join(
          var_encoder %>%
            filter(!grepl("^(CURR_)+",var)),
          by="var2"
        )
    }else if(y_str %in% y_lst_prvdr){
      #-- remove reverse cause
      trainX %<>%
        semi_join(
          var_encoder %>%
            filter(!grepl("^(PAST_)+",var)&!(var %in% y_lst_tx)),
          by="var2"
        )
    }else{
      trainX %<>%
        semi_join(
          var_encoder %>%
            filter(!grepl("^(PAST_)+",var)),
          by="var2"
        )
    }

    # sample split by treatment for separate outcome models
    train_oc<-list()
    train_oc[['all']][['t']]<-list(
      trainX = trainX,
      trainY = trainY
    )
    if(y_str %in% c(y_lst_tx,y_lst_prvdr)){
      for(oc in y_lst_oc){
        # oc<-"OC_death" # uncomment for line-by-line testing
        train_df<-readRDS(file.path(
          "./data",paste0("t_",deltat,"d"),
          "trainY_82.rda"
        )) %>%
          mutate(PATID2 = PATID) %>%
          unite("PATID_T",c("PATID","T_DAYS"),sep="_") %>%
          arrange(PATID_T) %>% 
          # collect y
          filter(var == oc)
        
        # treated
        train_oc[[oc]][['t1']]<-list(
          trainY = train_df %>%
            semi_join(trainY %>% filter(val==1),by="PATID_T"),
          trainX = train_df %>%
            semi_join(trainY %>% filter(val==1),by="PATID_T") %>%
            select(PATID_T) %>% left_join(trainX,by="PATID_T") # alignment
        )

        # untreated
        train_oc[[oc]][['t0']]<-list(
          trainY = train_df %>%
            semi_join(trainY %>% filter(val==0),by="PATID_T"),
          trainX = train_df %>%
            semi_join(trainY %>% filter(val==0),by="PATID_T") %>%
            select(PATID_T) %>% left_join(trainX,by="PATID_T") # alignment
        )
      }
    }
    #---------
    cat("finish sample splitting for target:",y_str,".\n")
    
    # main training process
    rslt_set<-list()
    for(m1 in names(train_oc)){
      # m1<-'OC_death' # uncomment for line-by-line testing 
      for(m2 in names(train_oc[[m1]])){
        # m2<-'t1'  # uncomment for line-by-line testing
        # transform to sparse matrix
        trX<-train_oc[[m1]][[m2]][['trainX']] %>%
          long_to_sparse_matrix(
            .,
            id = "PATID_T",
            variable = "var2",
            value = "val"
          )
        
        # for dropping due to lack of X
        trY<-train_oc[[m1]][[m2]][['trainY']] %>% 
          inner_join(data.frame(PATID_T = row.names(trX)),by="PATID_T")
        
        # customize folds (so same patient remain in the same fold)
        folds<-list()
        for(fold in 1:5){
          fold_lst<-readRDS(file.path(
            "./data",paste0("t_",deltat,"d"),
            "part_idx.rda"
          )) %>%
            filter(hdout82==0&cv5==fold) %>%
            select(PATID) %>%
            inner_join(
              trY %>% 
                select(PATID2) %>% rowid_to_column(),
              by = c("PATID" = "PATID2"),multiple = "all"
            ) %>%
            select(rowid)
          folds[[fold]]<-fold_lst$rowid
        }
        
        # align feature sets between training and testing
        shared<-colnames(trX)[colnames(trX) %in% colnames(testX)]
        trX<-trX[,shared]
        testX<-testX[,shared]
        
        # convert to DMatrix
        dtrain<-xgb.DMatrix(data = trX,label = trY$val)
        attr(dtrain,'id')<-trY$PATID_T # assume the order doesn't change
        dtest<-xgb.DMatrix(data = testX,label = testY$val)
        attr(dtest,'id')<-testY$PATID_T # assume the order doesn't change
        
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
        
        if(!grepl("^(OC)+",y_str) & m1=='all' & m2=='t'){
          # attach lagged target (for calculating wt stablizer)
          tgt_idx<-var_encoder %>% 
            filter(var==y_str) %>% select(var2) %>% unlist()
          xgb_rslt$pred_tr %<>%
            mutate(prev = trX[,tgt_idx])
          xgb_rslt$pred_ts %<>%
            mutate(prev = testX[,tgt_idx])
        }
        
        # shap explainer
        time_idx<-var_encoder %>%
          filter(var=="T_DAYS") %>% select(var2) %>% unlist()
        var_lst<-readRDS(file.path(
          "./data",paste0("t_",deltat,"d"),
          "var_encoder.rda"
        )) %>%
          filter(var %in% y_lst[!grepl("^(OC)+",y_lst)]) %>% 
          select(var2) %>% unlist()
        
        explainer<-explain_model(
          X = testX,
          y = testY$val,
          xgb_rslt = xgb_rslt,
          top_k = 50,
          var_lst = var_lst,
          boots = 20,
          nns = 30,
          shap_cond = time_idx, # time index
          verb = FALSE
        )
        
        # add to result set
        rslt_set[[m1]][[m2]]<-list(
          fit_model = xgb_rslt,
          explain_model = explainer
        )
        
        #---------
        cat("finish model training:",y_str,";",m1,";",m2,".\n")
      }
    }
 
    saveRDS(rslt_set,file=path_to_file)
    #---------
    cat("finish building covariate-adj model for:",y_str,".\n")
  }
}

