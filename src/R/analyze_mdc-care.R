#################################################################
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: analyze_neurologist-care.R
# Description: IPTW-adjusted model to evaluate causal effect of 
#              neurologist care on survival
#################################################################

#===== Setup ==============================
rm(list=ls())
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  magrittr,
  broom,
  survival,
  survminer,
  kableExtra,
  devtools,
  cmprsk,
  ggridges,
  Matrix,
  glmnet,
  scales,
  islasso,
  caret,
  rms,
  SurvMetrics,
  scales
)
source_url("https://raw.githubusercontent.com/sxinger/utils/master/sample_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/model_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/analysis_util.R")

#===== Load data ==========================================================
path_to_data_folder<-file.path(
  getwd(),
  "data"
)
aset<-readRDS(file.path(
  path_to_data_folder,
  "als_aset_mort.rds")
)
sample_idx<-readRDS(file.path(
  path_to_data_folder,
  "als_aset_mort_part.rds")
)

#===== Define Global Variables ===============================================
# Variables
var_lst<-colnames(aset)
var_demo<-c(
  "SEXF",
  "AGEGRP1", "AGEGRP3", "AGEGRP4", "AGEGRP5", "AGEGRP6", # ref = "AGEGRP2"
  "RACE_AA", "RACE_Other", "RACE_Unknown" # ref = "RACE_White"
  # ,"OREC_1","OREC_2","OREC_3",
)
var_sdh<-c(
  "PARTD_IND","LIS_DUAL_IND"
  ,"RUCAregrp_RUCA_micro","RUCAregrp_RUCA_small","RUCAregrp_RUCA_unknown"
  ,var_lst[grepl("^(SDH_)+",var_lst)]
)
var_phecd<-c(
  var_lst[grepl("^(PHECD_)+",var_lst)]
)
var_med<-c(
  var_lst[grepl("^(MED)+",var_lst)]
)
var_px<-c(
  var_lst[grepl("^(CCSPX_)+",var_lst)&!grepl("(00000)+",var_lst)]
)
var_tx<-c(
  "RILUZOLE_FLAG"
  ,"NIV_FLAG"
  ,"WHEELCHAIR_FLAG"
  ,"GASTROSTOMY_FLAG"
  ,"PT_FLAG"
)
var_ps<-unique(c(
   var_demo
  ,var_sdh
  ,var_phecd
  ,var_med
  ,var_px
))

# Global Parameters
boots<-3
nfold<-0.9
ptypes<-c(
  "NEUROLOGY",
  "NEUROPSYCH",
  "PHYSICAL_REHAB",
  "GASTROENTEROLOGY",
  "CARDIOLOGY",
  "NUTRITION"
)

# breakpoint knobs 
skip_ipw<-FALSE
skip_surv<-TRUE
boots_iter<-2:2

#===== Experiment Main ================================
for(ptype in ptypes){
  #===== De-biased Sampling ====================================================================
  path_to_dfile<-file.path(
    path_to_data_folder,
    paste0("als_care_",ptype,".rds")
  )
  path_to_sfile<-file.path(
    path_to_data_folder,
    paste0("als_care_",ptype,"_adjset_bts",boots,".rds")
  )
  if(!file.exists(path_to_sfile)){
    exposure_prvd<-readRDS(path_to_dfile) %>%
      mutate(bef_ind = as.numeric(!is.na(BEF_ALS1DX)),
             aft_ind = as.numeric(!is.na(AFT_ALS1DX)))
    
    ctrl<-aset  %>%
      semi_join(exposure_prvd %>% filter(aft_ind==0),by="PATID") %>%
      select(PATID,DEATH_time)
    
    case<-exposure_prvd %>%
      filter(aft_ind==1) %>%
      mutate(
        DAYS_SINCE_ALS1DX = case_when(bef_ind == 0 ~ AFT_ALS1DX,
                                      bef_ind == 1 ~ 0)
      ) %>%
      select(PATID,DAYS_SINCE_ALS1DX)
    
    case_ctrl<-matched_sample.ptdm(
      ref_dat = case, 
      match_dat = ctrl, 
      id_col="PATID",
      update_ref="DAYS_SINCE_ALS1DX", 
      update_col="DEATH_time", 
      boots=boots,
      attr_rt_bd = 0.05,
      replace=TRUE,
      verb=TRUE
    )
    saveRDS(case_ctrl,file=path_to_sfile)
  }else{
    case_ctrl<-readRDS(path_to_sfile)
  }
  
  #===== IPW-adjusted Survival Model ====================================================
  #==== : outer iteration
  for(boot_i in boots_iter){
    ########################################################
    print(sprintf("%s: start bootstrap: %i",ptype,boot_i))
    ########################################################
    # create subdir
    path_to_dir<-file.path(
      path_to_data_folder,
      "results",
      paste0("boot",boot_i)
    )
    if(!dir.exists(path_to_dir)) dir.create(path_to_dir)
  
    # get de-biased sample
    aset_adj<-case_ctrl[[boot_i]]$pos %>% select(PATID) %>%
      left_join(aset, by = "PATID") %>% mutate(care_ind=0) %>%
      bind_rows(
        # adjust immortal time bias
        case_ctrl[[boot_i]]$neg %>% select(PATID,adj_time) %>%
          left_join(aset, by = "PATID") %>%
          mutate(DEATH_time = adj_time,care_ind=1) %>% 
          select(-adj_time)
      )
    
    # load ipw
    if(nfold < 1){
      valid_type<-paste0("hdout",gsub("\\.","",nfold))
    }else{
      valid_type<-paste0("cv",nfold)
    }
    if(!skip_surv){
      ipw_cv<-readRDS(
        file.path(
          path_to_dir,
          paste0(ptype,"_ipw_lasso_",valid_type,".rds")
        )
      )
    }
    
    #===== : inner iteration 
    for(fold in 1:max(nfold,1)){
      #===== IPW ====================================================
      # build IPW model
      if(!skip_ipw){
        result_ipw<-list()
        if(!file.exists(file.path(path_to_dir,paste0(ptype,"_ipw_lasso_",valid_type,".rds")))){
          ########################################################
          print(sprintf("%s: ...start ipw for fold: %i",ptype,fold))
          ########################################################
          # take the training sample
          if(nfold < 1){
            # holdout
            split_by<-paste0("holdout",gsub('\\.','',nfold)) 
            tr<-aset_adj %>% 
              semi_join(sample_idx %>% filter(get(split_by)==1),
                        by="PATID")
          }else{
            # cv
            split_by<-paste0("fold",nfold)
            tr<-aset_adj %>% 
              semi_join(sample_idx %>% filter(get(split_by)!=fold),
                        by="PATID")
          }
          # generate weights
          result_ipw[[split_by]]<-ipw.lasso(
            data_df = tr,
            id_col = 'PATID',
            yc = 'care_ind',
            yo_vec = var_tx,
            xo_vec = var_ps
          )
          ######################################################## 
          print(sprintf("%s: ...complete ipw for fold: %i",ptype,fold))
          ######################################################## 
          # download results
          saveRDS(result_ipw,
                  file=file.path(
                    path_to_dir,
                    paste0(ptype,"_ipw_lasso_",valid_type,".rds")
                  ))
        }
      }
      
      #===== Survival Model ====================================================
      # build ipw-adjusted survival model
      if(!skip_surv){
        result_surv<-list()
        if(!file.exists(file.path(path_to_dir,paste0(ptype,"_coxph_mort_",valid_type,".rds")))){
          ########################################################
          print(sprintf("%s: ...start outcome model for fold: %i",ptype,fold))
          ########################################################
          # attach weight column
          tr_wt<-tr %>%
            inner_join(ipw_cv[[fold]][["care_ind"]]$ps_tw$care_ind,by=c("PATID"="id"))
          
          # get testing set
          ts<-aset_adj %>% anti_join(tr,by="PATID")
          
          # feature selection and model training
          coxph_rfe<-fast_rfe.coxph(
            data_df=tr_wt,
            time_col='DEATH_time',
            status_col='DEATH_status',
            yc = 'care_ind',
            x_wt = 'tw_comp',
            xo_vec = var_ps,
            pval_threshold = 0.05,
            verb = TRUE 
          )
          coef<-summary(coxph_rfe$fit_sel)$coefficient
          
          # generate performance metrics
          perf_summ<-get_perf_summ.surv(
            model = coxph_rfe$fit_sel,   # coxph model object
            data_ts  = ts, # testing data set
            time_col='DEATH_time',
            status_col='DEATH_status'
          )
          perf_calibr<-get_calibr.surv(
            model = coxph_rfe$fit_sel,   # coxph model object
            data_ts  = ts, # testing data set
            time_col='DEATH_time',
            status_col='DEATH_status',
            eval_times=seq(30,90*4*3,by=30)
          )
          
          # attach results
          result_lst[[fold]]<-list(
            var_sel = coef,
            perf_summ = perf_summ,
            perf_calibr = perf_calibr
          )
          ######################################################## 
          print(sprintf("%s: ...complete outcome model fold: %i",ptype,fold))
          ######################################################## 
          saveRDS(result_surv,
                  file= file.path(
                    path_to_dir,
                    paste0(ptype,"_coxph_mort_",valid_type,".rds")
                  ))
        }
      }
    }
    ######################################################## 
    print(sprintf("%s: complete bootstrap: %i",ptype,boot_i))
    ######################################################## 
  }
}
