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

pr<-y_lst[grepl("^(PRVDR)+",y_lst)]
rslt<-readRDS("./data/tvm_OC_death.rda")

time_idx<-readRDS("./data/var_encoder.rda") %>%
  filter(var=="T_DAYS") %>% select(var2) %>% unlist()
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
  filter(var2 != time_idx) %>%
  mutate(var_lbl = coalesce(var_lbl,var))

ggplot(
  explainer %>% 
    filter(grepl("^(TX|PRVDR|RX)+",var)),
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
    filter(!grepl("^(TX|PRVDR|RX|SDH|AGE|HISP)+",var)),
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

# fig<-plot_ly(
#   explainer %>% filter(grepl("^(AGE)+",var) & cond > 0),
#   x = ~val, 
#   y = ~cond, 
#   z = ~exp_eff_m,
#   type = 'scatter3d',
#   mode = 'lines',
#   line = list(color =~cond)
# )
# fig

