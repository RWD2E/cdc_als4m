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
  plotly
)

ps_comm<-c(
  "TX_fda",
  "TX_aot",
  "TX_gastrostomy",
  "TX_non-invasive-ventilator",
  "TX_wheelchair"
)

ps_tgt<-c(
  "PRVDR_neurology",
  "PRVDR_psychiatry",
  "PRVDR_home-health",
  "PRVDR_pcp",
  "PRVDR_eye",
  "PRVDR_surgery",
  "PRVDR_pt",
  "PRVDR_ent",
  "PRVDR_ot",
  # "PRVDR_social",
  # "PRVDR_rehap",
  # "PRVDR_genetic",
  # "PRVDR_pain",
  # "PRVDR_dietition",
  "PRVDR_nurse",
  "PRVDR_urology",
  "PRVDR_respiratory",
  "PRVDR_palliative",
  "PRVDR_cardiology",
  "PRVDR_intv-radiology",
  "PRVDR_slp"
  # "PRVDRGRP_neurology_pcp",
  # "PRVDRGRP_pcp_respiratory",
  # "PRVDRGRP_nurse_pcp",
  # "PRVDRGRP_pcp_pt",
  # "PRVDRGRP_pcp_psychiatry",
  # "PRVDRGRP_ent_pcp",
  # "PRVDRGRP_neurology_respiratory",
  # "PRVDRGRP_neurology_pcp_respiratory",
  # "PRVDRGRP_neurology_pcp_psychiatry",
  # "PRVDRGRP_neurology_pcp_pt",
  # "PRVDRGRP_neurology_nurse_pcp",
  # "PRVDRGRP_nurse_pcp_respiratory"
)

deltat<-60
var_encoder<-readRDS("./data/var_encoder.rda") %>%
  select(var,var2) %>% unique %>%
  left_join(
    readRDS("./data/data_dict.rds") %>%
      select(var,var_lbl),
    by="var")

# unadjusted model
rslt<-readRDS("./data/unadj/tvm_OC_death.rda")
feat_imp<-rslt$fit_model$feat_imp %>%
  rename(var2 = Feature) %>%
  inner_join(var_encoder,by="var2") %>%
  mutate(adj_ind = 'unadj',
         adj_by = 'unadj')

explainer<-rslt$explain_model %>%
  rename(var2 = var) %>%
  inner_join(var_encoder,by="var2") %>%
  group_by(val,cond,var,var2) %>%
  summarise(eff_m = median(effect,na.rm=T),
            eff_l = quantile(effect,probs = 0.025,na.rm=T),
            eff_u = quantile(effect,probs = 0.975,na.rm=T),
            .groups = "drop") %>%
  left_join(readRDS("./data/data_dict.rds"),by="var",multiple = "all") %>%
  mutate(exp_eff_m = exp(eff_m),
         exp_eff_l = exp(eff_l),
         exp_eff_u = exp(eff_u)) %>%
  mutate(var_lbl = coalesce(var_lbl,var)) %>%
  mutate(adj_ind = 'unadj',
         adj_by = 'unadj')

for(ps in c(ps_comm,ps_tgt)){
  # ps<-ps_tgt[1]
  rslt<-readRDS(file.path("./data/iptw",paste0("tvm_OC_death_",ps,".rda")))
  feat_imp %<>%
    bind_rows(
      rslt$fit_model$feat_imp %>%
        rename(var2 = Feature) %>%
        inner_join(var_encoder,by="var2") %>%
        mutate(adj_ind = 'adj',
               adj_by = ps)
    )
  explainer %<>%
    bind_rows(
      rslt$explain_model %>%
        rename(var2 = var) %>%
        inner_join(var_encoder,by="var2") %>%
        group_by(val,cond,var,var2) %>%
        summarise(eff_m = median(effect,na.rm=T),
                  eff_l = quantile(effect,probs = 0.025,na.rm=T),
                  eff_u = quantile(effect,probs = 0.975,na.rm=T),
                  .groups = "drop") %>%
        left_join(readRDS("./data/data_dict.rds"),by="var",multiple = "all") %>%
        mutate(exp_eff_m = exp(eff_m),
               exp_eff_l = exp(eff_l),
               exp_eff_u = exp(eff_u)) %>%
        mutate(var_lbl = coalesce(var_lbl,var)) %>%
        mutate(adj_ind = 'adj',
               adj_by = ps)
    )
}

# demographic
ggplot(
  explainer %>% 
    filter(grepl("^(DEMO)+",var) & !grepl("(AGE)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val),linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=3,scales ="free")

# save figure
ggsave(
  file.path(path_to_outdir,""),
  dpi = 250,
  width = 18, 
  height = 18, 
  units = "in",
  device = "png"
)

ggplot(
  explainer %>% 
    filter(grepl("^(DEMO)+",var) & grepl("(AGE)+",var)),
  aes(x=val,y=exp_eff_m,color = cond,group = cond, linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  scale_colour_gradient(low = "green", high = "red") +
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Age at Index", y = "Hazard Ratio", color = "days_since_index") +
  theme(
    text = element_text(face="bold")
  )

# interventions - soc
ggplot(
  explainer %>% 
    filter(adj_by == var | adj_ind == 'unadj') %>%
    filter(grepl("^(TX)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val),linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=3,scales ="free")

# interventions - providers
ggplot(
  explainer %>% 
    filter(adj_by == var | adj_ind == 'unadj') %>%
    filter(grepl("^(PRVDR_)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val),linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=4,scales ="free")

prdvr_rpt<-explainer %>% 
  filter(adj_by == var) %>%
  filter(grepl("^(PRVDR_)+",var)) %>%
  group_by(cond,var,var2,var_lbl,adj_ind,adj_by) %>%
  mutate(norm_by = eff_m[val==0]) %>%
  ungroup %>%
  mutate(
    eff_m = eff_m - norm_by,
    eff_l = eff_l - norm_by,
    eff_u = eff_u - norm_by,
    exp_eff_m = exp(eff_m),
    exp_eff_l = exp(eff_l),
    exp_eff_u = exp(eff_u)
  ) %>%
  filter(val == 1 & cond <= 1000) %>%
  group_by(var,var2,var_lbl,adj_ind,adj_by) %>%
  filter(exp_eff_m == min(exp_eff_m))

# marginal effect of time
ggplot(
  explainer %>% 
    filter(var=="T_DAYS"),
  aes(x=cond,y=exp_eff_m,color=adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    text = element_text(face="bold")
  )

time_idx<-readRDS("./data/var_encoder.rda") %>%
  filter(var=="T_DAYS") %>% select(var2) %>% unlist()

# comorbidities
ggplot(
  explainer %>% 
    filter(grepl("^(PHECD)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val),linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = y ~ x)+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=4,scales ="free")

# labs
ggplot(
  explainer %>% 
    filter(grepl("^(LAB)+",var) & val > 0),
  aes(x=val,y=exp_eff_m,color = cond,group = cond,linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = y ~ x,se = FALSE)+
  scale_colour_gradient(low = "green", high = "red") +
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "lab values", y = "Hazard Ratio") +
  theme(
    text = element_text(face="bold")
  ) +
  facet_wrap(~ var_lbl,ncol=6,scales ="free")

# sdoh
ggplot(
  explainer %>% 
    filter(grepl("(SDH)+",var) & !grepl("(ADI)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val),linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = y ~ x)+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=4,scales ="free")


ggplot(
  explainer %>% 
    filter(grepl("(SDH)+",var) & grepl("(ADI)+",var)),
  aes(x=val,y=exp_eff_m,color = cond,group = cond,linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  scale_colour_gradient(low = "green", high = "red") +
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    text = element_text(face="bold")
  )+
  facet_wrap(~ var,ncol=4,scales ="free")

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

