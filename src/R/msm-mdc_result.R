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
  "PRVDR_mdc",
  "PRVDR_NEURO_w4up",
  "PRVDR_neurology",
  "PRVDR_psychiatry",
  "PRVDR_home-health",
  "PRVDR_pcp",
  "PRVDR_eye",
  "PRVDR_surgery",
  "PRVDR_pt",
  "PRVDR_ent",
  "PRVDR_ot",
  "PRVDR_social",
  # "PRVDR_rehap",
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

deltat<-60
var_encoder<-readRDS("./data/var_encoder.rda") %>%
  select(var,var2) %>% unique %>%
  left_join(
    readRDS("./data/data_dict.rds") %>%
      select(VAR,VAR_LABEL),
    by=c("var"="VAR")) %>%
  mutate(var_lbl = coalesce(VAR_LABEL,var))

# unadjusted model
rslt<-readRDS("./data/unadj/tvm_OC_death.rda")
feat_imp<-rslt$fit_model$feat_imp %>%
  rename(var2 = Feature) %>%
  inner_join(var_encoder,by="var2") %>%
  mutate(
    adj_ind = 'unadj',
    adj_by = 'unadj'
  )

explainer<-rslt$explain_model %>%
  rename(var2 = var) %>%
  inner_join(var_encoder,by="var2") %>%
  filter(!var %in% c(ps_comm,ps_tgt)) %>%
  group_by(val,cond,var,var2,var_lbl) %>%
  summarise(
    eff_m = median(effect,na.rm=T),
    eff_l = quantile(effect,probs = 0.025,na.rm=T),
    eff_u = quantile(effect,probs = 0.975,na.rm=T),
    .groups = "drop"
  ) %>%
  mutate(
    exp_eff_m = exp(eff_m),
    exp_eff_l = exp(eff_l),
    exp_eff_u = exp(eff_u)
  ) %>%
  mutate(
    adj_ind = 'unadj',
    adj_by = 'unadj'
  )

tv_ate<-rslt$explain_model %>%
  rename(var2 = var) %>%
  inner_join(var_encoder,by="var2") %>%
  filter(var %in% c(ps_comm,ps_tgt)) %>%
  group_by(val,cond,var,var2,var_lbl) %>%
  summarise(
    at_m = mean(effect,na.rm=T),
    at_l = quantile(effect,probs = 0.025,na.rm=T),
    at_u = quantile(effect,probs = 0.975,na.rm=T),
    .groups="drop"
  ) %>%
  pivot_wider(
    names_from = val,
    values_from = c(at_m,at_l,at_u)
  ) %>%
  mutate(
    ate_m = at_m_1 - at_m_0,
    ate_l = at_l_1 - at_u_0,
    ate_u = at_u_1 - at_l_0,
    estmod = 'unadj'
  ) %>%
  select(
    var,cond,ate_m,ate_l,ate_u,estmod
  )

tve_df<-tv_ate
for(ps in c(ps_comm,ps_tgt)){
  # ps<-ps_tgt[1]
  for(m in c('iptw','tmle')){
    #---- treatment effect
    # m<-"tmle"
    rslt<-readRDS(file.path("./data",m,paste0("tvm_OC_death_",ps,".rda")))
    if(m=="iptw"){
      #--att
      tv_ate<-rslt$explain_model %>%
        rename(var2 = var) %>%
        inner_join(var_encoder,by="var2") %>%
        filter(var == ps) %>%
        group_by(val,cond,var,var2) %>%
        summarise(
          at_m = median(effect,na.rm=T),
          at_l = quantile(effect,probs = 0.025,na.rm=T),
          at_u = quantile(effect,probs = 0.975,na.rm=T),
          .groups="drop"
        ) %>%
        pivot_wider(
          names_from = val,
          values_from = c(at_m,at_l,at_u)
        ) %>%
        mutate(
          ate_m = at_m_1 - at_m_0,
          ate_l = at_l_1 - at_u_0,
          ate_u = at_u_1 - at_l_0,
          estmod = 'iptw'
        ) %>%
        select(
          var,cond,ate_m,ate_l,ate_u,estmod
        )
      #--mte
      tv_ate %<>%
        bind_rows(
          rslt$ite_df %>%
            group_by(T_DAYS) %>%
            summarise(
              ate_m = median(ITE),
              ate_l = quantile(ITE,probs = 0.025,na.rm=T),
              ate_u = quantile(ITE,probs = 0.975,na.rm=T),
              .groups = "drop"
            ) %>%
            mutate(
              cond = T_DAYS,var = ps,
              estmod = 'iptw_ate'
            ) %>%
            select(
              var,cond,ate_m,ate_l,ate_u,estmod
            )
        ) %>%
        bind_rows(
          rslt$ite_df %>%
            group_by(T_DAYS) %>%
            summarise(
              ate_m = median(ITE_aug),
              ate_l = quantile(ITE_aug,probs = 0.025,na.rm=T),
              ate_u = quantile(ITE_aug,probs = 0.975,na.rm=T),
              .groups = "drop"
            ) %>%
            mutate(
              cond = T_DAYS,var = ps,
              estmod = 'iptw_ate_dr'
            ) %>%
            select(
              var,cond,ate_m,ate_l,ate_u,estmod
            )
        )
      
      #---- other covariates
      explainer %<>%
        bind_rows(
          rslt$explain_model %>%
            rename(var2 = var) %>%
            inner_join(var_encoder,by="var2") %>%
            filter(!var %in% c(ps_comm,ps_tgt)) %>%
            group_by(val,cond,var,var2,var_lbl) %>%
            summarise(
              eff_m = median(effect,na.rm=T),
              eff_l = quantile(effect,probs = 0.025,na.rm=T),
              eff_u = quantile(effect,probs = 0.975,na.rm=T),
              .groups = "drop"
            ) %>%
            mutate(
              exp_eff_m = exp(eff_m),
              exp_eff_l = exp(eff_l),
              exp_eff_u = exp(eff_u)
            ) %>%
            mutate(
              adj_ind = m,
              adj_by = ps
            )
        )
      
    }else if(m=="tmle"){
      tv_ate<-rslt$ite_df %>%
        group_by(T_DAYS) %>%
        arrange(logit_ystar) %>%
        summarise(
          ate_m = median(logit_ystar),
          ate_l = quantile(logit_ystar,probs = 0.025,na.rm=T),
          ate_u = quantile(logit_ystar,probs = 0.975,na.rm=T),
          .groups = "drop"
        ) %>%
        mutate(
          cond = T_DAYS,var = ps,
          estmod = 'tmle'
        ) %>%
        select(
          var,cond,ate_m,ate_l,ate_u,estmod
        )
    }
    tve_df %<>% bind_rows(tv_ate) 
  }
}

tve_df %<>%
  mutate(
    exp_eff_m = exp(ate_m),
    exp_eff_l = exp(ate_l),
    exp_eff_u = exp(ate_u)
  )

# interventions - providers
sel_prvdr<-c(
  # "PRVDR_mdc",
  # "PRVDR_NEURO_w4up",
  "PRVDR_neurology",
  "PRVDR_psychiatry",
  # "PRVDR_home-health",
  "PRVDR_pcp",
  "PRVDR_eye",
  "PRVDR_surgery",
  "PRVDR_pt",
  # "PRVDR_ent",
  "PRVDR_ot",
  # "PRVDR_social",
  # "PRVDR_rehap",
  # "PRVDR_genetic",
  # "PRVDR_pain",
  # "PRVDR_dietition",
  "PRVDR_nurse",
  # "PRVDR_urology",
  "PRVDR_respiratory",
  "PRVDR_palliative",
  # "PRVDR_cardiology",
  "PRVDR_intv-radiology",
  "PRVDR_slp"
)

ggplot(
  tve_df %>% 
    filter(var %in% sel_prvdr) %>%
    filter(estmod %in% c('unadj','iptw')),
  aes(x=cond,y=exp_eff_m,color = estmod,group=estmod,linetype=estmod)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "right",
    text = element_text(face="bold",size=20)
  )+
  facet_wrap(~ var,ncol=3,scales ="free")

# save figure
ggsave(
  "./res/provider_tv_ate.pdf",
  dpi = 300,
  width = 15, 
  height = 8, 
  units = "in",
  device = "pdf"
)

# interventions - soc
sel_tx<-c(
  "TX_fda",
  "TX_aot",
  # "TX_gastrostomy",
  "TX_non-invasive-ventilator",
  "TX_wheelchair"
)
ggplot(
  tve_df %>% 
    filter(var %in% sel_tx) %>%
    filter(estmod %in% c('unadj','iptw')),
  aes(x=cond,y=exp_eff_m,color = estmod,group=estmod,linetype=estmod)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "right",
    text = element_text(face="bold",size=15)
  )+
  facet_wrap(~ var,ncol=2,scales ="free")

# save figure
ggsave(
  "./res/soc_tv_ate.pdf",
  dpi = 250,
  width = 8, 
  height = 6, 
  units = "in",
  device = "pdf"
)


# demographic - sex
ggplot(
  explainer %>% filter(grepl("^(DEMO)+",var) & grepl("(SEX)+",var)),
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

# demographic - age
ggplot(
  explainer %>% 
    filter(grepl("^(DEMO)+",var) & grepl("(AGE)+",var)),
  aes(x=val,y=exp_eff_m,color = cond, group = cond)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  scale_colour_gradient(low = "yellow", high = "blue") +
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Age at Index", y = "Hazard Ratio", color = "Days Since Index") +
  theme(
    text = element_text(face="bold"),
    legend.position="bottom"
  )

# save figure
ggsave(
  "./res/age_tv_ate.pdf",
  dpi = 150,
  width = 6, 
  height = 4, 
  units = "in",
  device = "pdf"
)

# comorbidities
ggplot(
  explainer %>% 
    filter(grepl("^(PHECD)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val),linetype = adj_ind)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = y ~ x)+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  scale_colour_manual(values = c("yellow","blue"))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=4,scales ="free")

# save figure
ggsave(
  "./res/phecd_tv_ate.pdf",
  dpi = 250,
  width = 12, 
  height = 25, 
  units = "in",
  device = "pdf"
)

# labs
ggplot(
  explainer %>% 
    filter(grepl("^(LAB)+",var) & val > 0),
  aes(x=val,y=exp_eff_m,color = cond,group = cond)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = y ~ x,se = FALSE)+
  scale_colour_gradient(low = "yellow", high = "blue") +
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "lab values", y = "Hazard Ratio", color="Days since Index") +
  theme(
    text = element_text(face="bold"),
    legend.position="bottom"
  ) +
  facet_wrap(~ var_lbl,ncol=4,scales ="free")

# save figure
ggsave(
  "./res/labs_tv_ate.pdf",
  dpi = 250,
  width = 12, 
  height = 15, 
  units = "in",
  device = "pdf"
)

# sdoh
ggplot(
  explainer %>% 
    filter(grepl("(SDH)+",var) & !grepl("(ADI)+",var)),
  aes(x=cond,y=exp_eff_m,color = factor(val))
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = y ~ x)+
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  scale_colour_manual(values = c("yellow","blue"))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio") +
  theme(
    legend.position = "none",
    text = element_text(face="bold")
  )+
  facet_wrap(~ var_lbl,ncol=2,scales ="free")

# save figure
ggsave(
  "./res/sdh_tv_ate.pdf",
  dpi = 150,
  width = 8, 
  height = 6, 
  units = "in",
  device = "pdf"
)

ggplot(
  explainer %>% 
    filter(grepl("(SDH)+",var) & grepl("(ADI)+",var)),
  aes(x=val,y=exp_eff_m,color = cond,group = cond)
) + 
  geom_point()+
  geom_smooth(method = 'loess',formula = 'y ~ x')+
  scale_colour_gradient(low = "yellow", high = "blue") +
  geom_errorbar(aes(ymax = exp_eff_u,ymin = exp_eff_l))+
  geom_hline(aes(yintercept = 1),linetype=2) + 
  labs(x = "Days Since Index", y = "Hazard Ratio", color="Days since Index") +
  theme(
    text = element_text(face="bold"),
    legend.position="bottom"
  )+
  facet_wrap(~ var,ncol=4,scales ="free")

# save figure
ggsave(
  "./res/adi_tv_ate.pdf",
  dpi = 150,
  width = 8, 
  height = 6, 
  units = "in",
  device = "pdf"
)

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

