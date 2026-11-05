pacman::p_load(
  tidyverse,
  scales,
  ggrepel
)

path_to_data<-"C:/repos/cdc_als4m/data"
path_to_res<-"C:/repos/cdc_als4m/res"

# ty<-""
# ty<-"t1"
ty<-"t2"

# =============================================================================
# demographic profile
# =============================================================================

df<-readRDS(
  file.path(
    path_to_data,
    paste0(ty,"dmals_demo_profile.rda")
  )
) %>%
  filter(
    YEAR <= 2021 & YEAR >= 2011
  ) %>%
  filter(
    COHORT %in% c(
      "ALS",
      "ALS_WITH_T2DM",
      "ALS_WITH_NON_DM"
    )
  )

demo_plt<-plot_demo_profile(
  df,ncol=3
)

ggplot2::ggsave(
  file.path(
    path_to_res,
    paste0(ty,"_als_demo_profile.pdf")
  ),
  demo_plt,
  width = 10,
  height = 4,
  units = "in",
  device = cairo_pdf
)

# =============================================================================
# disease prevalence curves
# =============================================================================

dat<-readRDS(
  file.path(
    path_to_data,
    paste0(ty,"dmals_annual.rda")
    )
  ) %>%
  filter(
    YEAR <= 2021 & YEAR >= 2011
  )

population_display <- function(x) {
  
  dplyr::case_when(
    x == "ALL"    ~ "the overall population",
    x == "DM"     ~ "persons with diabetes",
    x == "T1DM"   ~ "persons with T1DM",
    x == "T2DM"   ~ "persons with T2DM",
    x == "ALS"    ~ "persons with ALS",
    x == "NON_DM" ~ "persons without diabetes",
    TRUE          ~ tolower(x)
  )
}

p_dm_prev <- plot_rate_curves(
  dat,
  outcomes = "DM",
  populations = c("ALS", "ALL"),
  metrics = "PREVALENCE",
  units = "percent",
  
  label_style = "long",
  
  bar_population = "ALL",
  denominator_label = "All observable persons, N",
  
  title = "Annual T2DM Prevalence",
  y_label = "Annual prevalence (%)",
  
  caption = " "
)


ggplot2::ggsave(
  file.path(
    path_to_res,
    paste0(ty,"_dmByals_rt.pdf")
  ),
  p_dm_prev,
  width = 11,
  height = 7,
  units = "in",
  device = cairo_pdf
)


p_als_prev <- plot_rate_curves(
  dat,
  outcomes = "ALS",
  populations = c("DM", "ALL"),
  metrics = "PREVALENCE",
  units = "per 100,000",
  
  label_style = "long",
  
  bar_population = "DM",
  denominator_label = "All possible T2DM at-risk persons, N",
  
  title = "Annual ALS Prevalence",
  y_label = "Prevalence per 100,000 person",
  caption = " "
)

# p_als_prev

ggplot2::ggsave(
  file.path(
    path_to_res,
    paste0(ty,"_alsBydm_rt.pdf")
  ),
  p_als_prev,
  width = 11,
  height = 7,
  units = "in",
  device = cairo_pdf
)


# =============================================================================
# medication prevalence curves
# =============================================================================
dat<-readRDS(
  file.path(
    path_to_data,
    paste0(ty,"dmals_med_annual.rda")
  )
) %>%
  filter(
    YEAR <= 2021 & YEAR >= 2011
  )

p_med <- plot_rate_curves(
  data = dat,
  
  metrics = "PREVALENCE",
  units = "percent",
  
  title = "Annual Medication Prescription Prevalence",
  y_label = "Prescription prevalence (%)",
  
  ci_display = "ribbon",
  label_style = "short",
  
  bar_population = "T2DM",
  denominator_label = "Active T2DM at-risk person-years",
  
  caption =" "
)

ggplot2::ggsave(
  file.path(
    path_to_res,
    paste0(ty,"_meds_rt.pdf")
  ),
  p_med,
  width = 11,
  height = 7,
  units = "in",
  device = cairo_pdf
)

