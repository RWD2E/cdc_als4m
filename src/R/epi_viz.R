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


dat<-readRDS(
  file.path(
    path_to_data,
    paste0(ty,"dmals_annual.rda")
    )
  ) %>%
  filter(
    YEAR <= 2021 & YEAR >= 2011
  )

figures <- plot_als_dm_annual_rates(
  data = dat,
  estimates = c("adjusted", "crude"),
  ci_display = "errorbar",
  als_rate_per = 100000
)

# figures$dm_plot
# figures$als_plot

ggplot2::ggsave(
  file.path(
    path_to_res,
    paste0(ty,"_dmByals_rt.pdf")
  ),
  figures$dm_plot,
  width = 11,
  height = 7,
  units = "in",
  device = cairo_pdf
)

ggplot2::ggsave(
  file.path(
    path_to_res,
    paste0(ty,"_alsBydm_rt.pdf")
  ),
  figures$als_plot,
  width = 11,
  height = 7,
  units = "in",
  device = cairo_pdf
)

dat<-readRDS(
  file.path(
    path_to_data,
    paste0(ty,"dmals_med_annual.rda")
  )
) %>%
  filter(
    YEAR <= 2021 & YEAR >= 2011
  )

med_plot<-plot_med_prevalence(
  dat,
  meds = c(
    "BIGU",
    "DPP4",
    "GLP1RA",
    "TZD",
    "SGLT2",
    "MEG",
    "AGI",
    "AML",
    "INS",
    "SU"
  ),
  ty = "t2dm"
)
ggplot2::ggsave(
  file.path(
    path_to_res,
    paste0(ty,"_meds_rt.pdf")
  ),
  med_plot,
  width = 11,
  height = 7,
  units = "in",
  device = cairo_pdf
)
