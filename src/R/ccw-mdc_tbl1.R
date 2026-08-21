# ==============================================================================
# ALS MDT CCW COHORT — TABLE 1 & PRESENTATION SUMMARIES
#
# Standalone companion to the CCW analysis. Assumes the person-period CCW table
# `ccw` (from ALS_MDT_CCW_90D) is already in the workspace; if the main analysis
# script has been sourced, it will reuse REFERENCE_STRATEGY / strategy_levels /
# strategy_labels / OUTPUT_DIR, otherwise sensible defaults are derived.
#
# Design notes (worth a line on the slide):
#   * Table 1 is at the UNIQUE-PATIENT level. Cloning makes every strategy arm
#     identical at baseline, so a strategy-stratified baseline table is vacuous.
#   * Vital status and follow-up are taken from the REFERENCE ARM (A), the only
#     clone with no artificial censoring, so they reflect the true natural course.
#
# Outputs (in OUTPUT_DIR):
#   als_ccw_tables.pptx            one table per slide, talk-ready
#   T1_baseline_characteristics.*  html / png
#   T1_cohort_summary.*            html / png
#   T1_adherence_by_strategy.*     html / png
#   plus CSVs of the underlying numbers
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  gtsummary,
  gt,
  flextable,
  labelled,
  scales
)


# ------------------------------------------------------------------------------
# 1. Config fallbacks (so the script runs on its own)
# ------------------------------------------------------------------------------

ccw <- readRDS("./data/als_mdt_ccw.rda")

if (!exists("REFERENCE_STRATEGY")) REFERENCE_STRATEGY <- "A"

if (!exists("strategy_levels")) {
  strategy_levels <- sort(unique(as.character(ccw$STRATEGY)))
}

if (!exists("strategy_labels")) {
  strategy_labels <- set_names(strategy_levels, strategy_levels)
}

if (!exists("OUTPUT_DIR")) {
  OUTPUT_DIR <- "als_mdt_ccw_results"
}
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

GRACE_ANALYTIC_COLS <- c(
  "INTERVAL_START", "INTERVAL_END", "GRACE_END_DATE", "ANALYTIC_END_DATE"
)


# ------------------------------------------------------------------------------
# 2. Collapse to one row per patient
# ------------------------------------------------------------------------------

ref_arm <- ccw %>% filter(as.character(STRATEGY) == REFERENCE_STRATEGY)

# Baseline covariates: first interval of the reference arm (index values).
patient_baseline <- ref_arm %>%
  arrange(PATID, INTERVAL) %>%
  group_by(PATID) %>%
  slice_head(n = 1) %>%
  ungroup()

# True outcome / follow-up from the un-censored reference arm.
patient_outcome <- ref_arm %>%
  group_by(PATID) %>%
  summarise(
    DIED = as.integer(any(EVENT == 1, na.rm = TRUE)),
    FOLLOWUP_DAYS = max(TSTOP, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 3. Choose Table 1 variables
# ------------------------------------------------------------------------------

comorbid_vars <- str_subset(names(patient_baseline), "^IPCW_.*_PRIOR$")
lab_vars <- str_subset(
  names(patient_baseline),
  "^IPCW_(BMI|CBC|METABOLIC)_.*_PRIOR_LAST$"
)

patient_t1 <- patient_baseline %>%
  select(
    PATID, AGE_AT_INDEX, SEX, RACE, HISPANIC,
    any_of(c(comorbid_vars, lab_vars))
  ) %>%
  left_join(patient_outcome, by = "PATID") %>%
  mutate(
    FOLLOWUP_YEARS = FOLLOWUP_DAYS / 365.25,
    VITAL_STATUS = factor(
      if_else(DIED == 1, "Died", "Alive / censored"),
      levels = c("Died", "Alive / censored")
    ),
    AGE_GROUP = cut(
      AGE_AT_INDEX,
      breaks = c(-Inf, 55, 65, 75, Inf),
      labels = c("<55", "55-64", "65-74", "75+")
    )
  )

# Comorbidity flags -> logical; keep only informatively-prevalent ones so the
# slide stays readable. Edit these bounds (or set comorbid_keep by hand) to
# curate for the talk.
comorbid_keep <- comorbid_vars %>%
  keep(function(v) {
    p <- mean(as.numeric(patient_t1[[v]]) > 0, na.rm = TRUE)
    is.finite(p) && p >= 0.02 && p <= 0.98
  })

patient_t1 <- patient_t1 %>%
  mutate(across(all_of(comorbid_keep), ~ as.numeric(.x) > 0))


# ------------------------------------------------------------------------------
# 4. Human-readable variable labels
# ------------------------------------------------------------------------------

pretty_label <- function(x) {
  x %>%
    str_remove("^IPCW_") %>%
    str_remove("_PRIOR(_LAST)?$") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

label_map <- c(
  AGE_AT_INDEX   = "Age at ALS diagnosis, years",
  AGE_GROUP      = "Age group, years",
  SEX            = "Sex",
  RACE           = "Race",
  HISPANIC       = "Hispanic ethnicity",
  FOLLOWUP_YEARS = "Follow-up, years",
  DIED           = "Died during follow-up",
  set_names(pretty_label(comorbid_keep), comorbid_keep),
  set_names(pretty_label(lab_vars), lab_vars)
)

patient_t1 <- patient_t1 %>%
  labelled::set_variable_labels(.labels = as.list(label_map), .strict = FALSE)


# ------------------------------------------------------------------------------
# 5. Table 1 — baseline characteristics by vital status
# ------------------------------------------------------------------------------

theme_gtsummary_compact()

t1_body_vars <- c(
  "AGE_AT_INDEX", "AGE_GROUP", "SEX", "RACE", "HISPANIC",
  lab_vars, comorbid_keep, "FOLLOWUP_YEARS"
)

tbl1 <- patient_t1 %>%
  select(all_of(t1_body_vars), VITAL_STATUS) %>%
  tbl_summary(
    by = VITAL_STATUS,
    missing = "ifany",
    missing_text = "Missing",
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_overall(last = FALSE)

# Descriptive p-values (died vs alive). Wrapped: skip cleanly if any var chokes.
tbl1 <- tryCatch(add_p(tbl1), error = function(e) tbl1)

tbl1 <- tbl1 %>%
  bold_labels() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(
    all_stat_cols() ~ "**Vital status at end of follow-up**"
  ) %>%
  modify_caption(
    "**Table 1. Baseline characteristics of the ALS multidisciplinary-care cohort at index diagnosis**"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ paste(
      "Median (IQR) for continuous variables; n (%) for categorical.",
      "One row per patient; status and follow-up from the un-censored reference arm."
    )
  )


# ------------------------------------------------------------------------------
# 6. Cohort-at-a-glance (CCW structure + outcome)
# ------------------------------------------------------------------------------

n_patients <- n_distinct(ccw$PATID)
n_clones   <- if ("CLONE_ID" %in% names(ccw)) n_distinct(ccw$CLONE_ID) else NA_integer_
n_rows     <- nrow(ccw)
n_deaths   <- sum(patient_outcome$DIED)
person_yrs <- sum(patient_outcome$FOLLOWUP_DAYS, na.rm = TRUE) / 365.25
fu_q       <- quantile(patient_t1$FOLLOWUP_YEARS, c(.25, .5, .75), na.rm = TRUE)
age_q      <- quantile(patient_t1$AGE_AT_INDEX, c(.25, .5, .75), na.rm = TRUE)

cohort_summary <- tribble(
  ~Metric,                              ~Value,
  "Unique ALS patients",                scales::comma(n_patients),
  paste0("Strategy clones (", paste(range(strategy_levels), collapse = "-"), ")"),
  ifelse(is.na(n_clones), "-", scales::comma(n_clones)),
  "Person-period rows",                 scales::comma(n_rows),
  "Deaths",                             scales::comma(n_deaths),
  "Died, %",                            scales::percent(n_deaths / n_patients, 0.1),
  "Person-years of follow-up",          scales::comma(round(person_yrs)),
  "Median follow-up, years (IQR)",
  sprintf("%.1f (%.1f-%.1f)", fu_q[2], fu_q[1], fu_q[3]),
  "Median age at diagnosis, years (IQR)",
  sprintf("%.0f (%.0f-%.0f)", age_q[2], age_q[1], age_q[3])
)

cohort_gt <- cohort_summary %>%
  gt() %>%
  tab_header(
    title = md("**ALS multidisciplinary-care CCW cohort**"),
    subtitle = md("Clone-censor-weight analytic dataset at a glance")
  ) %>%
  cols_label(Metric = "", Value = "") %>%
  cols_align("left", Metric) %>%
  cols_align("right", Value) %>%
  tab_options(table.font.size = px(16), heading.title.font.size = px(20))

write_csv(cohort_summary, file.path(OUTPUT_DIR, "T1_cohort_summary.csv"))


# ------------------------------------------------------------------------------
# 7. Observed adherence to each strategy's composition by grace end
# ------------------------------------------------------------------------------

adherence_tbl <- if (all(GRACE_ANALYTIC_COLS %in% names(ccw))) {
  
  ccw %>%
    filter(
      INTERVAL_START <= GRACE_END_DATE,
      INTERVAL_END >= GRACE_END_DATE,
      ANALYTIC_END_DATE > GRACE_END_DATE
    ) %>%
    arrange(PATID, STRATEGY, INTERVAL) %>%
    group_by(PATID, STRATEGY) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    group_by(STRATEGY) %>%
    summarise(
      N_eligible = n(),
      N_adherent = sum(ADHERENT_AT_GRACE_END == 1, na.rm = TRUE),
      Rate = mean(ADHERENT_AT_GRACE_END == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(STRATEGY = factor(as.character(STRATEGY), levels = strategy_levels)) %>%
    arrange(STRATEGY)
  
} else {
  NULL
}

if (!is.null(adherence_tbl)) {
  
  adherence_display <- adherence_tbl %>%
    transmute(
      Strategy = recode(as.character(STRATEGY), !!!strategy_labels),
      `N eligible` = N_eligible,
      `N adherent` = N_adherent,
      `Adherence` = scales::percent(Rate, 0.1)
    )
  
  write_csv(adherence_tbl, file.path(OUTPUT_DIR, "T1_adherence_by_strategy.csv"))
}


# ------------------------------------------------------------------------------
# 8. flextables for slides (talk styling) + PPTX
# ------------------------------------------------------------------------------

style_ft <- function(ft) {
  ft %>%
    theme_booktabs() %>%
    fontsize(size = 13, part = "all") %>%
    bold(part = "header") %>%
    padding(padding = 4, part = "all") %>%
    autofit()
}

t1_ft <- tbl1 %>% as_flex_table() %>% style_ft()

cohort_ft <- cohort_summary %>%
  flextable() %>%
  set_header_labels(Metric = "", Value = "") %>%
  style_ft() %>%
  bold(j = 1, part = "body")

slides <- list(`Table 1 - Baseline` = t1_ft, `Cohort summary` = cohort_ft)

if (!is.null(adherence_tbl)) {
  slides[["Adherence by strategy"]] <- adherence_display %>%
    flextable() %>%
    style_ft()
}

flextable::save_as_pptx(
  values = slides,
  path = file.path(OUTPUT_DIR, "als_ccw_tables.pptx")
)


# ------------------------------------------------------------------------------
# 9. HTML (always) + PNG (best effort; needs webshot2/chromote)
# ------------------------------------------------------------------------------

gt::gtsave(as_gt(tbl1), file.path(OUTPUT_DIR, "T1_baseline_characteristics.html"))
gt::gtsave(cohort_gt,   file.path(OUTPUT_DIR, "T1_cohort_summary.html"))

save_png_quietly <- function(gt_obj, file) {
  tryCatch(
    gt::gtsave(gt_obj, file),
    error = function(e) message("PNG export skipped (", conditionMessage(e), "): ", file)
  )
}

save_png_quietly(as_gt(tbl1), file.path(OUTPUT_DIR, "T1_baseline_characteristics.png"))
save_png_quietly(cohort_gt,   file.path(OUTPUT_DIR, "T1_cohort_summary.png"))

if (!is.null(adherence_tbl)) {
  adherence_gt <- adherence_display %>%
    gt() %>%
    tab_header(title = md("**Observed adherence to assigned MDT strategy**")) %>%
    tab_options(table.font.size = px(15))
  gt::gtsave(adherence_gt, file.path(OUTPUT_DIR, "T1_adherence_by_strategy.html"))
  save_png_quietly(adherence_gt, file.path(OUTPUT_DIR, "T1_adherence_by_strategy.png"))
}

reset_gtsummary_theme()


# ------------------------------------------------------------------------------
# 10. Console echo
# ------------------------------------------------------------------------------

cat("\n=======================================\n")
cat("TABLE 1 & COHORT SUMMARIES\n")
cat("=======================================\n\n")

print(cohort_summary)

cat("\nBaseline Table 1:\n")
print(tbl1)

if (!is.null(adherence_tbl)) {
  cat("\nAdherence by strategy:\n")
  print(adherence_display)
}

cat("\nOutputs (incl. slide-ready PPTX) written to:\n",
    normalizePath(OUTPUT_DIR), "\n")