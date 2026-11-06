
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
#   * Vital status and follow-up use the original patient end dates, not the
#     potentially artificially censored reference-arm intervals.
#
# Outputs (in OUTPUT_DIR):
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

# Match the common cohort restriction in the main analysis.
ccw <- readRDS("./data/als_mdt_ccw.rda") %>%
  filter(DEATH_IND == 1 | CENSOR_YEAR <= 2020)

if (!exists("REFERENCE_STRATEGY")) REFERENCE_STRATEGY <- "A"
if (!exists("strategy_levels")) {
  strategy_levels <- sort(unique(as.character(ccw$STRATEGY)))
}
if (!exists("strategy_labels")) {
  strategy_labels <- set_names(strategy_levels, strategy_levels)
}
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "als_mdt_ccw_results"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

required <- c("PATID", "STRATEGY", "INTERVAL", "EVENT", "TSTOP",
              "AGE_AT_INDEX", "SEX", "RACE", "HISPANIC")
missing_required <- setdiff(required, names(ccw))
if (length(missing_required)) {
  stop("Missing CCW columns: ", paste(missing_required, collapse = ", "))
}
if (!REFERENCE_STRATEGY %in% as.character(ccw$STRATEGY)) {
  stop("REFERENCE_STRATEGY not found in ccw$STRATEGY")
}

GRACE_ANALYTIC_COLS <- c("INTERVAL_START", "INTERVAL_END",
                         "GRACE_END_DATE", "ANALYTIC_END_DATE")
COMPLEXITY_ORDER <- LETTERS[1:8]  # least to most complex: A < ... < H
FIXED_GRACE_DAYS <- 180L

# 2. Patient-level baseline and natural-course outcome -------------------------
ref_arm <- ccw %>%
  filter(as.character(STRATEGY) == REFERENCE_STRATEGY)

patient_baseline <- ref_arm %>%
  arrange(PATID, INTERVAL) %>%
  group_by(PATID) %>%
  slice_head(n = 1) %>%
  ungroup()

# Natural outcome / follow-up, regardless of reference-arm artificial censoring.
patient_outcome <- patient_baseline %>%
  transmute(
    PATID,
    DIED = as.integer(!is.na(DEATH_IND) & DEATH_IND == 1 & CENSOR_DATE <= ANALYTIC_END_DATE),
    FOLLOWUP_DAYS = OBSERVED_FOLLOWUP_DAYS + 1
  )

# 3. Select Table 1 variables -------------------------------------------------
comorbid_vars <- stringr::str_subset(names(patient_baseline),
                                     "^IPCW_.*_PRIOR$")
lab_vars <- stringr::str_subset(names(patient_baseline),
                                "^IPCW_(BMI|CBC|METABOLIC)_.*_PRIOR_LAST$")

patient_t1 <- patient_baseline %>%
  select(PATID, AGE_AT_INDEX, SEX, RACE, HISPANIC,
         any_of(c(comorbid_vars, lab_vars))) %>%
  left_join(patient_outcome, by = "PATID") %>%
  # Snowflake extracts may represent laboratory results as character strings.
  # Explicit conversion prevents gtsummary from treating each measured value
  # as a separate categorical level and launching a huge Fisher exact test.
  mutate(across(all_of(lab_vars), function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  })) %>%
  mutate(
    FOLLOWUP_YEARS = FOLLOWUP_DAYS / 365.25,
    VITAL_STATUS = factor(
      case_when(DIED == 1 ~ "Died", DIED == 0 ~ "Alive / censored"),
      levels = c("Died", "Alive / censored")
    ),
    AGE_GROUP = cut(AGE_AT_INDEX, breaks = c(-Inf, 55, 65, 75, Inf),
                    labels = c("<55", "55-64", "65-74", "75+"),
                    right = FALSE)
  )

# Select informative comorbidity indicators (2–98% prevalence).
comorbid_keep <- comorbid_vars %>%
  purrr::keep(function(v) {
    values <- suppressWarnings(as.numeric(as.character(patient_t1[[v]])))
    p <- mean(values > 0, na.rm = TRUE)
    is.finite(p) && p >= 0.02 && p <= 0.98
  })
patient_t1 <- patient_t1 %>%
  mutate(across(all_of(comorbid_keep), function(x) {
    v <- suppressWarnings(as.numeric(as.character(x)))
    if_else(is.na(v), NA, v > 0)
  }))

# 4. Readable labels ----------------------------------------------------------
pretty_label <- function(x) {
  x %>%
    str_remove("^IPCW_") %>%
    str_remove("_PRIOR(_LAST)?$") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}
label_map <- c(
  AGE_AT_INDEX = "Age at ALS diagnosis, years",
  AGE_GROUP = "Age group, years",
  SEX = "Sex", RACE = "Race", HISPANIC = "Hispanic ethnicity",
  FOLLOWUP_YEARS = "Follow-up, years",
  set_names(pretty_label(comorbid_keep), comorbid_keep),
  set_names(pretty_label(lab_vars), lab_vars)
)
patient_t1 <- labelled::set_variable_labels(
  patient_t1, .labels = as.list(label_map), .strict = FALSE
)

# 5. Observed A versus all others ---------------------------------------------
# An A clone may be artificially censored before grace end when observed care
# deviates from A. Classify using evidence from ALL surviving clones. Absence
# of A alone is NOT evidence of another strategy (death/loss to follow-up can
# also remove A). At least one assessable non-A clone is needed for "All others".
grace_required <- c(GRACE_ANALYTIC_COLS, "ADHERENT_AT_GRACE_END",
                    "INDEX_DATE")
grace_missing <- setdiff(grace_required, names(ccw))
if (length(grace_missing)) {
  stop("A-vs-others needs grace-end columns: ",
       paste(grace_missing, collapse = ", "))
}
if (any(!is.na(ccw$GRACE_END_DATE) & !is.na(ccw$INDEX_DATE) &
        as.numeric(ccw$GRACE_END_DATE - ccw$INDEX_DATE) != FIXED_GRACE_DAYS)) {
  stop("GRACE_END_DATE is not uniformly INDEX_DATE + 180 days. Check data.")
}
unknown_strategies <- setdiff(unique(as.character(ccw$STRATEGY)),
                              COMPLEXITY_ORDER)
if (length(unknown_strategies)) {
  stop("Unrecognized strategy codes: ", paste(unknown_strategies, collapse = ", "))
}

# Preserve every baseline-eligible clone. ADHERENT_AT_GRACE_END is assumed to
# summarize achievement by day 180, even when its clone was censored earlier;
# do not require an interval to span day 180 to read a recorded indicator.
clone_grace_summary <- ccw %>%
  mutate(
    GRACE_INTERVAL = !is.na(INTERVAL_START) & !is.na(INTERVAL_END) &
      !is.na(GRACE_END_DATE) &
      INTERVAL_START <= GRACE_END_DATE &
      INTERVAL_END >= GRACE_END_DATE,
    GRACE_ASSESSED_ROW = !is.na(ADHERENT_AT_GRACE_END),
    GRACE_ADHERENT_ROW = GRACE_ASSESSED_ROW & ADHERENT_AT_GRACE_END == 1
  ) %>%
  group_by(PATID, STRATEGY) %>%
  summarise(
    N_RECORDS = n(),
    HAS_GRACE_INTERVAL = any(GRACE_INTERVAL, na.rm = TRUE),
    N_FLAG_VALUES = n_distinct(ADHERENT_AT_GRACE_END, na.rm = TRUE),
    ASSESSED = any(GRACE_ASSESSED_ROW, na.rm = TRUE),
    ADHERENT = any(GRACE_ADHERENT_ROW, na.rm = TRUE),
    .groups = "drop"
  )
if (any(clone_grace_summary$N_FLAG_VALUES > 1L)) {
  stop("ADHERENT_AT_GRACE_END varies across intervals within a clone. ",
       "Derive one definitive day-180 achievement value before classification.")
}

# Interpret a positive ADHERENT_AT_GRACE_END as evidence that the corresponding
# strategy was ACTUALLY ACHIEVED by day 180. Validate that interpretation against
# the CCW derivation: mere compatibility with a hypothetical clone is not enough.
# Highest achieved complexity wins, regardless of whether A is also compatible.
grace_clone_status <- clone_grace_summary %>%
  mutate(COMPLEXITY_RANK = match(as.character(STRATEGY), COMPLEXITY_ORDER)) %>%
  group_by(PATID) %>%
  summarise(
    MAX_REACHED_RANK = if (any(ASSESSED & ADHERENT))
      max(COMPLEXITY_RANK[ASSESSED & ADHERENT]) else NA_integer_,
    .groups = "drop"
  ) %>%
  mutate(MOST_COMPLEX_REACHED = COMPLEXITY_ORDER[MAX_REACHED_RANK])

patient_t1 <- patient_t1 %>%
  left_join(grace_clone_status, by = "PATID") %>%
  mutate(
    OBSERVED_STRATEGY_GROUP = factor(
      case_when(
        MOST_COMPLEX_REACHED == "A" ~ "Strategy A",
        MOST_COMPLEX_REACHED %in% LETTERS[2:8] ~ "All others",
        TRUE ~ "Not assessable"
      ),
      levels = c("Strategy A", "All others", "Not assessable")
    )
  )
stopifnot(nrow(patient_t1) == n_distinct(patient_t1$PATID))

# 6. gtsummary setup and tables -----------------------------------------------
theme_gtsummary_compact()
t1_baseline_vars <- unique(c("AGE_AT_INDEX", "AGE_GROUP", "SEX", "RACE",
                             "HISPANIC", lab_vars, comorbid_keep))

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
      "One row per patient; status and follow-up use the natural or administrative end date."
    )
  )


# ------------------------------------------------------------------------------
# 6. Cohort-at-a-glance (CCW structure + outcome)
# ------------------------------------------------------------------------------

n_patients <- n_distinct(ccw$PATID)
n_clones <- if ("CLONE_ID" %in% names(ccw)) n_distinct(ccw$CLONE_ID) else NA_integer_
n_deaths <- sum(patient_outcome$DIED, na.rm = TRUE)
person_yrs <- sum(patient_outcome$FOLLOWUP_DAYS, na.rm = TRUE) / 365.25

cohort_summary <- tibble(
  Metric = c(
    "Unique ALS patients", "Strategy clones", "Person-period rows",
    "Deaths", "Died, %", "Person-years of follow-up",
    "Median follow-up, years (IQR)",
    "Median age at diagnosis, years (IQR)",
    "Strategy A observed / assessable", "All others observed / assessable",
    "Grace-end adherence not assessable"
  ),
  Value = c(
    scales::comma(n_patients),
    if (is.na(n_clones)) "-" else scales::comma(n_clones),
    scales::comma(nrow(ccw)), scales::comma(n_deaths),
    scales::percent(n_deaths / n_patients, accuracy = .1),
    scales::comma(round(person_yrs)),
    sprintf("%.1f (%.1f–%.1f)", fu_q[2], fu_q[1], fu_q[3]),
    sprintf("%.0f (%.0f–%.0f)", age_q[2], age_q[1], age_q[3]),
    scales::comma(sum(patient_t1$OBSERVED_STRATEGY_GROUP == "Strategy A")),
    scales::comma(sum(patient_t1$OBSERVED_STRATEGY_GROUP == "All others")),
    scales::comma(sum(patient_t1$OBSERVED_STRATEGY_GROUP == "Not assessable"))
  )
)
cohort_gt <- cohort_summary %>%
  gt() %>%
  tab_header(title = md("**ALS multidisciplinary-care CCW cohort**"),
             subtitle = md("Clone–censor–weight dataset at a glance")) %>%
  cols_label(Metric = "", Value = "") %>%
  cols_align("left", columns = Metric) %>%
  cols_align("right", columns = Value) %>%
  tab_options(table.font.size = px(16), heading.title.font.size = px(20))

# 8. Adherence by strategy ----------------------------------------------------
adherence_tbl <- NULL
if (all(c(GRACE_ANALYTIC_COLS, "ADHERENT_AT_GRACE_END") %in% names(ccw))) {
  adherence_tbl <- clone_grace_summary %>%
    group_by(STRATEGY) %>%
    summarise(
      N_eligible = n(),
      N_grace_observed = sum(HAS_GRACE_INTERVAL),
      N_assessed = sum(ASSESSED),
      N_adherent = sum(ADHERENT & ASSESSED),
      Rate_assessed = if (N_assessed > 0) N_adherent / N_assessed
      else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(STRATEGY = factor(as.character(STRATEGY), levels = strategy_levels)) %>%
    arrange(STRATEGY)
}

# 9. HTML and CSV exports -----------------------------------------------------
save_summary <- function(tbl, stem) {
  gt::gtsave(gtsummary::as_gt(tbl),
             file.path(OUTPUT_DIR, paste0(stem, ".html")))
  readr::write_csv(gtsummary::as_tibble(tbl, col_labels = TRUE),
                   file.path(OUTPUT_DIR, paste0(stem, ".csv")))
}
save_summary(tbl1, "T1_baseline_characteristics")
save_summary(tbl1_a, "T1_baseline_A_vs_others")

gt::gtsave(cohort_gt, file.path(OUTPUT_DIR, "T1_cohort_summary.html"))
readr::write_csv(cohort_summary,
                 file.path(OUTPUT_DIR, "T1_cohort_summary.csv"))

if (!is.null(adherence_tbl)) {
  adherence_display <- adherence_tbl %>%
    transmute(
      Strategy = recode(as.character(STRATEGY), !!!strategy_labels),
      `N eligible` = N_eligible,
      `N grace observed` = N_grace_observed,
      `N assessed` = N_assessed,
      `N adherent` = N_adherent,
      `Adherence` = scales::percent(Rate, 0.1)
    )
  
  write_csv(adherence_tbl, file.path(OUTPUT_DIR, "T1_adherence_by_strategy.csv"))
}

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
    tab_header(title = md("**Observed adherence to MDT strategies**")) %>%
    tab_options(table.font.size = px(15))
  gt::gtsave(adherence_gt,
             file.path(OUTPUT_DIR, "T1_adherence_by_strategy.html"))
  readr::write_csv(adherence_tbl,
                   file.path(OUTPUT_DIR, "T1_adherence_by_strategy.csv"))
}

# Each baseline clone remains represented, including early deaths and censoring.
readr::write_csv(
  clone_grace_summary,
  file.path(OUTPUT_DIR, "T1_clone_grace_assessment_audit.csv")
)

# Patient-level audit of A-versus-others classification.
readr::write_csv(
  patient_t1 %>% select(PATID, MAX_REACHED_RANK, MOST_COMPLEX_REACHED,
                        OBSERVED_STRATEGY_GROUP),
  file.path(OUTPUT_DIR, "T1_patient_strategy_classification.csv")
)

reset_gtsummary_theme()

# 10. Console summary ---------------------------------------------------------
cat("\n=======================================\n")
cat("ALS MDT CCW — TABLES AND SUMMARIES\n")
cat("=======================================\n\n")
print(cohort_summary)
cat("\nObserved A vs. others classification:\n")
print(patient_t1 %>% count(OBSERVED_STRATEGY_GROUP, .drop = FALSE))
cat("\nBaseline by vital status:\n")
print(tbl1)
cat("\nBaseline by highest MDT strategy reached by day 180:\n")
print(tbl1_a)
if (!is.null(adherence_tbl)) {
  cat("\nAdherence by strategy:\n")
  print(adherence_display)
}
cat("\nHTML and CSV files written to:\n", normalizePath(OUTPUT_DIR), "\n")



