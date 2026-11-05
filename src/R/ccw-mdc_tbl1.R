
# ==============================================================================
# ALS MDT CCW COHORT — TABLE 1 AND PRESENTATION SUMMARIES
# Outputs: HTML and CSV only (no PNG)
# ==============================================================================

# One row per unique patient for baseline descriptions. Every patient is cloned
# into every strategy; clone STRATEGY is NOT an observed treatment group.
# For the A-versus-others baseline table, each patient is classified by the
# highest COMPLEXITY strategy achieved by day 180 (H > G > ... > A). This is an
# observed post-index grouping, not the randomized/assigned clone strategy.

pacman::p_load(tidyverse, gtsummary, gt, labelled, scales)

# 1. Configuration ------------------------------------------------------------
if (!exists("ccw")) {
  ccw <- readRDS("./data/als_mdt_ccw_60_180.rda")  # RDS-format object despite suffix
}
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

# Assumes the reference arm is not artificially censored. Verify in the CCW
# construction; if A is censored for deviation, outcomes are not natural-course.
patient_outcome <- ref_arm %>%
  group_by(PATID) %>%
  summarise(
    DIED = as.integer(any(EVENT == 1, na.rm = TRUE)),
    FOLLOWUP_DAYS = if (all(is.na(TSTOP))) NA_real_
    else max(TSTOP, na.rm = TRUE),
    .groups = "drop"
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

make_t1 <- function(data, by_var, vars, caption, span, note, test = TRUE) {
  tab <- data %>%
    select(all_of(c(vars, by_var))) %>%
    tbl_summary(
      by = all_of(by_var),
      missing = "ifany", missing_text = "Missing",
      type = list(all_of(lab_vars) ~ "continuous"),
      statistic = list(
        all_continuous() ~ "{median} ({p25}, {p75})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1
    ) %>%
    add_overall(last = FALSE)
  if (test) {
    # Fisher's exact test is computationally prohibitive for large/sparse
    # contingency tables (FEXACT workspace errors). Use rank-sum tests for
    # continuous variables and simulated chi-square tests for categorical data.
    tab <- tryCatch(add_p(
      tab,
      test = list(
        all_continuous() ~ "wilcox.test",
        all_categorical() ~ "chisq.test"
      ),
      test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 2000)
    ), error = function(e) {
      warning("Could not add p-values: ", conditionMessage(e))
      tab
    })
  }
  tab %>%
    bold_labels() %>%
    modify_header(label ~ "**Characteristic**") %>%
    modify_spanning_header(all_stat_cols() ~ span) %>%
    modify_caption(caption) %>%
    modify_footnote(all_stat_cols() ~ note)
}

tbl1 <- make_t1(
  patient_t1, "VITAL_STATUS", c(t1_baseline_vars, "FOLLOWUP_YEARS"),
  "**Table 1. ALS cohort characteristics by vital status**",
  "**Vital status at end of follow-up**",
  paste("Median (IQR) or n (%); one row per patient.",
        "Follow-up and vital status are derived from the reference clone.")
)

patient_t1_a <- patient_t1 %>%
  filter(OBSERVED_STRATEGY_GROUP != "Not assessable") %>%
  droplevels()
if (nrow(patient_t1_a) == 0L) {
  stop("No patients have an assessable highest achieved strategy by day 180. ",
       "Check ADHERENT_AT_GRACE_END derivation and interval endpoints.")
}
has_both_groups <- n_distinct(patient_t1_a$OBSERVED_STRATEGY_GROUP) == 2L
if (!has_both_groups) {
  warning("Only one achieved-strategy group observed; Table 2 will show that ",
          "group without a between-group p-value.")
}

tbl1_a <- make_t1(
  patient_t1_a, "OBSERVED_STRATEGY_GROUP", t1_baseline_vars,
  "**Table 2. Baseline characteristics by highest MDT strategy achieved within 180 days**",
  "**Strategy A versus B–H (highest achieved)**",
  paste("Median (IQR) or n (%); one row per patient.",
        "A is least complex; H is most complex.",
        "Post-index descriptive classification; unassessable patients excluded."),
  test = has_both_groups
)

# 7. Cohort overview ----------------------------------------------------------
safe_quantile <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, 3L))
  unname(quantile(x, probs = c(.25, .5, .75), na.rm = TRUE))
}
fu_q <- safe_quantile(patient_t1$FOLLOWUP_YEARS)
age_q <- safe_quantile(patient_t1$AGE_AT_INDEX)
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
      `Adherence among assessed` = scales::percent(Rate_assessed, accuracy = .1)
    )
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



