# ==============================================================================
# ALS MDT CLONE-CENSOR-WEIGHT ANALYSIS  (tidyverse edition)
#
# Input:
#   GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_MDT_CCW_90D
#
# Strategies (branching lattice, not a single cumulative ladder):
#   A = Neurology-centered care
#   B = A + Rehab
#   C = A + SLP
#   D = A + Respiratory
#   E = B + SLP           (Rehab + SLP)
#   F = B + Respiratory   (Rehab + Respiratory)
#   G = E + Nutrition     (Rehab + SLP + Nutrition)
#   H = F + Nutrition     (Rehab + Respiratory + Nutrition)
#
# Outcome:
#   Overall survival from ALS1DX_DATE
#
# CCW:
#   - all patients cloned into strategies A-E
#   - B-H artificially censored at grace-period end if required
#     composition not achieved
#   - stabilized IPCW used after grace period
#
# Data manipulation is tidyverse throughout (dplyr / tidyr / purrr / stringr /
# readr). Modelling packages without a tidyverse analogue are retained:
#   - survival  (coxph, survfit, cox.zph)
#   - glmnet    (ridge logistic denominator)
#   - broom     (tidy model output)
#   - patchwork (figure composition)
#
# Note on scale: the person-period table is expanded 5x by cloning. dplyr will
# handle it, but for very large inputs a data.table or dtplyr backend for the
# weighting joins/aggregations will be materially faster.
# ==============================================================================


# ==============================================================================
# 0. PACKAGES
# ==============================================================================
pacman::p_load(
  tidyverse,
  survival,
  glmnet,
  patchwork,
  broom,
  scales
)


# ==============================================================================
# 1. ANALYSIS CONFIGURATION
# ==============================================================================

# ------------------------------------------------------------------------------
# Input
# ------------------------------------------------------------------------------

ccw <- readRDS("./data/als_mdt_ccw.rda")


# ------------------------------------------------------------------------------
# Grace period
# ------------------------------------------------------------------------------

GRACE_DAYS <- 180


# ------------------------------------------------------------------------------
# Reference strategy
# ------------------------------------------------------------------------------

REFERENCE_STRATEGY <- "A"

strategy_levels <- c("A", "B", "C", "D", "E", "F", "G", "H")

# Every non-reference arm is IPC-weighted; the reference arm carries weight 1.
treated_strategies <- setdiff(strategy_levels, REFERENCE_STRATEGY)

strategy_labels <- c(
  A = "Neurology-centered",
  B = "+ Rehab",
  C = "+ SLP",
  D = "+ Respiratory",
  E = "+ Rehab + SLP",
  F = "+ Rehab + Respiratory",
  G = "+ Rehab + SLP + Nutrition",
  H = "+ Rehab + Respiratory + Nutrition"
)


# ------------------------------------------------------------------------------
# IPCW settings
# ------------------------------------------------------------------------------

# Ridge regression is used for the denominator.
RIDGE_ALPHA <- 0

# Prediction truncation avoids division by probabilities extremely near zero.
PRED_PROB_MIN <- 0.01
PRED_PROB_MAX <- 0.99

# Weight truncation.
WEIGHT_LOWER_Q <- 0.01
WEIGHT_UPPER_Q <- 0.99

# Use counts as well as binary histories?
#
# FALSE is recommended initially because:
#   *_PRIOR already captures presence/history,
#   counts can add substantial collinearity/utilization intensity.
USE_EVENT_COUNTS <- FALSE

# Composite MDT histories can nearly deterministically encode whether a strategy
# was achieved. Do not include them by default in the denominator.
#
# The requested constituent PHE_TYPE variables such as FunctionalCare and
# OccupationalTherapy are still included through IPCW_* columns.
INCLUDE_MDT_HISTORY <- FALSE


# ------------------------------------------------------------------------------
# Survival reporting times
# ------------------------------------------------------------------------------

SURVIVAL_YEARS <- c(1, 2, 3, 5)

SURVIVAL_TIMES <- round(SURVIVAL_YEARS * 365.25)


# ------------------------------------------------------------------------------
# RMST horizon
# ------------------------------------------------------------------------------

RMST_YEARS <- 3

RMST_TAU <- round(RMST_YEARS * 365.25)


# ------------------------------------------------------------------------------
# Output directory
# ------------------------------------------------------------------------------

OUTPUT_DIR <- "als_mdt_ccw_results"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# 2. BASIC DATA PREPARATION
# ==============================================================================

ccw <- ccw %>%
  mutate(
    STRATEGY = factor(STRATEGY, levels = strategy_levels),
    PATID = as.character(PATID)
  ) %>%
  filter(
    (STRATEGY=='A' & (DEATH_IND == 1 | CENSOR_YEAR <= 2020)) | STRATEGY != 'A'
  )


# ==============================================================================
# 3. STRUCTURAL VALIDATION
# ==============================================================================

required_columns <- c(
  "PATID", "CLONE_ID", "STRATEGY", "INTERVAL",
  "INTERVAL_START", "INTERVAL_END", "GRACE_END_DATE",
  "ADHERENT_AT_GRACE_END", "ARTIFICIAL_CENSOR", "ARTIFICIAL_CENSOR_DATE",
  "TSTART", "TSTOP", "EVENT", "DEATH_IND",
  "AGE_AT_INDEX", "SEX", "RACE", "HISPANIC"
)

missing_columns <- setdiff(required_columns, names(ccw))

if (length(missing_columns) > 0) {
  stop(
    paste("Missing required columns:", paste(missing_columns, collapse = ", "))
  )
}


# ------------------------------------------------------------------------------
# Basic checks
# ------------------------------------------------------------------------------

cat("\n=======================================\n")
cat("STRUCTURAL CHECKS\n")
cat("=======================================\n")

cat("Unique patients:", n_distinct(ccw$PATID), "\n")
cat("Unique clones:", n_distinct(ccw$CLONE_ID), "\n")
cat(
  "Strategies:",
  paste(sort(unique(as.character(ccw$STRATEGY))), collapse = ", "),
  "\n"
)


# Each patient should normally have eight initial clones (A-H).

clone_counts <- ccw %>%
  distinct(PATID, STRATEGY) %>%
  count(PATID, name = "N_STRATEGIES") %>%
  count(N_STRATEGIES, name = "N_PATIENTS") %>%
  arrange(N_STRATEGIES)

print(clone_counts)


# ------------------------------------------------------------------------------
# Event count by strategy
# ------------------------------------------------------------------------------

event_diagnostics <- ccw %>%
  group_by(STRATEGY) %>%
  summarise(
    N_PATIENT_ROWS = n(),
    DEATHS = sum(EVENT, na.rm = TRUE),
    ARTIFICIAL_CENSORS = sum(ARTIFICIAL_CENSOR, na.rm = TRUE),
    .groups = "drop"
  )

print(event_diagnostics)


# Verify at most one death per clone.

death_per_clone <- ccw %>%
  group_by(PATID, STRATEGY) %>%
  summarise(N_EVENT = sum(EVENT, na.rm = TRUE), .groups = "drop")

if (any(death_per_clone$N_EVENT > 1)) {
  stop("Some patient-strategy clones contain more than one survival event.")
}


# Verify counting-process intervals.

bad_intervals <- ccw %>% filter(TSTOP <= TSTART)

if (nrow(bad_intervals) > 0) {
  warning(nrow(bad_intervals), " rows have TSTOP <= TSTART.")
}


# ==============================================================================
# 4. GRACE-PERIOD ADHERENCE DIAGNOSTICS
# ==============================================================================

# One row per clone containing the grace-period endpoint.
#
# Restrict to people who could actually remain at risk beyond grace.
# Patients who die or naturally end follow-up before/equal grace do not require
# a grace-period adherence probability for post-grace survival.

grace_risk <- ccw %>%
  filter(
    INTERVAL_START <= GRACE_END_DATE,
    INTERVAL_END >= GRACE_END_DATE,
    ANALYTIC_END_DATE > GRACE_END_DATE
  ) %>%
  arrange(PATID, STRATEGY, INTERVAL) %>%
  group_by(PATID, STRATEGY) %>%
  slice_head(n = 1) %>%          # one row per eligible clone
  ungroup()


# ------------------------------------------------------------------------------
# Adherence rates
# ------------------------------------------------------------------------------

adherence_summary <- grace_risk %>%
  group_by(STRATEGY) %>%
  summarise(
    N_AT_GRACE = n(),
    N_ADHERENT = sum(ADHERENT_AT_GRACE_END == 1, na.rm = TRUE),
    ADHERENCE_RATE = mean(ADHERENT_AT_GRACE_END == 1, na.rm = TRUE),
    .groups = "drop"
  )

print(adherence_summary)

write_csv(adherence_summary, file.path(OUTPUT_DIR, "01_strategy_adherence.csv"))


# ==============================================================================
# 5. IDENTIFY IPCW COVARIATES
# ==============================================================================

baseline_vars <- c("AGE_AT_INDEX", "SEX", "RACE", "HISPANIC")

# Binary / historical PHE covariates
phe_prior_vars <- str_subset(names(grace_risk), "^IPCW_.*_PRIOR$")

# Optional PHE event counts
phe_count_vars <- str_subset(names(grace_risk), "^IPCW_.*_PRIOR_N$")

# Most-recent prior lab values
lab_vars <- str_subset(
  names(grace_risk),
  "^IPCW_(BMI|CBC|METABOLIC)_.*_PRIOR_LAST$"
)

# MDT history
mdt_vars <- str_subset(names(grace_risk), "^MDT_.*_PRIOR$")


denominator_vars <- c(baseline_vars, phe_prior_vars, lab_vars)

if (USE_EVENT_COUNTS) {
  denominator_vars <- c(denominator_vars, phe_count_vars)
}

if (INCLUDE_MDT_HISTORY) {
  denominator_vars <- c(denominator_vars, mdt_vars)
}

denominator_vars <- denominator_vars %>%
  unique() %>%
  keep(~ .x %in% names(grace_risk))


cat("\nNumber of IPCW denominator variables:", length(denominator_vars), "\n")
cat("Baseline:", length(baseline_vars), "\n")
cat("PHE histories:", length(phe_prior_vars), "\n")
cat("Lab most-recent values:", length(lab_vars), "\n")


# ==============================================================================
# 6. COVARIATE PREPROCESSING
#
# Continuous missing values:
#   median imputation + missingness indicator
#
# Categorical missing values:
#   explicit "Missing" category
#
# This allows patients with missing labs to remain in the IPCW model.
# ==============================================================================

prepare_ipcw_matrix <- function(dt, variables) {
  
  x <- dt %>% select(all_of(variables))
  
  # ---------------------------------------------------------------------------
  # Logical -> integer
  # ---------------------------------------------------------------------------
  x <- x %>% mutate(across(where(is.logical), as.integer))
  
  # ---------------------------------------------------------------------------
  # Character / factor -> factor with explicit "Missing"
  # ---------------------------------------------------------------------------
  x <- x %>% mutate(across(
    where(~ is.character(.x) || is.factor(.x)),
    function(col) {
      z <- as.character(col)
      z[is.na(z) | trimws(z) == ""] <- "Missing"
      factor(z)
    }
  ))
  
  # ---------------------------------------------------------------------------
  # Numeric: missingness indicators (only where any values are missing)
  # ---------------------------------------------------------------------------
  numeric_vars <- x %>% select(where(is.numeric)) %>% names()
  
  missing_indicators <- x %>%
    select(all_of(numeric_vars)) %>%
    transmute(across(
      everything(),
      ~ as.integer(is.na(.x)),
      .names = "{.col}__MISSING"
    )) %>%
    select(where(~ any(.x == 1L)))
  
  # ---------------------------------------------------------------------------
  # Numeric: median imputation (0 if a column is entirely missing)
  # ---------------------------------------------------------------------------
  x <- x %>% mutate(across(
    all_of(numeric_vars),
    function(col) {
      if (all(is.na(col))) {
        return(replace(col, is.na(col), 0))
      }
      replace(col, is.na(col), median(col, na.rm = TRUE))
    }
  ))
  
  x <- bind_cols(x, missing_indicators)
  
  # ---------------------------------------------------------------------------
  # Model matrix
  # ---------------------------------------------------------------------------
  mm <- model.matrix(~ . - 1, data = x)
  
  # ---------------------------------------------------------------------------
  # Remove zero-variance columns
  # ---------------------------------------------------------------------------
  if (ncol(mm) > 0) {
    variances <- apply(mm, 2, var)
    keep <- is.finite(variances) & variances > 0
    mm <- mm[, keep, drop = FALSE]
  }
  
  mm
}


# ==============================================================================
# 7. FIT STRATEGY-SPECIFIC IPCW MODELS
#
#   A:      no artificial censoring; weight = 1
#   B-E:    model Pr(adherent at end of grace | covariates)
#
# Numerator:   strategy-specific marginal adherence probability
# Denominator: ridge logistic regression (baseline + time-varying history)
#
# SW = P(adherent | strategy) / P(adherent | strategy, covariate history)
# ==============================================================================

fit_ipcw_strategy <- function(
    dt,
    strategy_value,
    denominator_vars,
    probability_min = 0.01,
    probability_max = 0.99
) {
  
  d <- dt %>% filter(STRATEGY == strategy_value)
  
  if (strategy_value == REFERENCE_STRATEGY) {
    d <- d %>% mutate(P_NUM = 1, P_DEN = 1, SW_RAW = 1)
    return(list(data = d, model = NULL))
  }
  
  y <- as.integer(d$ADHERENT_AT_GRACE_END == 1)
  
  n_adherent <- sum(y == 1)
  n_nonadherent <- sum(y == 0)
  
  cat(
    "\nStrategy", strategy_value, ":",
    n_adherent, "adherent /", n_nonadherent, "nonadherent\n"
  )
  
  if (n_adherent < 5 || n_nonadherent < 5) {
    warning(
      paste0(
        "Strategy ", strategy_value,
        " has very sparse adherence/nonadherence. ",
        "IPCW estimates may be unstable."
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Numerator: marginal adherence probability within assigned clone strategy
  # ---------------------------------------------------------------------------
  p_num <- mean(y)
  p_num <- pmin(pmax(p_num, probability_min), probability_max)
  
  # ---------------------------------------------------------------------------
  # Denominator design matrix
  # ---------------------------------------------------------------------------
  x <- prepare_ipcw_matrix(d, denominator_vars)
  
  if (ncol(x) == 0) {
    stop("No usable denominator covariates for IPCW.")
  }
  
  # ---------------------------------------------------------------------------
  # Ridge logistic regression
  # ---------------------------------------------------------------------------
  nfolds <- min(10, nrow(d)) %>% max(3)
  
  set.seed(20260819 + match(strategy_value, strategy_levels))
  
  fit <- cv.glmnet(
    x = x,
    y = y,
    family = "binomial",
    alpha = RIDGE_ALPHA,
    nfolds = nfolds,
    type.measure = "deviance",
    standardize = TRUE
  )
  
  p_den <- as.numeric(
    predict(fit, newx = x, s = "lambda.1se", type = "response")
  )
  
  # ---------------------------------------------------------------------------
  # Probability truncation
  # ---------------------------------------------------------------------------
  p_den <- pmin(pmax(p_den, probability_min), probability_max)
  
  # Stabilized weights (relevant for adherent clones surviving beyond grace).
  d <- d %>%
    mutate(
      P_NUM = p_num,
      P_DEN = p_den,
      SW_RAW = P_NUM / P_DEN
    )
  
  list(data = d, model = fit)
}


# ==============================================================================
# 8. ESTIMATE IPCW FOR A-E
# ==============================================================================

ipcw_results <- strategy_levels %>%
  map(
    ~ fit_ipcw_strategy(
      dt = grace_risk,
      strategy_value = .x,
      denominator_vars = denominator_vars,
      probability_min = PRED_PROB_MIN,
      probability_max = PRED_PROB_MAX
    )
  ) %>%
  set_names(strategy_levels)


grace_weights <- ipcw_results %>%
  map("data") %>%
  bind_rows() %>%
  mutate(STRATEGY = factor(STRATEGY, levels = strategy_levels))


# ==============================================================================
# 9. WEIGHT TRUNCATION  (within strategy, among adherent clones)
# ==============================================================================

grace_weights <- grace_weights %>%
  group_by(STRATEGY) %>%
  mutate(
    .lo = quantile(
      SW_RAW[ADHERENT_AT_GRACE_END == 1], WEIGHT_LOWER_Q, na.rm = TRUE
    ),
    .hi = quantile(
      SW_RAW[ADHERENT_AT_GRACE_END == 1], WEIGHT_UPPER_Q, na.rm = TRUE
    ),
    SW_TRUNC = if_else(
      STRATEGY %in% treated_strategies & ADHERENT_AT_GRACE_END == 1,
      pmin(pmax(SW_RAW, .lo), .hi),
      SW_RAW
    )
  ) %>%
  select(-.lo, -.hi) %>%
  ungroup()


# ------------------------------------------------------------------------------
# Clone-level weight lookup
# ------------------------------------------------------------------------------

weight_lookup <- grace_weights %>%
  filter(ADHERENT_AT_GRACE_END == 1) %>%
  select(PATID, STRATEGY, P_NUM, P_DEN, SW_RAW, SW_TRUNC)


# ==============================================================================
# 10. ATTACH IPCW TO FULL PERSON-PERIOD TABLE
# ==============================================================================

ccw <- ccw %>%
  left_join(
    weight_lookup %>%
      rename(
        SW_RAW_POSTGRACE = SW_RAW,
        SW_TRUNC_POSTGRACE = SW_TRUNC
      ),
    by = c("PATID", "STRATEGY")
  )


# ------------------------------------------------------------------------------
# Final time-varying analysis weight
#
# Through grace:  weight = 1
# After grace:    A = 1 ; B-E adherent clones = stabilized weight
# ------------------------------------------------------------------------------

ccw <- ccw %>%
  mutate(
    IPCW = if_else(
      INTERVAL_START > GRACE_END_DATE & STRATEGY != REFERENCE_STRATEGY,
      SW_TRUNC_POSTGRACE,
      1
    )
  )

if (any(is.na(ccw$IPCW))) {
  stop("Missing IPCW values found in analysis rows.")
}


# ==============================================================================
# 11. IPCW / POSITIVITY DIAGNOSTICS
# ==============================================================================

effective_sample_size <- function(w) {
  w <- w[is.finite(w) & !is.na(w)]
  if (length(w) == 0) return(NA_real_)
  sum(w)^2 / sum(w^2)
}


weight_diagnostics <- grace_weights %>%
  filter(ADHERENT_AT_GRACE_END == 1) %>%
  group_by(STRATEGY) %>%
  summarise(
    N               = n(),
    P_DEN_MIN       = min(P_DEN, na.rm = TRUE),
    P_DEN_P01       = quantile(P_DEN, 0.01, na.rm = TRUE),
    P_DEN_P05       = quantile(P_DEN, 0.05, na.rm = TRUE),
    P_DEN_MEDIAN    = median(P_DEN, na.rm = TRUE),
    P_DEN_P95       = quantile(P_DEN, 0.95, na.rm = TRUE),
    P_DEN_MAX       = max(P_DEN, na.rm = TRUE),
    PROP_P_DEN_LT_005 = mean(P_DEN < 0.05),
    SW_MEAN         = mean(SW_TRUNC),
    SW_SD           = sd(SW_TRUNC),
    SW_MEDIAN       = median(SW_TRUNC),
    SW_P95          = quantile(SW_TRUNC, 0.95),
    SW_P99          = quantile(SW_TRUNC, 0.99),
    SW_MAX          = max(SW_TRUNC),
    ESS             = effective_sample_size(SW_TRUNC),
    .groups = "drop"
  )

print(weight_diagnostics)

write_csv(weight_diagnostics, file.path(OUTPUT_DIR, "02_ipcw_diagnostics.csv"))


# ==============================================================================
# 12. WEIGHT DISTRIBUTION PLOT
# ==============================================================================

plot_weight_data <- grace_weights %>%
  filter(STRATEGY != REFERENCE_STRATEGY, ADHERENT_AT_GRACE_END == 1)

p_weight <- ggplot(plot_weight_data, aes(x = SW_TRUNC, fill = STRATEGY)) +
  geom_histogram(bins = 40, alpha = 0.65, position = "identity") +
  facet_wrap(~ STRATEGY, scales = "free_y") +
  labs(
    title = "Distribution of stabilized IPC weights",
    subtitle = "Among clones adherent at the end of the grace period",
    x = "Truncated stabilized IPC weight",
    y = "Number of clones"
  ) +
  theme_bw() +
  theme(legend.position = "none")


# ==============================================================================
# 13. EFFECTIVE SAMPLE SIZE PLOT
# ==============================================================================

p_ess <- weight_diagnostics %>%
  filter(STRATEGY != REFERENCE_STRATEGY) %>%
  ggplot(aes(x = STRATEGY, y = ESS)) +
  geom_col() +
  geom_text(aes(label = round(ESS)), vjust = -0.4) +
  labs(
    title = "Effective sample size after IPC weighting",
    x = "MDT strategy",
    y = "Effective sample size"
  ) +
  theme_bw()


# ==============================================================================
# 14. ADHERENCE PLOT
# ==============================================================================

p_adherence <- ggplot(adherence_summary, aes(x = STRATEGY, y = ADHERENCE_RATE)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.1f%%", 100 * ADHERENCE_RATE)),
    vjust = -0.4
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "Observed adherence to assigned MDT strategy",
    subtitle = paste0("Required composition achieved by day ", GRACE_DAYS),
    x = "MDT strategy",
    y = "Adherent by grace-period end"
  ) +
  theme_bw()


# ==============================================================================
# 15. SELECTION-BALANCE DIAGNOSTICS
#
# Not baseline treatment balance (cloning makes that automatic). Instead:
# compare the full grace-period risk set with the selected adherent subset.
# Good IPCW should make the weighted adherent subset resemble the full
# strategy-specific grace-period risk set.
# ==============================================================================

selection_balance_one_strategy <- function(dt, strategy_value, denominator_vars) {
  
  d <- dt %>% filter(STRATEGY == strategy_value)
  
  if (strategy_value == REFERENCE_STRATEGY || nrow(d) == 0) {
    return(tibble())
  }
  
  x <- prepare_ipcw_matrix(d, denominator_vars)
  
  if (ncol(x) == 0) {
    return(tibble())
  }
  
  target_mean <- colMeans(x, na.rm = TRUE)
  target_sd   <- apply(x, 2, sd, na.rm = TRUE)
  
  adherent <- d$ADHERENT_AT_GRACE_END == 1
  
  if (!any(adherent)) {
    return(tibble())
  }
  
  unweighted_mean <- colMeans(x[adherent, , drop = FALSE], na.rm = TRUE)
  
  w <- d$SW_TRUNC[adherent]
  
  weighted_mean <- apply(
    x[adherent, , drop = FALSE],
    2,
    weighted.mean,
    w = w,
    na.rm = TRUE
  )
  
  smd_before <- (unweighted_mean - target_mean) / target_sd
  smd_after  <- (weighted_mean   - target_mean) / target_sd
  
  tibble(
    STRATEGY = strategy_value,
    COVARIATE = names(target_mean),
    SMD_BEFORE = smd_before,
    SMD_AFTER = smd_after
  ) %>%
    filter(is.finite(SMD_BEFORE) | is.finite(SMD_AFTER))
}


balance_dt <- treated_strategies %>%
  map(
    ~ selection_balance_one_strategy(
      grace_weights,
      strategy_value = .x,
      denominator_vars = denominator_vars
    )
  ) %>%
  bind_rows()

write_csv(balance_dt, file.path(OUTPUT_DIR, "03_selection_balance.csv"))


# ------------------------------------------------------------------------------
# Plot top imbalance variables
# ------------------------------------------------------------------------------

balance_long <- balance_dt %>%
  pivot_longer(
    cols = c(SMD_BEFORE, SMD_AFTER),
    names_to = "WEIGHTING",
    values_to = "SMD"
  ) %>%
  mutate(
    WEIGHTING = factor(
      WEIGHTING,
      levels = c("SMD_BEFORE", "SMD_AFTER"),
      labels = c("Before IPCW", "After IPCW")
    )
  )

# Keep the 25 most problematic variables.
top_balance_vars <- balance_dt %>%
  group_by(COVARIATE) %>%
  summarise(
    MAX_ABS_SMD = max(abs(c(SMD_BEFORE, SMD_AFTER)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(MAX_ABS_SMD)) %>%
  slice_head(n = 25) %>%
  pull(COVARIATE)


p_balance <- balance_long %>%
  filter(COVARIATE %in% top_balance_vars) %>%
  ggplot(aes(
    x = SMD,
    y = reorder(COVARIATE, abs(SMD)),
    shape = WEIGHTING
  )) +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = 2) +
  geom_point(size = 2.1) +
  facet_wrap(~ STRATEGY, scales = "free_y") +
  labs(
    title = "Selection-balance diagnostic at grace-period end",
    subtitle = "Standardized differences relative to the complete grace-period risk set",
    x = "Standardized mean difference",
    y = NULL,
    shape = NULL
  ) +
  theme_bw()


# ==============================================================================
# 16. WEIGHTED MARGINAL STRUCTURAL COX MODEL
#
# A is reference. Cluster on PATID because the same original patient contributes
# clones to multiple strategies.
# ==============================================================================

ccw <- ccw %>%
  mutate(
    STRATEGY = relevel(
      factor(STRATEGY, levels = strategy_levels),
      ref = REFERENCE_STRATEGY
    )
  )


msm_cox <- coxph(
  Surv(TSTART, TSTOP, EVENT) ~ STRATEGY + cluster(PATID),
  data = ccw,
  weights = IPCW,
  robust = TRUE,
  ties = "efron",
  model = TRUE,
  x = TRUE
)

cat("\n=======================================\n")
cat("MARGINAL STRUCTURAL COX MODEL\n")
cat("=======================================\n")

print(summary(msm_cox))


# ==============================================================================
# 17. HAZARD-RATIO TABLE
# ==============================================================================

hr_table <- msm_cox %>%
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%
  filter(str_detect(term, "^STRATEGY")) %>%
  transmute(
    STRATEGY = str_remove(term, "^STRATEGY"),
    HR = estimate,
    LCL = conf.low,
    UCL = conf.high,
    p.value = p.value
  ) %>%
  bind_rows(
    tibble(STRATEGY = REFERENCE_STRATEGY, HR = 1, LCL = 1, UCL = 1, p.value = NA_real_),
    .
  ) %>%
  mutate(STRATEGY = factor(STRATEGY, levels = strategy_levels)) %>%
  arrange(STRATEGY)

print(hr_table)

write_csv(hr_table, file.path(OUTPUT_DIR, "04_marginal_structural_cox.csv"))


# ==============================================================================
# 18. FOREST PLOT
# ==============================================================================

p_hr <- ggplot(hr_table, aes(y = STRATEGY, x = HR, xmin = LCL, xmax = UCL)) +
  geom_vline(xintercept = 1, linetype = 2) +
  geom_errorbarh(height = 0.15) +
  geom_point(size = 2.8) +
  scale_x_log10() +
  scale_y_discrete(labels = strategy_labels) +
  labs(
    title = "Association of MDT strategy with overall survival",
    subtitle = "IPCW marginal structural Cox model; Strategy A is reference",
    x = "Hazard ratio for death",
    y = NULL
  ) +
  theme_bw()


# ==============================================================================
# 19. PROPORTIONAL-HAZARDS DIAGNOSTICS
# ==============================================================================

ph_test <- cox.zph(msm_cox)

print(ph_test)

ph_table <- ph_test$table %>%
  as.data.frame() %>%
  rownames_to_column("TERM") %>%
  as_tibble()

write_csv(ph_table, file.path(OUTPUT_DIR, "05_proportional_hazards_test.csv"))

# Save standard Schoenfeld diagnostics.
#
# cox.zph() produces one Schoenfeld panel per model coefficient (STRATEGY
# expands to one panel per non-reference arm). A single png() device with no
# layout tries to write each panel as a separate "page" into one file, which
# yields an unreadable/partial PNG. Tile the panels into one figure instead.

n_ph_panels <- max(1, nrow(ph_test$table) - 1)   # exclude the GLOBAL row
ph_ncol <- ceiling(sqrt(n_ph_panels))
ph_nrow <- ceiling(n_ph_panels / ph_ncol)

png(
  file.path(OUTPUT_DIR, "05_proportional_hazards_diagnostic.png"),
  width = 900 * ph_ncol,
  height = 700 * ph_nrow,
  res = 150
)
op <- par(mfrow = c(ph_nrow, ph_ncol))
plot(ph_test)
par(op)
dev.off()

# ==============================================================================
# 20. WEIGHTED SURVIVAL CURVES
#
# PATID is supplied as the subject/cluster identifier because patients
# contribute multiple intervals and multiple strategy clones.
# ==============================================================================

weighted_survfit <- survfit(
  Surv(TSTART, TSTOP, EVENT) ~ STRATEGY,
  data = ccw,
  weights = IPCW,
  id = PATID,
  robust = TRUE,
  conf.type = "log"
)


# ==============================================================================
# 21. CONVERT SURVFIT TO TIBBLE
# ==============================================================================

surv_summary <- summary(weighted_survfit)

surv_dt <- tibble(
  TIME = surv_summary$time,
  SURVIVAL = surv_summary$surv,
  LOWER = surv_summary$lower,
  UPPER = surv_summary$upper,
  STRATEGY = str_remove(as.character(surv_summary$strata), "^STRATEGY=")
) %>%
  mutate(STRATEGY = factor(STRATEGY, levels = strategy_levels))


# ==============================================================================
# 22. MEDIAN SURVIVAL (IPCW-ADJUSTED)
#
# Medians and 95% CIs are read from the weighted survfit summary table, so they
# reflect the same IPC weighting as the curves. Median is NA for any arm whose
# weighted survival never reaches 0.5 within follow-up.
# ==============================================================================

median_matrix <- summary(weighted_survfit)$table

med_lcl_col <- grep("LCL$", colnames(median_matrix), value = TRUE)[1]
med_ucl_col <- grep("UCL$", colnames(median_matrix), value = TRUE)[1]

median_survival <- tibble(
  STRATEGY = str_remove(rownames(median_matrix), "^STRATEGY="),
  MEDIAN_DAYS = median_matrix[, "median"],
  MEDIAN_LCL_DAYS = if (!is.na(med_lcl_col)) median_matrix[, med_lcl_col] else NA_real_,
  MEDIAN_UCL_DAYS = if (!is.na(med_ucl_col)) median_matrix[, med_ucl_col] else NA_real_
) %>%
  mutate(
    MEDIAN_YEARS = MEDIAN_DAYS / 365.25,
    MEDIAN_LCL_YEARS = MEDIAN_LCL_DAYS / 365.25,
    MEDIAN_UCL_YEARS = MEDIAN_UCL_DAYS / 365.25,
    STRATEGY = factor(STRATEGY, levels = strategy_levels)
  ) %>%
  arrange(STRATEGY)

print(median_survival)

write_csv(median_survival, file.path(OUTPUT_DIR, "10_median_survival.csv"))


# ==============================================================================
# 22b. FINAL SURVIVAL CURVE  (with median-survival annotation)
# ==============================================================================

median_segments <- median_survival %>% filter(!is.na(MEDIAN_YEARS))

p_survival <- ggplot(
  surv_dt,
  aes(x = TIME / 365.25, y = SURVIVAL, group = STRATEGY, linetype = STRATEGY)
) +
  geom_ribbon(
    aes(ymin = LOWER, ymax = UPPER, fill = STRATEGY),
    alpha = 0.12,
    linewidth = 0
  ) +
  geom_hline(yintercept = 0.5, linetype = 3, colour = "grey50") +
  geom_segment(
    data = median_segments,
    aes(x = MEDIAN_YEARS, xend = MEDIAN_YEARS, y = 0, yend = 0.5),
    linetype = 3,
    colour = "grey50",
    inherit.aes = FALSE
  ) +
  geom_step(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "IPCW-adjusted overall survival by MDT strategy",
    subtitle = "Clone-censor-weight analysis",
    x = "Years since ALS index diagnosis",
    y = "Survival probability",
    linetype = "MDT strategy",
    fill = "MDT strategy"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


# ==============================================================================
# 23. FIXED-TIME SURVIVAL PROBABILITIES
# ==============================================================================

fixed_summary <- summary(weighted_survfit, times = SURVIVAL_TIMES, extend = TRUE)

fixed_survival <- tibble(
  TIME_DAYS = fixed_summary$time,
  SURVIVAL = fixed_summary$surv,
  LOWER = fixed_summary$lower,
  UPPER = fixed_summary$upper,
  STRATEGY = str_remove(as.character(fixed_summary$strata), "^STRATEGY=")
) %>%
  mutate(
    YEAR = TIME_DAYS / 365.25,
    STRATEGY = factor(STRATEGY, levels = strategy_levels)
  )

print(fixed_survival)

write_csv(fixed_survival, file.path(OUTPUT_DIR, "06_survival_probabilities.csv"))


# ==============================================================================
# 24. FIXED-TIME SURVIVAL DIFFERENCES VERSUS STRATEGY A
# ==============================================================================

reference_survival <- fixed_survival %>%
  filter(STRATEGY == REFERENCE_STRATEGY) %>%
  select(YEAR, REF_SURVIVAL = SURVIVAL)

survival_contrasts <- fixed_survival %>%
  left_join(reference_survival, by = "YEAR") %>%
  mutate(SURVIVAL_DIFFERENCE = SURVIVAL - REF_SURVIVAL)

print(survival_contrasts %>% filter(STRATEGY != REFERENCE_STRATEGY))

write_csv(
  survival_contrasts,
  file.path(OUTPUT_DIR, "07_survival_differences_vs_A.csv")
)


# ==============================================================================
# 25. RMST FROM WEIGHTED SURVIVAL CURVES
#
# Point estimates by numerical integration of the weighted survival curve.
# For formal RMST CIs/differences, a PATID-level bootstrap that re-estimates
# IPCW within each replicate is preferred.
# ==============================================================================

calculate_rmst <- function(survival_dt, tau) {
  
  survival_dt %>%
    group_by(STRATEGY) %>%
    group_modify(function(d, key) {
      
      d <- d %>% arrange(TIME)
      
      # Include S(0) = 1.
      times <- c(0, d$TIME[d$TIME < tau], tau)
      surv  <- c(1, d$SURVIVAL[d$TIME < tau])
      
      # Last survival immediately before tau.
      final_surv <- if (any(d$TIME <= tau)) {
        d %>% filter(TIME <= tau) %>% slice_tail(n = 1) %>% pull(SURVIVAL)
      } else {
        1
      }
      
      surv <- c(surv, final_surv)
      
      # Left-continuous step integration.
      dtimes <- diff(times)
      rmst <- sum(dtimes * surv[seq_along(dtimes)])
      
      tibble(
        TAU_DAYS = tau,
        RMST_DAYS = rmst,
        RMST_YEARS = rmst / 365.25
      )
    }) %>%
    ungroup() %>%
    mutate(STRATEGY = as.character(STRATEGY))
}


rmst_table <- calculate_rmst(surv_dt, tau = RMST_TAU)

reference_rmst <- rmst_table %>% filter(STRATEGY == REFERENCE_STRATEGY) %>% pull(RMST_DAYS)

rmst_table <- rmst_table %>%
  mutate(
    RMST_DIFFERENCE_VS_A_DAYS = RMST_DAYS - reference_rmst,
    RMST_DIFFERENCE_VS_A_MONTHS = RMST_DIFFERENCE_VS_A_DAYS / 30.4375
  )

print(rmst_table)

write_csv(
  rmst_table,
  file.path(OUTPUT_DIR, paste0("08_rmst_", RMST_YEARS, "year.csv"))
)


# ==============================================================================
# 26. EVENT / FOLLOW-UP SUMMARY
# ==============================================================================

outcome_summary <- ccw %>%
  group_by(STRATEGY) %>%
  summarise(
    N_UNIQUE_PATIENTS = n_distinct(PATID),
    N_CLONES = n_distinct(CLONE_ID),
    DEATH_EVENTS = sum(EVENT, na.rm = TRUE),
    ARTIFICIAL_CENSOR_EVENTS = sum(ARTIFICIAL_CENSOR, na.rm = TRUE),
    PERSON_YEARS = sum(PERSON_DAYS, na.rm = TRUE) / 365.25,
    .groups = "drop"
  )

print(outcome_summary)

write_csv(outcome_summary, file.path(OUTPUT_DIR, "09_outcome_summary.csv"))


# ==============================================================================
# 27. OVERALL DIAGNOSTIC PANEL
# ==============================================================================

diagnostic_panel <- (p_adherence | p_ess) /
  (p_weight | p_balance) +
  plot_annotation(title = "Clone-censor-weight diagnostics")

ggsave(
  filename = file.path(OUTPUT_DIR, "Figure_1_CCW_diagnostics.png"),
  plot = diagnostic_panel,
  width = 15, height = 10, dpi = 300
)


# ==============================================================================
# 28. FINAL OUTCOME FIGURE
# ==============================================================================

final_outcome_panel <- (p_survival | p_hr) +
  plot_annotation(title = "Multidisciplinary ALS care and overall survival")

ggsave(
  filename = file.path(OUTPUT_DIR, "Figure_2_MDT_survival_results.png"),
  plot = final_outcome_panel,
  width = 14, height = 6.5, dpi = 300
)


# Save individual figures as well.

ggsave(
  file.path(OUTPUT_DIR, "Figure_survival_curve.png"),
  p_survival, width = 8.5, height = 6, dpi = 300
)

ggsave(
  file.path(OUTPUT_DIR, "Figure_hazard_ratio_forest.png"),
  p_hr, width = 7, height = 5, dpi = 300
)

ggsave(
  file.path(OUTPUT_DIR, "Figure_weight_distribution.png"),
  p_weight, width = 10, height = 7, dpi = 300
)

ggsave(
  file.path(OUTPUT_DIR, "Figure_selection_balance.png"),
  p_balance, width = 13, height = 10, dpi = 300
)


# ==============================================================================
# 29. MODEL / ANALYSIS OBJECTS
# ==============================================================================

# saveRDS(msm_cox, file.path(OUTPUT_DIR, "msm_cox_model.rds"))
# saveRDS(weighted_survfit, file.path(OUTPUT_DIR, "weighted_survival_fit.rds"))
# saveRDS(ipcw_results, file.path(OUTPUT_DIR, "ipcw_models.rds"))


# ==============================================================================
# 30. SESSION INFORMATION
# ==============================================================================

# capture.output(
#   sessionInfo(),
#   file = file.path(OUTPUT_DIR, "sessionInfo.txt")
# )


# ==============================================================================
# 31. KEY RESULTS PRINTED TO CONSOLE
# ==============================================================================

# cat("\n\n============================================================\n")
# cat("FINAL RESULTS\n")
# cat("============================================================\n\n")
# 
# cat("Strategy adherence:\n")
# print(adherence_summary)
# 
# cat("\nIPCW diagnostics:\n")
# print(weight_diagnostics)
# 
# cat("\nMarginal structural Cox model:\n")
# print(hr_table)
# 
# cat("\nFixed-time adjusted survival:\n")
# print(fixed_survival)
# 
# cat("\nRMST through ", RMST_YEARS, " years:\n", sep = "")
# print(rmst_table)
# 
# cat("\nOutputs written to:\n", normalizePath(OUTPUT_DIR), "\n")