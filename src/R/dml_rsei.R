rm(list=ls()); gc()
setwd("C:/repos/cdc_als4m")

pacman::p_load(
  DBI,
  odbc,
  tidyverse,
  data.table,
  ranger,
  DoubleML,
  mlr3,
  mlr3learners
)

CFG <- list(
  source = 'GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_RSEI_DML_LONG',
  qa = 'GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_RSEI_DML_QA',
  windows = 2:5, # must match Snowpark Config.windows
  # Both ADI ranks are retained in Snowflake. National rank is the default
  # socioeconomic covariate; state rank can be used in sensitivity models.
  continuous = c('AGE','ADI_NATRANK'),
  categorical = c('SEX','RACE','HISPANIC','DUAL_LIS_ANY','RUCA','INDEX_YEAR'),
  min_n = 100L, min_cases = 30L, min_controls = 30L,
  min_nonzero = 20L, min_unique_exposure = 5L,
  max_zero_fraction = 0.98,
  min_geo_groups = 10L,
  min_inference_clusters = 10L,
  # Inference clusters may be coarser than holdout folds; default county.
  inference_geography = 'county',
  fold_geography = 'county', # 'county' (CBG first 5), 'cbg', 'state', 'patient'
  max_split_attempts = 200L,
  folds = 5L, repetitions = 3L,
  seed = 20261105L,
  output_prefix = 'als_rsei_dml'
)

sf_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "snowflake-id",
  uid = "xsm7f@umsystem.edu",
  pwd = ""
)

# Identifiers in CFG are developer-controlled, not externally supplied strings.
qa <- as.data.table(dbGetQuery(sf_conn, paste0('SELECT * FROM ', CFG$qa)))
eligible <- qa[N >= CFG$min_n & N_CASES >= CFG$min_cases &
                 N_CONTROLS >= CFG$min_controls &
                 N_POSITIVE >= CFG$min_nonzero &
                 N_ZERO / pmax(N, 1) <= CFG$max_zero_fraction &
                 N_UNIQUE_EXPOSURE >= CFG$min_unique_exposure &
                 is.finite(SD_EXPOSURE) & SD_EXPOSURE > 0,
               .(WINDOW_YEARS, CHEMICAL_NUMBER)]
qa[, selected := FALSE]
qa[eligible, on = .(WINDOW_YEARS, CHEMICAL_NUMBER), selected := TRUE]
qa[, exclusion_reason := fifelse(N < CFG$min_n, 'small_n',
                                 fifelse(N_CASES < CFG$min_cases, 'few_cases',
                                         fifelse(N_CONTROLS < CFG$min_controls, 'few_controls',
                                                 fifelse(N_POSITIVE < CFG$min_nonzero, 'few_positive_exposures',
                                                         fifelse(N_ZERO / pmax(N,1) > CFG$max_zero_fraction, 'mostly_zero',
                                                                 fifelse(N_UNIQUE_EXPOSURE < CFG$min_unique_exposure, 'few_unique_values',
                                                                         fifelse(is.na(SD_EXPOSURE) | SD_EXPOSURE <= 0, 'no_variation', 'eligible')))))))]
fwrite(qa, paste0(CFG$output_prefix, '_sparsity_audit.csv'))

# Cache person-level data for each window, avoiding ~500 round trips.
fetch_window <- function(w) {
  message('Reading ', w, '-year exposure data')
  q <- sprintf('SELECT * FROM %s WHERE WINDOW_YEARS = %d', CFG$source, w)
  as.data.table(dbGetQuery(sf_conn, q))
}

geo_key <- function(d, level = CFG$fold_geography) {
  if (level == 'patient') return(paste0('P_', d$PATID))
  if (level == 'state') {
    return(ifelse(is.na(d$INDEX_MINUS_1_STATE), 'STATE_MISSING',
                  paste0('STATE_', d$INDEX_MINUS_1_STATE)))
  }
  cbg <- as.character(d$INDEX_MINUS_1_CBG)
  # Preserve leading zeroes: CBG must have been stored as VARCHAR upstream.
  if (level == 'county') cbg <- substr(cbg, 1L, 5L)
  ifelse(!is.na(cbg) & nchar(cbg) >= 5L, paste0('G_', cbg),
         ifelse(!is.na(d$INDEX_MINUS_1_STATE),
                paste0('STATE_', d$INDEX_MINUS_1_STATE),
                'GEOGRAPHY_MISSING'))
}

make_design <- function(raw, chemical) {
  vars <- c('ALS','EXPOSURE_MEAN',CFG$continuous,CFG$categorical)
  d <- copy(raw[, c('PATID','INDEX_MINUS_1_CBG','INDEX_MINUS_1_STATE',vars), with = FALSE])
  if (anyDuplicated(d$PATID)) stop('duplicate PATID within chemical-window')
  d[, ALS := as.numeric(ALS)]
  for (v in CFG$continuous) d[, (v) := as.numeric(get(v))]
  for (v in CFG$categorical) d[, (v) := as.factor(get(v))]
  d <- d[complete.cases(d[, ..vars])]
  # Drop categorical columns with only one level in this chemical subset.
  use_cat <- CFG$categorical[vapply(CFG$categorical, function(v)
    uniqueN(d[[v]]) > 1L, logical(1))]
  if (nrow(d) < CFG$min_n || sum(d$ALS == 1) < CFG$min_cases ||
      sum(d$ALS == 0) < CFG$min_controls) return(NULL)
  if (sum(d$EXPOSURE_MEAN != 0) < CFG$min_nonzero ||
      mean(d$EXPOSURE_MEAN == 0) > CFG$max_zero_fraction ||
      uniqueN(d$EXPOSURE_MEAN) < CFG$min_unique_exposure) return(NULL)
  s <- sd(d$EXPOSURE_MEAN)
  if (!is.finite(s) || s <= 0) return(NULL)
  d[, D := (EXPOSURE_MEAN - mean(EXPOSURE_MEAN)) / s]
  terms <- c(CFG$continuous, use_cat)
  if (!length(terms)) return(NULL)
  f <- reformulate(terms)
  mm <- model.matrix(f, data = d)
  mm <- mm[, colnames(mm) != '(Intercept)', drop = FALSE]
  keep <- apply(mm, 2, function(x) all(is.finite(x)) && sd(x) > 0)
  mm <- mm[, keep, drop = FALSE]
  if (!ncol(mm)) return(NULL)
  colnames(mm) <- make.names(colnames(mm), unique = TRUE)
  design <- data.table(Y = d$ALS, D = d$D)
  design <- cbind(design, as.data.table(mm))
  list(data = design, id = d$PATID,
       geo = geo_key(d), inference_geo = geo_key(d, CFG$inference_geography),
       outcome = d$ALS,
       n = nrow(d), n_cases = sum(d$ALS == 1),
       n_nonzero = sum(d$EXPOSURE_MEAN != 0),
       n_unique = uniqueN(d$EXPOSURE_MEAN),
       zero_fraction = mean(d$EXPOSURE_MEAN == 0),
       sd_raw = s)
}

# DoubleML R requires a list of repetitions with named train_ids/test_ids.
# Every geographic group is entirely in a single test fold within a repeat.
# Every row is tested exactly once per repeat. All folds need both outcome classes.
make_group_splits <- function(a, seed) {
  group <- a$geo
  groups <- unique(group)
  if (length(groups) < max(CFG$folds, CFG$min_geo_groups))
    stop('too_few_geographic_groups')
  nfold <- CFG$folds
  result <- vector('list', CFG$repetitions)
  for (rep in seq_len(CFG$repetitions)) {
    accepted <- FALSE
    for (attempt in seq_len(CFG$max_split_attempts)) {
      set.seed(seed + rep * 1009L + attempt)
      # Random group ordering, then assign to the currently least-populated
      # fold. This balances persons without splitting geographical units.
      counts <- tabulate(match(group, groups), nbins = length(groups))
      order_g <- sample(seq_along(groups))
      fold_size <- rep(0L, nfold)
      group_fold <- integer(length(groups))
      for (j in order_g) {
        candidates <- which(fold_size == min(fold_size))
        f <- sample(candidates, 1L)
        group_fold[j] <- f
        fold_size[f] <- fold_size[f] + counts[j]
      }
      row_fold <- group_fold[match(group, groups)]
      good <- all(vapply(seq_len(nfold), function(f) {
        z <- a$outcome[row_fold == f]
        length(z) > 0L && any(z == 1) && any(z == 0)
      }, logical(1)))
      if (good) { accepted <- TRUE; break }
    }
    if (!accepted) stop('unable_to_balance_geographic_folds_by_outcome')
    tests <- lapply(seq_len(nfold), function(f) which(row_fold == f))
    trains <- lapply(tests, function(ids) setdiff(seq_len(a$n), ids))
    for (f in seq_len(nfold)) {
      if (length(intersect(tests[[f]], trains[[f]]))) stop('fold overlap')
    }
    result[[rep]] <- list(train_ids = trains, test_ids = tests)
  }
  result
}

# One-way geographic cluster-robust sandwich SE using the fitted orthogonal
# scores. For each repetition r, estimating equation is sum_i psi_i(theta)=0.
# Var(theta_r) = G/(G-1) * [(N-1)/(N-p)] *
#                sum_g (sum_{i in g} psi_i)^2 / (sum_i psi_a_i)^2.
# p=1 is the dimension of the target parameter (not RF complexity).
# This is a CR1-style finite-sample correction; it is not spatial HAC/Conley.
cluster_sandwich_dml <- function(model, clusters, min_clusters = 30L) {
  n <- length(clusters)
  G <- uniqueN(clusters)
  if (G < min_clusters) stop(sprintf('too_few_inference_clusters: %d < %d',
                                     G, min_clusters))
  psi <- model$psi
  psi_a <- model$psi_a
  if (length(dim(psi)) != 3L || length(dim(psi_a)) != 3L ||
      dim(psi)[1L] != n || dim(psi_a)[1L] != n)
    stop('orthogonal_score_dimensions_invalid')
  m <- dim(psi)[2L]
  theta_rep <- as.numeric(model$all_coef[1L, ])
  if (length(theta_rep) != m) stop('coefficient_repetition_mismatch')
  se_rep <- numeric(m)
  for (r in seq_len(m)) {
    score_r <- psi[, r, 1L]
    derivative_r <- psi_a[, r, 1L]
    if (any(!is.finite(c(score_r, derivative_r))))
      stop('nonfinite_orthogonal_scores')
    summed_by_cluster <- rowsum(score_r, group = clusters, reorder = FALSE)
    bread <- sum(derivative_r)
    if (!is.finite(bread) || abs(bread) < 1e-10)
      stop('near_zero_score_derivative')
    correction <- (G / (G - 1)) * ((n - 1) / (n - 1))
    se_rep[r] <- sqrt(correction * sum(summed_by_cluster^2) / bread^2)
  }
  # Same repeated-splitting median aggregation used by DoubleML R, but with
  # cluster-robust per-repetition variances substituted for iid variances.
  theta <- median(theta_rep)
  se <- sqrt(median(se_rep^2 + (theta_rep - theta)^2))
  if (!is.finite(se) || se <= 0) stop('invalid_cluster_robust_se')
  # Conservative t reference with G-1 df; this is approximate for repeated
  # splitting and does not replace a dedicated clustered-DML implementation.
  critical <- qt(0.975, df = G - 1)
  p <- 2 * pt(-abs(theta / se), df = G - 1)
  list(theta = theta, se = se, p = p,
       ci_low = theta - critical * se, ci_high = theta + critical * se,
       n_clusters = G, df = G - 1,
       min_rep_se = min(se_rep), max_rep_se = max(se_rep))
}

fit_one <- function(d, chem, w, population = 'all_eligible') {
  
  # w<-2
  # panel <- fetch_window(w)
  # ch<-114
  # d<-panel[CHEMICAL_NUMBER == ch]
  # chem<-ch
  
  a <- tryCatch(make_design(d, chem), error = function(e) e)
  if (inherits(a, 'error')) return(data.table(window = w,
                                              chemical_number = chem, population = population,
                                              status = paste0('design_error: ', conditionMessage(a))))
  if (is.null(a)) return(data.table(window = w, chemical_number = chem,
                                    population = population,
                                    status = 'insufficient_data'))
  tryCatch({
    captured_warnings <- character()
    splits <- make_group_splits(a, CFG$seed + as.integer(chem) %% 100000L + 100003L * w)
    obj <- DoubleMLData$new(a$data, y_col = 'Y', d_cols = 'D',
                            x_cols = setdiff(names(a$data), c('Y','D')))
    ml_l <- lrn('regr.ranger', num.trees = 500, min.node.size = 10)
    ml_m <- lrn('regr.ranger', num.trees = 500, min.node.size = 10)
    model <- DoubleMLPLR$new(obj, ml_l = ml_l, ml_m = ml_m,
                             n_folds = CFG$folds,
                             n_rep = CFG$repetitions,
                             score = 'partialling out',
                             draw_sample_splitting = FALSE)
    model$set_sample_splitting(splits)
    # Distinct seed per chemical/window for reproducibility.
    set.seed(CFG$seed + as.integer(chem) %% 100000L + 100003L * w)
    withCallingHandlers(model$fit(store_predictions = TRUE),
                        warning = function(w) {
                          captured_warnings <<- c(captured_warnings, conditionMessage(w))
                          invokeRestart('muffleWarning')
                        })
    theta <- unname(model$coef[1]); se <- unname(model$se[1])
    if (!is.finite(theta) || !is.finite(se) || se <= 0)
      stop('nonfinite_or_nonpositive_standard_error')
    robust <- cluster_sandwich_dml(model, a$inference_geo,
                                   CFG$min_inference_clusters)
    
    # DoubleML exposes out-of-fold nuisance RMSEs by learner. These describe
    # predictive fit, not DML inference validity or model convergence.
    get_nuisance_losses <- function(model, y, d) {
      
      extract_pred <- function(name) {
        p <- model$predictions[[name]]
        
        if (is.null(p)) {
          stop("Missing predictions for ", name)
        }
        
        matrix(
          p[, , 1, drop = FALSE],
          nrow = length(y)
        )
      }
      
      pred_y <- extract_pred("ml_l")
      pred_d <- extract_pred("ml_m")
      
      evaluate <- function(observed, predicted) {
        
        mse <- colMeans(
          (predicted - observed)^2
        )
        
        data.frame(
          RMSE = sqrt(mean(mse)),
          R2 = mean(1 - mse / var(observed)),
          RMSE_by_rep = paste(
            round(sqrt(mse), 4),
            collapse = ", "
          )
        )
      }
      
      rbind(
        data.frame(
          learner = "ml_l",
          evaluate(y, pred_y)
        ),
        data.frame(
          learner = "ml_m",
          evaluate(d, pred_d)
        )
      )
    }
    losses<-get_nuisance_losses(model = model,y = a$data$Y,d = a$data$D)
    
    # Approximate first-stage explanatory power, based on held-out RMSE.
    rmse_y <- losses$RMSE[1] 
    rmse_d <- losses$RMSE[2]
    r2_y <- losses$R2[1]
    r2_d <- losses$R2[2]
    pm <- model$predictions$ml_m
    resid_var <- NA_real_
    if (!is.null(pm) && length(dim(pm)) == 3L && dim(pm)[1L] == a$n) {
      pred_d <- pm[, , 1L, drop = FALSE]
      resid_var <- mean(vapply(seq_len(dim(pred_d)[2L]), function(r) {
        var(a$data$D - pred_d[, r, 1L])
      }, numeric(1)), na.rm = TRUE)
    }
    if (!is.finite(resid_var) || resid_var < 1e-5)
      stop('near_zero_residual_exposure_variance')
    diagnostic_flag <- if (r2_d < 0.01) 'low_exposure_nuisance_r2' else 'ok'
    if (abs(theta / se) > 100) diagnostic_flag <- paste(diagnostic_flag, 'extreme_z', sep = ';')
    data.table(window = w, chemical_number = chem,
               population = population, n = a$n, cases = a$n_cases,
               nonzero = a$n_nonzero, zero_fraction = a$zero_fraction,
               n_unique_exposure = a$n_unique,
               n_geo_groups = uniqueN(a$geo), fold_type = CFG$fold_geography,
               inference_cluster_type = CFG$inference_geography,
               n_inference_clusters = robust$n_clusters,
               inference_df = robust$df,
               raw_exposure_sd = a$sd_raw,
               nuisance_rmse_y = rmse_y, nuisance_rmse_d = rmse_d,
               nuisance_r2_y = r2_y, nuisance_r2_d = r2_d,
               residual_exposure_variance = resid_var,
               diagnostic_flag = diagnostic_flag,
               n_warnings = length(captured_warnings),
               warning_messages = paste(unique(captured_warnings), collapse = ' | '),
               estimate = theta, se_iid = se,
               ci_low_iid = theta - 1.96 * se,
               ci_high_iid = theta + 1.96 * se,
               p_iid = unname(model$pval[1]),
               se = robust$se, ci_low = robust$ci_low,
               ci_high = robust$ci_high, p = robust$p,
               min_rep_cluster_se = robust$min_rep_se,
               max_rep_cluster_se = robust$max_rep_se,
               status = 'ok')
  }, error = function(e) {
    data.table(window = w, chemical_number = chem,
               population = population, status = paste0('error: ',conditionMessage(e)))
  })
}

all_results <- list()
for (w in CFG$windows) {
  # w<-2
  panel <- fetch_window(w)
  candidates <- eligible[WINDOW_YEARS == w, unique(CHEMICAL_NUMBER)]
  all_results[[as.character(w)]] <- rbindlist(lapply(candidates, function(ch) {
    slice <- panel[CHEMICAL_NUMBER == ch]
    fit_one(slice, ch, w)
  }), fill = TRUE)
  rm(panel); gc(verbose = FALSE)
}
results <- rbindlist(all_results, fill = TRUE)
results[status == 'ok', q_bh := p.adjust(p, method = 'BH'), by = .(window, population)]
# Note: correction here covers all attempted chemicals with valid p-values.
# If a hypothesis was attempted but failed, document reasons; do not silently
# portray the valid subset as the entire prespecified testing family.
fwrite(results, paste0(CFG$output_prefix, '_screen.csv'))

# k-year sensitivity: compare every window with a prespecified reference
# window on the SAME chemical-specific people. Do not pool these contrasts as
# independent replications (overlapping person and exposure histories).
reference_window <- if (2L %in% CFG$windows) 2L else min(CFG$windows)
ref_panel <- fetch_window(reference_window)
common_result <- list()
for (k in setdiff(CFG$windows, reference_window)) {
  k_panel <- fetch_window(k)
  members <- merge(unique(ref_panel[, .(PATID,CHEMICAL_NUMBER)]),
                   unique(k_panel[, .(PATID,CHEMICAL_NUMBER)]),
                   by = c('PATID','CHEMICAL_NUMBER'))
  eligible_both <- intersect(
    eligible[WINDOW_YEARS == reference_window, CHEMICAL_NUMBER],
    eligible[WINDOW_YEARS == k, CHEMICAL_NUMBER])
  members <- members[CHEMICAL_NUMBER %in% eligible_both]
  ref_common <- merge(ref_panel, members, by = c('PATID','CHEMICAL_NUMBER'))
  k_common <- merge(k_panel, members, by = c('PATID','CHEMICAL_NUMBER'))
  common_result[[as.character(k)]] <- rbindlist(lapply(eligible_both, function(ch) {
    pair_name <- sprintf('common_%dy_%dy', reference_window, k)
    rbindlist(list(
      fit_one(ref_common[CHEMICAL_NUMBER == ch], ch, reference_window, pair_name),
      fit_one(k_common[CHEMICAL_NUMBER == ch], ch, k, pair_name)
    ), fill = TRUE)
  }), fill = TRUE)
  rm(k_panel, ref_common, k_common); gc(verbose = FALSE)
}
common <- rbindlist(common_result, fill = TRUE)
if (nrow(common)) {
  common[status == 'ok', q_bh := p.adjust(p, method = 'BH'),
         by = .(window, population)]
}
fwrite(common, paste0(CFG$output_prefix, '_common_cohort.csv'))

# Robust signals; do not characterize this as independent replication.
sig <- results[status == 'ok' & !is.na(q_bh) & q_bh < 0.05 & estimate > 0]
robust <- results[status == 'ok', .(
  windows_evaluated = uniqueN(window),
  positive_fdr_windows = sum(q_bh < 0.05 & estimate > 0, na.rm = TRUE),
  range_of_estimates = max(estimate) - min(estimate),
  min_n = min(n),
  windows = paste(sort(unique(window)), collapse = ',')
), by = chemical_number][order(-positive_fdr_windows, range_of_estimates)]
fwrite(robust, paste0(CFG$output_prefix, '_consistent_positive.csv'))
fwrite(results[status != 'ok'], paste0(CFG$output_prefix, '_failures.csv'))
message('Finished: ', nrow(results), ' chemical-window fit records.')

# dbDisconnect(sf_conn)



