pacman::p_load(
  tidyverse,
  scales
)

plot_als_dm_annual_rates <- function(
    data,
    width = 11,
    height = 7,
    dpi = 400,
    als_rate_per = 100000,
    estimates = c("adjusted", "crude"),
    ci_display = c("errorbar", "ribbon", "none"),
    count_bar_alpha = 0.20,
    count_bar_width = 0.72
) {
  # ===========================================================================
  # 1. Validate arguments
  # ===========================================================================
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.")
  }
  
  estimates <- unique(tolower(estimates))
  
  valid_estimates <- c("adjusted", "crude")
  
  invalid_estimates <- setdiff(
    estimates,
    valid_estimates
  )
  
  if (length(invalid_estimates) > 0) {
    stop(
      "`estimates` may contain only: ",
      paste(valid_estimates, collapse = ", ")
    )
  }
  
  if (length(estimates) == 0) {
    stop(
      "`estimates` must include at least one of ",
      "'adjusted' or 'crude'."
    )
  }
  
  ci_display <- match.arg(ci_display)
  
  if (!is.numeric(als_rate_per) ||
      length(als_rate_per) != 1 ||
      is.na(als_rate_per) ||
      als_rate_per <= 0) {
    stop("`als_rate_per` must be one positive number.")
  }
  
  if (!is.numeric(count_bar_alpha) ||
      length(count_bar_alpha) != 1 ||
      count_bar_alpha < 0 ||
      count_bar_alpha > 1) {
    stop("`count_bar_alpha` must be between 0 and 1.")
  }
  
  # ===========================================================================
  # 2. Define required columns
  # ===========================================================================
  
  general_columns <- c(
    "YEAR",
    "N_OBSERVED"
  )
  
  dm_adjusted_columns <- c(
    "ADJ_DM_PREVALENCE_PCT",
    "ADJ_DM_PREVALENCE_PCT_LCL",
    "ADJ_DM_PREVALENCE_PCT_UCL",
    "ADJ_DM_PREVALENCE_IN_ALS_PCT",
    "ADJ_DM_PREVALENCE_IN_ALS_PCT_LCL",
    "ADJ_DM_PREVALENCE_IN_ALS_PCT_UCL"
  )
  
  dm_crude_columns <- c(
    "CRUDE_DM_PREVALENCE_PCT",
    "CRUDE_DM_PREVALENCE_PCT_LCL",
    "CRUDE_DM_PREVALENCE_PCT_UCL",
    "CRUDE_DM_PREVALENCE_IN_ALS_PCT",
    "CRUDE_DM_PREVALENCE_IN_ALS_PCT_LCL",
    "CRUDE_DM_PREVALENCE_IN_ALS_PCT_UCL"
  )
  
  als_adjusted_columns <- c(
    "ADJ_ALS_PREVALENCE",
    "ADJ_ALS_PREVALENCE_LCL",
    "ADJ_ALS_PREVALENCE_UCL",
    "ADJ_ALS_PREVALENCE_IN_DM",
    "ADJ_ALS_PREVALENCE_IN_DM_LCL",
    "ADJ_ALS_PREVALENCE_IN_DM_UCL",
    "ADJ_ALS_INCIDENCE_IN_DM",
    "ADJ_ALS_INCIDENCE_IN_DM_LCL",
    "ADJ_ALS_INCIDENCE_IN_DM_UCL"
  )
  
  als_crude_columns <- c(
    "CRUDE_ALS_PREVALENCE",
    "CRUDE_ALS_PREVALENCE_LCL",
    "CRUDE_ALS_PREVALENCE_UCL",
    "CRUDE_ALS_PREVALENCE_IN_DM",
    "CRUDE_ALS_PREVALENCE_IN_DM_LCL",
    "CRUDE_ALS_PREVALENCE_IN_DM_UCL",
    "CRUDE_ALS_INCIDENCE_IN_DM",
    "CRUDE_ALS_INCIDENCE_IN_DM_LCL",
    "CRUDE_ALS_INCIDENCE_IN_DM_UCL"
  )
  
  required_columns <- general_columns
  
  if ("adjusted" %in% estimates) {
    required_columns <- c(
      required_columns,
      dm_adjusted_columns,
      als_adjusted_columns
    )
  }
  
  if ("crude" %in% estimates) {
    required_columns <- c(
      required_columns,
      dm_crude_columns,
      als_crude_columns
    )
  }
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 3. Prepare annual denominator data
  # ===========================================================================
  
  annual_data <- data |>
    dplyr::mutate(
      YEAR = as.integer(.data$YEAR),
      N_OBSERVED = as.numeric(.data$N_OBSERVED)
    ) |>
    dplyr::filter(!is.na(.data$YEAR)) |>
    dplyr::arrange(.data$YEAR)
  
  if (nrow(annual_data) == 0) {
    stop("No nonmissing annual observations are available.")
  }
  
  if (anyDuplicated(annual_data$YEAR)) {
    stop(
      "The input contains multiple rows for the same year. ",
      "Aggregate the data to one row per year before plotting."
    )
  }
  
  if (!any(
    is.finite(annual_data$N_OBSERVED) &
    annual_data$N_OBSERVED > 0
  )) {
    stop("`N_OBSERVED` must contain at least one positive value.")
  }
  
  bar_data <- annual_data |>
    dplyr::transmute(
      year = .data$YEAR,
      n_observed = .data$N_OBSERVED
    )
  
  # ===========================================================================
  # 4. Construct long-form DM data
  # ===========================================================================
  
  dm_data_list <- list()
  
  if ("adjusted" %in% estimates) {
    dm_data_list[["adjusted"]] <- annual_data |>
      dplyr::transmute(
        year = .data$YEAR,
        
        dm_estimate =
          as.numeric(.data$ADJ_DM_PREVALENCE_PCT),
        
        dm_lcl =
          as.numeric(.data$ADJ_DM_PREVALENCE_PCT_LCL),
        
        dm_ucl =
          as.numeric(.data$ADJ_DM_PREVALENCE_PCT_UCL),
        
        dm_in_als_estimate =
          as.numeric(.data$ADJ_DM_PREVALENCE_IN_ALS_PCT),
        
        dm_in_als_lcl =
          as.numeric(.data$ADJ_DM_PREVALENCE_IN_ALS_PCT_LCL),
        
        dm_in_als_ucl =
          as.numeric(.data$ADJ_DM_PREVALENCE_IN_ALS_PCT_UCL)
      ) |>
      tidyr::pivot_longer(
        cols = -year,
        names_to = c("measure_code", ".value"),
        names_pattern =
          "^(dm_in_als|dm)_(estimate|lcl|ucl)$"
      ) |>
      dplyr::mutate(
        estimate_type = "Adjusted"
      )
  }
  
  if ("crude" %in% estimates) {
    dm_data_list[["crude"]] <- annual_data |>
      dplyr::transmute(
        year = .data$YEAR,
        
        dm_estimate =
          as.numeric(.data$CRUDE_DM_PREVALENCE_PCT),
        
        dm_lcl =
          as.numeric(.data$CRUDE_DM_PREVALENCE_PCT_LCL),
        
        dm_ucl =
          as.numeric(.data$CRUDE_DM_PREVALENCE_PCT_UCL),
        
        dm_in_als_estimate =
          as.numeric(.data$CRUDE_DM_PREVALENCE_IN_ALS_PCT),
        
        dm_in_als_lcl =
          as.numeric(.data$CRUDE_DM_PREVALENCE_IN_ALS_PCT_LCL),
        
        dm_in_als_ucl =
          as.numeric(.data$CRUDE_DM_PREVALENCE_IN_ALS_PCT_UCL)
      ) |>
      tidyr::pivot_longer(
        cols = -year,
        names_to = c("measure_code", ".value"),
        names_pattern =
          "^(dm_in_als|dm)_(estimate|lcl|ucl)$"
      ) |>
      dplyr::mutate(
        estimate_type = "Crude"
      )
  }
  
  dm_rate_data <- dplyr::bind_rows(dm_data_list) |>
    dplyr::mutate(
      measure = factor(
        .data$measure_code,
        levels = c(
          "dm",
          "dm_in_als"
        ),
        labels = c(
          "Diabetes prevalence",
          "Diabetes prevalence among persons with ALS"
        )
      ),
      estimate_type = factor(
        .data$estimate_type,
        levels = c(
          "Adjusted",
          "Crude"
        )
      ),
      series_id = interaction(
        .data$measure,
        .data$estimate_type,
        drop = TRUE,
        lex.order = TRUE
      )
    )
  
  # ===========================================================================
  # 5. Construct long-form ALS data
  # ===========================================================================
  
  als_data_list <- list()
  
  if ("adjusted" %in% estimates) {
    als_data_list[["adjusted"]] <- annual_data |>
      dplyr::transmute(
        year = .data$YEAR,
        
        als_prevalence_estimate =
          as.numeric(.data$ADJ_ALS_PREVALENCE),
        
        als_prevalence_lcl =
          as.numeric(.data$ADJ_ALS_PREVALENCE_LCL),
        
        als_prevalence_ucl =
          as.numeric(.data$ADJ_ALS_PREVALENCE_UCL),
        
        als_prevalence_dm_estimate =
          as.numeric(.data$ADJ_ALS_PREVALENCE_IN_DM),
        
        als_prevalence_dm_lcl =
          as.numeric(.data$ADJ_ALS_PREVALENCE_IN_DM_LCL),
        
        als_prevalence_dm_ucl =
          as.numeric(.data$ADJ_ALS_PREVALENCE_IN_DM_UCL),
        
        als_incidence_dm_estimate =
          as.numeric(.data$ADJ_ALS_INCIDENCE_IN_DM),
        
        als_incidence_dm_lcl =
          as.numeric(.data$ADJ_ALS_INCIDENCE_IN_DM_LCL),
        
        als_incidence_dm_ucl =
          as.numeric(.data$ADJ_ALS_INCIDENCE_IN_DM_UCL)
      ) |>
      tidyr::pivot_longer(
        cols = -year,
        names_to = c("measure_code", ".value"),
        names_pattern = paste0(
          "^(als_prevalence_dm|",
          "als_prevalence|",
          "als_incidence_dm)_",
          "(estimate|lcl|ucl)$"
        )
      ) |>
      dplyr::mutate(
        estimate_type = "Adjusted"
      )
  }
  
  if ("crude" %in% estimates) {
    als_data_list[["crude"]] <- annual_data |>
      dplyr::transmute(
        year = .data$YEAR,
        
        als_prevalence_estimate =
          as.numeric(.data$CRUDE_ALS_PREVALENCE),
        
        als_prevalence_lcl =
          as.numeric(.data$CRUDE_ALS_PREVALENCE_LCL),
        
        als_prevalence_ucl =
          as.numeric(.data$CRUDE_ALS_PREVALENCE_UCL),
        
        als_prevalence_dm_estimate =
          as.numeric(.data$CRUDE_ALS_PREVALENCE_IN_DM),
        
        als_prevalence_dm_lcl =
          as.numeric(.data$CRUDE_ALS_PREVALENCE_IN_DM_LCL),
        
        als_prevalence_dm_ucl =
          as.numeric(.data$CRUDE_ALS_PREVALENCE_IN_DM_UCL),
        
        als_incidence_dm_estimate =
          as.numeric(.data$CRUDE_ALS_INCIDENCE_IN_DM),
        
        als_incidence_dm_lcl =
          as.numeric(.data$CRUDE_ALS_INCIDENCE_IN_DM_LCL),
        
        als_incidence_dm_ucl =
          as.numeric(.data$CRUDE_ALS_INCIDENCE_IN_DM_UCL)
      ) |>
      tidyr::pivot_longer(
        cols = -year,
        names_to = c("measure_code", ".value"),
        names_pattern = paste0(
          "^(als_prevalence_dm|",
          "als_prevalence|",
          "als_incidence_dm)_",
          "(estimate|lcl|ucl)$"
        )
      ) |>
      dplyr::mutate(
        estimate_type = "Crude"
      )
  }
  
  als_rate_data <- dplyr::bind_rows(als_data_list) |>
    dplyr::mutate(
      measure = factor(
        .data$measure_code,
        levels = c(
          "als_prevalence",
          "als_prevalence_dm",
          "als_incidence_dm"
        ),
        labels = c(
          "ALS prevalence",
          "ALS prevalence among persons with diabetes",
          "ALS incidence among persons with diabetes"
        )
      ),
      estimate_type = factor(
        .data$estimate_type,
        levels = c(
          "Adjusted",
          "Crude"
        )
      ),
      series_id = interaction(
        .data$measure,
        .data$estimate_type,
        drop = TRUE,
        lex.order = TRUE
      )
    )
  
  # ===========================================================================
  # 6. Hybrid plotting helper
  # ===========================================================================
  
  build_hybrid_plot <- function(
    rate_data,
    bar_data,
    primary_axis_label,
    title,
    subtitle,
    caption,
    measure_colors,
    measure_shapes,
    bar_axis_label = "Observed population, N"
  ) {
    rate_data <- rate_data |>
      dplyr::filter(
        is.finite(.data$estimate) |
          is.finite(.data$lcl) |
          is.finite(.data$ucl)
      )
    
    if (nrow(rate_data) == 0) {
      stop(
        "No valid rate estimates were found for the requested figure."
      )
    }
    
    all_rate_values <- c(
      rate_data$estimate,
      rate_data$lcl,
      rate_data$ucl
    )
    
    all_rate_values <- all_rate_values[
      is.finite(all_rate_values)
    ]
    
    maximum_rate <- max(all_rate_values)
    minimum_rate <- min(c(0, all_rate_values))
    
    if (!is.finite(maximum_rate) || maximum_rate <= 0) {
      stop(
        "The requested rate measures do not contain positive values."
      )
    }
    
    rate_range <- maximum_rate - minimum_rate
    
    if (!is.finite(rate_range) || rate_range <= 0) {
      rate_range <- maximum_rate
    }
    
    primary_lower <- min(0, minimum_rate)
    primary_upper <- maximum_rate + 0.12 * rate_range
    
    maximum_count <- max(
      bar_data$n_observed,
      na.rm = TRUE
    )
    
    usable_primary_height <- primary_upper - primary_lower
    
    # Bars occupy approximately 80% of the plotting height.
    bar_target_height <- usable_primary_height * 0.80
    
    count_scale_factor <- maximum_count / bar_target_height
    
    bar_data_scaled <- bar_data |>
      dplyr::mutate(
        n_observed_scaled =
          .data$n_observed / count_scale_factor +
          primary_lower
      )
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = bar_data_scaled,
        ggplot2::aes(
          x = .data$year,
          y = .data$n_observed_scaled
        ),
        width = count_bar_width,
        fill = "grey55",
        alpha = count_bar_alpha
      )
    
    # -------------------------------------------------------------------------
    # Confidence intervals
    # -------------------------------------------------------------------------
    
    if (ci_display == "errorbar") {
      # Offset crude and adjusted CIs slightly to reduce overlap.
      p <- p +
        ggplot2::geom_errorbar(
          data = rate_data,
          ggplot2::aes(
            x = .data$year,
            ymin = .data$lcl,
            ymax = .data$ucl,
            color = .data$measure,
            group = .data$series_id
          ),
          position = ggplot2::position_dodge(
            width = 0.20
          ),
          width = 0.10,
          linewidth = 0.45,
          alpha = 0.70,
          show.legend = FALSE,
          na.rm = TRUE
        )
    }
    
    if (ci_display == "ribbon") {
      # Ribbons may overlap substantially when crude and adjusted estimates
      # are similar, so separate linetypes are still used for the estimates.
      p <- p +
        ggplot2::geom_ribbon(
          data = rate_data,
          ggplot2::aes(
            x = .data$year,
            ymin = .data$lcl,
            ymax = .data$ucl,
            fill = .data$measure,
            group = .data$series_id
          ),
          alpha = 0.07,
          linewidth = 0,
          show.legend = FALSE,
          na.rm = TRUE
        )
    }
    
    # -------------------------------------------------------------------------
    # Estimates
    # -------------------------------------------------------------------------
    
    p <- p +
      ggplot2::geom_line(
        data = rate_data,
        ggplot2::aes(
          x = .data$year,
          y = .data$estimate,
          color = .data$measure,
          linetype = .data$estimate_type,
          group = .data$series_id
        ),
        linewidth = 1.00,
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = rate_data,
        ggplot2::aes(
          x = .data$year,
          y = .data$estimate,
          color = .data$measure,
          shape = .data$measure,
          group = .data$series_id
        ),
        position = ggplot2::position_dodge(
          width = 0.20
        ),
        size = 2.5,
        stroke = 0.45,
        na.rm = TRUE
      ) +
      ggplot2::scale_color_manual(
        name = "Measure",
        values = measure_colors,
        drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        name = "Measure",
        values = measure_colors,
        drop = FALSE
      ) +
      ggplot2::scale_shape_manual(
        name = "Measure",
        values = measure_shapes,
        drop = FALSE
      ) +
      ggplot2::scale_linetype_manual(
        name = "Estimate",
        values = c(
          "Adjusted" = "solid",
          "Crude" = "22"
        ),
        drop = FALSE
      ) +
      ggplot2::scale_x_continuous(
        name = "Year",
        breaks = bar_data$year,
        minor_breaks = NULL,
        expand = ggplot2::expansion(
          mult = c(0.025, 0.025)
        )
      ) +
      ggplot2::scale_y_continuous(
        name = primary_axis_label,
        limits = c(
          primary_lower,
          primary_upper
        ),
        labels = scales::label_number(
          accuracy = ifelse(
            maximum_rate < 1,
            0.01,
            0.1
          ),
          big.mark = ",",
          trim = TRUE
        ),
        expand = ggplot2::expansion(
          mult = c(0, 0.02)
        ),
        sec.axis = ggplot2::sec_axis(
          transform =
            ~ (. - primary_lower) * count_scale_factor,
          name = bar_axis_label,
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale(),
            accuracy = 0.1
          )
        )
      ) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = caption
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          order = 1,
          override.aes = list(
            linetype = "solid",
            linewidth = 1.1,
            alpha = 1
          )
        ),
        shape = "none",
        fill = "none",
        linetype = ggplot2::guide_legend(
          order = 2,
          override.aes = list(
            color = "grey20",
            linewidth = 1.1
          )
        )
      ) +
      ggplot2::theme_classic(
        base_size = 12
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 15,
          face = "bold",
          margin = ggplot2::margin(b = 5)
        ),
        plot.subtitle = ggplot2::element_text(
          size = 11,
          margin = ggplot2::margin(b = 12)
        ),
        plot.caption = ggplot2::element_text(
          size = 9,
          hjust = 0,
          color = "grey35",
          margin = ggplot2::margin(t = 10)
        ),
        axis.title.x = ggplot2::element_text(
          face = "bold",
          margin = ggplot2::margin(t = 8)
        ),
        axis.title.y = ggplot2::element_text(
          face = "bold",
          margin = ggplot2::margin(r = 8)
        ),
        axis.title.y.right = ggplot2::element_text(
          face = "bold",
          margin = ggplot2::margin(l = 8)
        ),
        axis.text.x = ggplot2::element_text(
          angle = ifelse(
            nrow(bar_data) > 12,
            45,
            0
          ),
          hjust = ifelse(
            nrow(bar_data) > 12,
            1,
            0.5
          ),
          vjust = ifelse(
            nrow(bar_data) > 12,
            1,
            0.5
          )
        ),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = ggplot2::element_text(
          size = 10
        ),
        legend.title = ggplot2::element_text(
          size = 10,
          face = "bold"
        ),
        legend.key.width = grid::unit(
          1.5,
          "cm"
        ),
        panel.grid.major.y = ggplot2::element_line(
          linewidth = 0.3,
          color = "grey88"
        ),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(
          12,
          18,
          10,
          12
        )
      )
    
    p
  }
  
  # ===========================================================================
  # 7. DM prevalence figure
  # ===========================================================================
  
  dm_colors <- c(
    "Diabetes prevalence" = "#0072B2",
    "Diabetes prevalence among persons with ALS" = "#D55E00"
  )
  
  dm_shapes <- c(
    "Diabetes prevalence" = 16,
    "Diabetes prevalence among persons with ALS" = 17
  )
  
  estimate_description <- if (
    all(c("adjusted", "crude") %in% estimates)
  ) {
    paste0(
      "Crude and adjusted annual estimates with ",
      "95% confidence intervals"
    )
  } else if ("adjusted" %in% estimates) {
    "Adjusted annual estimates with 95% confidence intervals"
  } else {
    "Crude annual estimates with 95% confidence intervals"
  }
  
  dm_plot <- build_hybrid_plot(
    rate_data = dm_rate_data,
    bar_data = bar_data,
    primary_axis_label = "Annual prevalence (%)",
    title = "Annual diabetes prevalence",
    subtitle = estimate_description,
    caption = paste0(
      "Bars represent the annual observed population. ",
      "Solid lines represent adjusted estimates and dashed lines ",
      "represent crude estimates. Confidence intervals are shown as ",
      ifelse(
        ci_display == "errorbar",
        "error bars.",
        ifelse(
          ci_display == "ribbon",
          "shaded bands.",
          "not displayed."
        )
      )
    ),
    measure_colors = dm_colors,
    measure_shapes = dm_shapes
  )
  
  # ===========================================================================
  # 8. ALS prevalence and incidence figure
  # ===========================================================================
  
  als_colors <- c(
    "ALS prevalence" = "#009E73",
    "ALS prevalence among persons with diabetes" = "#CC79A7",
    "ALS incidence among persons with diabetes" = "#E69F00"
  )
  
  als_shapes <- c(
    "ALS prevalence" = 16,
    "ALS prevalence among persons with diabetes" = 17,
    "ALS incidence among persons with diabetes" = 15
  )
  
  als_rate_unit_label <- paste0(
    "Rate per ",
    scales::comma(als_rate_per)
  )
  
  als_plot <- build_hybrid_plot(
    rate_data = als_rate_data,
    bar_data = bar_data,
    primary_axis_label = als_rate_unit_label,
    title = "Annual ALS prevalence and incidence",
    subtitle = estimate_description,
    caption = paste0(
      "Bars represent the annual observed population. ",
      "Solid lines represent adjusted estimates and dashed lines ",
      "represent crude estimates. Prevalence rates use persons as ",
      "the denominator, whereas incidence rates use person-years. ",
      "All rates are expressed per ",
      scales::comma(als_rate_per),
      "."
    ),
    measure_colors = als_colors,
    measure_shapes = als_shapes
  )
  
  # ===========================================================================
  # 9. Return plots and plotting data
  # ===========================================================================
  
  invisible(
    list(
      dm_plot = dm_plot,
      als_plot = als_plot,
      dm_plot_data = dm_rate_data,
      als_plot_data = als_rate_data,
      bar_data = bar_data
    )
  )
}

plot_med_prevalence <- function(
    data,
    meds,
    ty = c("all","t1dm","t2dm"),
    estimates = c("crude","adjusted"),
    show_counts = TRUE,
    reference_curve = NULL,    # NULL or "als"
    bar_alpha = 0.20,
    ribbon_alpha = 0.10,
    point_size = 2.5,
    line_width = 1,
    reference_line_width = 1
) {
  
  # ============================================================
  # 1. Standardize and validate inputs
  # ============================================================
  
  meds <- unique(toupper(meds))
  
  if (length(meds) == 0) {
    stop("At least one medication must be specified in `meds`.")
  }
  
  if (!is.null(reference_curve)) {
    
    reference_curve <- tolower(reference_curve)
    
    if (length(reference_curve) != 1 ||
        reference_curve != "als") {
      
      stop(
        "`reference_curve` must be either NULL or \"als\"."
      )
    }
  }
  
  
  # ============================================================
  # 2. Crude vs adjusted prefix
  # ============================================================
  
  estimates <- 
  prefix <- if (adjusted) "ADJ_" else "CRUDE_"
  
  
  # ============================================================
  # 3. Medication prevalence columns
  # ============================================================
  
  prev_cols <- paste0(
    prefix,
    "MED_", meds, "_PREVALENCE_IN_",toupper(ty),"_PCT"
  )
  
  lcl_cols <- paste0(
    prefix,
    "MED_", meds, "_PREVALENCE_IN_",toupper(ty),"_PCT_LCL"
  )
  
  ucl_cols <- paste0(
    prefix,
    "MED_", meds, "_PREVALENCE_IN_",toupper(ty),"_PCT_UCL"
  )
  
  
  # ============================================================
  # 4. Validate medication and denominator columns
  # ============================================================
  
  required_cols <- c(
    "YEAR",
    paste0("DENOM_",toupper(ty),"_CASES"),
    prev_cols,
    lcl_cols,
    ucl_cols
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(data)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "Missing required columns:\n",
      paste(
        missing_cols,
        collapse = "\n"
      )
    )
  }
  
  # ============================================================
  # 5. Medication prevalence data -> long format
  # ============================================================
  
  plot_data <- lapply(
    meds,
    function(med) {
      
      data.frame(
        YEAR = data$YEAR,
        MEDICATION = med,
        
        PREVALENCE = data[[
          paste0(
            prefix,
            "MED_",
            med,
            "_PREVALENCE_IN_",toupper(ty),"_PCT"
          )
        ]],
        
        LCL = data[[
          paste0(
            prefix,
            "MED_",
            med,
            "_PREVALENCE_IN_",toupper(ty),"_PCT_LCL"
          )
        ]],
        
        UCL = data[[
          paste0(
            prefix,
            "MED_",
            med,
            "_PREVALENCE_IN_",toupper(ty),"_PCT_UCL"
          )
        ]]
      )
    }
  ) %>%
    bind_rows() %>%
    arrange(
      YEAR,
      MEDICATION
    )
  
  
  # Preserve order supplied by user
  plot_data$MEDICATION <- factor(
    plot_data$MEDICATION,
    levels = meds
  )
  
  
  # ============================================================
  # 6. DM denominator counts
  # ============================================================
  
  count_data <- data.frame(
    YEAR = data$YEAR,
    COUNT = data[[paste0("DENOM_",toupper(ty),"_CASES")]]
  ) %>%
    arrange(YEAR)
  
  
  # ============================================================
  # 7. Optional ALS prevalence among DM reference curve
  # ============================================================
  
  reference_data <- NULL
  
  if (!is.null(reference_curve)) {
    
    als_cols <- c(
      paste0(
        prefix,
        "ALS_PREVALENCE_IN_DM"
      ),
      paste0(
        prefix,
        "ALS_PREVALENCE_IN_DM_LCL"
      ),
      paste0(
        prefix,
        "ALS_PREVALENCE_IN_DM_UCL"
      )
    )
    
    missing_als_cols <- setdiff(
      als_cols,
      names(data)
    )
    
    if (length(missing_als_cols) > 0) {
      
      stop(
        "Missing ALS prevalence-among-DM columns:\n",
        paste(
          missing_als_cols,
          collapse = "\n"
        )
      )
    }
    
    
    reference_data <- data.frame(
      YEAR = data$YEAR,
      
      PREVALENCE = data[[
        paste0(
          prefix,
          "ALS_PREVALENCE_IN_DM"
        )
      ]],
      
      LCL = data[[
        paste0(
          prefix,
          "ALS_PREVALENCE_IN_DM_LCL"
        )
      ]],
      
      UCL = data[[
        paste0(
          prefix,
          "ALS_PREVALENCE_IN_DM_UCL"
        )
      ]]
    )
  }
  
  
  # ============================================================
  # 8. Determine prevalence-axis range
  # ============================================================
  
  max_prev <- max(
    plot_data$UCL,
    na.rm = TRUE
  )
  
  if (!is.null(reference_data)) {
    
    max_prev <- max(
      max_prev,
      max(
        reference_data$UCL,
        na.rm = TRUE
      )
    )
  }
  
  
  if (!is.finite(max_prev) ||
      max_prev <= 0) {
    
    stop(
      "Prevalence values are missing or invalid."
    )
  }
  
  
  # ============================================================
  # 9. Scale DM denominator counts onto primary axis
  # ============================================================
  
  if (show_counts) {
    
    max_count <- max(
      count_data$COUNT,
      na.rm = TRUE
    )
    
    if (!is.finite(max_count) ||
        max_count <= 0) {
      
      stop(
        "DM_PREVALENT_CASES is missing or invalid."
      )
    }
    
    scale_factor <- max_count / max_prev
  }
  
  
  # ============================================================
  # 10. Initialize plot
  # ============================================================
  
  p <- ggplot()
  
  
  # ============================================================
  # 11. DM denominator bars
  # ============================================================
  
  if (show_counts) {
    
    p <- p +
      geom_col(
        data = count_data,
        aes(
          x = YEAR,
          y = COUNT / scale_factor
        ),
        width = 0.70,
        alpha = bar_alpha,
        fill = "grey55"
      )
  }
  
  
  # ============================================================
  # 12. Medication 95% confidence intervals
  # ============================================================
  
  p <- p +
    geom_ribbon(
      data = plot_data,
      aes(
        x = YEAR,
        ymin = LCL,
        ymax = UCL,
        fill = MEDICATION,
        group = MEDICATION
      ),
      alpha = ribbon_alpha,
      color = "grey"
    )
  
  
  # ============================================================
  # 13. Medication prevalence lines
  # ============================================================
  
  p <- p +
    geom_line(
      data = plot_data,
      aes(
        x = YEAR,
        y = PREVALENCE,
        color = MEDICATION,
        group = MEDICATION
      ),
      linewidth = line_width
    ) +
    
    geom_point(
      data = plot_data,
      aes(
        x = YEAR,
        y = PREVALENCE,
        color = MEDICATION
      ),
      size = point_size
    )
  
  
  # ============================================================
  # 14. Optional ALS prevalence among DM reference
  # ============================================================
  
  if (!is.null(reference_data)) {
    
    # ALS 95% CI
    p <- p +
      geom_ribbon(
        data = reference_data,
        aes(
          x = YEAR,
          ymin = LCL,
          ymax = UCL
        ),
        inherit.aes = FALSE,
        fill = "grey30",
        alpha = 0.08
      )
    
    # ALS reference line
    p <- p +
      geom_line(
        data = reference_data,
        aes(
          x = YEAR,
          y = PREVALENCE
        ),
        inherit.aes = FALSE,
        color = "grey15",
        linetype = "dashed",
        linewidth = reference_line_width
      )
  }
  
  
  # ============================================================
  # 15. Y axes
  # ============================================================
  
  prevalence_label <- if (adjusted) {
    "Adjusted Prescription Prevalence"
  } else {
    "Crude Prescription Prevalence"
  }
  
  
  if (show_counts) {
    
    p <- p +
      scale_y_continuous(
        name = prevalence_label,
        
        labels = label_number(
          accuracy = 0.1,
          suffix = "%"
        ),
        
        sec.axis = sec_axis(
          ~ . * scale_factor,
          name = "Number of observed patients with DM",
          labels = comma
        ),
        
        expand = expansion(
          mult = c(0, 0.08)
        )
      )
    
  } else {
    
    p <- p +
      scale_y_continuous(
        name = prevalence_label,
        
        labels = label_number(
          accuracy = 0.1,
          suffix = "%"
        ),
        
        expand = expansion(
          mult = c(0, 0.08)
        )
      )
  }
  
  
  # ============================================================
  # 16. X axis
  # ============================================================
  
  p <- p +
    scale_x_continuous(
      breaks = sort(
        unique(plot_data$YEAR)
      )
    )
  
  
  # ============================================================
  # 17. Caption
  # ============================================================
  
  caption_parts <- c(
    "Solid lines = medication prevalence",
    "shaded areas = 95% CI"
  )
  
  if (show_counts) {
    
    caption_parts <- c(
      caption_parts,
      "bars = annual DM denominator population"
    )
  }
  
  if (!is.null(reference_data)) {
    
    caption_parts <- c(
      caption_parts,
      "dashed line = ALS prevalence among patients with DM"
    )
  }
  
  
  # ============================================================
  # 18. Labels
  # ============================================================
  
  p <- p +
    labs(
      title = "Annual Medication Use Prevalence Among Patients With DM",
      
      subtitle = paste0(
        if (adjusted) "Adjusted" else "Crude",
        " prevalence with 95% confidence intervals"
      ),
      
      x = "Year",
      color = "Medication",
      fill = "Medication",
      
      caption = paste(
        caption_parts,
        collapse = "; "
      )
    )
  
  
  # ============================================================
  # 19. Theme
  # ============================================================
  
  p <- p +
    theme_minimal(
      base_size = 13
    ) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      
      plot.subtitle = element_text(
        size = 11
      ),
      
      axis.title = element_text(
        face = "bold"
      ),
      
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold"
      ),
      
      plot.margin = margin(
        10,
        15,
        10,
        10
      )
    )
  
  
  return(p)
}
