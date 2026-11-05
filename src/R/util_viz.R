# =============================================================================
# Shared publication style
# =============================================================================

pacman::p_load(
  tidyverse,
  scales
)

PUB_PALETTE <- c(
  "#0072B2",  # blue
  "#D55E00",  # vermillion
  "#009E73",  # bluish green
  "#CC79A7",  # reddish purple
  "#E69F00",  # orange
  "#56B4E9",  # sky blue
  "#F0E442",  # yellow
  "#000000"   # black
)

ESTIMATE_LINETYPES <- c(
  "Adjusted" = "solid",
  "Crude"    = "22"
)

make_pub_palette <- function(x) {
  x <- unique(as.character(x))
  
  stats::setNames(
    rep(PUB_PALETTE, length.out = length(x)),
    x
  )
}


theme_publication <- function(
    base_size = 11,
    rotate_x = FALSE
) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(
        size = base_size + 4,
        face = "bold",
        margin = margin(b = 5)
      ),
      
      plot.subtitle = element_text(
        size = base_size,
        margin = margin(b = 10)
      ),
      
      plot.caption = element_text(
        size = base_size - 2,
        hjust = 0,
        color = "grey35",
        lineheight = 1.15,
        margin = margin(t = 10)
      ),
      
      axis.title = element_text(
        face = "bold"
      ),
      
      axis.title.x = element_text(
        margin = margin(t = 8)
      ),
      
      axis.title.y = element_text(
        margin = margin(r = 8)
      ),
      
      axis.title.y.right = element_text(
        margin = margin(l = 8)
      ),
      
      axis.text.x = element_text(
        angle = if (rotate_x) 45 else 0,
        hjust = if (rotate_x) 1 else 0.5,
        vjust = if (rotate_x) 1 else 0.5
      ),
      
      panel.grid.major.y = element_line(
        linewidth = 0.30,
        color = "grey88"
      ),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      legend.box = "vertical",
      
      legend.title = element_text(
        face = "bold"
      ),
      
      legend.key.width = grid::unit(
        1.4,
        "cm"
      ),
      
      plot.margin = margin(
        10, 15, 10, 10
      )
    )
}

# =============================================================================
# General annual rate-curve plot
# =============================================================================

plot_rate_curves <- function(
    data,
    outcomes = NULL,
    populations = NULL,
    metrics = NULL,
    units = NULL,
    estimates = c("ADJUSTED", "CRUDE"),
    
    # Curve labels
    label_style = c("long", "short"),
    
    # Optional denominator overlay
    # NULL = no bars and no secondary axis
    bar_population = NULL,
    denominator_label = NULL,
    denominator_height = 0.80,
    bar_alpha = 0.18,
    bar_width = 0.72,
    
    # Confidence intervals
    ci_display = c("errorbar", "ribbon", "none"),
    ci_alpha = 0.08,
    
    # Figure labels
    title = NULL,
    subtitle = NULL,
    y_label = NULL,
    x_label = "Calendar year",
    caption = NULL,
    
    # Appearance
    series_labels = NULL,
    series_colors = NULL,
    series_shapes = NULL,
    line_width = 1.0,
    point_size = 2.4,
    
    # Y-axis
    y_limits = NULL,
    y_accuracy = NULL
) {
  
  # ===========================================================================
  # 1. Validate arguments
  # ===========================================================================
  
  required_cols <- c(
    "YEAR",
    "OUTCOME",
    "POPULATION",
    "METRIC",
    "ESTIMATE_TYPE",
    "NUM",
    "DEN",
    "ESTIMATE",
    "LCL",
    "UCL",
    "UNIT"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(data)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  label_style <- match.arg(label_style)
  ci_display <- match.arg(ci_display)
  
  estimates <- unique(
    toupper(estimates)
  )
  
  valid_estimates <- c(
    "ADJUSTED",
    "CRUDE"
  )
  
  if (
    length(estimates) == 0 ||
    any(!estimates %in% valid_estimates)
  ) {
    stop(
      "`estimates` must contain one or both of: ",
      "'ADJUSTED', 'CRUDE'."
    )
  }
  
  if (
    !is.null(bar_population) &&
    length(bar_population) != 1
  ) {
    stop(
      "`bar_population` must be NULL or a single population."
    )
  }
  
  if (
    !is.numeric(denominator_height) ||
    length(denominator_height) != 1 ||
    is.na(denominator_height) ||
    denominator_height <= 0 ||
    denominator_height > 1
  ) {
    stop(
      "`denominator_height` must be between 0 and 1."
    )
  }
  
  if (
    !is.numeric(bar_alpha) ||
    length(bar_alpha) != 1 ||
    is.na(bar_alpha) ||
    bar_alpha < 0 ||
    bar_alpha > 1
  ) {
    stop(
      "`bar_alpha` must be between 0 and 1."
    )
  }
  
  
  # ===========================================================================
  # 2. Standardize input
  # ===========================================================================
  
  full_data <- data |>
    dplyr::transmute(
      
      YEAR =
        as.integer(.data$YEAR),
      
      OUTCOME =
        as.character(.data$OUTCOME),
      
      POPULATION =
        as.character(.data$POPULATION),
      
      METRIC =
        toupper(as.character(.data$METRIC)),
      
      ESTIMATE_TYPE =
        toupper(as.character(.data$ESTIMATE_TYPE)),
      
      NUM =
        as.numeric(.data$NUM),
      
      DEN =
        as.numeric(.data$DEN),
      
      ESTIMATE =
        as.numeric(.data$ESTIMATE),
      
      LCL =
        as.numeric(.data$LCL),
      
      UCL =
        as.numeric(.data$UCL),
      
      UNIT =
        as.character(.data$UNIT)
    ) |>
    
    dplyr::filter(
      .data$ESTIMATE_TYPE %in%
        estimates
    )
  
  
  # ===========================================================================
  # 3. Apply scientific-measure filters
  #
  # Population is deliberately NOT filtered here.
  #
  # analysis_data provides:
  #   1. the source for plotted curves
  #   2. the source for bar_population
  #
  # This means:
  #
  # populations = c("DM", "NON_DM")
  # bar_population = "ALL"
  #
  # is valid.
  # ===========================================================================
  
  analysis_data <- full_data
  
  
  if (!is.null(outcomes)) {
    
    analysis_data <- analysis_data |>
      dplyr::filter(
        .data$OUTCOME %in%
          outcomes
      )
  }
  
  
  if (!is.null(metrics)) {
    
    metrics <- toupper(metrics)
    
    analysis_data <- analysis_data |>
      dplyr::filter(
        .data$METRIC %in%
          metrics
      )
  }
  
  
  if (!is.null(units)) {
    
    analysis_data <- analysis_data |>
      dplyr::filter(
        .data$UNIT %in%
          units
      )
  }
  
  
  if (nrow(analysis_data) == 0) {
    
    stop(
      paste0(
        "No observations remain after applying ",
        "outcome, metric, unit, and estimate filters."
      )
    )
  }
  
  
  # ===========================================================================
  # 4. Select populations used for rate curves
  # ===========================================================================
  
  plot_data <- analysis_data
  
  
  if (!is.null(populations)) {
    
    plot_data <- plot_data |>
      dplyr::filter(
        .data$POPULATION %in%
          populations
      )
  }
  
  
  if (nrow(plot_data) == 0) {
    
    stop(
      "No curve observations remain after applying `populations`."
    )
  }
  
  
  # ===========================================================================
  # 5. Require a common unit across plotted curves
  # ===========================================================================
  
  unit_values <- unique(
    stats::na.omit(
      plot_data$UNIT
    )
  )
  
  
  if (length(unit_values) > 1) {
    
    stop(
      paste0(
        "The selected curves use multiple units: ",
        paste(
          unit_values,
          collapse = ", "
        ),
        ". Plot measures with a common unit together ",
        "or use `units` to select one."
      )
    )
  }
  
  
  plot_unit <- if (
    length(unit_values) == 1
  ) {
    unit_values[[1]]
  } else {
    NULL
  }
  
  
  # ===========================================================================
  # 6. Population display names
  # ===========================================================================
  
  population_display <- function(x) {
    
    dplyr::case_when(
      
      x == "ALL" ~
        "the overall population",
      
      x == "DM" ~
        "persons with diabetes",
      
      x == "T1DM" ~
        "persons with T1DM",
      
      x == "T2DM" ~
        "persons with T2DM",
      
      x == "ALS" ~
        "persons with ALS",
      
      x == "NON_DM" ~
        "persons without diabetes",
      
      TRUE ~
        tolower(x)
    )
  }
  
  
  # ===========================================================================
  # 7. Construct unique series identifiers
  #
  # SERIES_ID is always unique and is used internally for:
  #   color
  #   shape
  #   grouping
  #
  # SERIES_LABEL is display-only.
  # ===========================================================================
  
  plot_data <- plot_data |>
    dplyr::mutate(
      
      SERIES_ID = paste(
        .data$OUTCOME,
        .data$METRIC,
        .data$POPULATION,
        sep = "__"
      )
    )
  
  
  # ===========================================================================
  # 8. Construct display labels
  #
  # short:
  #   ALS
  #   DM
  #   GLP1RA
  #
  # long:
  #   ALS incidence
  #   ALS incidence among persons with diabetes
  #   DM prevalence among persons with ALS
  # ===========================================================================
  
  plot_data <- plot_data |>
    dplyr::mutate(
      
      SERIES_LABEL = if (
        label_style == "short"
      ) {
        
        .data$OUTCOME
        
      } else {
        
        dplyr::case_when(
          
          .data$POPULATION == "ALL" ~
            paste(
              .data$OUTCOME,
              tolower(.data$METRIC)
            ),
          
          TRUE ~
            paste0(
              .data$OUTCOME,
              " ",
              tolower(.data$METRIC),
              " among ",
              population_display(
                .data$POPULATION
              )
            )
        )
      }
    )
  
  
  # ===========================================================================
  # 9. Optional custom series labels
  #
  # series_labels should be a named vector keyed by SERIES_ID:
  #
  # c(
  #   "ALS__INCIDENCE__ALL" = "Overall",
  #   "ALS__INCIDENCE__DM" = "Diabetes",
  #   "ALS__INCIDENCE__NON_DM" = "No diabetes"
  # )
  # ===========================================================================
  
  if (!is.null(series_labels)) {
    
    custom_labels <- unname(
      series_labels[
        plot_data$SERIES_ID
      ]
    )
    
    
    plot_data <- plot_data |>
      dplyr::mutate(
        
        SERIES_LABEL =
          dplyr::if_else(
            !is.na(custom_labels),
            custom_labels,
            .data$SERIES_LABEL
          )
      )
  }
  
  
  # ===========================================================================
  # 10. Series lookup table
  # ===========================================================================
  
  series_lookup <- plot_data |>
    dplyr::distinct(
      .data$SERIES_ID,
      .data$SERIES_LABEL
    )
  
  
  series_ids <-
    series_lookup$SERIES_ID
  
  display_labels <-
    series_lookup$SERIES_LABEL
  
  
  # ===========================================================================
  # 11. Estimate factor + grouping
  #
  # Important:
  # GROUP_ID uses SERIES_ID, not SERIES_LABEL.
  #
  # Therefore repeated short labels such as:
  #
  # ALS
  # ALS
  # ALS
  #
  # do not merge the three trajectories.
  # ===========================================================================
  
  plot_data <- plot_data |>
    dplyr::mutate(
      
      ESTIMATE_TYPE = factor(
        .data$ESTIMATE_TYPE,
        
        levels = c(
          "ADJUSTED",
          "CRUDE"
        ),
        
        labels = c(
          "Adjusted",
          "Crude"
        )
      ),
      
      GROUP_ID = interaction(
        .data$SERIES_ID,
        .data$ESTIMATE_TYPE,
        drop = TRUE,
        lex.order = TRUE
      )
    )
  
  
  # ===========================================================================
  # 12. Publication colors
  # ===========================================================================
  
  if (is.null(series_colors)) {
    
    series_colors <- stats::setNames(
      rep(
        PUB_PALETTE,
        length.out =
          length(series_ids)
      ),
      series_ids
    )
    
  } else {
    
    # Require a color for every SERIES_ID
    missing_colors <- setdiff(
      series_ids,
      names(series_colors)
    )
    
    
    if (length(missing_colors) > 0) {
      
      stop(
        paste0(
          "`series_colors` is missing colors for: ",
          paste(
            missing_colors,
            collapse = ", "
          )
        )
      )
    }
    
    
    series_colors <-
      series_colors[
        series_ids
      ]
  }
  
  
  # ===========================================================================
  # 13. Publication shapes
  # ===========================================================================
  
  if (is.null(series_shapes)) {
    
    series_shapes <- stats::setNames(
      rep(
        c(
          16,
          17,
          15,
          18,
          8,
          3,
          7,
          4
        ),
        
        length.out =
          length(series_ids)
      ),
      
      series_ids
    )
    
  } else {
    
    missing_shapes <- setdiff(
      series_ids,
      names(series_shapes)
    )
    
    
    if (length(missing_shapes) > 0) {
      
      stop(
        paste0(
          "`series_shapes` is missing shapes for: ",
          paste(
            missing_shapes,
            collapse = ", "
          )
        )
      )
    }
    
    
    series_shapes <-
      series_shapes[
        series_ids
      ]
  }
  
  
  # Labels supplied to ggplot scales
  series_label_map <- stats::setNames(
    display_labels,
    series_ids
  )
  
  
  # ===========================================================================
  # 14. Determine primary y-axis range
  # ===========================================================================
  
  rate_values <- c(
    plot_data$ESTIMATE,
    plot_data$LCL,
    plot_data$UCL
  )
  
  
  rate_values <- rate_values[
    is.finite(rate_values)
  ]
  
  
  if (length(rate_values) == 0) {
    
    stop(
      "No finite estimates or confidence limits are available."
    )
  }
  
  
  if (is.null(y_limits)) {
    
    y_lower <- min(
      0,
      min(rate_values)
    )
    
    
    y_upper_raw <- max(
      rate_values
    )
    
    
    y_range <-
      y_upper_raw -
      y_lower
    
    
    if (
      !is.finite(y_range) ||
      y_range <= 0
    ) {
      
      y_range <- max(
        abs(rate_values)
      )
    }
    
    
    y_upper <-
      y_upper_raw +
      0.12 * y_range
    
  } else {
    
    if (
      length(y_limits) != 2 ||
      any(!is.finite(y_limits)) ||
      y_limits[1] >= y_limits[2]
    ) {
      
      stop(
        "`y_limits` must contain two increasing finite values."
      )
    }
    
    
    y_lower <-
      y_limits[1]
    
    y_upper <-
      y_limits[2]
  }
  
  
  # ===========================================================================
  # 15. Y-axis label accuracy
  # ===========================================================================
  
  if (is.null(y_accuracy)) {
    
    max_rate <- max(
      abs(rate_values),
      na.rm = TRUE
    )
    
    
    y_accuracy <- dplyr::case_when(
      
      max_rate < 1 ~
        0.01,
      
      max_rate < 100 ~
        0.1,
      
      TRUE ~
        1
    )
  }
  
  
  # ===========================================================================
  # 16. Optional denominator bars
  #
  # bar_population = NULL
  #     -> no bars
  #     -> no secondary y-axis
  #
  # bar_population != NULL
  #     -> select DEN for that population from analysis_data
  #
  # Note that bar_population is independent from populations.
  # ===========================================================================
  
  denominator_data <- NULL
  denominator_scale <- NULL
  
  
  if (!is.null(bar_population)) {
    
    # -------------------------------------------------------------------------
    # Check requested population exists in the scientifically filtered data
    # -------------------------------------------------------------------------
    
    available_bar_populations <- analysis_data |>
      dplyr::distinct(
        .data$POPULATION
      ) |>
      dplyr::pull(
        .data$POPULATION
      )
    
    
    if (
      !bar_population %in%
      available_bar_populations
    ) {
      
      stop(
        paste0(
          "`bar_population = \"",
          bar_population,
          "\"` is not available after applying ",
          "outcome/metric/unit filters. ",
          "Available populations are: ",
          paste(
            available_bar_populations,
            collapse = ", "
          ),
          "."
        )
      )
    }
    
    
    # -------------------------------------------------------------------------
    # Extract annual denominator
    #
    # CRUDE and ADJUSTED rows normally repeat the same observed DEN,
    # so distinct() removes those duplicates.
    # -------------------------------------------------------------------------
    
    denominator_data <- analysis_data |>
      dplyr::filter(
        .data$POPULATION ==
          bar_population
      ) |>
      dplyr::distinct(
        .data$YEAR,
        .data$DEN
      )
    
    
    # -------------------------------------------------------------------------
    # Require one denominator per year
    # -------------------------------------------------------------------------
    
    denominator_check <- denominator_data |>
      dplyr::count(
        .data$YEAR,
        name = "N_DENOMINATORS"
      ) |>
      dplyr::filter(
        .data$N_DENOMINATORS > 1
      )
    
    
    if (
      nrow(denominator_check) > 0
    ) {
      
      stop(
        paste0(
          "`bar_population = \"",
          bar_population,
          "\"` has multiple denominator values within some years. ",
          "Restrict `outcomes`, `metrics`, and/or `units` so ",
          "the selected bar population corresponds to a single ",
          "denominator definition."
        )
      )
    }
    
    
    denominator_data <- denominator_data |>
      dplyr::arrange(
        .data$YEAR
      )
    
    
    # -------------------------------------------------------------------------
    # Validate denominator values
    # -------------------------------------------------------------------------
    
    max_denominator <- max(
      denominator_data$DEN,
      na.rm = TRUE
    )
    
    
    if (
      !is.finite(max_denominator) ||
      max_denominator <= 0
    ) {
      
      stop(
        paste0(
          "`DEN` for bar_population = \"",
          bar_population,
          "\" must contain at least one positive value."
        )
      )
    }
    
    
    # -------------------------------------------------------------------------
    # Transform denominator values onto primary axis coordinates
    #
    # Largest bar occupies denominator_height proportion of plotting height.
    # -------------------------------------------------------------------------
    
    primary_height <-
      y_upper -
      y_lower
    
    
    denominator_scale <-
      max_denominator /
      (
        primary_height *
          denominator_height
      )
    
    
    denominator_data <- denominator_data |>
      dplyr::mutate(
        
        DEN_SCALED =
          y_lower +
          .data$DEN /
          denominator_scale
      )
    
    
    # -------------------------------------------------------------------------
    # Automatic right-axis label
    # -------------------------------------------------------------------------
    
    if (
      is.null(denominator_label)
    ) {
      
      denominator_label <- dplyr::case_when(
        
        bar_population == "ALL" ~
          "Overall denominator",
        
        bar_population == "DM" ~
          "Diabetes population",
        
        bar_population == "T1DM" ~
          "T1DM population",
        
        bar_population == "T2DM" ~
          "T2DM population",
        
        bar_population == "ALS" ~
          "ALS population",
        
        bar_population == "NON_DM" ~
          "Non-diabetes population",
        
        TRUE ~
          paste0(
            bar_population,
            " denominator"
          )
      )
    }
  }
  
  
  # ===========================================================================
  # 17. Initialize ggplot
  # ===========================================================================
  
  p <- ggplot2::ggplot()
  
  
  # ===========================================================================
  # 18. Denominator bars
  #
  # Draw first so all epidemiologic estimates remain visually foregrounded.
  # ===========================================================================
  
  if (!is.null(bar_population)) {
    
    p <- p +
      
      ggplot2::geom_col(
        data =
          denominator_data,
        
        ggplot2::aes(
          x = .data$YEAR,
          y = .data$DEN_SCALED
        ),
        
        width =
          bar_width,
        
        fill =
          "grey70",
        
        color =
          NA,
        
        alpha =
          bar_alpha
      )
  }
  
  
  # ===========================================================================
  # 19. Confidence intervals
  # ===========================================================================
  
  if (
    ci_display == "ribbon"
  ) {
    
    p <- p +
      
      ggplot2::geom_ribbon(
        data =
          plot_data,
        
        ggplot2::aes(
          x = .data$YEAR,
          ymin = .data$LCL,
          ymax = .data$UCL,
          
          fill = .data$SERIES_ID,
          
          group = .data$GROUP_ID
        ),
        
        alpha =
          ci_alpha,
        
        color =
          NA,
        
        show.legend =
          FALSE,
        
        na.rm =
          TRUE
      )
  }
  
  
  if (
    ci_display == "errorbar"
  ) {
    
    p <- p +
      
      ggplot2::geom_errorbar(
        data =
          plot_data,
        
        ggplot2::aes(
          x = .data$YEAR,
          ymin = .data$LCL,
          ymax = .data$UCL,
          
          color = .data$SERIES_ID,
          
          group = .data$GROUP_ID
        ),
        
        position =
          ggplot2::position_dodge(
            width = 0.18
          ),
        
        width =
          0.10,
        
        linewidth =
          0.45,
        
        alpha =
          0.65,
        
        show.legend =
          FALSE,
        
        na.rm =
          TRUE
      )
  }
  
  
  # ===========================================================================
  # 20. Estimate curves
  # ===========================================================================
  
  p <- p +
    
    ggplot2::geom_line(
      data =
        plot_data,
      
      ggplot2::aes(
        x = .data$YEAR,
        y = .data$ESTIMATE,
        
        color = .data$SERIES_ID,
        
        linetype =
          .data$ESTIMATE_TYPE,
        
        group =
          .data$GROUP_ID
      ),
      
      linewidth =
        line_width,
      
      na.rm =
        TRUE
    ) +
    
    ggplot2::geom_point(
      data =
        plot_data,
      
      ggplot2::aes(
        x = .data$YEAR,
        y = .data$ESTIMATE,
        
        color =
          .data$SERIES_ID,
        
        shape =
          .data$SERIES_ID,
        
        group =
          .data$GROUP_ID
      ),
      
      size =
        point_size,
      
      stroke =
        0.45,
      
      na.rm =
        TRUE
    )
  
  
  # ===========================================================================
  # 21. Series scales
  # ===========================================================================
  
  p <- p +
    
    ggplot2::scale_color_manual(
      name = "Measure",
      
      values =
        series_colors,
      
      breaks =
        series_ids,
      
      labels =
        series_label_map,
      
      drop =
        FALSE
    ) +
    
    ggplot2::scale_fill_manual(
      values =
        series_colors,
      
      breaks =
        series_ids,
      
      labels =
        series_label_map,
      
      drop =
        FALSE
    ) +
    
    ggplot2::scale_shape_manual(
      name = "Measure",
      
      values =
        series_shapes,
      
      breaks =
        series_ids,
      
      labels =
        series_label_map,
      
      drop =
        FALSE
    ) +
    
    ggplot2::scale_linetype_manual(
      name = "Estimate",
      
      values = c(
        "Adjusted" = "solid",
        "Crude" = "22"
      ),
      
      drop =
        FALSE
    )
  
  
  # ===========================================================================
  # 22. X axis
  # ===========================================================================
  
  p <- p +
    
    ggplot2::scale_x_continuous(
      name =
        x_label,
      
      breaks =
        sort(
          unique(
            plot_data$YEAR
          )
        ),
      
      minor_breaks =
        NULL,
      
      expand =
        ggplot2::expansion(
          mult = c(
            0.025,
            0.025
          )
        )
    )
  
  
  # ===========================================================================
  # 23. Default primary y-axis label
  # ===========================================================================
  
  if (
    is.null(y_label)
  ) {
    
    y_label <- if (
      !is.null(plot_unit)
    ) {
      plot_unit
    } else {
      "Rate"
    }
  }
  
  
  # ===========================================================================
  # 24. Y axis
  #
  # Secondary axis exists only when bar_population is supplied.
  # ===========================================================================
  
  if (!is.null(bar_population)) {
    
    p <- p +
      
      ggplot2::scale_y_continuous(
        name =
          y_label,
        
        limits = c(
          y_lower,
          y_upper
        ),
        
        labels =
          scales::label_number(
            accuracy =
              y_accuracy,
            
            big.mark =
              ","
          ),
        
        expand =
          ggplot2::expansion(
            mult = c(
              0,
              0.02
            )
          ),
        
        sec.axis =
          ggplot2::sec_axis(
            
            # Inverse transformation:
            #
            # DEN_SCALED =
            #   y_lower +
            #   DEN / denominator_scale
            
            ~ (. - y_lower) *
              denominator_scale,
            
            name =
              denominator_label,
            
            labels =
              scales::label_number(
                scale_cut =
                  scales::cut_short_scale(),
                
                accuracy =
                  0.1
              )
          )
      )
    
  } else {
    
    p <- p +
      
      ggplot2::scale_y_continuous(
        name =
          y_label,
        
        limits = c(
          y_lower,
          y_upper
        ),
        
        labels =
          scales::label_number(
            accuracy =
              y_accuracy,
            
            big.mark =
              ","
          ),
        
        expand =
          ggplot2::expansion(
            mult = c(
              0,
              0.02
            )
          )
      )
  }
  
  
  # ===========================================================================
  # 25. Automatic subtitle
  # ===========================================================================
  
  if (
    is.null(subtitle)
  ) {
    
    subtitle <- dplyr::case_when(
      
      all(
        c(
          "ADJUSTED",
          "CRUDE"
        ) %in%
          estimates
      ) ~
        paste0(
          "Crude and adjusted annual estimates ",
          "with 95% confidence intervals"
        ),
      
      "ADJUSTED" %in%
        estimates ~
        paste0(
          "Adjusted annual estimates ",
          "with 95% confidence intervals"
        ),
      
      TRUE ~
        paste0(
          "Crude annual estimates ",
          "with 95% confidence intervals"
        )
    )
  }
  
  
  # ===========================================================================
  # 26. Automatic caption
  # ===========================================================================
  
  if (
    is.null(caption)
  ) {
    
    caption_parts <-
      character()
    
    
    if (
      all(
        c(
          "ADJUSTED",
          "CRUDE"
        ) %in%
        estimates
      )
    ) {
      
      caption_parts <- c(
        caption_parts,
        
        paste0(
          "Adjusted estimates are shown with solid lines ",
          "and crude estimates with dashed lines"
        )
      )
    }
    
    
    if (
      ci_display ==
      "errorbar"
    ) {
      
      caption_parts <- c(
        caption_parts,
        "Error bars indicate 95% confidence intervals"
      )
    }
    
    
    if (
      ci_display ==
      "ribbon"
    ) {
      
      caption_parts <- c(
        caption_parts,
        "Shaded bands indicate 95% confidence intervals"
      )
    }
    
    
    if (
      !is.null(bar_population)
    ) {
      
      caption_parts <- c(
        caption_parts,
        
        paste0(
          "Light-grey bars represent the annual denominator ",
          "for population = ",
          bar_population
        )
      )
    }
    
    
    caption <- if (
      length(caption_parts) > 0
    ) {
      
      paste0(
        paste(
          caption_parts,
          collapse = "; "
        ),
        "."
      )
      
    } else {
      NULL
    }
  }
  
  
  # ===========================================================================
  # 27. Final labels and styling
  # ===========================================================================
  
  p <- p +
    
    ggplot2::labs(
      title =
        title,
      
      subtitle =
        subtitle,
      
      caption =
        caption
    ) +
    
    ggplot2::guides(
      
      color =
        ggplot2::guide_legend(
          order = 1,
          
          override.aes = list(
            linetype = "solid",
            linewidth = 1.1
          )
        ),
      
      # Shape is encoded in the color legend.
      shape =
        "none",
      
      fill =
        "none",
      
      linetype =
        ggplot2::guide_legend(
          order = 2,
          
          override.aes = list(
            color = "grey20",
            linewidth = 1.1
          )
        )
    ) +
    
    theme_publication(
      base_size = 11,
      
      rotate_x =
        length(
          unique(
            plot_data$YEAR
          )
        ) > 12
    )
  
  
  return(p)
}






