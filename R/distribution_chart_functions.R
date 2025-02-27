create_bar_plot_distribution <- function(data, pod_filter, title_text) {
  ggplot(filter(data, pod == pod_filter),
         aes(x = principal, y = measure, fill = scenario)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
    geom_errorbar(aes(xmin = lwr_ci, xmax = upr_ci), width = 0.6, position = position_dodge(0.7)) +
    scale_x_continuous(labels = scales::comma) +
    ggtitle(title_text) +
    ylab("measure") +
    xlab("Principal Projection") +
    scale_fill_manual(values = c("#f9bf07", "#686f73"), name = "Scenario", labels = c(scenario_1_name, scenario_2_name)) +
    easy_center_title() + 
    theme(text = element_text(family = "Segoe UI")) +
    theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(legend.title = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(legend.text = element_text(family = "Segoe UI", size = 12, color = "black"))
}

# new function for beeswarm -----------------------------------------------

mod_model_results_distribution_beeswarm_plot_scenario <- function(data, show_origin) {
  b <- data$baseline[[1]]
  # two lines instead of 1 below for the separate principal projections
  p1 <- data$principal[data$scenario=="scenario_1"][[1]]
  p2 <- data$principal[data$scenario=="scenario_2"][[1]]
  
  x_placeholder <- "100%" # dummy label to help line up beeswarm and ECDF plots
  
  data |>
    require_rows() |>
    ggplot2::ggplot() +
    suppressWarnings(
      ggbeeswarm::geom_quasirandom(
        ggplot2::aes(
          x = x_placeholder,
          y = .data$value,
          # colour by scenario instead of variant here
          colour = .data$scenario,
          text = glue::glue("Value: {scales::comma(value, accuracy = 1)}\nVariant: {variant}")
        ),
        alpha = 0.5
      )
    ) +
    # new line here for manually setting the colours
    ggplot2::scale_color_manual(values = c(scenario_1 = "red", scenario_2 = "blue"), 
                                labels = c(scenario_1_name, scenario_2_name)) +
    ggplot2::geom_hline(yintercept = b, colour = "dimgrey") +
    # two lines instead of 1 below for the separate principal projections
    ggplot2::geom_hline(yintercept = p1, linetype = "dashed", colour = "red") +
    ggplot2::geom_hline(yintercept = p2, linetype = "dashed", colour = "blue") +
    ggplot2::expand_limits(y = ifelse(show_origin, 0, b)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(10),
      labels = scales::comma,
      expand = c(0.002, 0)
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      # keep y-axis labels to help line up beeswarm/ECDF, but make 'invisible'
      axis.text.y = ggplot2::element_text(colour = "white"),
      axis.title.y = ggplot2::element_text(colour = "white")
    )
}


# new function for s-curve ------------------------------------------------

mod_model_results_distribution_ecdf_plot_scenario <- function(data, show_origin) {
  
  percentiles <- data |>
    group_by(scenario) |>
    summarise(
      baseline = baseline[[1]],
      p10 = quantile(value, 0.1),
      principal = principal[[1]],
      p90 = quantile(value, 0.9)
    )
  
  # Calculate y value for principal x value (find nearest % for the principal)
  # x_vals <- sort(data[["value"]])
  # y_vals <- sort(ecdf_fn(data[["value"]]))
  # principal_diffs <- abs(p - x_vals) # nearest x in ECDF to the principal
  # min_principal_diff_i <- which(principal_diffs == min(principal_diffs))[1]
  # p_pcnt <- y_vals[min_principal_diff_i]
  
  min_x <- min(percentiles$baseline, min(data[["value"]]))
  min_x <- dplyr::if_else(show_origin, 0, min_x)
  
  # line_guides <- tibble::tibble(
  #   x_start = c(rep(min_x, 3), x_quantiles, p),
  #   x_end   = rep(c(x_quantiles, p), 2),
  #   y_start = c(probs_pcnts, p_pcnt, rep(0, 3)),
  #   y_end   = rep(c(probs_pcnts, p_pcnt), 2),
  #   colour  = "cornflowerblue"
  # )
  # 
  # lines_n <- nrow(line_guides)
  # line_guides[c(lines_n, lines_n / 2), "colour"] <- "red"
  
  ggplot(data, aes(x = value, color = scenario)) +
    stat_ecdf(geom = "step") +
    # Add vertical dashed lines at 10% and 90%
    geom_segment(data = percentiles, aes(x = p10, xend = p10, y = 0, yend = 0.1, color = "blue"), linetype = "dashed", show.legend = FALSE) +
    geom_segment(data = percentiles, aes(x = p90, xend = p90, y = 0, yend = 0.9, color = "blue"), linetype = "dashed", show.legend = FALSE) +
    geom_segment(data = percentiles, aes(x = principal, xend = principal, y = 0, yend = 0.9, color = "red"), linetype = "dashed", show.legend = FALSE) +
    # Add horizontal dashed lines at 10% and 90%
    geom_segment(data = percentiles, aes(x = -Inf, xend = p10, y = 0.1, yend = 0.1, color = "blue"), linetype = "dashed", show.legend = FALSE) +
    geom_segment(data = percentiles, aes(x = -Inf, xend = p90, y = 0.9, yend = 0.9, color = "blue"), linetype = "dashed", show.legend = FALSE) +
    geom_segment(data = percentiles, aes(x = -Inf, xend = principal, y = 0.9, yend = 0.9, color = "red"), linetype = "dashed", show.legend = FALSE) +
    ggplot2::geom_vline(xintercept = percentiles$baseline, colour = "dimgrey") +
    labs(
      x = "Value",
      y = "Proportion (ECDF)",
      title = "Empirical Cumulative Distribution Function with 10% and 90% Markers"
    ) +
    ggplot2::ylab("Percentage of model runs") +
    ggplot2::expand_limits(x = ifelse(FALSE, 0, percentiles$baseline[[1]])) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(10),
      labels = scales::comma,
      expand = c(0.002, 0),
      limits = c(min_x, NA)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(seq(0, 1, 0.1)),
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c(scenario_1 = "red", scenario_2 = "blue"),
                                labels = c(scenario_1_name, scenario_2_name)) 
    
}


