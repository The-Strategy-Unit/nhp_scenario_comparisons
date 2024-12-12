mod_model_results_distribution_get_data <- function(r, selected_measure, site_codes) {
  activity_type <- pod <- measure <- NULL
  zeallot::`%<-%`(c(activity_type, pod, measure), selected_measure)
  get_model_run_distribution(r, pod, measure, site_codes)
}

mod_model_results_distribution_beeswarm_plot <- function(data, show_origin) {
  b <- data$baseline[[1]]
  p <- data$principal[[1]]

  x_placeholder <- "100%" # dummy label to help line up beeswarm and ECDF plots

  data |>
    require_rows() |>
    ggplot2::ggplot() +
    suppressWarnings(
      ggbeeswarm::geom_quasirandom(
        ggplot2::aes(
          x = x_placeholder,
          y = .data$value,
          colour = .data$variant,
          text = glue::glue("Value: {scales::comma(value, accuracy = 1)}\nVariant: {variant}")
        ),
        alpha = 0.5
      )
    ) +
    ggplot2::geom_hline(yintercept = b, colour = "dimgrey") +
    ggplot2::geom_hline(yintercept = p, linetype = "dashed", colour = "red") +
    ggplot2::expand_limits(y = ifelse(show_origin, 0, b)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_extended(9),
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

get_ecdf_quantiles_data <- function(data) {
  ecdf_fn <- stats::ecdf(data[["value"]])

  # Calculate x values for y-axis quantiles
  probs_pcnts <- c(0.1, 0.9)
  x_quantiles <- stats::quantile(ecdf_fn, probs = probs_pcnts)

  return(x_quantiles)
}


mod_model_results_distribution_ecdf_plot <- function(data, show_origin) {
  b <- data$baseline[[1]]
  p <- data$principal[[1]]

  ecdf_fn <- stats::ecdf(data[["value"]])

  # Calculate x values for y-axis quantiles
  probs_pcnts <- c(0.1, 0.9)
  x_quantiles <- stats::quantile(ecdf_fn, probs = probs_pcnts)

  # Calculate y value for principal x value (find nearest % for the principal)
  x_vals <- sort(data[["value"]])
  y_vals <- sort(ecdf_fn(data[["value"]]))
  principal_diffs <- abs(p - x_vals) # nearest x in ECDF to the principal
  min_principal_diff_i <- which(principal_diffs == min(principal_diffs))[1]
  p_pcnt <- y_vals[min_principal_diff_i]

  min_x <- min(b, min(data[["value"]]))
  min_x <- dplyr::if_else(show_origin, 0, min_x)

  line_guides <- tibble::tibble(
    x_start = c(rep(min_x, 3), x_quantiles, p),
    x_end   = rep(c(x_quantiles, p), 2),
    y_start = c(probs_pcnts, p_pcnt, rep(0, 3)),
    y_end   = rep(c(probs_pcnts, p_pcnt), 2),
    colour  = "cornflowerblue"
  )

  lines_n <- nrow(line_guides)
  line_guides[c(lines_n, lines_n / 2), "colour"] <- "red"

  data |>
    require_rows() |>
    ggplot2::ggplot() +
    suppressWarnings(
      ggplot2::geom_point(
        ggplot2::aes(
          x_vals,
          y_vals,
          text = glue::glue(
            "Percentage: {scales::percent(y_vals, accuracy = 1)}\n",
            "Value: {scales::comma(x_vals, accuracy = 1)}"
          )
        ),
        alpha = 0.01,
        size = 0.01
      )
    ) +
    ggplot2::geom_step(ggplot2::aes(x_vals, y_vals)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$x_start,
        y = .data$y_start,
        xend = .data$x_end,
        yend = .data$y_end
      ),
      data = line_guides,
      linetype = "dashed",
      colour = line_guides[["colour"]]
    ) +
    ggplot2::geom_vline(xintercept = b, colour = "dimgrey") +
    ggplot2::ylab("Percentage of model runs") +
    ggplot2::expand_limits(x = ifelse(show_origin, 0, b)) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(9),
      labels = scales::comma,
      expand = c(0.002, 0),
      limits = c(min_x, NA)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(seq(0, 1, 0.1)),
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
}
