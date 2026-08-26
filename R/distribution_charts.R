create_beeswarm_chart <- function(beeswarm_data, at, measure, show_zero) {
  beeswarm_data <- beeswarm_data |>
    dplyr::filter(
      dplyr::if_any("activity_type_label", \(x) x == {{ at }}),
      dplyr::if_any("measure_label", \(x) x == {{ measure }})
    )
  title <- glue::glue("{at} {measure} - Distribution of Model Runs")
  min_x_value <- min(beeswarm_data[["value"]], beeswarm_data[["baseline"]])
  min_x_value <- ifelse(show_zero, 0, min_x_value)

  summary_tbl <- beeswarm_data |>
    dplyr::summarise(
      dplyr::across(c("baseline", "principal"), unique),
      .by = "scenario"
    )

  summary_tbl |>
    ggplot2::ggplot() +
    ggbeeswarm::geom_quasirandom(
      data = beeswarm_data,
      ggplot2::aes(.data[["value"]], 1, colour = .data[["scenario"]]),
      orientation = "y",
      size = 1.4,
      alpha = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = baseline_value,
      colour = "dimgrey",
      linewidth = 1.2
    ) +
    # Add text labels at the baseline positions
    ggplot2::annotate(
      "text",
      x = baseline_value,
      y = 0.5,
      label = "baseline",
      colour = "dimgrey",
      angle = 90,
      vjust = 1,
      size = 4
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data[["principal"]]),
      colour = "white",
      linewidth = 1.2
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(
        colour = .data[["scenario"]],
        xintercept = .data[["principal"]]
      ),
      show.legend = FALSE,
      linetype = "6111",
      linewidth = 0.6
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data[["baseline"]]),
      colour = "grey50",
      linewidth = 1.2
    ) +
    ggplot2::scale_colour_manual(values = c("red", "blue")) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(8),
      labels = scales::label_comma(),
      limits = c(min_x_value, NA),
      expand = ggplot2::expansion(0.01)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.15)) +
    ggplot2::labs(title = title, x = measure) +
    core_chart_theme() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(rows = dplyr::vars(.data[["scenario"]]))
}


create_ecdf_chart <- function(ecdf_data, activity_type, measure, show_zero) {
  chart_info <- "S-curve\n(empirical cumulative distribution function)"
  title_text <- glue::glue("{activity_type} {tolower(measure)} - {chart_info}")

  ecdf_data <- ecdf_data |>
    dplyr::filter(
      dplyr::if_any("activity_type_label", \(x) x == {{ activity_type }}),
      dplyr::if_any("measure_label", \(x) x == {{ measure }})
    )
  min_x_value <- min(ecdf_data[["value"]], ecdf_data[["baseline"]])
  min_x_value <- ifelse(show_zero, 0, min_x_value)

  ecdf_data_list <- ecdf_data |>
    tidyr::nest(.by = "scenario") |>
    tibble::deframe()
  sns <- names(ecdf_data_list)

  p_quantiles <- c(0.1, 0.9)
  get_quantiles <- \(x) stats::quantile(x, p_quantiles)
  ecdf_fns <- purrr::map(ecdf_data_list, \(x) stats::ecdf(x[["value"]]))
  x_quantiles <- purrr::map(ecdf_fns, get_quantiles)
  y_vals <- purrr::map2(ecdf_data_list, ecdf_fns, \(x, y) sort(y(x[["value"]])))
  y_vals_tbl <- tibble::enframe(y_vals, "scenario", "y_vals")
  summary_tbl <- ecdf_data |>
    dplyr::summarise(
      dplyr::across(c("baseline", "principal"), unique),
      x_vals = list(sort(.data[["value"]])),
      .by = "scenario"
    ) |>
    dplyr::left_join(y_vals_tbl, "scenario")

  # data to support positioning of dashed lines indicating p10 and p90 values
  line_guides <- x_quantiles |>
    tibble::enframe("scenario", "x") |>
    dplyr::mutate(
      y_start = 0,
      y_end = list(p_quantiles),
      # Try to distinguish some lines at least (we can affect dashed but not
      # solid) in the case where the two scenarios have the same values and so
      # their lines are overplotted and one scenario becomes invisible.
      dashtype = dplyr::if_else(.data[["scenario"]] == sns[[1]], "2262", "6222")
    ) |>
    tidyr::unnest_longer(c("x", "y_end"), indices_include = FALSE)

  baseline_value <- summary_tbl[["baseline"]][[1]] # should be 1 value
  summary_tbl |>
    dplyr::select(c("scenario", "principal", "x_vals", "y_vals")) |>
    tidyr::unnest_longer(c("x_vals", "y_vals")) |>
    ggplot2::ggplot(ggplot2::aes(
      .data[["x_vals"]],
      .data[["y_vals"]],
      colour = .data[["scenario"]]
    )) +
    ggplot2::geom_step(ggplot2::aes(group = .data[["scenario"]])) +
    ggplot2::geom_segment(
      data = line_guides,
      ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y_start"]],
        yend = .data[["y_end"]],
        linetype = .data[["dashtype"]]
      ),
      linewidth = 0.6,
      show.legend = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = p_quantiles,
      colour = "dimgrey",
      linetype = "dashed",
      alpha = 0.6,
      linewidth = 0.6
    ) +
    ggplot2::geom_vline(
      xintercept = baseline_value,
      colour = "dimgrey",
      linewidth = 1.2
    ) +
    # Add text labels at the baseline positions
    ggplot2::annotate(
      "text",
      x = baseline_value,
      y = 0.5,
      label = "baseline",
      colour = "dimgrey",
      angle = 90,
      vjust = 1,
      size = 4
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data[["principal"]]),
      colour = "white",
      linewidth = 1.2
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(
        xintercept = .data[["principal"]],
        colour = .data[["scenario"]]
      ),
      show.legend = FALSE,
      linewidth = 0.6,
      linetype = "6111"
    ) +
    ggplot2::scale_colour_manual(values = c("red", "blue")) +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(8),
      labels = scales::label_comma(),
      limits = c(min_x_value, NA),
      expand = ggplot2::expansion(0.01)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.1),
      labels = scales::label_percent()
    ) +
    ggplot2::labs(title = title_text, x = measure, y = "% of model runs") +
    core_chart_theme()
}
