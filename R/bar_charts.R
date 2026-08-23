create_summary_chart <- function(summary_data, activity_type, measure) {
  title_text <- glue::glue("{activity_type} {measure} - Summary Comparison")
  fill_colours <- c("#f9bf07", "#686f73")

  summary_data |>
    dplyr::filter(
      .data[["activity_type_label"]] == .env[["activity_type"]],
      .data[["measure"]] == .env[["measure"]]
    ) |>
    ggplot2::ggplot(ggplot2::aes(.data[["principal"]], .data[["pod_label"]])) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data[["scenario"]]),
      position = "dodge"
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = title_text, x = measure, y = "Point of delivery") +
    core_chart_theme()
}


create_los_chart <- function(los_data, pod) {
  title_text <- glue::glue("{pod} - Length of Stay Comparison")
  fill_colours <- c("#f9bf07", "#686f73")

  los_data <- los_data |>
    dplyr::filter(.data[["pod_label"]] == .env[["pod"]])
  measure <- pull_unique(los_data, "measure")

  los_data |>
    ggplot2::ggplot(ggplot2::aes(.data[["principal"]], .data[["los_group"]])) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data[["scenario"]]),
      position = "dodge"
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = title_text, x = measure, y = "Length of stay group") +
    core_chart_theme()
}


create_principal_pi_chart <- function(principal_pi_data, at, pod) {
  pod_lab <- glue::glue("{at} {pod}")
  title <- glue::glue("{pod_lab} - Principal projection (with p10 and p90 bar)")
  fill_colours <- c("#f9bf07", "#686f73")

  principal_pi_data |>
    dplyr::filter(
      .data[["activity_type_label"]] == .env[["at"]],
      .data[["pod_label"]] == .env[["pod"]]
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      .data[["principal"]],
      .data[["measure"]],
      fill = .data[["scenario"]]
    )) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[["lower"]], xmax = .data[["upper"]]),
      width = 0.6,
      position = ggplot2::position_dodge(0.7)
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = title, x = "Principal projection", y = "Measure") +
    core_chart_theme()
}


create_waterfall_chart <- function(waterfall_data, activity_type, measure) {
  waterfall_data |>
    dplyr::filter(
      .data[["activity_type_label"]] == .env[["activity_type"]],
      .data[["measure_label"]] == .env[["measure"]]
    ) |>
    reskit::make_overall_cf_plot() +
    core_chart_theme() +
    ggplot2::facet_grid(rows = dplyr::vars(.data[["scenario"]]))
}


create_impact_chart <- function(icf_impact_data, cf, at, measure) {
  cf_label <- ifelse(cf == "efficiencies", "Efficiencies", "Activity Avoidance")
  title_2 <- glue::glue("Impact of Individual {cf_label} TPMA Assumptions")
  title_text <- glue::glue("{at} {measure} - {title_2}")
  fill_colours <- c("#f9bf07", "#686f73")

  icf_impact_data |>
    # In the app we will actually provide the data pre-filtered, but I have
    # decided to superfluously retain the equivalent filter step in this
    # function so that it would still work with unfiltered icf_impact_data
    dplyr::filter(
      .data[["change_factor"]] == .env[["cf"]],
      .data[["activity_type_label"]] == .env[["at"]],
      .data[["measure_label"]] == .env[["measure"]],
      .data[["value"]] < 0
    ) |>
    dplyr::mutate(dplyr::across("tpma_label", \(x) stringr::str_wrap(x, 60))) |>
    ggplot2::ggplot(ggplot2::aes(.data[["value"]], .data[["tpma_label"]])) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data[["scenario"]]),
      position = "dodge"
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.5, 0.5))) +
    ggplot2::labs(title = title_text, x = measure, y = "TPMA") +
    core_chart_theme()
}
