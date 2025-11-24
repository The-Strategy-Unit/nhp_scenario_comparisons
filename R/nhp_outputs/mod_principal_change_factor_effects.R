#function to create waterfall chart


mod_principal_change_factor_effects_cf_plot <- function(data) {
  
  #labels <- c("scenario_1" = scenario_1_name, "scenario_2" = scenario_2_name)
  
  data |>
    dplyr::mutate(
      tooltip = ifelse(
        .data[["name"]] == "hidden",
        0,
        .data[["value"]]
      ),
      tooltip = glue::glue(
        "{snakecase::to_title_case(as.character(change_factor))}: ",
        "{scales::comma(sum(tooltip), accuracy = 1)}"
      ),
      .by = "change_factor"
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        .data[["value"]],
        .data[["change_factor"]],
        text = .data[["tooltip"]]
      )
    ) +
    ggplot2::geom_col(
      ggplot2::aes(
        fill = .data[["colour"]]
      ),
      show.legend = FALSE,
      position = "stack"
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(5),
      labels = scales::comma
    ) +
    ggplot2::scale_y_discrete(labels = snakecase::to_title_case) +
    ggplot2::labs(x = "", y = "") + 
    ggplot2::facet_grid(
      rows = vars(scenario)#,
      #labeller = labeller(scenario = labels)
      )
}

mod_principal_change_factor_effects_ind_plot <- function(data, change_factor, colour, title, x_axis_label) {
  data |>
    dplyr::filter(.data$change_factor == .env$change_factor) |>
    dplyr::mutate(
      tooltip = glue::glue("{mitigator_name}: {scales::comma(value, accuracy = 1)}")
    ) |>
    require_rows() |>
    ggplot2::ggplot(
      ggplot2::aes(.data$value, .data$mitigator_name, text = .data[["tooltip"]])
    ) +
    ggplot2::geom_col(fill = "#2c2825") +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(5),
      labels = scales::comma
    ) +
    ggplot2::labs(title = title, x = x_axis_label, y = "")
}
