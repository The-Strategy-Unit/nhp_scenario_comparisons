mod_principal_change_factor_effects_summarised <- function(data, measure, include_baseline) {
  data <- data |>
    dplyr::filter(
      .data$measure == .env$measure,
      include_baseline | .data$change_factor != "baseline",
      .data$value != 0
    ) |>
    tidyr::drop_na("value") |>
    dplyr::mutate(
      dplyr::across(
        "change_factor",
        \(.x) forcats::fct_reorder(.x, -.data$value)
      ),
      # baseline may now not be the first item, move it back to start
      dplyr::across(
        "change_factor",
        \(.x) forcats::fct_relevel(.x, "baseline")
      )
    )

  cfs <- data |>
    dplyr::group_by(.data$change_factor) |>
    dplyr::summarise(dplyr::across("value", \(.x) sum(.x, na.rm = TRUE))) |>
    dplyr::mutate(cuvalue = cumsum(.data$value)) |>
    dplyr::mutate(
      hidden = tidyr::replace_na(dplyr::lag(.data$cuvalue) + pmin(.data$value, 0), 0),
      colour = dplyr::case_when(
        .data$change_factor == "Baseline" ~ "#686f73",
        .data$value >= 0 ~ "#f9bf07",
        TRUE ~ "#2c2825"
      ),
      dplyr::across("value", abs)
    ) |>
    dplyr::select(-"cuvalue")

  levels <- unique(c("baseline", levels(forcats::fct_drop(cfs$change_factor)), "Estimate"))
  if (!include_baseline) {
    levels <- levels[-1]
  }

  cfs |>
    dplyr::bind_rows(
      dplyr::tibble(
        change_factor = "Estimate",
        value = sum(data$value),
        hidden = 0,
        colour = "#ec6555"
      )
    ) |>
    tidyr::pivot_longer(c("value", "hidden")) |>
    dplyr::mutate(
      dplyr::across("colour", \(.x) ifelse(.data$name == "hidden", NA, .x)),
      dplyr::across("name", \(.x) forcats::fct_relevel(.x, "hidden", "value")),
      dplyr::across("change_factor", \(.x) factor(.x, rev(levels)))
    )
}

mod_principal_change_factor_effects_cf_plot <- function(data) {
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
    ggplot2::labs(x = "", y = "")
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
