# visualisation function to create a bar chart in summary table 1
create_bar_plot <- function(
  data,
  chosen_activity_type,
  chosen_measure,
  title_text = "Example"
) {
  ggplot2::ggplot(
    dplyr::filter(
      data,
      .data$activity_type == chosen_activity_type,
      .data$measure == chosen_measure
    ),
    ggplot2::aes(x = .data$principal, y = .data$pod_name, fill = .data$id)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::ggtitle(title_text) +
    ggplot2::ylab("Point of delivery") +
    ggplot2::xlab(chosen_measure) +
    ggplot2::scale_fill_manual(
      values = c("#f9bf07", "#686f73"),
      name = "Scenario",
      labels = get_label_map(data)
    ) +
    ggeasy::easy_center_title() +
    ggplot2::theme(text = ggplot2::element_text(family = "Segoe UI")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      legend.text = ggtext::element_markdown(
        family = "Segoe UI",
        size = 12,
        color = "black",
        hjust = 0.5,
        lineheight = 1.5
      ),
      legend.position = "bottom"
    )
}


create_bar_plot_los <- function(
  data,
  chosen_pod_name,
  chosen_measure,
  title_text = "Example"
) {
  ggplot2::ggplot(
    dplyr::filter(
      data,
      .data$pod_name == chosen_pod_name,
      .data$measure == chosen_measure
    ) |>
      dplyr::mutate(
        los_group = factor(
          .data$los_group,
          levels = .data$los_group[order(as.numeric(stringr::str_extract(
            unique(.data$los_group),
            "^\\d+"
          )))]
        )
      ),
    ggplot2::aes(
      x = .data$principal,
      y = forcats::fct_rev(.data$los_group),
      fill = .data$id
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::ggtitle(title_text) +
    ggplot2::ylab("Length of stay group") +
    ggplot2::xlab(chosen_measure) +
    ggplot2::scale_fill_manual(
      values = c("#f9bf07", "#686f73"),
      name = "Scenario",
      labels = get_label_map(data)
    ) +
    ggeasy::easy_center_title() +
    ggplot2::theme(text = ggplot2::element_text(family = "Segoe UI")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        family = "Segoe UI",
        size = 12,
        color = "black"
      )
    ) +
    ggplot2::theme(
      legend.text = ggtext::element_markdown(
        family = "Segoe UI",
        size = 12,
        color = "black",
        hjust = 0.5,
        lineheight = 1.5
      ),
      legend.position = "bottom"
    )
}
