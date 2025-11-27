# visualisation function to create a bar chart in summary table 1
create_bar_plot <- function(data, chosen_activity_type, chosen_measure, title_text = "Example") {
  ggplot2::ggplot(
    dplyr::filter(data,
                  activity_type == chosen_activity_type,
                  measure == chosen_measure),
    ggplot2::aes(x = principal, y = pod_name, fill = scenario)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::ggtitle(title_text) +
    ggplot2::ylab("Point of delivery") +
    ggplot2::xlab(chosen_measure) +
    ggplot2::scale_fill_manual(values = c("#f9bf07", "#686f73"), name = "Scenario"#, labels = c(scenario_1_name, scenario_2_name)
    ) +
    ggeasy::easy_center_title() +
    ggplot2::theme(text = element_text(family = "Segoe UI")) +
    ggplot2::theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(legend.title = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(legend.text = element_text(family = "Segoe UI", size = 12, color = "black"))
}


create_bar_plot_los <- function(data, chosen_pod_name, chosen_measure, title_text = "Example") {
  ggplot2::ggplot(
    dplyr::filter(data, pod_name == chosen_pod_name, measure == chosen_measure) |> 
      dplyr::mutate(los_group = factor(los_group,
                                       levels = los_group[order(as.numeric(str_extract(unique(los_group), "^\\d+")))])
      ),
    ggplot2::aes(x = principal, y = forcats::fct_rev(los_group), fill = scenario)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::ggtitle(title_text) +
    ggplot2::ylab("Length of stay group") +
    ggplot2::xlab(chosen_measure) +
    ggplot2::scale_fill_manual(values = c("#f9bf07", "#686f73"), name = "Scenario", #labels = c(scenario_1_name, scenario_2_name)
    ) +
    ggeasy::easy_center_title() +
    ggplot2::theme(text = element_text(family = "Segoe UI")) +
    ggplot2::theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(legend.title = element_text(family = "Segoe UI", size = 12, color = "black")) +
    ggplot2::theme(legend.text = element_text(family = "Segoe UI", size = 12, color = "black"))
}
