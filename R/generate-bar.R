# visualisation function to create a bar chart in summary table 1
create_bar_plot <- function(data, chosen_activity_type, chosen_measure, title_text = "Example") {
  ggplot(filter(data, activity_type==chosen_activity_type, measure==chosen_measure),
         aes(x=principal, y=pod_name, fill = scenario)) +
    geom_col(position = "dodge") +
    scale_x_continuous(labels = scales::comma) +
    ggtitle(title_text) +
    ylab("Point of delivery") +
    xlab(chosen_measure) +
    scale_fill_discrete("Scenario") +
    scale_fill_manual(values = c("#f9bf07","#686f73"), name="Scenario") +
    easy_center_title() + theme(text = element_text(family = "Segoe UI")) +
    theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.title = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.text = element_text(family = "Segoe UI", size = 12, color="black"))
}


create_bar_plot_los <- function(data, chosen_pod_name, chosen_measure, title_text = "Example") {
  ggplot(filter(data, pod_name==chosen_pod_name, measure==chosen_measure),
         aes(x=principal, y=fct_rev(los_group), fill = scenario)) +
    geom_col(position = "dodge") +
    scale_x_continuous(labels = scales::comma) +
    ggtitle(title_text) +
    ylab("Length of stay group") +
    xlab(chosen_measure) +
    scale_fill_discrete("Scenario") +
    scale_fill_manual(values = c("#f9bf07","#686f73"), name="Scenario") +
    easy_center_title() + theme(text = element_text(family = "Segoe UI")) +
    theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.title = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.text = element_text(family = "Segoe UI", size = 12, color="black"))
}