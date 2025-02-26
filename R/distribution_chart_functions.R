create_bar_plot_distribution <- function(data, pod_filter, title_text) {
  ggplot(filter(data, pod == pod_filter),
         aes(x = principal, y = measure, fill = scenario)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
    geom_errorbar(aes(xmin = lwr_ci, xmax = upr_ci), width = 0.6, position = position_dodge(0.7)) +
    scale_x_continuous(labels = scales::comma) +
    ggtitle(title_text) +
    ylab("measure") +
    xlab("Principal Projection") +
    scale_fill_manual(values = c("#f9bf07", "#686f73"), name = "Scenario") +
    easy_center_title() + 
    theme(text = element_text(family = "Segoe UI")) +
    theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(legend.title = element_text(family = "Segoe UI", size = 12, color = "black")) +
    theme(legend.text = element_text(family = "Segoe UI", size = 12, color = "black"))
}
