library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)

# Functions
file_names <- list.files(path = 'R', pattern = "\\.R$")
lapply(paste0('R/',file_names), source)

file_names_nhs_output <- list.files(path = 'R/nhp_outputs', pattern = "\\.R$")
lapply(paste0('R/nhp_outputs/',file_names_nhs_output), source)

# load dataset 
#source(here('R', 'load-data.R'))
data_distribution_summary <- bind_rows(
  scenario_1 = result_1$results$default, 
  scenario_2 = result_2$results$default, 
  .id = "scenario")

#p <- mod_model_results_distribution_get_data(result_1,selected_measure = c("Ip","ip_elective_admission","admissions"),site_codes = NULL)
data_distribution_summary <- data_distribution_summary |> 
  select(scenario,pod,measure,principal,lwr_ci,upr_ci) |> 
      group_by(scenario,pod,measure) |> 
      summarise(principal = sum(principal),
                lwr_ci = sum(lwr_ci),
                upr_ci = sum(upr_ci)) |> 
      ungroup()

data_distribution_summary <- data_distribution_summary |> 
  mutate(
    activity_type = case_when(
      substr(pod, 1, 2) == "ip" ~ "Inpatient",
      substr(pod, 1, 2) == "op" ~ "Outpatient",
      substr(pod, 1, 2) == "aa" ~ "A&E",
      TRUE ~ "Other"
    )
  ) |> relocate(activity_type, .before = 1)


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


create_bar_plot_distribution(data_distribution_summary,"ip_elective_admission","Imperial NDG Inpatient elective admissions Scenarios")
create_bar_plot_distribution(data_distribution_summary,"ip_elective_daycase","Imperial NDG Inpatient Elective daycase Scenarios")
create_bar_plot_distribution(data_distribution_summary,"ip_non-elective_admission","Imperial NDG Inpatient Non-elective Admissions Scenarios")
create_bar_plot_distribution(data_distribution_summary,"ip_maternity_admission" ,"Imperial NDG Inpatient Maternity Admissions Scenarios")
create_bar_plot_distribution(data_distribution_summary,"ip_regular_day_attender","Imperial NDG Inpatient Regular Day Attender Scenarios")
create_bar_plot_distribution(data_distribution_summary,"op_follow-up","Imperial NDG Outpatient Follow-up Scenarios")
create_bar_plot_distribution(data_distribution_summary,"op_first","Imperial NDG Outpatient First Scenarios")
create_bar_plot_distribution(data_distribution_summary,"op_procedure","Imperial NDG Outpatient Procedure Scenarios")
create_bar_plot_distribution(data_distribution_summary,"aae_type-01","Imperial NDG A&E Type 01 Department Scenarios")
create_bar_plot_distribution(data_distribution_summary,"aae_type-02","Imperial NDG A&E Type 02 Department Scenarios")


