library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)
library(zeallot)
#library(waterfalls)
library(plotly)

# Functions
file_names <- list.files(path = 'R', pattern = "\\.R$")
lapply(paste0('R/',file_names), source)

file_names_nhs_output <- list.files(path = 'R/nhp_outputs', pattern = "\\.R$")
lapply(paste0('R/nhp_outputs/',file_names_nhs_output), source)

# read Json ndg1
result_ndg1  <- "jsons/Imperialv1-ndg1-20241022_100916.json.gz" |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the

# read Json ndg2
result_ndg2 <- "jsons/Imperialv1-ndg2-20241023_122644.json.gz" |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data

## admissions dataset
data_ndg1_adm <- result_ndg1 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "admissions") 

data_ndg2_adm <- result_ndg2 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "admissions") 

## Bed days dataset
data_ndg1_bed <- result_ndg1 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "beddays") 

data_ndg2_bed <- result_ndg2 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "beddays") 

# data processing
data_admissions <- bind_rows(ndg1 = data_ndg1_adm, ndg2 = data_ndg2_adm, .id = "scenario")
data_bed <- bind_rows(ndg1 = data_ndg1_bed, ndg2 = data_ndg2_bed, .id = "scenario")
data_combine <- bind_rows("Bed Days" = data_bed, admissions = data_admissions, .id = "measure")

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

list <- data.frame(data_combine$pod_name |> unique())
# admission plot
create_bar_plot_los(data_combine, "Non-Elective Admission", "admissions", "Imperial NDG Non-Elective Admission Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Elective Admission", "admissions", "Imperial NDG Elective Admission Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Daycase Admission", "admissions", "Imperial NDG Daycase Admission Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Maternity Admission", "admissions", "Imperial NDG Maternity Admission Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Regular Day Attender Admission", "admissions", "Imperial NDG Regular Day Attender Admission Scenarios Length of Stay")

# bed days plot
create_bar_plot_los(data_combine, "Non-Elective Admission", "Bed Days", "Imperial NDG Non-Elective Bed Days Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Elective Admission", "Bed Days", "Imperial NDG Elective Bed Days Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Daycase Admission", "Bed Days", "Imperial NDG Daycase Bed Days Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Maternity Admission", "Bed Days", "Imperial NDG Maternity Bed Days Scenarios Length of Stay")
create_bar_plot_los(data_combine, "Regular Day Attender Admission", "Bed Days", "Imperial NDG Regular Day Attender Bed Days Scenarios Length of Stay")
