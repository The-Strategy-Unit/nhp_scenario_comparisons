library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)

file_names <- list.files(path = 'R/nhp_outputs', pattern = "\\.R$")
lapply(paste0('R/nhp_outputs/',file_names), source)

# read Json ndg1
path_ndg1 <- "jsons/Imperialv1-ndg1-20241022_100916.json.gz"

result_ndg1 <- path_ndg1 |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data

df_inpatient <- result_ndg1 |>
  mod_principal_summary_data_inpatient(sites = NULL)

df_outpatient <- result_ndg1 |>
  mod_principal_summary_data_outpatient(sites = NULL)

df_ae <- result_ndg1 |>
  mod_principal_summary_data_ae(sites = NULL)

df <- bind_rows(df_inpatient,df_outpatient,df_ae)


# read Json ndg2
path_ndg2 <- "jsons/Imperialv1-ndg2-20241023_122644.json.gz"

result_ndg2 <-  path_ndg2 |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data

df2_inpatient <- result_ndg2 |>
  mod_principal_summary_data(sites = NULL)

df2_outpatient <- result_ndg2 |>
  mod_principal_summary_data_outpatient(sites = NULL)

df2_beds <- result_ndg2 |>
   mod_principal_summary_data_beds(sites = NULL)

df2_ae <- result_ndg2 |>
  mod_principal_summary_data_ae(sites = NULL)

df2 <- bind_rows(df2_inpatient,df2_outpatient,df2_ae)

# data processing
data <- bind_rows(ndg1 = df, ndg2 = df2, .id = "scenario")



# visualisation function
create_bar_plot <- function(data, chosen_activity_type, chosen_measure, title_text = "Example") {
  ggplot(filter(data, activity_type==chosen_activity_type, measure==chosen_measure), 
         aes(x=principal, y=pod_name, fill = scenario)) +
    geom_col(position = "dodge") +
    scale_x_continuous(labels = scales::comma) +
    ggtitle(title_text) +
    ylab("Point of delivery") +
    xlab(chosen_measure) +
    scale_fill_discrete("Scenario")
} 

# Inpatient admissions
create_bar_plot(data, "Inpatient", "Admissions", "Imperial NDG Inpatient Scenarios")

# Inpatient bed days
create_bar_plot(data, "Inpatient", "Bed days", "Imperial NDG Inpatient Scenarios")

# Outpatient attendances
create_bar_plot(data, "Outpatient", "Attendance / procedure", "Imperial NDG Outpatient Scenarios")

# A&E attendances
create_bar_plot(data, "A&E", "Attendance / procedure", "Imperial NDG A&E Scenarios")

