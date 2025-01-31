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

# Prep a list of summary dataframes, one per activity type
pcfs_ndg1 <- prepare_all_principal_change_factors(
  r = result_ndg1,
  site_codes = list(ip = NULL, op = NULL, aae = NULL)
)

pcfs_ndg2 <- prepare_all_principal_change_factors(
  r = result_ndg2,
  site_codes = list(ip = NULL, op = NULL, aae = NULL)
)


### Viz ###
#strategyunit_colours <-c("#686f73", "#f9bf07", "#5881c1", "#ec6555")

generate_waterfall_plot(pcfs_ndg1, pcfs_ndg2, 
                        activity_type_ndg1 = "ip", 
                        activity_type_ndg2 = "ip",
                        measure = "admissions", 
                        title = "Inpatient admissions waterfall scenario comparison", 
                        x_label = "Admissions", 
                        y_label = "Change factor")

generate_waterfall_plot(pcfs_ndg1, pcfs_ndg2, 
                        activity_type_ndg1 = "ip",
                        activity_type_ndg2 = "ip",
                        measure = "beddays", 
                        title = "Inpatient bed days waterfall scenario comparison", 
                        x_label = "Bed days", 
                        y_label = "Change factor")

generate_waterfall_plot(pcfs_ndg1, pcfs_ndg2, 
                        activity_type_ndg1 = "op",
                        activity_type_ndg2 = "op",
                        measure = "attendances", 
                        title = "Outpatient attendances waterfall scenario comparison", 
                        x_label = "Attendances", 
                        y_label = "Change factor")

generate_waterfall_plot(pcfs_ndg1, pcfs_ndg2, 
                        activity_type_ndg1 = "op", 
                        activity_type_ndg2 = "op",
                        measure = "tele_attendances",
                        title = "Outpatient Tele-attendances waterfall scenario comparison", 
                        x_label = "Tele-attendances", 
                        y_label = "Change factor")

generate_waterfall_plot(pcfs_ndg1, pcfs_ndg2, 
                        activity_type_ndg1 = "aae", 
                        activity_type_ndg2 = "aae",
                        measure = "arrivals",
                        title = "Outpatient arrivals waterfall scenario comparison", 
                        x_label = "Arrivals", 
                        y_label = "Change factor")

# we combine the data together at this stage of the process
ndg_variants_sc_comparison <- bind_rows(
  ndg1 = as.data.frame(bind_rows(pcfs_ndg1)),
  ndg2 = as.data.frame(bind_rows(pcfs_ndg2)),
  .id = "scenario")

#### Activity avoidance
impact_bar_plot(ndg_variants_sc_comparison,"activity_avoidance", "ip", "admissions", "Imperial NDG Inpatient Admissions Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"activity_avoidance", "ip", "beddays", "Imperial NDG Inpatient Bed Days Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"activity_avoidance", "op", "attendances", "Imperial NDG outpatient Attendances Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"activity_avoidance", "op", "tele_attendances", "Imperial NDG outpatient Tele-attendances Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"activity_avoidance", "aae", "arrivals", "Imperial NDG A&E Scenarios")

### efficiencies
impact_bar_plot(ndg_variants_sc_comparison,"efficiencies", "ip", "admissions", "Imperial NDG Inpatient Admissions Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"efficiencies", "ip", "beddays", "Imperial NDG Inpatient Bed days Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"efficiencies", "op", "attendances", "Imperial NDG outpatient Attendances Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"efficiencies", "op", "tele_attendances", "Imperial NDG outpatient Tele-attendances Scenarios")
impact_bar_plot(ndg_variants_sc_comparison,"efficiencies", "aae", "arrivals", "Imperial NDG A&E Scenarios")


