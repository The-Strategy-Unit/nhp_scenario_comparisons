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

#Convert Json into dataframe
df1_convert_to_table <- as.data.frame(do.call(rbind, pcfs_ndg1))
df2_convert_to_table <- as.data.frame(do.call(rbind, pcfs_ndg2))

### data pre-processing ####


ndg_variants_sc_comparison <- bind_rows(
  ndg_1 = df1_convert_to_table,
  ndg_2 = df2_convert_to_table,
  .id = "scenario"
) |>
  mutate(strategy = fct_reorder(strategy, desc(value)))


### Viz ###
#strategyunit_colours <-c("#686f73", "#f9bf07", "#5881c1", "#ec6555")

generate_data_waterfall_ndg1 <- function(data = data, measure = measure, include_baseline = TRUE) {
  
  activity_type <- deparse(substitute(data))
  
  result <- mod_principal_change_factor_effects_summarised_ndg1(data = data, 
                                                               measure = measure, 
                                                               include_baseline = include_baseline)
  
  result$activity_type <- activity_type
  result$measure <- measure
  
  return(result)
}

generate_data_waterfall_ndg2 <- function(data = data, measure = measure, include_baseline = TRUE) {
  
  activity_type <- deparse(substitute(data))
  result <- mod_principal_change_factor_effects_summarised_ndg2(data = data, 
                                                                measure = measure, 
                                                                include_baseline = include_baseline)
  
  result$activity_type <- activity_type
  result$measure <- measure
  
  return(result)
}

ndg1_ip_ad <- generate_data_waterfall_ndg1(pcfs_ndg1$ip, "admissions", include_baseline =TRUE)
ndg1_ip_at <- generate_data_waterfall_ndg1(pcfs_ndg1$ip, "beddays", include_baseline =TRUE)
ndg1_op_at <- generate_data_waterfall_ndg1(pcfs_ndg1$op, "attendances", include_baseline =TRUE)
ndg1_op_tele <- generate_data_waterfall_ndg1(pcfs_ndg1$op, "tele_attendances", include_baseline =TRUE)
ndg1_ae_ar <- generate_data_waterfall_ndg1(pcfs_ndg1$aae,  "arrivals", include_baseline =TRUE)

ndg2_ip_ad <- generate_data_waterfall_ndg2(pcfs_ndg2$ip, "admissions", include_baseline =TRUE)
ndg2_ip_at <- generate_data_waterfall_ndg2(pcfs_ndg2$ip, "beddays", include_baseline =TRUE)
ndg2_op_at <- generate_data_waterfall_ndg2(pcfs_ndg2$op, "attendances", include_baseline =TRUE)
ndg2_op_tele <- generate_data_waterfall_ndg2(pcfs_ndg2$op, "tele_attendances", include_baseline =TRUE)
ndg2_ae_ar <- generate_data_waterfall_ndg2(pcfs_ndg2$aae,  "arrivals", include_baseline =TRUE)

data_ndg1_waterfall <- bind_rows(ndg1_ip_ad,ndg1_ip_at,ndg1_op_at,ndg1_op_tele,ndg1_ae_ar)
data_ndg2_waterfall <- bind_rows(ndg2_ip_ad,ndg2_ip_at,ndg2_op_at,ndg2_op_tele,ndg2_ae_ar)

combine_data_waterfall <- bind_rows(
  ndg_1 = data_ndg1_waterfall ,
  ndg_2 = data_ndg2_waterfall ,
  .id = "scenario"
) |>
  mutate(activity_type=case_when( activity_type == "pcfs_ndg1$ip" ~ "inpatient",
                                  activity_type == "pcfs_ndg1$op"  ~ "outpatient",
                                  activity_type == "pcfs_ndg1$aae"  ~ "aae",
                                  activity_type == "pcfs_ndg2$ip" ~ "inpatient",
                                  activity_type == "pcfs_ndg2$op" ~ "outpatient",
                                  activity_type == "pcfs_ndg2$aae" ~ "aae",
                                  TRUE ~ "rename"))


water_plot_compare <- function(data,chosen_activity_type,chosen_measure, title_text = "Example") {
  data |> 
    filter(chosen_activity_type == activity_type, chosen_measure == measure) |> 
    mod_principal_change_factor_effects_cf_plot() +
    ggtitle(title_text)
}

water_plot_compare(combine_data_waterfall,"inpatient","admissions","Imperial NDG Inpatient admissions Scenarios")

water_plot_compare(combine_data_waterfall,"inpatient","beddays","Imperial NDG Inpatient Bed days Scenarios")

water_plot_compare(combine_data_waterfall,"outpatient","attendances","Imperial NDG Outpatient Attendances Scenarios")

water_plot_compare(combine_data_waterfall,"outpatient","tele_attendances","Imperial NDG Outpatint Tele-attendances Scenarios")

water_plot_compare(combine_data_waterfall, "aae","arrivals","Imperial NDG A&E Scenarios")


#####

impact_bar_plot <- function(data, chosen_change_factor,chosen_activity_type, chosen_measure, title_text = "Example") {
  ggplot(filter(data, change_factor==chosen_change_factor,activity_type==chosen_activity_type, chosen_measure==measure,value != 0.00),
         aes(x=value, y=reorder(mitigator_name,desc(value)), fill = scenario)) +
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


