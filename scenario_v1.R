library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)

# Functions
source(here('R','nhp_outputs','fct_get_data.R'))
source(here('R','nhp_outputs','fct_gt_bar.R'))
source(here('R','nhp_outputs','fct_gt_theme.R'))
source(here('R','nhp_outputs','fct_report.R'))
source(here('R','nhp_outputs','get_activity_type_pod_measure_options.R'))
source(here('R','nhp_outputs','mod_info_params_fct_tables.R'))
source(here('R','nhp_outputs','mod_model_results_distribution.R'))
source(here('R','nhp_outputs','mod_principal_change_factor_effects.R'))
source(here('R','nhp_outputs','mod_principal_summary_los.R'))
source(here('R','nhp_outputs','mod_principal_summary.R'))

# read Json ndg1
path_ndg1 <- "jsons/Imperialv1-ndg1-20241022_100916.json.gz"

result_ndg1 <- path_ndg1 |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data

df_inpatient <- result_ndg1 |>
  mod_principal_summary_data(sites = NULL)

df_outpatient <- result_ndg1 |>
  mod_principal_summary_data_outpatient(sites = NULL)

df_ae <- result_ndg1 |>
  mod_principal_summary_data_ae(sites = NULL)

df <- rbind(df_inpatient,df_outpatient,df_ae)


# read Json ndg2
path_ndg2 <- r"{C:\Users\fikriyudin.mufasir\OneDrive - Midlands and Lancashire CSU\Documents\NHP_ndg_variance\Imperialv1-ndg2-20241023_122644.json.gz}"  # change this to the location of the json file

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
only_principal_ndg1 <- df |> select(pod_name,activity_type,principal) |> rename(principal_ndg1 = principal)
only_principal_ndg2 <- df2 |> select(principal) |> rename(principal_ndg2 = principal)

data <- cbind(only_principal_ndg1,only_principal_ndg2)

data_ip_ad <- data |> select(pod_name,principal_ndg1,principal_ndg2)|>
  filter(pod_name %in% c("Daycase Admission","Elective Admission","Maternity Admission","Non-Elective Admission","Regular Day Attender Admission")) |>
  pivot_longer(!pod_name, names_to = "ndg", values_to = "count")
data_ip_at <- data |> select(pod_name,principal_ndg1,principal_ndg2) |>
  filter(pod_name %in% c("Daycase Bed Days","Elective Bed Days","Maternity Bed Days","Non-Elective Bed Days","Regular Day Attender Bed Days" )) |>
  pivot_longer(!pod_name, names_to = "ndg", values_to = "count")
data_outpatient <- data |> select(pod_name,principal_ndg1,principal_ndg2)  |> filter(pod_name %in% c("First Outpatient Attendance","Follow-up Outpatient Attendance","Outpatient Procedure" )) |>
  pivot_longer(!pod_name, names_to = "ndg", values_to = "count")
data_ae <- data |> select(pod_name,principal_ndg1,principal_ndg2)  |> filter(pod_name == "A&E Attendance" ) |>
  pivot_longer(!pod_name, names_to = "ndg", values_to = "count")

# data_ip_ad <- data |> filter(pod_name %in% c("Daycase Admission","Elective Admission","Maternity Admission","Non-Elective Admission","Regular Day Attender Admission"))
# data_ip_at <- data |> filter(pod_name %in% c("Daycase Bed Days","Elective Bed Days","Maternity Bed Days","Non-Elective Bed Days","Regular Day Attender Bed Days" ))
# data_outpatient <- data |> filter(pod_name %in% c("First Outpatient Attendance","Follow-up Outpatient Attendance","Outpatient Procedure" ))
# data_ae <- data |> filter(pod_name == "A&E Attendance" )

# Visualisation
strategyunit_colours <-c("#f9bf07","#686f73", "#5881c1", "#ec6555")

ggplot(data_ip_ad, aes(y = reorder(pod_name, count), x = count)) +
    geom_col(aes(fill = ndg),
             position = position_dodge(width = 0.6), width = .6 ) +
    scale_x_continuous(expand = c(0, 0, .05, 0),labels = scales::comma) +
    scale_fill_manual(values=strategyunit_colours, name="Scenario") +
    theme_classic() +
    ggtitle("Example of Scenario Analysis on Imperial College NDG Variance") +
    ggeasy::easy_center_title() +
    theme(text = element_text(family = "Segoe UI")) +
    ylab("Inpatients Category")+
    xlab("Total Number")  +
    theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.title = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.text = element_text(family = "Segoe UI", size = 12, color="black"))

# ggplot(data_ip_at, aes(y = reorder(pod_name, count), x = count)) +
#   geom_col(aes(fill = ndg),
#            position = position_dodge(width = 0.6), width = .6 ) +
#   scale_x_continuous(expand = c(0, 0, .05, 0),breaks = scales::pretty_breaks()) +
#   scale_fill_manual(values=strategyunit_colours, name="Scenario") +
#   theme_classic() +
#   ggtitle("Example of Scenario Analysis on Imperial NDG Variance") +
#   ggeasy::easy_center_title() +
#   theme(text = element_text(family = "Segoe UI")) +
#   ylab("Inpatients Category") +
#   xlab("Total Number") +
#   theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color="black")) +
#   theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color="black")) +
#   theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color="black")) +
#   theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color="black")) +
#   theme(legend.title = element_text(family = "Segoe UI", size = 12, color="black")) +
#   theme(legend.text = element_text(family = "Segoe UI", size = 12, color="black"))
#
