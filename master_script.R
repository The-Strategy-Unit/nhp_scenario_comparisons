scenario_1 <- "Imperialv1-ndg1-20241022_100916.json.gz"
scenario_2 <- "Imperialv1-ndg2-20241023_122644.json.gz"
# packages ----------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)


# functions ---------------------------------------------------------------

file_names <- list.files(path = 'R', pattern = "\\.R$")
lapply(paste0('R/',file_names), source)

file_names_nhs_output <- list.files(path = 'R/nhp_outputs', pattern = "\\.R$")
lapply(paste0('R/nhp_outputs/',file_names_nhs_output), source)

# load data ---------------------------------------------------------------

source("load-data.R")

# summary tables / plots --------------------------------------------------

# Inpatient admissions
create_bar_plot(data, "Inpatient", "Admissions", "Imperial NDG Inpatient Scenarios")

# Inpatient bed days
create_bar_plot(data, "Inpatient", "Bed days", "Imperial NDG Inpatient Scenarios")

# Outpatient attendances
create_bar_plot(data, "Outpatient", "Attendance / procedure", "Imperial NDG Outpatient Scenarios")

# A&E attendances
create_bar_plot(data, "A&E", "Attendance / procedure", "Imperial NDG A&E Scenarios")


# LoS breakdowns ----------------------------------------------------------

# we may produce for any combination of inpatient pod & measure

# admissions plots
create_bar_plot_los(data_combine, "Non-Elective Admission", "admissions", "Imperial NDG Non-Elective Admission Scenarios Length of Stay")

# activity in detail ------------------------------------------------------

# we may produce for any combination of pod, measure and aggregation (age/tretspef)
#admissions
activity_detail_bar(
  detailed_activity_data$ip_elective_admission_admissions_tretspef, 
  chosen_sex = "Male", 
  title_text = "Imperial NDG Male Inpatient  Elective Admissions", 
  ylab = "Treatment Specification",
  xlab = "Admissions")


# waterfalls --------------------------------------------------------------

generate_waterfall_plot(
  pcfs_1, 
  pcfs_2, 
  activity_type = "ip", 
  measure = "admissions", 
  title = "Inpatient admissions waterfall scenario comparison", 
  x_label = "Admissions", 
  y_label = "Change factor")

generate_waterfall_plot(
  pcfs_1, 
  pcfs_2, 
  activity_type = "ip",
  measure = "beddays", 
  title = "Inpatient bed days waterfall scenario comparison", 
  x_label = "Bed days", 
  y_label = "Change factor")

generate_waterfall_plot(
  pcfs_1, 
  pcfs_2, 
  activity_type = "op",
  measure = "attendances", 
  title = "Outpatient attendances waterfall scenario comparison", 
  x_label = "Attendances", 
  y_label = "Change factor")

generate_waterfall_plot(
  pcfs_1, 
  pcfs_2, 
  activity_type = "op", 
  measure = "tele_attendances",
  title = "Outpatient Tele-attendances waterfall scenario comparison", 
  x_label = "Tele-attendances", 
  y_label = "Change factor")

generate_waterfall_plot(
  pcfs_1, 
  pcfs_2, 
  activity_type = "aae", 
  measure = "arrivals",
  title = "Outpatient arrivals waterfall scenario comparison", 
  x_label = "Arrivals", 
  y_label = "Change factor")


# mitigator impacts bar charts --------------------------------------------

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


# CI bar plots ------------------------------------------------------------

# can do this for each pod and it will generate the value for each measure with 
# an error line for the 80% CI
create_bar_plot_distribution(data_distribution_summary,"ip_elective_admission","Imperial NDG Inpatient elective admissions Scenarios")


# ecdf and beeswarm -------------------------------------------------------

mod_model_results_distribution_beeswarm_plot_scenario(ip_admissions_dist_comparison, FALSE) +
  ggtitle("Beeswarm plot for two scenarios")

mod_model_results_distribution_ecdf_plot_scenario(ip_admissions_dist_comparison, show_origin = FALSE) +
  ggtitle("ECDF plot two scenarios")

# clini-plan --------------------------------------------------------------

occupancy_rate <- 0.85
# create new table ro calculate 85% occupancy bed rate
adjusted_bed_table <- ndg_variants_baseline_adjustment |> 
  select(activity_type,change_factor,strategy,admissions,adj_bed_days,adj_bed_days_percent) |> 
  filter(change_factor %in% c("activity_avoidance","efficiencies")) |> 
  mutate(adj_bed_days_percent = abs(round(adj_bed_days/365/occupancy_rate, digits = 4)),
         adj_bed_days = abs(round(adj_bed_days,digits = 0)),
         admissions = abs(round(admissions,digits = 0))) |> 
  rename(at_85 = adj_bed_days_percent) |> ungroup() |>  arrange(desc(at_85)) 

top_12 <- head(adjusted_bed_table, 12) |> ungroup()  |>  arrange(desc(at_85)) |> ungroup()
other <- adjusted_bed_table[-(1:12), ]  |>  arrange(desc(at_85)) |> group_by(activity_type) |> 
  summarise(admissions = sum(admissions),
            adj_bed_days = sum(adj_bed_days),
            at_85 = sum(at_85),
            .groups = "drop") |> ungroup() |> mutate(change_factor = "other_mitigator",
                                                     strategy = "other_mitigator") 
## Cliniplan 
calc_85_rate <- bind_rows(top_12,other,
                          .id = "Type") |>  
  mutate(strategy = factor(strategy, levels = unique(strategy)))


ggplot(calc_85_rate,
       aes(x=reorder(strategy,at_85), y=at_85, fill = Type)) + 
  geom_bar(stat = "identity") +
  coord_flip()  +
  ggtitle("Adjusted Beds at 85% Occupancy") +
  ylab("Beds") +
  xlab("Mitigator") +
  scale_fill_manual(values = c("#f9bf07","#686f73"), name="Type",
                    labels = c("Top 12", "Others")) +
  easy_center_title() + theme(text = element_text(family = "Segoe UI")) +
  theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(legend.title = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(legend.text = element_text(family = "Segoe UI", size = 12, color="black"))

