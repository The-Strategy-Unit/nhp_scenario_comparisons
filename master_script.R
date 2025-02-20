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
source('load-data.R')

# Inpatient admissions
create_bar_plot(data, "Inpatient", "Admissions", "Imperial NDG Inpatient Scenarios")

# Inpatient bed days
create_bar_plot(data, "Inpatient", "Bed days", "Imperial NDG Inpatient Scenarios")

# Outpatient attendances
create_bar_plot(data, "Outpatient", "Attendance / procedure", "Imperial NDG Outpatient Scenarios")

# A&E attendances
create_bar_plot(data, "A&E", "Attendance / procedure", "Imperial NDG A&E Scenarios")

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


#admissions
activity_detail_bar(data_tretspef_elective_admission , "Male", "Imperial NDG Male Inpatient  Elective Admissions", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_elective_admission , "Female", "Imperial NDG Female Inpatient  Elective Admissions", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_maternity_admission , "Male", "Imperial NDG Male Inpatient  Maternity Admissions", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_maternity_admission , "Female", "Imperial NDG Female Inpatient  Maternity Admissions", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_non_elective_admission , "Male", "Imperial NDG Male Inpatient  Non-Elective Admissions", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_non_elective_admission , "Female", "Imperial NDG Female Inpatient  Non-Elective Admissions", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_regular_day_attender_admission, "Male", "Imperial NDG Male Inpatient  Regular Day Attender", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_regular_day_attender_admission, "Female", "Imperial NDG  Female Inpatient  Regular Day Attender", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_elective_daycase_admission , "Male", "Imperial NDG Male Inpatient  Elective Daycase", "Treatment Spesification","Admissions")
activity_detail_bar(data_tretspef_elective_daycase_admission, "Female", "Imperial NDG Female Inpatient  Elective Daycase", "Treatment Spesification","Admissions")

#Bed Days
activity_detail_bar(data_tretspef_elective_beddays , "Male", "Imperial NDG Male Inpatient  Elective Admissions", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_elective_beddays , "Female", "Imperial NDG Female Inpatient  Elective Admissions", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_maternity_beddays , "Male", "Imperial NDG Male Inpatient  Maternity Admissions", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_maternity_beddays , "Female", "Imperial NDG Female Inpatient Maternity Admissions", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_non_elective_beddays , "Male", "Imperial NDG Male Inpatient  Non-Elective Admissions", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_non_elective_beddays , "Female", "Imperial NDG Female Inpatient  Non-Elective Admissions", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_regular_day_attender_beddays, "Male", "Imperial NDG Male Inpatient  Regular Day Attender", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_regular_day_attender_beddays, "Female", "Imperial NDG Female Inpatient  Regular Day Attender", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_elective_daycase_beddays , "Male", "Imperial NDG Male Inpatient Elective Daycase", "Treatment Spesification","Bed Days")
activity_detail_bar(data_tretspef_elective_daycase_beddays, "Female", "Imperial NDG Female Inpatient  Elective Daycase", "Treatment Spesification","Bed Days")


### op

## attendances
activity_detail_bar(data_tretspef_op_first_attendance , "Male", "Imperial NDG Male Outpatient First Attendances", "Treatment Spesification","Attendances")
activity_detail_bar(data_tretspef_op_first_attendance , "Female", "Imperial NDG Female Outpatient First Attendances", "Treatment Spesification","Attendances")
activity_detail_bar(data_tretspef_op_followup_attendance , "Male", "Imperial NDG Male Outpatient Follow-up Attendances", "Treatment Spesification","Attendances")
activity_detail_bar(data_tretspef_op_followup_attendance , "Female", "Imperial NDG Female Outpatient Follow-up Attendances", "Treatment Spesification","Attendances")
activity_detail_bar(data_tretspef_op_procedure_attendance , "Male", "Imperial NDG Male Outpatient Procedures Attendances", "Treatment Spesification","Attendances")
activity_detail_bar(data_tretspef_op_procedure_attendance , "Female", "Imperial NDG Female Outpatient Procedures Attendances", "Treatment Spesification","Attendances")

## tele-attendances
activity_detail_bar(data_tretspef_op_first_tele_attendance , "Male", "Imperial NDG Male Outpatient First Tele-attendances", "Treatment Spesification","Tele-attendances")
activity_detail_bar(data_tretspef_op_first_tele_attendance , "Female", "Imperial NDG Female Outpatient First Tele-attendances", "Treatment Spesification","Tele-attendances")
activity_detail_bar(data_tretspef_op_followup_tele_attendance , "Male", "Imperial NDG Male Outpatient Follow-up Tele-attendances", "Treatment Spesification","Tele-attendances")
activity_detail_bar(data_tretspef_op_followup_tele_attendance , "Female", "Imperial NDG Female Outpatient Follow-up Tele-attendances", "Treatment Spesification","Tele-attendances")
activity_detail_bar(data_tretspef_op_procedure_tele_attendance , "Male", "Imperial NDG Male Outpatient Procedures Tele-attendances", "Treatment Spesification","Tele-attendances")
activity_detail_bar(data_tretspef_op_procedure_tele_attendance , "Female", "Imperial NDG Female Outpatient Procedures Tele-attendances", "Treatment Spesification","Tele-attendances")


## age-group

#Admissions
activity_detail_bar(data_age_elective_admission , "Male", "Imperial NDG Male Inpatient  Elective Admissions", "Age Group","Admissions")
activity_detail_bar(data_age_elective_admission , "Female", "Imperial NDG Female Inpatient  Elective Admissions",  "Age Group","Admissions")
activity_detail_bar(data_age_maternity_admission , "Male", "Imperial NDG Male Inpatient  Maternity Admissions",  "Age Group","Admissions")
activity_detail_bar(data_age_maternity_admission , "Female", "Imperial NDG Female Inpatient  Maternity Admissions",  "Age Group","Admissions")
activity_detail_bar(data_age_non_elective_admission , "Male", "Imperial NDG Male Inpatient  Non-Elective Admissions",  "Age Group","Admissions")
activity_detail_bar(data_age_non_elective_admission , "Female", "Imperial NDG Female Inpatient  Non-Elective Admissions",  "Age Group","Admissions")
activity_detail_bar(data_age_regular_day_attender_admission, "Male", "Imperial NDG Male Inpatient  Regular Day Attender",  "Age Group","Admissions")
activity_detail_bar(data_age_regular_day_attender_admission, "Female", "Imperial NDG  Female Inpatient  Regular Day Attender",  "Age Group","Admissions")
activity_detail_bar(data_age_elective_daycase_admission , "Male", "Imperial NDG Male Inpatient  Elective Daycase",  "Age Group","Admissions")
activity_detail_bar(data_age_elective_daycase_admission, "Female", "Imperial NDG Female Inpatient  Elective Daycase",  "Age Group","Admissions")

#Bed Days
activity_detail_bar(data_age_elective_beddays , "Male", "Imperial NDG Male Inpatient  Elective Admissions", "Age Group","Bed Days")
activity_detail_bar(data_age_elective_beddays , "Female", "Imperial NDG Female Inpatient  Elective Admissions", "Age Group","Bed Days")
activity_detail_bar(data_age_maternity_beddays , "Male", "Imperial NDG Male Inpatient  Maternity Admissions", "Age Group","Bed Days")
activity_detail_bar(data_age_maternity_beddays , "Female", "Imperial NDG Female Inpatient Maternity Admissions", "Age Group","Bed Days")
activity_detail_bar(data_age_non_elective_beddays , "Male", "Imperial NDG Male Inpatient  Non-Elective Admissions", "Age Group","Bed Days")
activity_detail_bar(data_age_non_elective_beddays , "Female", "Imperial NDG Female Inpatient  Non-Elective Admissions", "Age Group","Bed Days")
activity_detail_bar(data_age_regular_day_attender_beddays, "Male", "Imperial NDG Male Inpatient  Regular Day Attender", "Age Group","Bed Days")
activity_detail_bar(data_age_regular_day_attender_beddays, "Female", "Imperial NDG Female Inpatient  Regular Day Attender", "Age Group","Bed Days")
activity_detail_bar(data_age_elective_daycase_beddays , "Male", "Imperial NDG Male Inpatient Elective Daycase", "Age Group","Bed Days")
activity_detail_bar(data_age_elective_daycase_beddays, "Female", "Imperial NDG Female Inpatient  Elective Daycase", "Age Group","Bed Days")

## op 

## attendances
activity_detail_bar(data_age_op_first_attendance , "Male", "Imperial NDG Male Outpatient First Attendances", "Age_group","Attendances")
activity_detail_bar(data_age_op_first_attendance , "Female", "Imperial NDG Female Outpatient First Attendances", "Age_group","Attendances")
activity_detail_bar(data_age_op_followup_attendance , "Male", "Imperial NDG Male Outpatient Follow-up Attendances", "Age_group","Attendances")
activity_detail_bar(data_age_op_followup_attendance , "Female", "Imperial NDG Female Outpatient Follow-up Attendances", "Age_group","Attendances")
activity_detail_bar(data_age_op_procedure_attendance , "Male", "Imperial NDG Male Outpatient Procedures Attendances", "Age_group","Attendances")
activity_detail_bar(data_age_op_procedure_attendance , "Female", "Imperial NDG Female Outpatient Procedures Attendances", "Age_group","Attendances")

## tele-attendances
activity_detail_bar(data_age_op_first_tele_attendance , "Male", "Imperial NDG Male Outpatient First Tele-attendances", "Age group","Tele-attendances")
activity_detail_bar(data_age_op_first_tele_attendance , "Female", "Imperial NDG Female Outpatient First Tele-attendances", "Age group","Tele-attendances")
activity_detail_bar(data_age_op_followup_tele_attendance , "Male", "Imperial NDG Male Outpatient Follow-up Tele-attendances", "Age group","Tele-attendances")
activity_detail_bar(data_age_op_followup_tele_attendance , "Female", "Imperial NDG Female Outpatient Follow-up Tele-attendances", "Age group","Tele-attendances")
activity_detail_bar(data_age_op_procedure_tele_attendance , "Male", "Imperial NDG Male Outpatient Procedures Tele-attendances", "Age group","Tele-attendances")
activity_detail_bar(data_age_op_procedure_tele_attendance , "Female", "Imperial NDG Female Outpatient Procedures Tele-attendances", "Age group","Tele-attendances")

### A&E

#### ambulance
activity_detail_bar(data_age_aae_type1_ambulance , "Male", "Imperial NDG Male A&E Ambulance", "Age_group","Admissions")
activity_detail_bar(data_age_aae_type1_ambulance, "Female", "Imperial NDG Female A&E Ambulance", "Age_group","Admissions")
activity_detail_bar(data_age_aae_type2_ambulance , "Male", "Imperial NDG Male A&E  Ambulance", "Age_group","Admissions")
activity_detail_bar(data_age_aae_type2_ambulance , "Female", "Imperial NDG Female A&E  Ambulance", "Age_group","Admissions")

# Walk-in
activity_detail_bar(data_age_aae_type1_walkin , "Male", "Imperial NDG Male A&E Walk-in", "Age_group","Admissions")
activity_detail_bar(data_age_aae_type1_walkin, "Female", "Imperial NDG Female A&E Walk-in", "Age_group","Admissions")
activity_detail_bar(data_age_aae_type2_walkin , "Male", "Imperial NDG Male A&E  Walk-in", "Age_group","Admissions")
activity_detail_bar(data_age_aae_type2_walkin , "Female", "Imperial NDG Female A&E Walk-in", "Age_group","Admissions")


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


