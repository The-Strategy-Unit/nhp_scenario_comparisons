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

# read Json ndg1
result_ndg1  <- "jsons/Imperialv1-ndg1-20241022_100916.json.gz" |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the

# read Json ndg2
result_ndg2 <- "jsons/Imperialv1-ndg2-20241023_122644.json.gz" |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data

p <- result_ndg1[["results"]][["sex+tretspef"]][["pod"]] |> unique()
# load the tretspef lookup
tretspef_lookup <- jsonlite::read_json("data/tx-lookup.json",
                                       simplifyVector = TRUE
) |>
  dplyr::mutate(
    dplyr::across("Description", \(x) stringr::str_remove(x, " Service$")),
    dplyr::across("Description", \(x) paste0(.data$Code, ": ", .data$Description)),
  ) |>
  dplyr::select(-"Group") |>
  dplyr::add_row(Code = "&", Description = "Not known")  # as per HES dictionary 

  
data_tretspef_elective_admission <- combine_activity_data(data1 = result_ndg1,
                              data2 = result_ndg2,
                              tretspefs = tretspef_lookup,
                              activity_type = "ip",
                              pod = "ip_elective_admission",
                              measure = "admissions",
                              agg_col ="tretspef")

data_tretspef_maternity_admission <- combine_activity_data(data1 = result_ndg1,
                                                data2 = result_ndg2,
                                                tretspefs = tretspef_lookup,
                                                activity_type = "ip",
                                                pod = "ip_maternity_admission",
                                                measure = "admissions",
                                                agg_col ="tretspef") 


data_tretspef_elective_daycase_admission <- combine_activity_data(data1 = result_ndg1,
                                                           data2 = result_ndg2,
                                                           tretspefs = tretspef_lookup,
                                                           activity_type = "ip",
                                                           pod = "ip_elective_daycase",
                                                           measure = "admissions",
                                                           agg_col ="tretspef") 

data_tretspef_regular_day_attender_admission <- combine_activity_data(data1 = result_ndg1,
                                                        data2 = result_ndg2,
                                                        tretspefs = tretspef_lookup,
                                                        activity_type = "ip",
                                                        pod = "ip_regular_day_attender",
                                                        measure = "admissions",
                                                        agg_col ="tretspef")

data_tretspef_non_elective_admission <- combine_activity_data(data1 = result_ndg1,
                                                            data2 = result_ndg2,
                                                            tretspefs = tretspef_lookup,
                                                            activity_type = "ip",
                                                            pod = "ip_non-elective_admission",
                                                            measure = "admissions",
                                                            agg_col ="tretspef") 

# data_tretspef_regular_night_attender <- combine_activity_data(data1 = result_ndg1,
#                                                               data2 = result_ndg2,
#                                                               tretspefs = tretspef_lookup,
#                                                               activity_type = "ip",
#                                                               pod = "ip_regular_night_attender",
#                                                               measure = "admissions",
#                                                               agg_col ="tretspef") 

data_tretspef_elective_beddays <- combine_activity_data(data1 = result_ndg1,
                                                          data2 = result_ndg2,
                                                          tretspefs = tretspef_lookup,
                                                          activity_type = "ip",
                                                          pod = "ip_elective_admission",
                                                          measure = "beddays",
                                                          agg_col ="tretspef")

data_tretspef_maternity_beddays <- combine_activity_data(data1 = result_ndg1,
                                                           data2 = result_ndg2,
                                                           tretspefs = tretspef_lookup,
                                                           activity_type = "ip",
                                                           pod = "ip_maternity_admission",
                                                           measure = "beddays",
                                                           agg_col ="tretspef") 


data_tretspef_elective_daycase_beddays <- combine_activity_data(data1 = result_ndg1,
                                                        data2 = result_ndg2,
                                                        tretspefs = tretspef_lookup,
                                                        activity_type = "ip",
                                                        pod = "ip_elective_daycase",
                                                        measure = "beddays",
                                                        agg_col ="tretspef") 

data_tretspef_regular_day_attender_beddays <- combine_activity_data(data1 = result_ndg1,
                                                            data2 = result_ndg2,
                                                            tretspefs = tretspef_lookup,
                                                            activity_type = "ip",
                                                            pod = "ip_regular_day_attender",
                                                            measure = "beddays",
                                                            agg_col ="tretspef")

data_tretspef_non_elective_beddays <- combine_activity_data(data1 = result_ndg1,
                                                              data2 = result_ndg2,
                                                              tretspefs = tretspef_lookup,
                                                              activity_type = "ip",
                                                              pod = "ip_non-elective_admission",
                                                              measure = "beddays",
                                                              agg_col ="tretspef") 

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


# generating outpatient appointments by age group[]

### op

data_tretspef_op_procedure_attendance <- combine_activity_data(data1 = result_ndg1,
                                                          data2 = result_ndg2,
                                                          tretspefs = tretspef_lookup,
                                                          activity_type = "op",
                                                          pod = "op_procedure",
                                                          measure = "attendances",
                                                          agg_col ="tretspef")

data_tretspef_op_followup_attendance <- combine_activity_data(data1 = result_ndg1,
                                                           data2 = result_ndg2,
                                                           tretspefs = tretspef_lookup,
                                                           activity_type = "op",
                                                           pod = "op_follow-up",
                                                           measure = "attendances",
                                                           agg_col ="tretspef") 


data_tretspef_op_first_attendance <- combine_activity_data(data1 = result_ndg1,
                                                                  data2 = result_ndg2,
                                                                  tretspefs = tretspef_lookup,
                                                                  activity_type = "op",
                                                                  pod = "op_first",
                                                                  measure = "attendances",
                                                                  agg_col ="tretspef") 

data_tretspef_op_procedure_tele_attendance <- combine_activity_data(data1 = result_ndg1,
                                                               data2 = result_ndg2,
                                                               tretspefs = tretspef_lookup,
                                                               activity_type = "op",
                                                               pod = "op_procedure",
                                                               measure = "tele_attendances",
                                                               agg_col ="tretspef")

data_tretspef_op_followup_tele_attendance <- combine_activity_data(data1 = result_ndg1,
                                                              data2 = result_ndg2,
                                                              tretspefs = tretspef_lookup,
                                                              activity_type = "op",
                                                              pod = "op_follow-up",
                                                              measure = "tele_attendances",
                                                              agg_col ="tretspef") 


data_tretspef_op_first_tele_attendance <- combine_activity_data(data1 = result_ndg1,
                                                           data2 = result_ndg2,
                                                           tretspefs = tretspef_lookup,
                                                           activity_type = "op",
                                                           pod = "op_first",
                                                           measure = "tele_attendances",
                                                           agg_col ="tretspef") 
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
generate_activity_in_detail_table(
  data = result_ndg1,
  sites = NULL,
  activity_type = "op",
  pod = "op_first",
  measure = "attendances",
  agg_col ="age_group")

data_age_elective_admission <- combine_activity_data(data1 = result_ndg1,
                                                          data2 = result_ndg2,
                                                          activity_type = "ip",
                                                          pod = "ip_elective_admission",
                                                          measure = "admissions",
                                                          agg_col ="age_group")

data_age_maternity_admission <- combine_activity_data(data1 = result_ndg1,
                                                           data2 = result_ndg2,
                                                           activity_type = "ip",
                                                           pod = "ip_maternity_admission",
                                                           measure = "admissions",
                                                           agg_col ="age_group") 


data_age_elective_daycase_admission <- combine_activity_data(data1 = result_ndg1,
                                                                  data2 = result_ndg2,
                                                                  activity_type = "ip",
                                                                  pod = "ip_elective_daycase",
                                                                  measure = "admissions",
                                                                  agg_col ="age_group") 

data_age_regular_day_attender_admission <- combine_activity_data(data1 = result_ndg1,
                                                                      data2 = result_ndg2,
                                                                      activity_type = "ip",
                                                                      pod = "ip_regular_day_attender",
                                                                      measure = "admissions",
                                                                      agg_col ="age_group")

data_age_non_elective_admission <- combine_activity_data(data1 = result_ndg1,
                                                              data2 = result_ndg2,
                                                              activity_type = "ip",
                                                              pod = "ip_non-elective_admission",
                                                              measure = "admissions",
                                                              agg_col ="age_group")

data_age_elective_beddays <- combine_activity_data(data1 = result_ndg1,
                                                  data2 = result_ndg2,
                                                  activity_type = "ip",
                                                  pod = "ip_elective_admission",
                                                  measure = "beddays",
                                                  agg_col ="age_group")

data_age_maternity_beddays <- combine_activity_data(data1 = result_ndg1,
                                                   data2 = result_ndg2,
                                                   activity_type = "ip",
                                                   pod = "ip_maternity_admission",
                                                   measure = "beddays",
                                                   agg_col ="age_group") 


data_age_elective_daycase_beddays <- combine_activity_data(data1 = result_ndg1,
                                                          data2 = result_ndg2,
                                                          activity_type = "ip",
                                                          pod = "ip_elective_daycase",
                                                          measure = "beddays",
                                                          agg_col ="age_group") 

data_age_regular_day_attender_beddays <- combine_activity_data(data1 = result_ndg1,
                                                              data2 = result_ndg2,
                                                              activity_type = "ip",
                                                              pod = "ip_regular_day_attender",
                                                              measure = "beddays",
                                                              agg_col ="age_group")

data_age_non_elective_beddays <- combine_activity_data(data1 = result_ndg1,
                                                      data2 = result_ndg2,
                                                      activity_type = "ip",
                                                      pod = "ip_non-elective_admission",
                                                      measure = "beddays",
                                                      agg_col ="age_group") 


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
data_age_op_procedure_attendance <- combine_activity_data(data1 = result_ndg1,
                                                               data2 = result_ndg2,
                                                               activity_type = "op",
                                                               pod = "op_procedure",
                                                               measure = "attendances",
                                                               agg_col ="age_group")

data_age_op_followup_attendance <- combine_activity_data(data1 = result_ndg1,
                                                         data2 = result_ndg2,
                                                         activity_type = "op",
                                                         pod = "op_follow-up",
                                                         measure = "attendances",
                                                         agg_col ="age_group") 


data_age_op_first_attendance <- combine_activity_data(data1 = result_ndg1,
                                                      data2 = result_ndg2,
                                                      activity_type = "op",
                                                      pod = "op_first",
                                                      measure = "attendances",
                                                      agg_col ="age_group")


data_age_op_procedure_tele_attendance <- combine_activity_data(data1 = result_ndg1,
                                                               data2 = result_ndg2,
                                                               activity_type = "op",
                                                               pod = "op_procedure",
                                                               measure = "tele_attendances",
                                                               agg_col ="age_group")

data_age_op_followup_tele_attendance <- combine_activity_data(data1 = result_ndg1,
                                                              data2 = result_ndg2,
                                                              activity_type = "op",
                                                              pod = "op_follow-up",
                                                              measure = "tele_attendances",
                                                              agg_col ="age_group") 


data_age_op_first_tele_attendance <- combine_activity_data(data1 = result_ndg1,
                                                           data2 = result_ndg2,
                                                           activity_type = "op",
                                                           pod = "op_first",
                                                           measure = "tele_attendances",
                                                           agg_col ="age_group") 
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

data_age_aae_type1_ambulance <- combine_activity_data(data1 = result_ndg1,
                                                          data2 = result_ndg2,
                                                          activity_type = "aae",
                                                          pod = "aae_type-01",
                                                          measure = "ambulance",
                                                          agg_col ="age_group")

data_age_aae_type2_ambulance <- combine_activity_data(data1 = result_ndg1,
                                                      data2 = result_ndg2,
                                                      activity_type = "aae",
                                                      pod = "aae_type-02",
                                                      measure = "ambulance",
                                                      agg_col ="age_group")

data_age_aae_type1_walkin <- combine_activity_data(data1 = result_ndg1,
                                                      data2 = result_ndg2,
                                                      activity_type = "aae",
                                                      pod = "aae_type-01",
                                                      measure = "walk-in",
                                                      agg_col ="age_group")

data_age_aae_type2_walkin <- combine_activity_data(data1 = result_ndg1,
                                                    data2 = result_ndg2,
                                                      activity_type = "aae",
                                                      pod = "aae_type-02",
                                                      measure = "walk-in",
                                                      agg_col ="age_group")

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
