library(here)
library(jsonlite)
library(tidyverse)

# read Json ndg1
result_ndg1  <- "jsons/Imperialv1-ndg1-20241022_100916.json.gz" |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the

# read Json ndg2
result_ndg2 <- "jsons/Imperialv1-ndg2-20241023_122644.json.gz" |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data


df_inpatient <- result_ndg1 |>
  mod_principal_summary_data_inpatient(sites = NULL)

df_outpatient <- result_ndg1 |>
  mod_principal_summary_data_outpatient(sites = NULL)

df_ae <- result_ndg1 |>
  mod_principal_summary_data_ae(sites = NULL)

df <- bind_rows(df_inpatient,df_outpatient,df_ae)


df2_inpatient <- result_ndg2 |>
  mod_principal_summary_data_inpatient(sites = NULL)

df2_outpatient <- result_ndg2 |>
  mod_principal_summary_data_outpatient(sites = NULL)

df2_ae <- result_ndg2 |>
  mod_principal_summary_data_ae(sites = NULL)

df2 <- bind_rows(df2_inpatient,df2_outpatient,df2_ae)

# data processing
data <- bind_rows(scenario_1 = df1, scenario_2 = df2, .id = "scenario")


# get the measure from the pod name
data <- data |>
  mutate(measure=case_when(grepl("Admission", pod_name) ~ "Admissions",
                           grepl("Bed Day", pod_name) ~ "Bed days",
                           TRUE ~ "Attendance / procedure"))



## admissions dataset
data_1_adm <- result_1 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "admissions") 

data_2_adm <- result_2 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "admissions") 

## Bed days dataset
data_1_bed <- result_1 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "beddays") 

data_2_bed <- result_2 |> 
  mod_principal_summary_los_data (sites = NULL, measure = "beddays") 

# data processing
data_admissions <- bind_rows(scenario_1 = data_1_adm, scenario_2 = data_2_adm, .id = "scenario")
data_bed <- bind_rows(scenario_1 = data_1_bed, scenario_2 = data_2_bed, .id = "scenario")
data_combine <- bind_rows("Bed Days" = data_bed, admissions = data_admissions, .id = "measure")


# Prep a list of summary dataframes, one per activity type
pcfs_1 <- prepare_all_principal_change_factors(
  r = result_1,
  site_codes = list(ip = NULL, op = NULL, aae = NULL)
)

pcfs_2 <- prepare_all_principal_change_factors(
  r = result_2,
  site_codes = list(ip = NULL, op = NULL, aae = NULL)
)

ndg_variants_sc_comparison <- bind_rows(
  scenario_1 = as.data.frame(bind_rows(pcfs_1)),
  scenario_2 = as.data.frame(bind_rows(pcfs_2)),
  .id = "scenario")

# load the tretspef lookup
tretspef_lookup <- jsonlite::read_json("supporting_data/tx-lookup.json",
                                       simplifyVector = TRUE
) |>
  dplyr::mutate(
    dplyr::across("Description", \(x) stringr::str_remove(x, " Service$")),
    dplyr::across("Description", \(x) paste0(.data$Code, ": ", .data$Description)),
  ) |>
  dplyr::select(-"Group") |>
  dplyr::add_row(Code = "&", Description = "Not known")  # as per HES dictionary 

## Activity in Details

data_tretspef_elective_admission <- combine_activity_data(data1 = result_1,
                                                          data2 = result_2,
                                                          tretspefs = tretspef_lookup,
                                                          activity_type = "ip",
                                                          pod = "ip_elective_admission",
                                                          measure = "admissions",
                                                          agg_col ="tretspef")

data_tretspef_maternity_admission <- combine_activity_data(data1 = result_1,
                                                           data2 = result_2,
                                                           tretspefs = tretspef_lookup,
                                                           activity_type = "ip",
                                                           pod = "ip_maternity_admission",
                                                           measure = "admissions",
                                                           agg_col ="tretspef") 


data_tretspef_elective_daycase_admission <- combine_activity_data(data1 = result_1,
                                                                  data2 = result_2,
                                                                  tretspefs = tretspef_lookup,
                                                                  activity_type = "ip",
                                                                  pod = "ip_elective_daycase",
                                                                  measure = "admissions",
                                                                  agg_col ="tretspef") 

data_tretspef_regular_day_attender_admission <- combine_activity_data(data1 = result_1,
                                                                      data2 = result_2,
                                                                      tretspefs = tretspef_lookup,
                                                                      activity_type = "ip",
                                                                      pod = "ip_regular_day_attender",
                                                                      measure = "admissions",
                                                                      agg_col ="tretspef")

data_tretspef_non_elective_admission <- combine_activity_data(data1 = result_1,
                                                              data2 = result_2,
                                                              tretspefs = tretspef_lookup,
                                                              activity_type = "ip",
                                                              pod = "ip_non-elective_admission",
                                                              measure = "admissions",
                                                              agg_col ="tretspef") 


data_tretspef_elective_beddays <- combine_activity_data(data1 = result_1,
                                                        data2 = result_2,
                                                        tretspefs = tretspef_lookup,
                                                        activity_type = "ip",
                                                        pod = "ip_elective_admission",
                                                        measure = "beddays",
                                                        agg_col ="tretspef")

data_tretspef_maternity_beddays <- combine_activity_data(data1 = result_1,
                                                         data2 = result_2,
                                                         tretspefs = tretspef_lookup,
                                                         activity_type = "ip",
                                                         pod = "ip_maternity_admission",
                                                         measure = "beddays",
                                                         agg_col ="tretspef") 


data_tretspef_elective_daycase_beddays <- combine_activity_data(data1 = result_1,
                                                                data2 = result_2,
                                                                tretspefs = tretspef_lookup,
                                                                activity_type = "ip",
                                                                pod = "ip_elective_daycase",
                                                                measure = "beddays",
                                                                agg_col ="tretspef") 

data_tretspef_regular_day_attender_beddays <- combine_activity_data(data1 = result_1,
                                                                    data2 = result_2,
                                                                    tretspefs = tretspef_lookup,
                                                                    activity_type = "ip",
                                                                    pod = "ip_regular_day_attender",
                                                                    measure = "beddays",
                                                                    agg_col ="tretspef")

data_tretspef_non_elective_beddays <- combine_activity_data(data1 = result_1,
                                                            data2 = result_2,
                                                            tretspefs = tretspef_lookup,
                                                            activity_type = "ip",
                                                            pod = "ip_non-elective_admission",
                                                            measure = "beddays",
                                                            agg_col ="tretspef") 

data_tretspef_op_procedure_attendance <- combine_activity_data(data1 = result_1,
                                                               data2 = result_2,
                                                               tretspefs = tretspef_lookup,
                                                               activity_type = "op",
                                                               pod = "op_procedure",
                                                               measure = "attendances",
                                                               agg_col ="tretspef")

data_tretspef_op_followup_attendance <- combine_activity_data(data1 = result_1,
                                                              data2 = result_2,
                                                              tretspefs = tretspef_lookup,
                                                              activity_type = "op",
                                                              pod = "op_follow-up",
                                                              measure = "attendances",
                                                              agg_col ="tretspef") 


data_tretspef_op_first_attendance <- combine_activity_data(data1 = result_1,
                                                           data2 = result_2,
                                                           tretspefs = tretspef_lookup,
                                                           activity_type = "op",
                                                           pod = "op_first",
                                                           measure = "attendances",
                                                           agg_col ="tretspef") 

data_tretspef_op_procedure_tele_attendance <- combine_activity_data(data1 = result_1,
                                                                    data2 = result_2,
                                                                    tretspefs = tretspef_lookup,
                                                                    activity_type = "op",
                                                                    pod = "op_procedure",
                                                                    measure = "tele_attendances",
                                                                    agg_col ="tretspef")

data_tretspef_op_followup_tele_attendance <- combine_activity_data(data1 = result_1,
                                                                   data2 = result_2,
                                                                   tretspefs = tretspef_lookup,
                                                                   activity_type = "op",
                                                                   pod = "op_follow-up",
                                                                   measure = "tele_attendances",
                                                                   agg_col ="tretspef") 


data_tretspef_op_first_tele_attendance <- combine_activity_data(data1 = result_1,
                                                                data2 = result_2,
                                                                tretspefs = tretspef_lookup,
                                                                activity_type = "op",
                                                                pod = "op_first",
                                                                measure = "tele_attendances",
                                                                agg_col ="tretspef") 


# activity in detail - age group ------------------------------------------

data_age_elective_admission <- combine_activity_data(data1 = result_1,
                                                     data2 = result_2,
                                                     activity_type = "ip",
                                                     pod = "ip_elective_admission",
                                                     measure = "admissions",
                                                     agg_col ="age_group")

data_age_maternity_admission <- combine_activity_data(data1 = result_1,
                                                      data2 = result_2,
                                                      activity_type = "ip",
                                                      pod = "ip_maternity_admission",
                                                      measure = "admissions",
                                                      agg_col ="age_group") 


data_age_elective_daycase_admission <- combine_activity_data(data1 = result_1,
                                                             data2 = result_2,
                                                             activity_type = "ip",
                                                             pod = "ip_elective_daycase",
                                                             measure = "admissions",
                                                             agg_col ="age_group") 

data_age_regular_day_attender_admission <- combine_activity_data(data1 = result_1,
                                                                 data2 = result_2,
                                                                 activity_type = "ip",
                                                                 pod = "ip_regular_day_attender",
                                                                 measure = "admissions",
                                                                 agg_col ="age_group")

data_age_non_elective_admission <- combine_activity_data(data1 = result_1,
                                                         data2 = result_2,
                                                         activity_type = "ip",
                                                         pod = "ip_non-elective_admission",
                                                         measure = "admissions",
                                                         agg_col ="age_group")

data_age_elective_beddays <- combine_activity_data(data1 = result_1,
                                                   data2 = result_2,
                                                   activity_type = "ip",
                                                   pod = "ip_elective_admission",
                                                   measure = "beddays",
                                                   agg_col ="age_group")

data_age_maternity_beddays <- combine_activity_data(data1 = result_1,
                                                    data2 = result_2,
                                                    activity_type = "ip",
                                                    pod = "ip_maternity_admission",
                                                    measure = "beddays",
                                                    agg_col ="age_group") 


data_age_elective_daycase_beddays <- combine_activity_data(data1 = result_1,
                                                           data2 = result_2,
                                                           activity_type = "ip",
                                                           pod = "ip_elective_daycase",
                                                           measure = "beddays",
                                                           agg_col ="age_group") 

data_age_regular_day_attender_beddays <- combine_activity_data(data1 = result_1,
                                                               data2 = result_2,
                                                               activity_type = "ip",
                                                               pod = "ip_regular_day_attender",
                                                               measure = "beddays",
                                                               agg_col ="age_group")

data_age_non_elective_beddays <- combine_activity_data(data1 = result_1,
                                                       data2 = result_2,
                                                       activity_type = "ip",
                                                       pod = "ip_non-elective_admission",
                                                       measure = "beddays",
                                                       agg_col ="age_group") 

data_age_op_procedure_attendance <- combine_activity_data(data1 = result_1,
                                                          data2 = result_2,
                                                          activity_type = "op",
                                                          pod = "op_procedure",
                                                          measure = "attendances",
                                                          agg_col ="age_group")

data_age_op_followup_attendance <- combine_activity_data(data1 = result_1,
                                                         data2 = result_2,
                                                         activity_type = "op",
                                                         pod = "op_follow-up",
                                                         measure = "attendances",
                                                         agg_col ="age_group") 


data_age_op_first_attendance <- combine_activity_data(data1 = result_1,
                                                      data2 = result_2,
                                                      activity_type = "op",
                                                      pod = "op_first",
                                                      measure = "attendances",
                                                      agg_col ="age_group")


data_age_op_procedure_tele_attendance <- combine_activity_data(data1 = result_1,
                                                               data2 = result_2,
                                                               activity_type = "op",
                                                               pod = "op_procedure",
                                                               measure = "tele_attendances",
                                                               agg_col ="age_group")

data_age_op_followup_tele_attendance <- combine_activity_data(data1 = result_1,
                                                              data2 = result_2,
                                                              activity_type = "op",
                                                              pod = "op_follow-up",
                                                              measure = "tele_attendances",
                                                              agg_col ="age_group") 


data_age_op_first_tele_attendance <- combine_activity_data(data1 = result_1,
                                                           data2 = result_2,
                                                           activity_type = "op",
                                                           pod = "op_first",
                                                           measure = "tele_attendances",
                                                           agg_col ="age_group") 

data_age_aae_type1_ambulance <- combine_activity_data(data1 = result_1,
                                                      data2 = result_2,
                                                      activity_type = "aae",
                                                      pod = "aae_type-01",
                                                      measure = "ambulance",
                                                      agg_col ="age_group")

data_age_aae_type2_ambulance <- combine_activity_data(data1 = result_1,
                                                      data2 = result_2,
                                                      activity_type = "aae",
                                                      pod = "aae_type-02",
                                                      measure = "ambulance",
                                                      agg_col ="age_group")

data_age_aae_type1_walkin <- combine_activity_data(data1 = result_1,
                                                   data2 = result_2,
                                                   activity_type = "aae",
                                                   pod = "aae_type-01",
                                                   measure = "walk-in",
                                                   agg_col ="age_group")

data_age_aae_type2_walkin <- combine_activity_data(data1 = result_1,
                                                   data2 = result_2,
                                                   activity_type = "aae",
                                                   pod = "aae_type-02",
                                                   measure = "walk-in",
                                                   agg_col ="age_group")



# Cliniplan Nottingham ICB dataset
result_notts  <- "jsons/rx1-241204-rx1-low-sc04-01-20241206-165358_results.json" |>
  jsonlite::read_json() |>
  parse_results() 

#Convert Json into dataframe
df1_convert_to_table <- as.data.frame(bind_rows(result_notts$results$step_counts))|> 
  select(pod,change_factor,
         activity_type,
         strategy,measure,value)  |> 
  filter(pod != "ip_elective_daycase", 
         measure %in% c("admissions",
                        "beddays")) |> select(-pod) |> group_by(activity_type,change_factor,
                                                                strategy,measure) |> 
  summarize(value = sum(value))


### data pre-processing ####

### creating step change 

pivot_data <- df1_convert_to_table  |> 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

pivot_data_adj <- pivot_data |> mutate( adj_bed_days = +beddays-admissions)
denominator <- pivot_data_adj |> 
  filter(change_factor == "baseline" ) |> 
  pull(adj_bed_days) |> 
  as.numeric()  


pivot_data_adj2 <- pivot_data_adj |> 
  mutate( adj_bed_days_percent = +adj_bed_days/denominator,
          adj_bed_days_year = +adj_bed_days/365)

calc_baseline_adjustment <- pivot_data_adj2 |> 
  filter (change_factor == "baseline_adjustment") |> 
  mutate(adj_bed_test = +denominator/365) |>  pull (adj_bed_test)


ndg_variants_baseline_adjustment <- pivot_data_adj2 |> 
  mutate(change_factor = factor(change_factor, levels=c("baseline","baseline_adjustment","birth_adjustment","covid_adjustment","health_status_adjustment",
                                                        "demographic_adjustment","non-demographic_adjustment","activity_avoidance", "efficiencies","model_interaction_term","waiting_list_adjustment"))) |> 
  arrange(change_factor)  

ndg_variants_baseline_adjustment <- ndg_variants_baseline_adjustment |>
  ungroup() |> 
  mutate(
    calc_adj_bed_percent_v2 = c(
      NA_real_, # First row
      calc_baseline_adjustment * (1 + adj_bed_days_percent[2]), # Second row
      accumulate(adj_bed_days_percent[-c(1, 2)], ~ .x * (1 + .y), .init = calc_baseline_adjustment)[-1] # Rest of the rows
    )
  )


ndg_variants_baseline_adjustment <- ndg_variants_baseline_adjustment |> 
  mutate(adj_lag = calc_adj_bed_percent_v2 - lag(calc_adj_bed_percent_v2))

calc_change_birth <- ndg_variants_baseline_adjustment |> 
  filter (change_factor %in% c("birth_adjustment","covid_adjustment", "health_status_adjustment" ,"demographic_adjustment","non-demographic_adjustment")) |> 
  group_by(activity_type) |> 
  summarise(adj_sum = sum(adj_lag), .groups = "drop") |> mutate(change_factor = "birth_adjustment",
                                                                strategy = NA)

calc_activity_avoidance <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "activity_avoidance" ) |> group_by(activity_type) |> 
  summarise(adj_sum = sum(adj_lag), .groups = "drop") |> mutate(change_factor = "activity_avoidance",
                                                                strategy = "alcohol_partially_attributable_acute")

calc_efficiencies <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "efficiencies" ) |> group_by(activity_type) |> 
  summarise(adj_sum = sum(adj_lag), .groups = "drop")  |> mutate(change_factor = "efficiencies",
                                                                 strategy = "ambulatory_emergency_care_high")

calc_model_interaction_term  <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "model_interaction_term" ) |> group_by(activity_type) |> 
  summarise(adj_sum = sum(calc_adj_bed_percent_v2)/1661, .groups = "drop") |> mutate(change_factor = "model_interaction_term",
                                                                                     strategy = NA)

adj_sum <- bind_rows(calc_change_birth,
                     calc_activity_avoidance,
                     calc_efficiencies,
                     calc_model_interaction_term)

ndg_variants_baseline_adjustment <- left_join(x = ndg_variants_baseline_adjustment,
                                              y = adj_sum,
                                              by = c("activity_type","change_factor","strategy"))

calc_change_birth_sum <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "birth_adjustment") |> group_by(activity_type) |> 
  summarise(adj_sum_v2 = sum(adj_sum), .groups = "drop") |> mutate(change_factor = "birth_adjustment",
                                                                   strategy = NA)
