library(here)
library(jsonlite)
library(tidyverse)

# read Json 1
result_1  <- here("jsons", scenario_1) |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the

# read Json 2
result_2 <- here("jsons", scenario_2) |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data

# grab the scenario_names
scenario_1_name <- result_1$params$scenario
scenario_2_name <- result_2$params$scenario

df1 <- mod_principal_summary_data(result_1, sites = NULL)
df2 <- mod_principal_summary_data(result_2, sites = NULL)

# data processing
data <- bind_rows(scenario_1 = df1, scenario_2 = df2, .id = "scenario")


# get the measure from the pod name
data <- data |>
  mutate(measure=case_when(grepl("Admission", pod_name) ~ "Admissions",
                           grepl("Bed Day", pod_name) ~ "Bed days",
                           TRUE ~ "Attendance / procedure"))



# LoS summary -------------------------------------------------------------


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


# impacts -----------------------------------------------------------------




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



# activity in detail  -----------------------------------------------------

tretspef_lookup <- jsonlite::read_json("supporting_data/tx-lookup.json",
                                       simplifyVector = TRUE
) |>
  dplyr::mutate(
    dplyr::across("Description", \(x) stringr::str_remove(x, " Service$")),
    dplyr::across("Description", \(x) paste0(.data$Code, ": ", .data$Description)),
  ) |>
  dplyr::select(-"Group") |>
  dplyr::add_row(Code = "&", Description = "Not known")  # as per HES dictionary 

## We get a list of all the combinations of activity type, pod, measure and 
# aggregation we are interested in

# For inpatients, it is admissions and bed days
ip_parameter_matrix <- expand.grid(
  activity_type = "ip", 
  pod = c("ip_elective_admission", "ip_maternity_admission", "ip_elective_daycase",
          "ip_regular_day_attender", "ip_non-elective_admission"),
  measure = c("admissions", "beddays"),
  agg_col = c("tretspef", "age_group"),
  stringsAsFactors = FALSE)

# for outpatients, attendances and tele-attendances
op_parameter_matrix <- expand.grid(
  activity_type = "op",
  pod = c("op_procedure", "op_follow-up", "op_first"),
  measure = c("attendances", "tele_attendances"),
  agg_col = c("tretspef", "age_group"),
  stringsAsFactors = FALSE)

# for A&E, ambulance and walk-in and there is no tretspef
aae_parameter_matrix <- expand.grid(
  activity_type = "aae",
  pod = c("aae_type-01", "aae_type-02"),
  measure = c("ambulance", "walk-in"),
  agg_col = c("age_group"),
  stringsAsFactors = FALSE)

# combine them all together
parameter_matrix <- bind_rows(
  ip_parameter_matrix,
  op_parameter_matrix, 
  aae_parameter_matrix)



# We now insert into our function for iterating the `combine_activity_data` 
# function across all combinations
detailed_activity_data <- run_combinations_list(parameter_matrix, result_1, result_2)


# 80% CI -----------------------------------------------------------

# load dataset 
data_distribution_summary <- bind_rows(
  scenario_1 = result_1$results$default, 
  scenario_2 = result_2$results$default, 
  .id = "scenario")

#p <- mod_model_results_distribution_get_data(result_1,selected_measure = c("Ip","ip_elective_admission","admissions"),site_codes = NULL)
data_distribution_summary <- data_distribution_summary |> 
  select(scenario,pod,measure,principal,lwr_pi,upr_pi) |> 
  group_by(scenario,pod,measure) |> 
  summarise(principal = sum(principal),
            lwr_ci = sum(lwr_pi),
            upr_ci = sum(upr_pi)) |> 
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



# model run distributions --------------------------------------------------

scenario_1_ip_admission_dist <- get_model_run_distribution(
  result_1, 
  pod = c("ip_elective_daycase", 
          "ip_non-elective_admission",
          "ip_regular_day_attender", 
          "ip_elective_admission", 
          "ip_maternity_admission"
  ),
  measure = "admissions",
  sites = NULL
)

# ndg 2
scenario_2_ip_admission_dist <- get_model_run_distribution(
  result_2, 
  pod = c("ip_elective_daycase", 
          "ip_non-elective_admission",
          "ip_regular_day_attender", 
          "ip_elective_admission", 
          "ip_maternity_admission"
  ),
  measure = "admissions",
  sites = NULL
)

# join them together
ip_admissions_dist_comparison <- dplyr::bind_rows(
  scenario_1 = scenario_1_ip_admission_dist,
  scenario_2 = scenario_2_ip_admission_dist,
  .id = "scenario"
)



# Cliniplan stuff ---------------------------------------------------------

result_notts  <- "jsons/rx1-241204-rx1-low-sc04-01-20241206-165358_results.json" |>
  jsonlite::read_json() |>
  parse_results_nott() 

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


### data pre-processing

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

