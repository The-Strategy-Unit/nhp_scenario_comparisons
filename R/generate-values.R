generate_values_list <- function(
    r_ndg1,
    r_ndg2,
    site_codes,
    char_out = TRUE  # convert all output values to strings?
) {

  # Get different cuts of the model output data
  trust <- get_stepcounts(r_ndg2) # variant2
  trust_v1 <- get_stepcounts(r_ndg1) # variant1

  trust_los <- get_losgroup(r_ndg2) # variant2
  trust_los_v1 <- get_losgroup(r_ndg1) # variant1

  trust_default <- get_baseline_and_projections(r_ndg2) # variant2
  trust_default_v1 <- get_baseline_and_projections(r_ndg1) # variant1

  # Need to get the number of years to the forecast... eg 2041-2019 = 22
  years_to_forecast <- as.numeric(
    r_ndg2[["params"]][["end_year"]] - r_ndg2[["params"]][["start_year"]]
  )

 # Get scheme lookup
 schemes <- readr::read_csv("data/scheme-lookup.csv", show_col_types = FALSE)

  # Get various metadata to insert into the report
 scheme_code <- r_ndg2[["params"]][["dataset"]]
 scenariondg1 <- r_ndg1[["params"]][["scenario"]]
 scenariondg2 <- r_ndg2[["params"]][["scenario"]]
 site_codes_ip <- if (is.null(site_codes$ip)) "all" else site_codes$ip
 site_codes_op <- if (is.null(site_codes$op)) "all" else site_codes$op
 site_codes_aae <- if (is.null(site_codes$aae)) "all" else site_codes$aae
 create_datetimendg1 <- r_ndg1[["params"]][["create_datetime"]]
 create_datetimendg2 <- r_ndg2[["params"]][["create_datetime"]]
 start_year <- r_ndg2[["params"]][["start_year"]]
 trust_name <- schemes |> dplyr::filter(scheme==scheme_code) |> dplyr::pull(trust_name)
 hosp_site <- schemes |> dplyr::filter(scheme==scheme_code) |> dplyr::pull(hosp_site)

  # Items 1 and 3 - admissions
  baseline <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_1 <- janitor::round_half_up((((baseline + demographic_adjustment) / baseline)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  item_3 <- janitor::round_half_up((((baseline + non_demographic_adjustment) / baseline)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # Items 2 and 4 - bed days
  baseline <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_2 <- janitor::round_half_up((((baseline + demographic_adjustment) / baseline)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  item_4 <- janitor::round_half_up((((baseline + non_demographic_adjustment) / baseline)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # items 5 and 6 come from variant 1 version of the model run - admissions and beddays
  baseline_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_5 <- janitor::round_half_up((((baseline_v1 + non_demographic_adjustment_v1) / baseline_v1)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  baseline_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_6 <- janitor::round_half_up((((baseline_v1 + non_demographic_adjustment_v1) / baseline_v1)^(1 / years_to_forecast) - 1) * 100, digits = 2)


  # Figure 8.1
  # Items 7 to 18
  # Items 7 & 9 - admissions
  baseline_admissions <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_admissions <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_admissions <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_7 <- janitor::round_half_up((((baseline_admissions + activity_avoidance_admissions) / baseline_admissions)^(1 / years_to_forecast) - 1) * 100, digits = 2)
  item_9 <- janitor::round_half_up((((baseline_admissions + efficiencies_admissions) / baseline_admissions)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # only display value when there is one. If it is 0 then the relevant mitigators weren't set and therefore it is N/A
item_9 <- ifelse(item_9 == 0, "N/A", item_9)

  # Items 8 & 10 - beddays
  baseline_beddays <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_beddays <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_beddays <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_8 <- janitor::round_half_up((((baseline_beddays + activity_avoidance_beddays) / baseline_beddays)^(1 / years_to_forecast) - 1) * 100, digits = 2)
  item_10 <- janitor::round_half_up((((baseline_beddays + efficiencies_beddays) / baseline_beddays)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  item_11 <- janitor::round_half_up((((baseline_admissions + activity_avoidance_admissions + efficiencies_admissions) / baseline_admissions)^(1 / years_to_forecast) - 1) * 100, digits = 2)
  item_12 <- janitor::round_half_up((((baseline_beddays + activity_avoidance_beddays + efficiencies_beddays) / baseline_beddays)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # outpatients
  baseline_op <- trust |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_op <- trust |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_op <- trust |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()


  # item13
  item_13 <- janitor::round_half_up((((baseline_op + activity_avoidance_op) / baseline_op)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # item14
  item_14 <- janitor::round_half_up((((baseline_op + efficiencies_op) / baseline_op)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # only display value when there is one. If it is 0 then the relevant mitigators weren't set and therefore it is N/A
item_14 <- ifelse(item_14 == 0, "N/A", item_14)



  # item15
  item_15 <- janitor::round_half_up((((baseline_op + activity_avoidance_op + efficiencies_op) / baseline_op)^(1 / years_to_forecast) - 1) * 100, digits = 2)


  # a&e

  baseline_ae <- trust |>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_ae <- trust |>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_ae <- trust |>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  # item16
  item_16 <- janitor::round_half_up((((baseline_ae + activity_avoidance_ae) / baseline_ae)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # item17
  item_17 <- janitor::round_half_up((((baseline_ae + efficiencies_ae) / baseline_ae)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # only display value when there is one. If it is 0 then the relevant mitigators weren't set and therefore it is N/A
  item_17 <- ifelse(item_17 == 0, "N/A", item_17)

  # item18
  item_18 <- janitor::round_half_up((((baseline_ae + activity_avoidance_ae + efficiencies_ae) / baseline_ae)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # items19
  # see below
  baseline_default <- trust_default |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::ungroup() |>
    dplyr::summarise(baseline = sum(baseline)) |>
    dplyr::pull()

  lwrci <- trust_default |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::ungroup() |>
    dplyr::summarise(lwr_ci = sum(lwr_ci)) |>
    dplyr::pull()

  uprci <- trust_default |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::ungroup() |>
    dplyr::summarise(upr_ci = sum(upr_ci)) |>
    dplyr::pull()

  # item19 see below = item 24

  quants <- get_ecdf_quantiles(data=r_ndg2, site_codes, activity_type="inpatients",
                               pods = c("ip_non-elective_admission",

                                         "ip_elective_admission",
                                         "ip_elective_daycase",
                                         "ip_maternity_admission",
                                         "ip_regular_day_attender",
                                         "ip_regular_night_attender"), measure="beddays")
  item_20 <- janitor::round_half_up((((quants[1,2] / baseline_default) - 1) * 100), digits = 2)
  item_21 <- janitor::round_half_up((((quants[2,2] / baseline_default) - 1) * 100), digits = 2)

    # variant 2 results para above fig 9.1

  # item22

  baseline_los_0 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(baseline = sum(baseline)) |>
    dplyr::pull()

  principal_los <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(principal = sum(principal)) |>
    dplyr::pull()

  lowerci_los <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(lwr_ci = sum(lwr_ci)) |>
    dplyr::pull()

  upperci_los <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(upr_ci = sum(upr_ci)) |>
    dplyr::pull()

  principal_los_0 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(principal = sum(principal)) |>
    dplyr::pull()

  lowerci_los_0 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(lwr_ci = sum(lwr_ci)) |>
    dplyr::pull()

  upperci_los_0 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(upr_ci = sum(upr_ci)) |>
    dplyr::pull()

  baseline_los_1 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(!startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(baseline = sum(baseline)) |>
    dplyr::pull()

  principal_los_1 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(!startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(principal = sum(principal)) |>
    dplyr::pull()

  lowerci_los_1 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(!startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(lwr_ci = sum(lwr_ci)) |>
    dplyr::pull()

  upperci_los_1 <- trust_los |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(!startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(upr_ci = sum(upr_ci)) |>
    dplyr::pull()

  item_22 <- janitor::round_half_up(((principal_los_0 - baseline_los_0) / baseline_default) * 100, digits = 1)
  # item_22 <- item_19

  item_23 <- janitor::round_half_up(((principal_los_1 - baseline_los_1) / baseline_default) * 100, digits = 1)

  item_24 <- janitor::round_half_up(((principal_los - baseline_default) / baseline_default) * 100, digits = 1)
  item_19 <- item_24

  item_65 <- janitor::round_half_up(((principal_los_1 - baseline_los_1) / baseline_los_1) * 100, digits = 1)

  item_57 <-  janitor::round_half_up(((lowerci_los_0 - baseline_los_0) / baseline_default) * 100, digits = 1)
  item_58 <-  janitor::round_half_up(((upperci_los_0 - baseline_los_0) / baseline_default) * 100, digits = 1)
  item_59 <-  janitor::round_half_up(((lowerci_los_1 - baseline_los_1) / baseline_default) * 100, digits = 1)
  item_60 <-  janitor::round_half_up(((upperci_los_1 - baseline_los_1) / baseline_default) * 100, digits = 1)
  # Note re item_61 and item_62: It might seem that this code would give the prediction intervals for the total, but in fact it
  # is correct to use those generated from the default table, rather than the los breakdown. This is described in the project
  # information code that could be used but gives different results  -
  # janitor::round_half_up(((upperci_los - baseline_default) / baseline_default) * 100, digits = 1)
  item_61 <-  item_20
  item_62 <-  item_21

  item_48 <- janitor::round_half_up(principal_los - lwrci, digits = 0)
  item_49 <- janitor::round_half_up((principal_los - uprci) * -1, digits = 0)

  # items25,26,27, 50, 51 from variant 1
  baseline_default_v1 <- trust_default_v1 |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::ungroup() |>
    dplyr::summarise(baseline = sum(baseline)) |>
    dplyr::pull()

  baseline_los_0_v1 <- trust_los_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(baseline = sum(baseline)) |>
    dplyr::pull()

  principal_los_v1 <- trust_los_v1 |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(principal = sum(principal)) |>
    dplyr::pull()

  principal_los_0_v1 <- trust_los_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(principal = sum(principal)) |>
    dplyr::pull()

  baseline_los_1_v1 <- trust_los_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(!startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(baseline = sum(baseline)) |>
    dplyr::pull()

  principal_los_1_v1 <- trust_los_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(!startsWith(as.character(los_group), "0")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(principal = sum(principal)) |>
    dplyr::pull()

  lwrci_v1 <- trust_default_v1 |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::ungroup() |>
    dplyr::summarise(lwr_ci = sum(lwr_ci)) |>
    dplyr::pull()

  uprci_v1 <- trust_default_v1 |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::ungroup() |>
    dplyr::summarise(upr_ci = sum(upr_ci)) |>
    dplyr::pull()

  baseline_los_v1 <- baseline_los_1_v1 + baseline_los_0_v1
  principal_los_v1 <- principal_los_1_v1 + principal_los_0_v1

  item_25 <- janitor::round_half_up(((principal_los_v1 - baseline_los_v1) / baseline_default_v1) * 100, digits = 1)

  item_26 <- janitor::round_half_up(((principal_los_0_v1 - baseline_los_0_v1) / baseline_los_v1) * 100, digits = 1)

  item_27 <- janitor::round_half_up(((principal_los_1_v1 - baseline_los_1_v1) / baseline_default_v1) * 100, digits = 1)

  item_50 <- janitor::round_half_up(principal_los_v1 - lwrci_v1, digits = 0)

  item_51 <- janitor::round_half_up((principal_los_v1 - uprci_v1) * -1, digits = 0)

  # table 11.1
  # items28-37
  # items 28,29
  baseline <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  birth_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "birth_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  health_status_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "health_status_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  waiting_list_adjustment <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "waiting_list_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  model_interaction_term <- trust |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "model_interaction_term") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()


  item_28 <- janitor::round_half_up((((baseline + demographic_adjustment + non_demographic_adjustment + birth_adjustment +
                                         health_status_adjustment + covid_adjustment + waiting_list_adjustment + model_interaction_term)
                                      / baseline)^(1 / years_to_forecast) - 1) * 100, digits = 2)
  item_29 <- item_1

  # items 30,31

  baseline <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  birth_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "birth_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  health_status_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "health_status_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  waiting_list_adjustment <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "waiting_list_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  model_interaction_term <- trust |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "model_interaction_term") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_30 <- janitor::round_half_up((((baseline + demographic_adjustment + non_demographic_adjustment + birth_adjustment +
                                         health_status_adjustment + covid_adjustment + waiting_list_adjustment + model_interaction_term)
                                      / baseline)^(1 / years_to_forecast) - 1) * 100, digits = 2)
  item_31 <- item_2

  item_52 <- dplyr::case_when(
    waiting_list_adjustment > 0 ~ as.character(janitor::round_half_up(waiting_list_adjustment, digits = 0)),
    .default = "[NO WAITING LIST ADJ]"
  )

  item_53 <- dplyr::case_when(
    waiting_list_adjustment > 0 ~ as.character(janitor::round_half_up(waiting_list_adjustment / 365.25 / 0.92, digits = 1)),
    .default = "[NO WAITING LIST ADJ]"
  )

  # var1
  # items 32,33
  baseline_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  birth_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "birth_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  health_status_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "health_status_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  waiting_list_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "waiting_list_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  model_interaction_term_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "model_interaction_term") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_32 <- janitor::round_half_up((((baseline_v1 + demographic_adjustment_v1 + non_demographic_adjustment_v1 + birth_adjustment_v1 +
                                         health_status_adjustment_v1 + covid_adjustment_v1 + waiting_list_adjustment_v1 + model_interaction_term_v1)
                                      / baseline_v1)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  baseline_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  birth_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "birth_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  health_status_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "health_status_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  waiting_list_adjustment_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "waiting_list_adjustment") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  model_interaction_term_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "model_interaction_term") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_33 <- janitor::round_half_up((((baseline_v1 + demographic_adjustment_v1 + non_demographic_adjustment_v1 + birth_adjustment_v1 +
                                         health_status_adjustment_v1 + covid_adjustment_v1 + waiting_list_adjustment_v1 + model_interaction_term_v1)
                                      / baseline_v1)^(1 / years_to_forecast) - 1) * 100, digits = 2)


  # var2
  # items 34,35
  item_34 <- item_7
  item_35 <- item_8

  # var1
  # items 36,37
  baseline_admissions_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_admissions_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_admissions_v1 <- trust_v1 |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_36 <- janitor::round_half_up((((baseline_admissions_v1 + activity_avoidance_admissions_v1) / baseline_admissions_v1)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  baseline_beddays_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_beddays_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_beddays_v1 <- trust_v1 |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_37 <- janitor::round_half_up((((baseline_beddays_v1 + activity_avoidance_beddays_v1) / baseline_beddays_v1)^(1 / years_to_forecast) - 1) * 100, digits = 2)

  # total los, avg los, cagr section
  # items 38,39,40,41,42, 43, 44, 45,46,47

  admissions <- trust |>
    dplyr::filter(measure == "admissions") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  beddays <- trust |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_38 <- baseline - baseline_admissions
  item_39 <- janitor::round_half_up((baseline - baseline_admissions) / baseline_admissions, digits = 2)

  item_40 <- janitor::round_half_up(((((beddays - admissions) / admissions) / item_39)^(1 / years_to_forecast) - 1) * 100, digits = 1)
  item_41 <- janitor::round_half_up((beddays - admissions), digits=2)
  item_42 <- janitor::round_half_up((beddays - admissions) / admissions, digits = 3)
  item_43 <- item_39
  item_44 <- janitor::round_half_up(((baseline - baseline_admissions) / baseline_admissions) * ((-0.50 / 100) + 1)^years_to_forecast, digits = 2)
  item_45 <- janitor::round_half_up((((baseline - baseline_admissions) / baseline_admissions) * ((-0.50 / 100) + 1)^years_to_forecast) * 0.92, digits = 2)
  item_46 <- item_45
  item_47 <- janitor::round_half_up(((((((baseline - baseline_admissions) / baseline_admissions) * ((-0.50 / 100) + 1)^years_to_forecast) * 0.92) / ((baseline - baseline_admissions) / baseline_admissions))^(1 / years_to_forecast) - 1) * 100, digits = 1)

  repat <- trust |>
    dplyr::filter(change_factor == "repat") |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_54 <- dplyr::case_when(
    repat > 0 ~ as.character(janitor::round_half_up(repat / 365.25 / 0.92, digits = 1)),
    .default = "[NO REPATRIATION DATA]"
  )
  repat_beds <- janitor::round_half_up(repat / 365.25 / 0.92, digits = 1)

  expat <- trust |>
    dplyr::filter(change_factor == "expat") |>
    dplyr::filter(measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  item_55 <- dplyr::case_when(
    expat < 0 ~ as.character(janitor::round_half_up((expat * -1) / 365.25 / 0.92, digits = 1)),
    .default = "[NO EXPATRIATION DATA]"
  )

  expat_beds <- janitor::round_half_up(expat / 365.25 / 0.92, digits = 1)
  item_56 <- dplyr::case_when(
    (repat_beds + expat_beds) > 0 ~ paste0("+", as.character(repat_beds + expat_beds)),
    (repat + expat) == 0 ~ "[NO REPAT/EXPAT DATA]",
    .default = as.character(repat_beds + expat_beds)
  )


  item_63 <- as.numeric(r_ndg2[["params"]][["end_year"]])
  item_64 <- stringr::str_sub((item_63+1),start=-2)

  item_66 <- scheme_code
  item_67 <- scenariondg1
  item_68 <- scenariondg2
  item_69 <- paste(site_codes_ip,collapse = ", ")
  item_70 <- paste(site_codes_op,collapse = ", ")
  item_71 <- paste(site_codes_aae,collapse = ", ")
  item_72 <- create_datetimendg2 |> lubridate::as_datetime()

  item_73 <- as.numeric(start_year)
  item_74 <- trust_name
  item_75 <- hosp_site
  item_76 <- stringr::str_sub((item_73+1),start=-2)
  item_77 <- create_datetimendg1 |> lubridate::as_datetime()



  # Put items into list
  values_list <- list(
    item_01 = item_1,
    item_02 = item_2,
    item_03 = item_3,
    item_04 = item_4,
    item_05 = item_5,
    item_06 = item_6,
    item_07 = item_7,
    item_08 = item_8,
    item_09 = item_9,
    item_10 = item_10,
    item_11 = item_11,
    item_12 = item_12,
    item_13 = item_13,
    item_14 = item_14,
    item_15 = item_15,
    item_16 = item_16,
    item_17 = item_17,
    item_18 = item_18,
    item_19 = item_19,
    item_20 = item_20,
    item_21 = item_21,
    item_22 = item_22,
    item_23 = item_23,
    item_24 = item_24,
    item_25 = item_25,
    item_26 = item_26,
    item_27 = item_27,
    item_28 = item_28,
    item_29 = item_29,
    item_30 = item_30,
    item_31 = item_31,
    item_32 = item_32,
    item_33 = item_33,
    item_34 = item_34,
    item_35 = item_35,
    item_36 = item_36,
    item_37 = item_37,
    item_38 = item_38,
    item_39 = item_39,
    item_40 = item_40,
    item_41 = item_41,
    item_42 = item_42,
    item_43 = item_43,
    item_44 = item_44,
    item_45 = item_45,
    item_46 = item_46,
    item_47 = item_47,
    item_48 = item_48,
    item_49 = item_49,
    item_50 = item_50,
    item_51 = item_51,
    item_52 = item_52,
    item_53 = item_53,
    item_54 = item_54,
    item_55 = item_55,
    item_56 = item_56,
    item_57 = item_57,
    item_58 = item_58,
    item_59 = item_59,
    item_60 = item_60,
    item_61 = item_61,
    item_62 = item_62,
    item_63 = item_63,
    item_64 = item_64,
    item_65 = item_65,
    item_66 = item_66,
    item_67 = item_67,
    item_68 = item_68,
    item_69 = item_69,
    item_70 = item_70,
    item_71 = item_71,
    item_72 = item_72,
    item_73 = item_73,
    item_74 = item_74,
    item_75 = item_75,
    item_76 = item_76,
    item_77 = item_77
  )

  if (char_out) values_list <- values_list |> purrr::map(as.character)

  values_list

}

filter_sites_conditionally <- function(trust, site_codes) {
  has_sites <- !is.null(site_codes)
  if (has_sites) dplyr::filter(trust, sitetret %in% site_codes) else trust
}

get_ecdf_quantiles <- function(data, site_codes, activity_type, pods, measure) {
  selected_measure <- list(activity_type, pods, measure)

  activity_type_short <-
    switch(activity_type, "inpatients" = "ip", "outpatients" = "op", "aae")
  site_codes <- site_codes[[activity_type_short]]

  aggregated_data <- data |>
    mod_model_results_distribution_get_data(selected_measure, site_codes)


  tibble::enframe(get_ecdf_quantiles_data(aggregated_data),name="quant",value="value")
}
