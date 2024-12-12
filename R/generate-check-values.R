generate_check_values <- function(
    r_ndg1,
    r_ndg2,
    site_codes,
    char_out = TRUE,  # convert all output values to strings?,
    values_list
) {
  # Get step counts from the model output data
  trust <- get_stepcounts(r_ndg2) # variant2

  # Need to get the horizon year... eg 2041, 2034 etc
  horizon_year <- as.character(r_ndg2[["params"]][["end_year"]])

  # Demographic Growth 2&3
  admissions_growth <- values_list$item_01
  beddays_growth <- values_list$item_02

  #Non-demographic growth 4&5
  admissions_nd_growth <- values_list$item_03
  beddays_nd_growth <- values_list$item_04

  #impact of mitigation figure 8.1 Inpatient/outpatients and A&E? 6-9
  ip_all_ad <- values_list$item_11
  ip_all_bd <- values_list$item_12
  op_all_at <- values_list$item_15
  ae_all_at <- values_list$item_18

  #top 10 inpatient activity avoidance admissions mitigators (fig8.2) 10-19
data <- trust |>
  dplyr::filter(activity_type=="ip", measure=="admissions", change_factor=="activity_avoidance")|>
  filter_if_any_sites(site_codes) |>
  dplyr::select(-model_runs,-time_profiles) |>
  dplyr::group_by(activity_type,measure,change_factor,strategy)|>
  dplyr::summarise (value=sum(value))
top_ip_ad_aa <- utils::head(data[order(data$value, decreasing= FALSE),], n = 10)

  #top 10 inpatient activity avoidance beddays mitigators (fig8.3) 20-29
data <- trust |>
  dplyr::filter(activity_type=="ip", measure=="beddays", change_factor=="activity_avoidance")|>
  filter_if_any_sites(site_codes) |>
  dplyr::select(-model_runs,-time_profiles) |>
  dplyr::group_by(activity_type,measure,change_factor,strategy)|>
  dplyr::summarise (value=sum(value))
top_ip_bd_aa <- utils::head(data[order(data$value, decreasing= FALSE),], n = 10)

  #top 10 inpatient efficiencies beddays mitigators (fig8.4) 30-39
data <- trust |>
  dplyr::filter(activity_type=="ip", measure=="beddays", change_factor=="efficiencies")|>
  filter_if_any_sites(site_codes) |>
  dplyr::select(-model_runs,-time_profiles) |>
  dplyr::group_by(activity_type,measure,change_factor,strategy)|>
  dplyr::summarise (value=sum(value))
top_ip_bd_ef <- utils::head(data[order(data$value, decreasing= FALSE),], n = 10)

  #total beddays increase by horizon % ----section 9 40
  total_beddays_increase <- values_list$item_19

  #Split of above between 0 and overnight …….section 9 41-42
  # 0 LOS Beddays
  zero_los_beddays <- values_list$item_22

  # >0 LOS Beddays
  non_zero_los_beddays <- values_list$item_23

  #Gross activity growth for admissions and beddays as CAGR …..section 11 43-44
  gag_cagr_ad <- values_list$item_28
  gag_cagr_bd <- values_list$item_30

  #Impact of Activity avoidance mitigators - all IP admissions & beddays - as CAGR (Assumed
  #BAU admissions avoided as CAGR (and beddays resultant) …..section 11 45-46)
  bau_cagr_ad <- values_list$item_34
  bau_cagr_bd <- values_list$item_35

  #Overall impact on average LOS as CAGR (Assumed BAU  productivity …..as CAGR
  #for overall los impact ….section 11 40)
  bau_cagr_prod <- values_list$item_40

  #Impact of  efficiency mitigators - all IP admissions & beddays - as CAGR (Assumed BAU
  #admissions avoided as CAGR (and beddays resultant) …..section ??)
  impact_ef_ad_cagr <- values_list$item_09
  impact_ef_bd_cagr <- values_list$item_10

  #app version
  app_version <- as.character(r_ndg2[["params"]][["app_version"]])

  # Put items into list
  check_list <- list(
    item_01 = horizon_year,
    item_02 = admissions_growth,
    item_03 = beddays_growth,
    item_04 = admissions_nd_growth,
    item_05 = beddays_nd_growth,
    item_45 = bau_cagr_ad,
    item_46 = bau_cagr_bd,
    item_48 = impact_ef_ad_cagr,
    item_49 = impact_ef_bd_cagr,
    item_06 = ip_all_ad,
    item_07 = ip_all_bd,
    item_08 = op_all_at,
    item_09 = ae_all_at,
    item_10 = top_ip_ad_aa,
    item_20 = top_ip_bd_aa,
    item_30 = top_ip_bd_ef,
    item_40 = total_beddays_increase,
    item_41 = zero_los_beddays,
    item_42 = non_zero_los_beddays,
    item_43 = gag_cagr_ad,
    item_44 = gag_cagr_bd,
    item_47 = bau_cagr_prod,
    item_50 = app_version
  )

  # df <- data.frame(check_list) |>
  #   tidyr::pivot_longer()
  check_list

}

filter_if_any_sites <- function(df, site_codes) {
  has_all_sites <- is.null(site_codes$ip)
  if (has_all_sites) return(df)
  df |> dplyr::filter(sitetret %in% site_codes$ip)
}


get_comparison_data <- function(scheme_code,schemes,site_codes=NULL){
  meta <- get_final_run_metadata(scheme_code, result_sets)

  scheme_name <- dplyr::filter(schemes, scheme == scheme_code) |> dplyr::pull(hosp_site)
  cat("* Scheme name: ",scheme_name,"(",scheme_code,")","\n")

  if (is.null(site_codes)) site_codes <- get_sites(scheme_code)
  cat("* Sites:\n")
  cat("- IP: ", if(is.null(site_codes$ip))  "all" else site_codes$ip,  "\n")
  cat("- OP: ", if(is.null(site_codes$op))  "all" else site_codes$op,  "\n")
  cat("- A&E:", if(is.null(site_codes$aae)) "all" else site_codes$aae, "\n")

  cat("* Fetching results...\n")
  r_ndg1 <- meta$metadata_ndg1 |> dplyr::pull(file) |> get_nhp_results(file = _)
  r_ndg2 <- meta$metadata_ndg2 |> dplyr::pull(file) |> get_nhp_results(file = _)


  cat("* Generate values list...\n")
  values_list <- generate_values_list(r_ndg1, r_ndg2, site_codes)

  cat("* Generate check list...\n")
  check_list <- generate_check_values(r_ndg1, r_ndg2, site_codes, char_out = TRUE,values_list)
  return(check_list)
}

get_the_mitigators <- function(check_list){
  check_list <- data.frame(check_list[c(14,15,16)])
  return(check_list)
}

get_check_list_no_mitigators <- function(check_list){
  check_list <- data.frame(check_list[-c(14,15,16)])
  return(check_list)
}
