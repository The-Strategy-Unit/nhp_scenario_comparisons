rds_root <- "rds/v3.1/RXN"
scenario1_name <- "20240114NDG1V1"
scenario2_name <- "20241212NDG2V1"


list_dirs <- purrr::partial(dir, full.names = TRUE, recursive = TRUE)

# grp <- "nhp_provider_RXN"
# results_metadata_tbl <- get_results_metadata(get_user_allowed_datasets(grp))
# scenario1_dir <- results_metadata_tbl |>
#   dplyr::filter(
#     .data[["scenario"]] == scenario1_name
#   ) |>
#   dplyr::pull("aggregated_results_path")
# scenario2_dir <- results_metadata_tbl |>
#   dplyr::filter(
#     .data[["scenario"]] == scenario2_name
#   ) |>
#   dplyr::pull("aggregated_results_path")

# results1 <- read_azure_results(scenario1_dir)
# results2 <- read_azure_results(scenario2_dir)

results1_file <- list_dirs(file.path(rds_root, scenario1_name))
results2_file <- list_dirs(file.path(rds_root, scenario2_name))

# readr::write_rds(results1, results1_file)
# readr::write_rds(results2, results2_file)

results1 <- readr::read_rds(results1_file)
results2 <- readr::read_rds(results2_file)

full_apm_lookup <- get_full_apm_lookup()
cond_apm_lookup <- get_condensed_apm_lookup()
full_ap_lookup <- dplyr::select(full_apm_lookup, !"measure") |>
  dplyr::distinct()
cond_ap_lookup <- dplyr::select(cond_apm_lookup, !"measure") |>
  dplyr::distinct()

tpma_lookup <- reskit::get_tpma_label_lookup()


# Create core table with a row for each pair of measure and activity_type,
# for pmapping over
full_mat_lookup <- full_apm_lookup |>
  dplyr::distinct(dplyr::pick(c("measure", "activity_type")))
# cond_mat_lookup <- cond_apm_lookup |>
#   dplyr::distinct(dplyr::pick(c("measure", "activity_type")))
atl_lookup <- full_apm_lookup |>
  dplyr::distinct(dplyr::pick(c("activity_type", "activity_type_label"))) |>
  dplyr::mutate(dplyr::across("activity_type_label", \(x) sub("s$", "", x)))


# Prepare data for Summary chart

summary_data <- list(
  pt_compile_principal_pod_data(results1),
  pt_compile_principal_pod_data(results2)
) |>
  rlang::set_names(c(scenario1_name, scenario2_name)) |>
  purrr::list_rbind(names_to = "scenario") |>
  dplyr::mutate(
    dplyr::across("activity_type_label", \(x) {
      dplyr::if_else(grepl("^Inp", x), x, paste0(x, " Activity"))
    }),
    dplyr::across("pod_label", \(x) {
      forcats::fct_reorder(x, .data[["baseline"]])
    })
  )


# Test creation of Summary chart

create_summary_bar_chart(summary_data, "Inpatient Admissions")

# Prepare data for LoS chart

pt_compile_principal_los_data <- function(...) {
  purrr::partial(
    reskit::compile_principal_los_data,
    pod_lookup = cond_ap_lookup
  )(...)
}

admissions_data <- list(results1, results2) |>
  purrr::map(\(x) pt_compile_principal_los_data(x, "admissions"))
beddays_data <- list(results1, results2) |>
  purrr::map(\(x) pt_compile_principal_los_data(x, "beddays"))
los_data <- list(admissions_data, beddays_data) |>
  purrr::map(\(x) {
    rlang::set_names(x, c(scenario1_name, scenario2_name)) |>
      purrr::list_rbind(names_to = "scenario")
  }) |>
  rlang::set_names(c("Admissions", "Bed Days")) |>
  purrr::list_rbind(names_to = "measure")


# Test creation of LoS chart
create_los_bar_chart(los_data, "Elective Admission", "Admissions")


# Prepare data for Waterfall chart

pt_compile_cf_data <- function(...) {
  purrr::partial(
    reskit::compile_change_factor_data,
    pod_lookup = cond_ap_lookup,
    tpma_lookup = tpma_lookup
  )(...)
}
pt_compile_cf_data1 <- function(...) {
  purrr::partial(pt_compile_cf_data, results = results1)(...)
}
pt_compile_cf_data2 <- function(...) {
  purrr::partial(pt_compile_cf_data, results = results2)(...)
}


waterfall_data <- mat_combos_tbl_full |>
  dplyr::mutate(
    !!scenario1_name := purrr::pmap(mat_combos_tbl_full, pt_compile_cf_data1),
    !!scenario2_name := purrr::pmap(mat_combos_tbl_full, pt_compile_cf_data2)
  ) |>
  # listify_mat_scenarios_tbl()
  unnest_mat_scenarios_tbl() |>
  dplyr::left_join(at_lookup, "activity_type")


# Test creation of Waterfall chart

create_waterfall_chart(waterfall_data, "Inpatient", "Admissions")

# Prepare data for individual change factor (TPMA) impact charts

pt_compile_icf_data <- function(...) {
  purrr::partial(
    reskit::compile_indiv_change_factor_data,
    pod_lookup = cond_ap_lookup,
    tpma_lookup = tpma_lookup
  )(...)
}
pt_compile_icf_data1 <- function(...) {
  purrr::partial(pt_compile_icf_data, results = results1)(...)
}
pt_compile_icf_data2 <- function(...) {
  purrr::partial(pt_compile_icf_data, results = results2)(...)
}


impact_data <- mat_combos_tbl |>
  dplyr::mutate(
    !!scenario1_name := purrr::pmap(mat_combos_tbl, pt_compile_icf_data1),
    !!scenario2_name := purrr::pmap(mat_combos_tbl, pt_compile_icf_data2)
  ) |>
  # listify_cfmat_scenarios_tbl()
  unnest_cfmat_scenarios_tbl() |>
  dplyr::left_join(at_lookup, "activity_type")

# Check production of Activity Avoidance Individual Change Factors chart

create_impact_chart(
  impact_data,
  "activity_avoidance",
  "Inpatient",
  "Admissions"
)

# Check production of Efficiencies Individual Change Factors chart
create_impact_chart(impact_data, "efficiencies", "Inpatient", "Bed Days")

# Prepare data for p10/p90 chart

principal_pi_data <- list(
  reskit::compile_distribution_summary_data(results1, "principal"),
  reskit::compile_distribution_summary_data(results2, "principal")
) |>
  rlang::set_names(c(scenario1_name, scenario2_name)) |>
  purrr::list_rbind(names_to = "scenario")

# Test creation of p10/p90 chart

create_principal_pi_bar_chart(
  principal_pi_data,
  "Inpatients",
  "Elective Admission"
)

# Prepare data for Beeswarm and S-curve charts

pt_compile_distr_data <- function(...) {
  purrr::partial(
    reskit::compile_distribution_plot_data,
    pod_lookup = full_ap_lookup
  )(...)
}
pt_compile_dst_data1 <- function(...) {
  purrr::partial(pt_compile_distr_data, results = results1)(...)
}
pt_compile_dst_data2 <- function(...) {
  purrr::partial(pt_compile_distr_data, results = results2)(...)
}

beeswarm_data <- mat_combos_tbl_full |>
  dplyr::mutate(
    !!scenario1_name := purrr::pmap(mat_combos_tbl_full, pt_compile_dst_data1),
    !!scenario2_name := purrr::pmap(mat_combos_tbl_full, pt_compile_dst_data2)
  ) |>
  unnest_mat_scenarios_tbl() |>
  dplyr::left_join(at_lookup, "activity_type") |>
  dplyr::mutate(measure_label = create_measure_label(.data[["measure"]]))


# Test creation of beeswarm chart

create_beeswarm_chart(beeswarm_data, "Inpatient", "Admissions")
create_beeswarm_chart(beeswarm_data, "Outpatient", "Tele-attendances")
create_beeswarm_chart(beeswarm_data, "A&E", "Ambulance")


# Test creation of ecdf chart

ecdf_data <- beeswarm_data
create_ecdf_chart(ecdf_data, "Inpatient", "Admissions")
create_ecdf_chart(ecdf_data, "Outpatient", "Tele-attendances")
create_ecdf_chart(ecdf_data, "A&E", "Ambulance")
