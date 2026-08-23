prepare_beeswarm_data <- function(
  results1,
  results2,
  scenario1_name,
  scenario2_name,
  core_mat_tbl,
  full_ap_lookup,
  atl_lookup
) {
  pt_compile_distr_data <- purrr::partial(
    reskit::compile_distribution_plot_data,
    pod_lookup = full_ap_lookup
  )
  pt_compile_dst_data1 <- purrr::partial(
    pt_compile_distr_data,
    results = results1
  )
  pt_compile_dst_data2 <- purrr::partial(
    pt_compile_distr_data,
    results = results2
  )
  if (scenario1_name == scenario2_name) {
    scenario1_name <- paste0(scenario1_name, " (s1)")
    scenario2_name <- paste0(scenario1_name, " (s2)")
  }
  core_mat_tbl |>
    dplyr::mutate(
      !!scenario1_name := purrr::pmap(core_mat_tbl, pt_compile_dst_data1),
      !!scenario2_name := purrr::pmap(core_mat_tbl, pt_compile_dst_data2)
    ) |>
    unnest_mat_scenarios_tbl() |>
    dplyr::left_join(atl_lookup, "activity_type") |>
    dplyr::mutate(
      measure_label = create_measure_label(.data[["measure"]])
    )
}


prepare_principal_pi_data <- function(
  results1,
  results2,
  scenario1_name,
  scenario2_name,
  full_atp_lookup
) {
  pt_compile_principal_pi_data <- purrr::partial(
    reskit::compile_distribution_summary_data,
    value_type = "principal",
    pod_lookup = full_atp_lookup
  )
  if (scenario1_name == scenario2_name) {
    scenario1_name <- paste0(scenario1_name, " (s1)")
    scenario2_name <- paste0(scenario1_name, " (s2)")
  }
  list(results1, results2) |>
    purrr::map(pt_compile_principal_pi_data) |>
    rlang::set_names(c(scenario1_name, scenario2_name)) |>
    purrr::list_rbind(names_to = "scenario") |>
    # If https://github.com/The-Strategy-Unit/nhp_reskit/issues/185
    # gets resolved and merged we should be able to ditch this step
    split_on_space("pod_label", names = c("activity_type_label", "pod_label"))
}


prepare_icf_impact_data <- function(
  results1,
  results2,
  scenario1_name,
  scenario2_name,
  core_mat_tbl,
  cond_ap_lookup,
  tpma_lookup,
  atl_lookup
) {
  pt_compile_icf_data <- purrr::partial(
    reskit::compile_indiv_change_factor_data,
    pod_lookup = cond_ap_lookup,
    tpma_lookup = tpma_lookup
  )
  pt_compile_icf_data1 <- purrr::partial(
    pt_compile_icf_data,
    results = results1
  )
  pt_compile_icf_data2 <- purrr::partial(
    pt_compile_icf_data,
    results = results2
  )
  if (scenario1_name == scenario2_name) {
    scenario1_name <- paste0(scenario1_name, " (s1)")
    scenario2_name <- paste0(scenario1_name, " (s2)")
  }
  core_mat_tbl |>
    dplyr::mutate(
      !!scenario1_name := purrr::pmap(core_mat_tbl, pt_compile_icf_data1),
      !!scenario2_name := purrr::pmap(core_mat_tbl, pt_compile_icf_data2)
    ) |>
    unnest_cfmat_scenarios_tbl() |>
    dplyr::left_join(atl_lookup, "activity_type") |>
    dplyr::mutate(measure_label = create_measure_label(.data[["measure"]]))
}


prepare_waterfall_data <- function(
  results1,
  results2,
  scenario1_name,
  scenario2_name,
  core_mat_tbl,
  full_ap_lookup,
  tpma_lookup,
  atl_lookup
) {
  pt_compile_cf_data <- purrr::partial(
    reskit::compile_change_factor_data,
    pod_lookup = full_ap_lookup,
    tpma_lookup = tpma_lookup
  )
  pt_compile_cf_data1 <- purrr::partial(pt_compile_cf_data, results = results1)
  pt_compile_cf_data2 <- purrr::partial(pt_compile_cf_data, results = results2)
  if (scenario1_name == scenario2_name) {
    scenario1_name <- paste0(scenario1_name, " (s1)")
    scenario2_name <- paste0(scenario1_name, " (s2)")
  }
  core_mat_tbl |>
    dplyr::mutate(
      !!scenario1_name := purrr::pmap(core_mat_tbl, pt_compile_cf_data1),
      !!scenario2_name := purrr::pmap(core_mat_tbl, pt_compile_cf_data2)
    ) |>
    unnest_cfmat_scenarios_tbl() |>
    dplyr::left_join(atl_lookup, "activity_type") |>
    dplyr::mutate(measure_label = create_measure_label(.data[["measure"]]))
}


prepare_los_data <- function(
  results1,
  results2,
  scenario1_name,
  scenario2_name,
  cond_ap_lookup
) {
  pt_compile_principal_los_data <- purrr::partial(
    reskit::compile_principal_los_data,
    pod_lookup = cond_ap_lookup
  )
  admissions_data <- list(results1, results2) |>
    purrr::map(\(x) pt_compile_principal_los_data(x, "admissions"))
  beddays_data <- list(results1, results2) |>
    purrr::map(\(x) pt_compile_principal_los_data(x, "beddays"))
  if (scenario1_name == scenario2_name) {
    scenario1_name <- paste0(scenario1_name, " (s1)")
    scenario2_name <- paste0(scenario1_name, " (s2)")
  }
  list(admissions_data, beddays_data) |>
    purrr::map(\(x) {
      rlang::set_names(x, c(scenario1_name, scenario2_name)) |>
        purrr::list_rbind(names_to = "scenario")
    }) |>
    rlang::set_names(c("Admissions", "Bed Days")) |>
    purrr::list_rbind(names_to = "measure")
}


prepare_summary_data <- function(
  results1,
  results2,
  scenario1_name,
  scenario2_name,
  cond_ap_lookup
) {
  pt_compile_principal_pod_data <- purrr::partial(
    reskit::compile_principal_pod_data,
    pod_lookup = cond_ap_lookup
  )
  if (scenario1_name == scenario2_name) {
    scenario1_name <- paste0(scenario1_name, " (s1)")
    scenario2_name <- paste0(scenario1_name, " (s2)")
  }
  list(results1, results2) |>
    purrr::map(pt_compile_principal_pod_data) |>
    rlang::set_names(c(scenario1_name, scenario2_name)) |>
    purrr::list_rbind(names_to = "scenario") |>
    dplyr::mutate(
      dplyr::across("activity_type_label", \(x) {
        dplyr::if_else(grepl("^Inp", x), x, paste0(x, " Activity"))
      })
    ) |>
    dplyr::mutate(
      dplyr::across("pod_label", \(x) {
        forcats::fct_reorder(x, .data[["baseline"]])
      }),
      dplyr::across("pod_label", \(x) sub(" (Admission|Bed Days)$", "", x)),
      .by = "measure"
    )
}


split_on_space <- function(...) {
  purrr::partial(tidyr::separate_wider_delim, delim = " ", too_many = "merge")(
    ...
  )
}

unnest_mat_scenarios_tbl <- function(mat_scenarios_tbl) {
  intermediate <- mat_scenarios_tbl |>
    tidyr::pivot_longer(
      !c("measure", "activity_type"),
      names_to = "scenario"
    ) |>
    dplyr::filter_out(purrr::map_int(.data[["value"]], nrow) == 0)
  if (nrow(intermediate) == 0) {
    tibble::tibble(
      activity_type = character(0),
      scenario = character(0),
      change_factor = character(0),
      measure = character(0),
      tpma_label = factor(0),
      value = numeric(0)
    )
  } else {
    tidyr::unnest(intermediate, "value")
  }
}

unnest_cfmat_scenarios_tbl <- function(mat_scenarios_tbl) {
  intermediate <- mat_scenarios_tbl |>
    dplyr::select(!"measure") |>
    tidyr::pivot_longer(!"activity_type", names_to = "scenario") |>
    dplyr::filter_out(purrr::map_int(.data[["value"]], nrow) == 0)
  if (nrow(intermediate) == 0) {
    tibble::tibble(
      activity_type = character(0),
      scenario = character(0),
      change_factor = character(0),
      measure = character(0),
      tpma_label = factor(0),
      value = numeric(0)
    )
  } else {
    tidyr::unnest(intermediate, "value")
  }
}
