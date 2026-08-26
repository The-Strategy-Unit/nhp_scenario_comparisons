mod_processing_server <- function(id, selections, trigger, use_local_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # This requires a call to GitHub to retrieve a file.
    # Run once and then pass data to reskit functions, rather than running
    # each time a function is called
    full_apm_lookup <- get_full_apm_lookup()
    cond_apm_lookup <- get_condensed_apm_lookup(full_apm_lookup)
    full_ap_lookup <- dplyr::select(full_apm_lookup, !"measure") |>
      dplyr::distinct()
    cond_ap_lookup <- dplyr::select(cond_apm_lookup, !"measure") |>
      dplyr::distinct()
    # matches reskit's get_detailed_pods()
    full_atp_lookup <- dplyr::select(full_ap_lookup, !"activity_type")
    atl_lookup <- full_apm_lookup |>
      dplyr::distinct(dplyr::pick(tidyselect::starts_with(
        "activity_type"
      )))
    tpma_lookup <- reskit::get_tpma_label_lookup()

    # Create core table with a row for each pair of measure and
    # activity_type, for pmapping over
    core_mat_tbl <- full_apm_lookup |>
      dplyr::distinct(dplyr::pick(c("measure", "activity_type")))

    # create pre-processed data bundle
    processed_data <- shiny::eventReactive(
      trigger(),
      {
        shiny::req(selections$main_scenario, selections$comp_scenario)
        shiny::req(
          nrow(selections$main_scenario) == 1,
          nrow(selections$comp_scenario) == 1
        )

        shiny::withProgress(message = "Fetching scenarios...", value = 0, {
          scenario1_name <- selections$main_scenario[["scenario"]]
          scenario2_name <- selections$comp_scenario[["scenario"]]
          shiny::req(all(lengths(c(scenario1_name, scenario2_name)) == 1))

          scenario1_dir <- selections$main_scenario[["aggregated_results_path"]]
          scenario2_dir <- selections$comp_scenario[["aggregated_results_path"]]

          shiny::incProgress(0.1)
          shiny::req(all(lengths(c(scenario1_dir, scenario2_dir)) == 1))

          if (use_local_data) {
            list_dirs <- purrr::partial(
              dir,
              full.names = TRUE,
              recursive = TRUE
            )
            rds_paths <- list_dirs("rds", "\\.rds$")
            rds_path1 <- grepv(scenario1_name, rds_paths)
            rds_path2 <- grepv(scenario2_name, rds_paths)
            results1 <- readr::read_rds(rds_path1)
            results2 <- readr::read_rds(rds_path2)
            shiny::incProgress(0.6)
          } else {
            results1 <- read_azure_results(scenario1_dir)
            shiny::incProgress(0.3)

            results2 <- read_azure_results(scenario2_dir)
            shiny::incProgress(0.3)
          }

          # Prepare data for Summary chart
          summary_data <- prepare_summary_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            cond_ap_lookup
          )
          shiny::incProgress(0.05)

          # Prepare data for LoS chart
          los_data <- prepare_los_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            cond_ap_lookup
          )
          shiny::incProgress(0.05)

          # Prepare data for Waterfall chart
          waterfall_data <- prepare_waterfall_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            core_mat_tbl,
            full_ap_lookup,
            tpma_lookup,
            atl_lookup
          )
          shiny::incProgress(0.05)

          # Prepare data for individual change factor (TPMA) impact charts
          icf_impact_data <- prepare_icf_impact_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            core_mat_tbl,
            cond_ap_lookup,
            tpma_lookup,
            atl_lookup
          )
          shiny::incProgress(0.05)

          # Prepare data for p10/p90 chart
          principal_pi_data <- prepare_principal_pi_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            full_atp_lookup
          )
          shiny::incProgress(0.05)

          # Prepare data for Beeswarm and S-curve charts
          beeswarm_data <- prepare_beeswarm_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            core_mat_tbl,
            full_ap_lookup,
            atl_lookup
          )
          shiny::incProgress(0.05)

          # Create a list to export data as `processed_data`
          list(
            summary_data = summary_data,
            los_data = los_data,
            waterfall_data = waterfall_data,
            icf_impact_data = icf_impact_data,
            principal_pi_data = principal_pi_data,
            beeswarm_data = beeswarm_data
          )
        })
      },
      ignoreInit = TRUE
    )
    processed_data
  })
}
