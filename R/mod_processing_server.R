mod_processing_server <- function(id, selections, trigger, use_local_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # create pre-processed data bundle
    rendered_data <- shiny::eventReactive(
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

          # Fetched from GitHub on first use, then cached for the lifetime of
          # the R process, so only the first render in this process pays.
          lookups <- get_app_lookups()
          full_ap_lookup <- lookups[["full_ap_lookup"]]
          cond_ap_lookup <- lookups[["cond_ap_lookup"]]
          atl_lookup <- lookups[["atl_lookup"]]
          tpma_lookup <- lookups[["tpma_lookup"]]
          # `results$default` has A&E walk-in + ambulance measures, whereas
          # `results$step_counts` (change factors) has only A&E "arrivals"
          core_mat_tbl <- lookups[["core_mat_tbl"]]
          cond_mat_tbl <- lookups[["cond_mat_tbl"]]

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
            cond_mat_tbl,
            full_ap_lookup,
            tpma_lookup
          )
          shiny::incProgress(0.05)

          # Prepare data for individual change factor (TPMA) impact charts
          tpma_impact_data <- prepare_tpma_impact_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            cond_mat_tbl,
            # step_counts pods are aae_type-XX, which cond_ap_lookup lacks
            full_ap_lookup,
            tpma_lookup
          )
          shiny::incProgress(0.05)

          # Prepare data for p10/p90 chart
          principal_pi_data <- prepare_principal_pi_data(
            results1,
            results2,
            scenario1_name,
            scenario2_name,
            full_ap_lookup
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
            tpma_impact_data = tpma_impact_data,
            principal_pi_data = principal_pi_data,
            beeswarm_data = beeswarm_data
          )
        })
      },
      ignoreInit = TRUE
    )
    shiny::observe(rendered_data())

    # Until Render Plots is first clicked the event reactive holds no value and
    # reading it pauses dependent outputs silently, leaving every tab blank.
    # Wrapping it in a validation turns that into a prompt for the user.
    shiny::reactive({
      shiny::validate(
        shiny::need(
          isTRUE(trigger() > 0),
          paste(
            "Select a scheme and two scenarios in the sidebar,",
            "then press Render Plots to view."
          )
        )
      )
      rendered_data()
    })
  })
}
