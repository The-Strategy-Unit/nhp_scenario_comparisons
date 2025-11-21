mod_processing_server <- function(id, result_sets, scenario_selections, errors, trigger
){
  shiny::moduleServer(id, function(input, output, session){
    
    processed <- shiny::eventReactive(trigger(), {
      shiny::req(length(errors()) == 0)
      shiny::req(scenario_selections()$scenario_1, 
                 scenario_selections()$scenario_1_runtime,
                 scenario_selections()$scenario_2,
                 scenario_selections()$scenario_2_runtime)
      
      selected <- scenario_selections()
      
      nhp_model_runs <- result_sets
      
      #get files
      scenario_1_file <- nhp_model_runs |>
        dplyr::filter(scenario == selected$scenario_1,
                      create_datetime == selected$scenario_1_runtime) |>
        dplyr::pull(file)
      
      scenario_2_file <- nhp_model_runs |>
        dplyr::filter(scenario == selected$scenario_2,
                      create_datetime == selected$scenario_2_runtime) |>
        dplyr::pull(file)
      
      shiny::req(length(scenario_1_file) > 0, 
                 length(scenario_2_file) > 0)
      
      result_1 <- get_nhp_results(file = scenario_1_file)
      result_2 <- get_nhp_results(file = scenario_2_file)
      
      
      # grab the scenario_names
      scenario_1_name <- result_1$params$scenario
      scenario_2_name <- result_2$params$scenario
      
      df1 <- mod_principal_summary_data(result_1, sites = NULL) |> 
        dplyr::mutate(scenario = scenario_1_name)
      df2 <- mod_principal_summary_data(result_2, sites = NULL) |> 
        dplyr::mutate(scenario = scenario_2_name)
      
      # data processing
      data <- bind_rows(df1, df2)
      
      
      # get the measure from the pod name
      data <- data |>
        dplyr::mutate(measure=dplyr::case_when(grepl("Admission", pod_name) ~ "Admissions",
                                               grepl("Bed Day", pod_name) ~ "Bed days",
                                               TRUE ~ "Attendance / procedure"))
      # 
      # # LoS summary -------------------------------------------------------------
      # 
      # 
      # ## admissions dataset
      # data_1_adm <- result_1 |> 
      #   mod_principal_summary_los_data(sites = NULL, measure = "admissions") 
      # 
      # data_2_adm <- result_2 |> 
      #   mod_principal_summary_los_data(sites = NULL, measure = "admissions") 
      # 
      # ## Bed days dataset
      # data_1_bed <- result_1 |> 
      #   mod_principal_summary_los_data(sites = NULL, measure = "beddays") 
      # 
      # data_2_bed <- result_2 |> 
      #   mod_principal_summary_los_data(sites = NULL, measure = "beddays") 
      # 
      # # data processing
      # data_admissions <- dplyr::bind_rows(scenario_1 = data_1_adm, scenario_2 = data_2_adm, .id = "scenario")
      # data_bed <- dplyr::bind_rows(scenario_1 = data_1_bed, scenario_2 = data_2_bed, .id = "scenario")
      # data_combine <- dplyr::bind_rows("Bed Days" = data_bed, admissions = data_admissions, .id = "measure")
      # 
      # # impacts -----------------------------------------------------------------
      # 
      # 
      # 
      # 
      # # Prep a list of summary dataframes, one per activity type
      # pcfs_1 <- prepare_all_principal_change_factors(
      #   r = result_1,
      #   site_codes = list(ip = NULL, op = NULL, aae = NULL)
      # )
      # 
      # pcfs_2 <- prepare_all_principal_change_factors(
      #   r = result_2,
      #   site_codes = list(ip = NULL, op = NULL, aae = NULL)
      # )
      # 
      # ndg_variants_sc_comparison <- dplyr::bind_rows(
      #   scenario_1 = as.data.frame(bind_rows(pcfs_1)),
      #   scenario_2 = as.data.frame(bind_rows(pcfs_2)),
      #   .id = "scenario")
      # 
      # 
      # 
      # # activity in detail  -----------------------------------------------------
      # 
      # tretspef_lookup <- jsonlite::read_json("supporting_data/tx-lookup.json",
      #                                        simplifyVector = TRUE
      # ) |>
      #   dplyr::mutate(
      #     dplyr::across("Description", \(x) stringr::str_remove(x, " Service$")),
      #     dplyr::across("Description", \(x) paste0(.data$Code, ": ", .data$Description)),
      #   ) |>
      #   dplyr::select(-"Group") |>
      #   dplyr::add_row(Code = "&", Description = "Not known")  # as per HES dictionary 
      # 
      # ## We get a list of all the combinations of activity type, pod, measure and 
      # # aggregation we are interested in
      # 
      # # For inpatients, it is admissions and bed days
      # ip_parameter_matrix <- expand.grid(
      #   activity_type = "ip", 
      #   pod = c("ip_elective_admission", "ip_maternity_admission", "ip_elective_daycase",
      #           "ip_regular_day_attender", "ip_non-elective_admission"),
      #   measure = c("admissions", "beddays"),
      #   agg_col = c("tretspef", "age_group"),
      #   stringsAsFactors = FALSE)
      # 
      # # for outpatients, attendances and tele-attendances
      # op_parameter_matrix <- expand.grid(
      #   activity_type = "op",
      #   pod = c("op_procedure", "op_follow-up", "op_first"),
      #   measure = c("attendances", "tele_attendances"),
      #   agg_col = c("tretspef", "age_group"),
      #   stringsAsFactors = FALSE)
      # 
      # # for A&E, ambulance and walk-in and there is no tretspef
      # aae_parameter_matrix <- expand.grid(
      #   activity_type = "aae",
      #   pod = c("aae_type-01", "aae_type-02"),
      #   measure = c("ambulance", "walk-in"),
      #   agg_col = c("age_group"),
      #   stringsAsFactors = FALSE)
      # 
      # # combine them all together
      # parameter_matrix <- dplyr::bind_rows(
      #   ip_parameter_matrix,
      #   op_parameter_matrix, 
      #   aae_parameter_matrix)
      # 
      # 
      # 
      # # We now insert into our function for iterating the `combine_activity_data` 
      # # function across all combinations
      # # detailed_activity_data <- run_combinations_list(parameter_matrix, result_1, result_2)
      # 
      # 
      # # 80% CI -----------------------------------------------------------
      # 
      # # load dataset 
      # data_distribution_summary <- dplyr::bind_rows(
      #   scenario_1 = result_1$results$default, 
      #   scenario_2 = result_2$results$default, 
      #   .id = "scenario")
      # 
      # #p <- mod_model_results_distribution_get_data(result_1,selected_measure = c("Ip","ip_elective_admission","admissions"),site_codes = NULL)
      # data_distribution_summary <- data_distribution_summary |> 
      #   dplyr::select(scenario,pod,measure,principal,lwr_ci,upr_ci) |> 
      #   dplyr::group_by(scenario,pod,measure) |> 
      #   dplyr::summarise(principal = sum(principal),
      #                    lwr_ci = sum(lwr_ci),
      #                    upr_ci = sum(upr_ci)) |> 
      #   dplyr::ungroup()
      # 
      # data_distribution_summary <- data_distribution_summary |> 
      #   dplyr::mutate(
      #     activity_type = dplyr::case_when(
      #       substr(pod, 1, 2) == "ip" ~ "Inpatient",
      #       substr(pod, 1, 2) == "op" ~ "Outpatient",
      #       substr(pod, 1, 2) == "aa" ~ "A&E",
      #       TRUE ~ "Other"
      #     )
      #   ) |> dplyr::relocate(activity_type, .before = 1)
      # 
      # 
      # 
      # # model run distributions --------------------------------------------------
      # 
      # scenario_1_ip_admission_dist <- get_model_run_distribution(
      #   result_1, 
      #   pod = c("ip_elective_daycase", 
      #           "ip_non-elective_admission",
      #           "ip_regular_day_attender", 
      #           "ip_elective_admission", 
      #           "ip_maternity_admission"
      #   ),
      #   measure = "admissions",
      #   site_codes = NULL
      # )
      # 
      # # ndg 2
      # scenario_2_ip_admission_dist <- get_model_run_distribution(
      #   result_2, 
      #   pod = c("ip_elective_daycase", 
      #           "ip_non-elective_admission",
      #           "ip_regular_day_attender", 
      #           "ip_elective_admission", 
      #           "ip_maternity_admission"
      #   ),
      #   measure = "admissions",
      #   site_codes = NULL
      # )
      # 
      # # join them together
      # ip_admissions_dist_comparison <- dplyr::bind_rows(
      #   scenario_1 = scenario_1_ip_admission_dist,
      #   scenario_2 = scenario_2_ip_admission_dist,
      #   .id = "scenario"
      # )
      
      
      # return(list(
      #   data = data
      # )
      
      
      list(data = data)
    }
    )
    
    #output$quarto_summary <- shiny::renderTable(processed())
    
    return(processed)
    
    #return(list(data = processed()))
    
  })
}
