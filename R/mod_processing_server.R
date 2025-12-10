mod_processing_server <- function(id, 
                                  result_sets,
                                  scenario_selections,
                                  errors,
                                  trigger,
                                  local_data_flag
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
      
      if(local_data_flag == TRUE){
        
        jsons <- tibble::tibble(paths =list.files("jsons/",
                                 full.names = TRUE)
        ) |> 
          dplyr::filter(stringr::str_ends(paths, "\\.json\\.gz")) |> 
          dplyr::mutate(file_name = stringr::str_remove(paths, "jsons/"))
        
        json_1_file <- jsons$paths[jsons$file_name == stringr::str_extract(
          scenario_1_file, "[^/]+$")]
        
        json_2_file <- jsons$paths[jsons$file_name == stringr::str_extract(
          scenario_2_file, "[^/]+$")]
        
        get_json_results <- function(path){
        readBin(path, raw(), n = file.size(path)) |>
          jsonlite::parse_gzjson_raw(simplifyVector = FALSE) |>
          parse_results()
        }
        
        result_1 <- get_json_results(json_1_file)
        
        result_2 <- get_json_results(json_2_file)
        
      } else{
      
      result_1 <- get_nhp_results(file = scenario_1_file)
      result_2 <- get_nhp_results(file = scenario_2_file)
      
      }
      
      # grab the scenario_names
      scenario_1_name <- result_1$params$scenario
      scenario_2_name <- result_2$params$scenario
      
      df1 <- mod_principal_summary_data(result_1, sites = NULL) |> 
        dplyr::mutate(scenario = scenario_1_name)
      df2 <- mod_principal_summary_data(result_2, sites = NULL) |> 
        dplyr::mutate(scenario = scenario_2_name)
      
      # data processing
      data <- dplyr::bind_rows(df1, df2)
      
      
      # get the measure from the pod name
      # cols [1] "scenario"      "pod_name"     
      #[3] "activity_type" "baseline"     
      #[5] "principal"     "change"       
      #[7] "change_pcnt"   "measure"  
      # activity type drives the main plot
      # measure drives the y axis
      # pod_name is the y axis. This might be able to be combined
      data <- data |>
        dplyr::mutate(measure=dplyr::case_when(grepl("Admission", pod_name) ~ "Admissions",
                                               grepl("Bed Day", pod_name) ~ "Bed days",
                                               TRUE ~ "Attendance / procedure"))
      # 
      # # LoS summary -------------------------------------------------------------
      # 
      # 
      # admissions dataset
      data_1_adm <- result_1 |>
        mod_principal_summary_los_data(sites = NULL, measure = "admissions")
      
      data_2_adm <- result_2 |>
        mod_principal_summary_los_data(sites = NULL, measure = "admissions")
      
      # Bed days dataset
      data_1_bed <- result_1 |>
        mod_principal_summary_los_data(sites = NULL, measure = "beddays")
      
      data_2_bed <- result_2 |>
        mod_principal_summary_los_data(sites = NULL, measure = "beddays")
      
      # data processing
      data_admissions <- dplyr::bind_rows(scenario_1 = data_1_adm, scenario_2 = data_2_adm, .id = "scenario") |> 
        dplyr::mutate(scenario = dplyr::case_when(scenario == "scenario_1" ~ scenario_1_name,
                                                  scenario == "scenario_2" ~ scenario_2_name,
                                                  T ~ scenario))
      data_bed <- dplyr::bind_rows(scenario_1 = data_1_bed, scenario_2 = data_2_bed, .id = "scenario") |> 
        dplyr::mutate(scenario = dplyr::case_when(scenario == "scenario_1" ~ scenario_1_name,
                                                  scenario == "scenario_2" ~ scenario_2_name,
                                                  T ~ scenario))
      
      # cols, measure, scenario, pod_name, los_group, baseline, principal. change. change.pcnt
      # apparently pods drive the chart data
      #length of stay is the y axis and admissions on the x axis
      
      data_combine <- dplyr::bind_rows("Bed Days" = data_bed, admissions = data_admissions, .id = "measure")
      #
      # # impacts -----------------------------------------------------------------
      # 
      # 
      # # Waterfalls use a list, containing one df for ip, op, aae
      # # 2 lists are used to make the water falls
      # 
      # # Prep a list of summary dataframes, one per activity type
      pcfs_1 <- prepare_all_principal_change_factors(
        r = result_1,
        site_codes = list(ip = NULL, op = NULL, aae = NULL)
      ) 
      
      pcfs_2 <- prepare_all_principal_change_factors(
        r = result_2,
        site_codes = list(ip = NULL, op = NULL, aae = NULL)
      )
      # 
      # # impact bars
      # # [1] "scenario"       "measure"       
      # # [3] "activity_type"  "change_factor" 
      # # [5] "strategy"       "mitigator_name"
      # # [7] "value" 
      # # change factor drives the plot at the highest level
      # # activity type and measure controls the data shown
      # # mitigator_name controls y axis labels
      # # change factor activity_type and measure combination controls the strategies shown in plot
      # # pod on y axis
      # # this isn't pod on the y axis label is it, error here
      # # could have module ui be like 'tab variable', 'filter1 var', 'filter2 var'
      ndg_variants_sc_comparison <- dplyr::bind_rows(
        scenario_1 = as.data.frame(dplyr::bind_rows(pcfs_1))|> 
          dplyr::mutate(scenario = scenario_1_name),
        scenario_2 = as.data.frame(dplyr::bind_rows(pcfs_2))|> 
          dplyr::mutate(scenario = scenario_2_name))
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
      data_distribution_summary <- dplyr::bind_rows(
        result_1$results$default |> dplyr::mutate(scenario = scenario_1_name),
        result_2$results$default |> dplyr::mutate(scenario = scenario_2_name)
      )
      
      #p <- mod_model_results_distribution_get_data(result_1,selected_measure = c("Ip","ip_elective_admission","admissions"),site_codes = NULL)
      data_distribution_summary <- data_distribution_summary |>
        dplyr::select(scenario,pod,measure,principal,lwr_ci,upr_ci) |>
        dplyr::group_by(scenario,pod,measure) |>
        dplyr::summarise(principal = sum(principal),
                         lwr_ci = sum(lwr_ci),
                         upr_ci = sum(upr_ci)) |>
        dplyr::ungroup()
      # 
      data_distribution_summary <- data_distribution_summary |>
        dplyr::mutate(
          activity_type = dplyr::case_when(
            substr(pod, 1, 2) == "ip" ~ "Inpatient",
            substr(pod, 1, 2) == "op" ~ "Outpatient",
            substr(pod, 1, 2) == "aa" ~ "A&E",
            TRUE ~ "Other"
          )
        ) |> dplyr::relocate(activity_type, .before = 1)
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
      
      
      list(data = data,
           data_combine = data_combine,
           waterfall_data = list(pcfs_1 = pcfs_1,
                                 pcfs_2 = pcfs_2,
                                 scenario_1_name = scenario_1_name,
                                 scenario_2_name = scenario_2_name),
           ndg_variants_sc_comparison = ndg_variants_sc_comparison,
           data_distribution_summary = data_distribution_summary,
           distribution_data = list(result_1 = result_1,
                                    result_2 = result_2,
                                    scenario_1_name = scenario_1_name,
                                    scenario_2_name = scenario_2_name)
      )
    }
    )
    
    #output$quarto_summary <- shiny::renderTable(processed())
    
    return(processed)
    
    #return(list(data = processed()))
    
  })
}
