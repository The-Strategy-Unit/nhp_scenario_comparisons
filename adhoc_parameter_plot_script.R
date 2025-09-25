

params <- jsonlite::fromJSON("C:/Users/ozayr.mohammed/Downloads/params-128-test.json", flatten = TRUE)


params <- scenarios_results[1]

df_parsed <- as.data.frame(parsed)

report_params_table <- function(
    p, # a single scheme's params
    parameter = c("activity_avoidance", "efficiencies")
) {
  parameter_data <- p[["params"]][[parameter]]
  
  time_profiles <- p[["params"]][["time_profile_mappings"]][[parameter]] |>
    purrr::map(unlist) |>
    purrr::map(tibble::enframe, "strategy", "time_profile") |>
    data.table::rbindlist(idcol = "activity_type") |>
    dplyr::tibble()
  
  parameter_data |>
    purrr::map_depth(2, "interval") |>
    purrr::map(tibble::enframe, "strategy") |>
    dplyr::bind_rows(.id = "activity_type") |>
    tidyr::unnest_wider("value", names_sep = "_") |>
    dplyr::left_join(
      time_profiles,
      by = dplyr::join_by("activity_type", "strategy")
    ) |>
    dplyr::arrange("activity_type_name", "mitigator_name") |>
    dplyr::mutate(
      parameter = parameter,
      peer = p[["params"]][["dataset"]],
      baseline_year = p[["params"]][["start_year"]],
      horizon_year = p[["params"]][["end_year"]]
    ) |> 
    dplyr::mutate(scenario = p[["params"]][["scenario"]])
}


#extract_params <- function(params, runs_meta) {
  possibly_report_params_table <- purrr::possibly(report_params_table)
  
  activity_avoidance <- params |>
    purrr::map(possibly_report_params_table, "activity_avoidance") |>
    purrr::list_rbind()
  
  efficiencies <- params |>
    purrr::map(possibly_report_params_table, "efficiencies") |>
    purrr::list_rbind()
  
  # runs_meta <- runs_meta |>
  #   dplyr::select(.data$dataset, .data$scenario, .data$run_stage)
  
  tmp <- activity_avoidance |>
    dplyr::bind_rows(efficiencies) 
  
  
tmp |>
  dplyr::filter(parameter == "efficiencies",
                activity_type == "op") |> 
  ggplot()+
  geom_pointrange(aes(x = (value_1+value_2)/2, 
                      xmin = value_1, xmax = value_2, 
                      y = reorder(strategy, desc(strategy)) ,
                      colour = time_profile,
                      shape = parameter),
                  linewidth = 2)+
  facet_grid(parameter~activity_type,
             scale = "free_y")+
  xlim(0, 1)


# tmp |>
#   dplyr::filter(strategy == "smoking") |> 
#   ggplot()+
#   geom_boxplot(aes(xlower = value_1,
#                    xupper = value_2,
#                    xmiddle = (value_1+value_2)/2,
#                    y = strategy),
#                stat = 'identity')
  
                  