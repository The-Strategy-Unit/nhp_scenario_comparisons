mod_processing_server <- function(id, result_sets, scenario_selections, trigger){
  shiny::moduleServer(id, function(input, output, session){
    
    processed <- shiny::eventReactive(trigger()). {
      req(scenario_selections())
      
      selected <- scenario_selections()
      nhp_model_runs <- result_sets
      
      scenario_1_file <- nhp_model_runs |>
        dplyr::filter(scenario == selected$scenario_1,
               create_datetime == selected$scenario_1_runtime) |>
        dplyr::pull(file)
      
      scenario_2_file <- nhp_model_runs |>
        dplyr::filter(scenario == selected$scenario_2,
               create_datetime == selected$scenario_2_runtime) |>
        dplyr::pull(file)
      
      
      
      
      
      
      
    }
    
    
  })
}
