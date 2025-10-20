#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server = function(input, output, session) {
  
  nhp_model_runs <- get_nhp_result_sets() |> 
    dplyr::filter(!app_version == "dev")
  
  shiny::observe(
    shiny::updateSelectInput(session, 
                             "selected_scheme", 
                             choices = unique(nhp_model_runs$dataset))
  )
  
  get_runtime_choices <- function(data, scheme, chosen_scenario) {
    data |>
      dplyr::filter(dataset == scheme, scenario == chosen_scenario) |>
      dplyr::pull(create_datetime) |>
      unique() |>
      sort() |>
      rev()
  }
  
  # Dynamically update scenarios when scheme is selected
  shiny::observeEvent(input$selected_scheme, {
    shiny::req(nhp_model_runs)
    filtered_scenarios <- nhp_model_runs |> 
      dplyr::filter(dataset == input$selected_scheme) |> 
      dplyr::pull(scenario) |> 
      unique()
    
    shiny::updateSelectInput(session, "scenario_1", choices = filtered_scenarios)
    shiny::updateSelectInput(session, "scenario_2", choices = filtered_scenarios)
  })
  
  
  shiny::observeEvent(list(input$selected_scheme, input$scenario_1, input$scenario_2), {
    shiny::req(nhp_model_runs)
    purrr::walk(c("scenario_1", "scenario_2"), function(scn) {
      runtimes <- get_runtime_choices(
        nhp_model_runs, 
        input$selected_scheme, 
        input[[scn]]
      )
      shiny::updateSelectInput(session, paste0(scn, "_runtime"), choices = runtimes)
    })
  })
  
  output$result_text <- shiny::renderText({
    paste("You have selected",
          input$scenario_1,
          paste0(
            "(",
            lubridate::as_datetime(input$scenario_1_runtime),
            ")"),
          paste0("(model version: ", 
                 nhp_model_runs |> 
                   dplyr::filter(scenario == input$scenario_1,
                                 create_datetime == input$scenario_1_runtime) |> 
                   dplyr::pull(app_version),
                 ")"),
          "and",
          input$scenario_2,
          paste0(
            "(",
            lubridate::as_datetime(input$scenario_2_runtime),
            ")"),
          paste0("(model version: ", 
                 nhp_model_runs |> 
                   dplyr::filter(scenario == input$scenario_2,
                                 create_datetime == input$scenario_2_runtime) |> 
                   dplyr::pull(app_version),
                 ")"),
          "from the scheme",
          input$selected_scheme)
  })
  
  output$warning_text <- shiny::renderUI({
    shiny::req(nhp_model_runs)
    s1 <- nhp_model_runs |> dplyr::filter(
      scenario == input$scenario_1, 
      dataset == input$selected_scheme,
      create_datetime == input$scenario_1_runtime)
    s2 <- nhp_model_runs |> dplyr::filter(
      scenario == input$scenario_2, 
      dataset == input$selected_scheme,
      create_datetime == input$scenario_2_runtime)
    
    # tests that the start and end years match
    starts_match <- identical(s1$start_year, s2$start_year) 
    ends_match <- identical(s1$end_year, s2$end_year)  # fixed typo
    
    # collect warnings
    warnings <- c()
    
    if (input$scenario_1 == input$scenario_2 &
        input$scenario_1_runtime == input$scenario_2_runtime) {
      warnings <- c(warnings, "Warning: Scenario 1 and Scenario 2 must be different.")
    }
    
    if (!starts_match || !ends_match) {
      warnings <- c(warnings, "Warning: The start and end years of the selected scenarios must match.")
    }
    
    if ((nhp_model_runs |> 
         dplyr::filter(scenario == input$scenario_1,
                       create_datetime == input$scenario_1_runtime) |> 
         dplyr::pull(app_version)) != 
        (nhp_model_runs |> 
         dplyr::filter(scenario == input$scenario_2,
                       create_datetime == input$scenario_2_runtime) |> 
         dplyr::pull(app_version))){
      warnings <- c(warnings, "Warning: Selected scenarios must have been built on the same version of the model.")
    }
    
    # collapse into a single string, separated by new lines
    HTML(paste(warnings, collapse = "<br>"))
  })
  
  shiny::observeEvent(input$render_quarto, {
    if (!(input$scenario_1 == input$scenario_2 &
          input$scenario_1_runtime == input$scenario_2_runtime)) {
      quarto::quarto_render(
        "scenario_analysis_summary.qmd", 
        output_file = "scenario_analysis_summary.html", 
        execute_params = list(
          scenario_1 = input$scenario_1,
          scenario_1_runtime = 
            as.character(
              lubridate::as_datetime(
                input$scenario_1_runtime
              )
            ),
          scenario_2 = input$scenario_2,
          scenario_2_runtime = 
            as.character(
              lubridate::as_datetime(
                input$scenario_2_runtime 
              )
            )
        ))
      output$quarto_summary <- shiny::renderUI({
        htmltools::includeHTML("scenario_analysis_summary.html")
      })
    } else {
      output$quarto_summary <- shiny::renderUI({
        htmltools::HTML("<p style='color:red;'>Scenarios must be different to render the summary.</p>")
      })
    }
  })
  
}