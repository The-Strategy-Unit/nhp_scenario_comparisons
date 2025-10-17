#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server = function(input, output, session) {
    
  get_runtime_choices <- function(data, scheme, chosen_scenario) {
    data |>
      filter(dataset == scheme, scenario == chosen_scenario) |>
      pull(create_datetime) |>
      unique() |>
      sort() |>
      rev()
  }
  
  # Dynamically update scenarios when scheme is selected
  observeEvent(input$selected_scheme, {
    filtered_scenarios <- nhp_model_runs |> 
      filter(dataset == input$selected_scheme) |> 
      pull(scenario) |> 
      unique()
    
    updateSelectInput(session, "scenario_1", choices = filtered_scenarios)
    updateSelectInput(session, "scenario_2", choices = filtered_scenarios)
  })
  
  
  observeEvent(list(input$selected_scheme, input$scenario_1, input$scenario_2), {
    purrr::walk(c("scenario_1", "scenario_2"), function(scn) {
      runtimes <- get_runtime_choices(
        nhp_model_runs, 
        input$selected_scheme, 
        input[[scn]]
      )
      updateSelectInput(session, paste0(scn, "_runtime"), choices = runtimes)
    })
  })
  
  output$result_text <- renderText({
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
                   pull(app_version),
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
                   pull(app_version),
                 ")"),
          "from the scheme",
          input$selected_scheme)
  })
  
  output$warning_text <- renderUI({
    s1 <- nhp_model_runs |> filter(
      scenario == input$scenario_1, 
      dataset == input$selected_scheme,
      create_datetime == input$scenario_1_runtime)
    s2 <- nhp_model_runs |> filter(
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
         pull(app_version)) != 
        (nhp_model_runs |> 
         dplyr::filter(scenario == input$scenario_2,
                       create_datetime == input$scenario_2_runtime) |> 
         pull(app_version))){
      warnings <- c(warnings, "Warning: Selected scenarios must have been built on the same version of the model.")
    }
    
    # collapse into a single string, separated by new lines
    HTML(paste(warnings, collapse = "<br>"))
  })
  
  observeEvent(input$render_quarto, {
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
      output$quarto_summary <- renderUI({
        includeHTML("scenario_analysis_summary.html")
      })
    } else {
      output$quarto_summary <- renderUI({
        HTML("<p style='color:red;'>Scenarios must be different to render the summary.</p>")
      })
    }
  })
  
}