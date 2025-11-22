#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
#' 

# nhp_model_runs <- get_nhp_result_sets() |> 
#   dplyr::filter(!app_version == "dev")

library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)
library(shiny)

file_names_nhs_output <- list.files(path = 'R/nhp_outputs', pattern = "\\.R$")
lapply(paste0('R/nhp_outputs/',file_names_nhs_output), source)

#this should be commented out in live versions
nhp_model_runs <- readRDS("inst/app/tmp_runs_file.rds") |> #tmp_runs_file.rds is an rds of the output of get_nhp_result_sets()
  dplyr::filter(!app_version == "dev") 

app_server = function(input, output, session) {

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
  
  errors_reactive <- shiny::reactiveVal()
  
  
  output$warning_text <- shiny::renderUI({
    shiny::req(nhp_model_runs, input$scenario_1, input$scenario_1_runtime, input$scenario_2, input$scenario_2_runtime)
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
    
    # collect errors
    errors <- c()
    
    if (input$scenario_1 == input$scenario_2 &
        input$scenario_1_runtime == input$scenario_2_runtime) {
      errors <- c(errors, "Error: Scenario 1 and Scenario 2 must be different.")
    }
    
    if (!starts_match || !ends_match) {
      errors <- c(errors, "Error: The start and end years of the selected scenarios must match.")
    }
    
    if ((nhp_model_runs |> 
         dplyr::filter(scenario == input$scenario_1,
                       create_datetime == input$scenario_1_runtime) |> 
         dplyr::pull(app_version)) != 
        (nhp_model_runs |> 
         dplyr::filter(scenario == input$scenario_2,
                       create_datetime == input$scenario_2_runtime) |> 
         dplyr::pull(app_version))){
      errors <- c(errors, "Error: Selected scenarios must have been built on the same version of the model.")
    }
    
    errors_reactive(errors)
    # collapse into a single string, separated by new lines
    HTML(paste(errors, collapse = "<br>"))
  })
  
  shiny::observe({
    if(length(errors_reactive()) == 0){
      shinyjs::enable("render_plot")
      output$errors <- shiny::renderUI(NULL)
    } else {
      shinyjs::disable("render_plot")
      output$errors <- shiny::renderUI(
        htmltools::HTML("<p style='color:red;'>Please resolve scenario selection errors to produce plots.</p>")
      )
    }


  })
  
  
  processed <- 
    mod_processing_server("processing1",
                          result_sets = nhp_model_runs,
                          scenario_selections = shiny::reactive(
                            list(scenario_1 = input$scenario_1,
                                 scenario_1_runtime = input$scenario_1_runtime,
                                 scenario_2 = input$scenario_2,
                                 scenario_2_runtime = input$scenario_2_runtime)
                          ),
                          errors = errors_reactive,
                          trigger = shiny::reactive(input$render_plot)
    )
  
  
  mod_summary_server("summary1",
                     processed = processed)
  mod_los_server("los1",
                    processed = processed)
  mod_waterfall_server("waterfall1",
                       processed = processed)
  mod_activity_avoidance_impact_server("activity_avoidance1",
                                       processed = processed)
  
  
  # shiny::observeEvent(input$render_plot, {
  #   if (!(input$scenario_1 == input$scenario_2 &
  #         input$scenario_1_runtime == input$scenario_2_runtime) & 
  #       is.null(errors_reactive()) # Errors will prevent output rendering
  #   ){
  #     quarto::quarto_render(
  #       "scenario_analysis_summary.qmd", 
  #       output_file = "scenario_analysis_summary.html", 
  #       execute_params = list(
  #         scenario_1 = input$scenario_1,
  #         scenario_1_runtime = 
  #           as.character(
  #             lubridate::as_datetime(
  #               input$scenario_1_runtime
  #             )
  #           ),
  #         scenario_2 = input$scenario_2,
  #         scenario_2_runtime = 
  #           as.character(
  #             lubridate::as_datetime(
  #               input$scenario_2_runtime 
  #             )
  #           )
  #       ))
  #     output$quarto_summary <- shiny::renderUI({
  #       htmltools::includeHTML("scenario_analysis_summary.html")
  #     })
  #   } else {
  #     output$quarto_summary <- shiny::renderUI({
  #       htmltools::HTML("<p style='color:red;'>Resolve scenario selection errors to produce plots.</p>")
  #     })
  #   }
  # })
  
}