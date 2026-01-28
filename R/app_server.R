#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd

#this should be commented out in live versions

# load_local_data <- TRUE
# nhp_model_runs <- readRDS("inst/app/tmp_runs_file.rds") |> #tmp_runs_file.rds is an rds of the output of get_nhp_result_sets()
#   dplyr::filter(!app_version == "dev") |> 
#   dplyr::filter(stringr::str_extract(file, "[^/]+$") %in% 
#                   list.files("jsons/")
#   )

app_server = function(input, output, session) {
  load_local_data <- FALSE
  
  allowed_datasets <- shiny::reactive({
    get_user_allowed_datasets(session$groups)
  })
  
  nhp_model_runs <- shiny::reactive({
    rs <- get_nhp_result_sets(
      allowed_datasets = allowed_datasets()
    ) 
    
    # if a user isn't in the nhp_dev group, then do not display un-viewable/dev results
    if (any(c("nhp_devs") %in% session$groups)) {
      return(rs)
    }
    
    dplyr::filter(
      rs,
      .data[["viewable"]],
      .data[["app_version"]] != "dev"
    )
  })
  
  # static data files ----
  datasets_list <- jsonlite::read_json("supporting_data/datasets.json", simplifyVector = TRUE)
  datasets_list <- purrr::set_names(names(datasets_list), unname(datasets_list))
  
  # logic for improved selectInput logic, this should become a module ----
  # once remaining code in this script is refactored to take reactive values
  # instead of input$ values
  selections <- shiny::reactiveValues()
  
  
  shiny::observe(
    shiny::updateSelectInput(session, 
                             "selected_scheme", 
                             choices = datasets_list[datasets_list %in% nhp_model_runs()$dataset])
  )
  
  shiny::observe(
    selections$scheme <- input$selected_scheme
  )
  
  shiny::observe(
    selections$scheme_scenarios <- nhp_model_runs() |>
      dplyr::filter(dataset == selections$scheme)
  )
  
  shiny::observe(
    shiny::updateSelectInput(session, "scenario_1", choices = selections$scheme_scenarios |> 
                               dplyr::pull(scenario) |> 
                               unique()
    )
  )
  
  shiny::observeEvent(input$scenario_1, {
    runtime_choices <- selections$scheme_scenarios |> 
      dplyr::filter(scenario == input$scenario_1) |> 
      dplyr::pull(create_datetime)
    
    
    shiny::updateSelectInput(session, "scenario_1_runtime", choices =  runtime_choices)
  })
  
  shiny::observe(
    selections$main_scenario <- selections$scheme_scenarios |> 
      dplyr::filter(scenario == input$scenario_1,
                    create_datetime == input$scenario_1_runtime)
  )
  
  shiny::observe({
    
    criteria <- selections$main_scenario |>
      dplyr::select(start_year, end_year, app_version) 
    
    comparable_scenarios <- selections$scheme_scenarios |>
      dplyr::inner_join(criteria) |>
      dplyr::anti_join(selections$main_scenario) |>
      dplyr::pull(scenario) |> 
      unique()
    
    default <- if(input$scenario_2 %in% comparable_scenarios){
      input$scenario_2
    } else {
      character(0)
    }
    
    shiny::updateSelectInput(session, "scenario_2", 
                             choices = comparable_scenarios,
                             selected = default
    )
    
    selections$comparator_scenario <- selections$scheme_scenarios |> 
      dplyr::filter(
        scenario %in% default,
        create_datetime %in% input$scenario_2_runtime
      )
    
  })
  
  shiny::observe({
    criteria <- selections$main_scenario |>
      dplyr::select(start_year, end_year, app_version) 
    
    comparable_runtimes <- selections$scheme_scenarios |>
      dplyr::filter(scenario == input$scenario_2) |> 
      dplyr::inner_join(criteria) |>
      dplyr::anti_join(selections$main_scenario) |>
      dplyr::pull(create_datetime)
    
    default <- if(input$scenario_2_runtime %in% comparable_runtimes){
      input$scenario_2_runtime
    } else {
      character(0)
    }
    
    shiny::updateSelectInput(session, 
                             "scenario_2_runtime", 
                             choices = comparable_runtimes,
                             selected = default)
    
    selections$comparator_scenario <- selections$scheme_scenarios |> 
      dplyr::filter(
        scenario %in% input$scenario_2,
        create_datetime %in% default
      )
    
  })
  
  # Alias management ----
  # scenario_1_display <- shiny::reactive({
  #   if(input$create_new_names && nzchar(input$scenario_1_alias)) {
  #     input$scenario_1_alias
  #   } else {
  #     input$scenario_1
  #   }
  # })
  # 
  # scenario_2_display <- shiny::reactive({
  #   if(input$create_new_names && nzchar(input$scenario_2_alias)) {
  #     input$scenario_2_alias
  #   } else {
  #     input$scenario_2
  #   }
  #   
  # })
  
  # Update alias text inputs with default values from selected scenarios
  # shiny::observeEvent(c(input$scenario_1, input$scenario_2), {
  #   # Only update if checkbox is NOT ticked (to preserve user edits when ticked)
  #   if (!isTRUE(input$create_new_names)) {
  #     shiny::updateTextInput(session, "scenario_1_alias", value = input$scenario_1)
  #     shiny::updateTextInput(session, "scenario_2_alias", value = input$scenario_2)
  #   }
  # }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Enable/disable text inputs based on checkbox
  # shiny::observeEvent(input$create_new_names, {
  #   if (isTRUE(input$create_new_names)) {
  #     shinyjs::enable("scenario_1_alias")
  #     shinyjs::enable("scenario_2_alias")
  #   } else {
  #     shinyjs::disable("scenario_1_alias")
  #     shinyjs::disable("scenario_2_alias")
  #     # Reset to default values when unchecked
  #     shiny::updateTextInput(session, "scenario_1_alias", value = input$scenario_1)
  #     shiny::updateTextInput(session, "scenario_2_alias", value = input$scenario_2)
  #   }
  # }, ignoreInit = TRUE)
  
  # Disable text inputs on startup
  # shinyjs::disable("scenario_1_alias")
  # shinyjs::disable("scenario_2_alias")
  
  # End of alias management ----
  
  # End of selectInput reactive logic ----
  
  
  output$metadata <- DT::renderDT({
    
    possibly_get_metadata <- purrr::possibly(
      get_metadata,
      otherwise = tibble::tibble()
    )
    
    df <- dplyr::bind_rows(
      possibly_get_metadata(nhp_model_runs(), selections$main_scenario),
      possibly_get_metadata(nhp_model_runs(), selections$comparator_scenario)
    )
    
    if (nrow(df) < 2) {
      return(
        DT::datatable(
          tibble::tibble(
            Message = "Fewer than 2 scenarios have been selected. Please ensure you have selected both scenario names and run times."
          ),
          rownames = FALSE,
          options = list(
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            dom = "t"
          )
        )
      )
    }
    
    DT::datatable(
      df #|>
        # dplyr::mutate(
        #   scenario_alias = c(
        #     scenario_1_display(),
        #     scenario_2_display()
        #   )
        # ) |>
        # dplyr::select(scenario_alias, dplyr::everything())
        ,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      )
    )
  })
  
  output$result_text <- shiny::renderText({
    paste("You have selected",
          input$scenario_1,
          paste0(
            "(",
            lubridate::as_datetime(input$scenario_1_runtime),
            ")"),
          paste0("(model version: ", 
                 nhp_model_runs() |> 
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
                 nhp_model_runs() |> 
                   dplyr::filter(scenario == input$scenario_2,
                                 create_datetime == input$scenario_2_runtime) |> 
                   dplyr::pull(app_version),
                 ")"),
          "from the scheme",
          input$selected_scheme)
  })
  
  errors_reactive <- shiny::reactiveVal()
  
  
  output$warning_text <- shiny::renderUI({
    shiny::req(nhp_model_runs(), input$scenario_1, input$scenario_1_runtime, input$scenario_2, input$scenario_2_runtime)
    s1 <- nhp_model_runs() |> dplyr::filter(
      scenario == input$scenario_1, 
      dataset == input$selected_scheme,
      create_datetime == input$scenario_1_runtime)
    s2 <- nhp_model_runs() |> dplyr::filter(
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
    
    if ((nhp_model_runs() |> 
         dplyr::filter(scenario == input$scenario_1,
                       create_datetime == input$scenario_1_runtime) |> 
         dplyr::pull(app_version)) != 
        (nhp_model_runs() |> 
         dplyr::filter(scenario == input$scenario_2,
                       create_datetime == input$scenario_2_runtime) |> 
         dplyr::pull(app_version))){
      errors <- c(errors, "Error: Selected scenarios must have been built on the same version of the model.")
    }
    
    errors_reactive(errors)
    # collapse into a single string, separated by new lines
    shiny::HTML(paste(errors, collapse = "<br>"))
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
                          result_sets = nhp_model_runs(),
                          scenario_selections = shiny::reactive(
                            list(scenario_1 = input$scenario_1,
                                 scenario_1_runtime = input$scenario_1_runtime,
                                 scenario_2 = input$scenario_2,
                                 scenario_2_runtime = input$scenario_2_runtime)
                          ),
                          errors = errors_reactive,
                          trigger = shiny::reactive(input$render_plot),
                          local_data_flag = load_local_data
    )
  
  
  mod_summary_server("summary1",
                     processed = processed)
  mod_los_server("los1",
                 processed = processed)
  mod_waterfall_server("waterfall1",
                       processed = processed)
  mod_activity_avoidance_impact_server("activity_avoidance1",
                                       processed = processed)
  mod_efficiencies_impact_server("efficiencies1",
                                 processed = processed)
  mod_p10_p90_bar_server("p10p90_bar1",
                         processed = processed)
  mod_beeswarm_server("beeswarm1",
                      processed = processed)
  mod_ecdf_server("ecdf1",
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
