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
    
    req(input$scenario_1)
    
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
    shiny::req(input$scenario_2)
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
  
  # End of selectInput reactive logic ----
  
  
  shiny::observe({
    shiny::req(selections$main_scenario,
               selections$comparator_scenario)
    
    main <- selections$main_scenario
    comparator <- selections$comparator_scenario
    
    if(nrow(main) > 0 &&
       nrow(comparator) > 0 &&
       main$start_year == comparator$start_year &&
       main$end_year == comparator$end_year &&
       main$app_version == comparator$app_version
    ) {
      shinyjs::enable("render_plot")
    } else {
      shinyjs::disable("render_plot")
    }
    
  })
  
  
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
      df,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      )
    )
  })
  
  last_render <- shiny::reactiveVal(NULL)
  
  shiny::observeEvent(input$render_plot, {
    
    shiny::req(nhp_model_runs(), input$scenario_1, input$scenario_1_runtime, input$scenario_2, input$scenario_2_runtime)
    
    
    last_render(list(
      s1 = input$scenario_1,
      s1_time = input$scenario_1_runtime,
      s2 = input$scenario_2,
      s2_time = input$scenario_2_runtime,
      scheme = input$selected_scheme,
      version = nhp_model_runs() |> 
        dplyr::filter(scenario == input$scenario_1,
                      create_datetime == input$scenario_1_runtime) |> 
        dplyr::pull(app_version)
      
    ))
  })
  
  
  output$result_text <- shiny::renderUI({
    
    state <- last_render()
    shiny::req(state)
    
    shiny::tags$span(
      "You have selected ",
      shiny::tags$b(state$s1),
      " (", 
      lubridate::as_datetime(state$s1_time), 
      ") and ",
      shiny::tags$b(state$s2), 
      " (", 
      lubridate::as_datetime(state$s2_time), 
      ") from the scheme ",
      state$scheme,
      " and model version ",
      nhp_model_runs() |>
        dplyr::filter(
          scenario == input$scenario_1,
          create_datetime == input$scenario_1_runtime
        ) |>
        dplyr::pull(app_version)
    )
  })
  
  shiny::observe({
    
    errors <- c()
    
    model_runs <- nhp_model_runs()
    
    if(is.null(model_runs) || nrow(model_runs) == 0){
      errors <- c(errors, 
                  "<b>No Scenarios have met inclusion criteria for your Scheme (v3.1+, viewable = TRUE)</b>"
      )
    }
    
    
    state <- last_render()
    if(!is.null(state)){ # no render yet
      
      # detect if selections have changed since last render
      changed <- state$s1 != input$scenario_1 ||
        state$s1_time != input$scenario_1_runtime ||
        state$s2 != input$scenario_2 ||
        state$s2_time != input$scenario_2_runtime
      
      if(changed) {
        errors <- c(errors, 
                    "<b><p style='color:red;'>Scenario Selections have changed. Press Render Plots to view. </p><b>"
        )
        
      }
    }
    
    if(length(errors) > 0){
      
      output$errors <- shiny::renderUI(shiny::HTML(paste(errors, collapse = "<br>")))
    } else {
      output$errors <- shiny::renderUI(NULL)
    }
    
  })
  
  
  processed <- 
    mod_processing_server("processing1",
                          result_sets = nhp_model_runs(),
                          selections = selections,
                          scenario_selections = shiny::reactive(
                            list(scenario_1 = input$scenario_1,
                                 scenario_1_runtime = input$scenario_1_runtime,
                                 scenario_2 = input$scenario_2,
                                 scenario_2_runtime = input$scenario_2_runtime)
                          ),
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
  
}
