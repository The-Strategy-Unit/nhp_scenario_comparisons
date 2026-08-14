app_server <- function(input, output, session) {
  nhp_model_runs <- shiny::reactive({
    allowed_datasets <- get_user_allowed_datasets(session$groups)
    results_metadata_tbl <- get_results_metadata(allowed_datasets)

    # only show non-viewable scenarios to members of nhp_devs group
    if ("nhp_devs" %in% session$groups || is.null(session$groups)) {
      results_metadata_tbl
    } else {
      results_metadata_tbl |>
        dplyr::filter(.data[["viewable"]], .data[["app_version"]] != "dev") |>
        require_rows()
    }
  })

  all_schemes <- swap_names(yyjsonr::read_json_file("inst/ref/datasets.json"))
  selections <- shiny::reactiveValues()

  shiny::observe({
    available_schemes <- vctrs::vec_set_intersect(
      all_schemes,
      nhp_model_runs()[["dataset"]]
    )
    disabled_schemes <- vctrs::vec_set_difference(
      all_schemes,
      available_schemes
    )

    selected_scheme <- input$selected_scheme

    # Only keep the current selection if it is available (i.e. not disabled)
    if (
      shiny::isTruthy(selected_scheme) && selected_scheme %in% available_schemes
    ) {
      shinyWidgets::updatePickerInput(
        session,
        "selected_scheme",
        selected = selected_scheme,
        choices = available_schemes
        # choicesOpt = list(
        #   disabled = disabled_schemes,
        #   style = ifelse(
        #     disabled_schemes,
        #     "color: rgba(119, 119, 119, 0.5);",
        #     ""
        #   )
        # )
      )
    } else {
      shinyWidgets::updatePickerInput(
        session,
        "selected_scheme",
        choices = available_schemes
        # choicesOpt = list(
        #   disabled = disabled_schemes,
        #   style = ifelse(
        #     disabled_schemes,
        #     "color: rgba(119, 119, 119, 0.5);",
        #     ""
        #   )
        # )
      )
    }
  })

  shiny::observe({
    selections$scheme <- input$selected_scheme
  })

  shiny::observe({
    selections$scheme_scenarios <- get_comparable_scenarios(
      nhp_model_runs(),
      selections$scheme
    )
  })

  shiny::observe({
    # Protect against NULL / empty scheme_scenarios
    if (
      is.null(selections$scheme_scenarios) ||
        nrow(selections$scheme_scenarios) == 0
    ) {
      shinyWidgets::updatePickerInput(
        session,
        "scenario1",
        choices = character(0)
      )
      return()
    }

    selected_scenario <- input$scenario1
    choices <- pull_unique(selections$scheme_scenarios, "scenario")
    if (shiny::isTruthy(selected_scenario) && selected_scenario %in% choices) {
      shinyWidgets::updatePickerInput(
        session,
        "scenario1",
        choices = choices,
        selected = selected_scenario
      )
    } else {
      shinyWidgets::updatePickerInput(
        session,
        "scenario1",
        choices = choices
      )
    }
  })

  shiny::observe({
    if (
      all(
        shiny::isTruthy(input$scenario1),
        !is.null(selections$scheme_scenarios),
        input$scenario1 %in% selections$scheme_scenarios$scenario
      )
    ) {
      runtime_choices <- selections$scheme_scenarios |>
        dplyr::filter(.data[["scenario"]] == input$scenario1) |>
        dplyr::pull("create_datetime")

      shinyWidgets::updatePickerInput(
        session,
        "scenario1_rt",
        choices = runtime_choices
      )
    } else {
      shinyWidgets::updatePickerInput(
        session,
        "scenario1_rt",
        choices = character(0)
      )
      return()
    }
  })

  shiny::observe({
    if (is.null(selections$scheme_scenarios)) {
      selections$main_scenario <- NULL
      return()
    }
    # %in% safely handles NULL / character(0) → returns 0-row data.frame
    selections$main_scenario <- selections$scheme_scenarios |>
      dplyr::filter(
        .data[["scenario"]] %in% input$scenario1,
        .data[["create_datetime"]] %in% input$scenario1_rt
      )
  })

  shiny::observe({
    shiny::req(
      input$scenario1,
      selections$scheme_scenarios,
      selections$main_scenario
    )

    if (nrow(selections$main_scenario) == 0) {
      shinyWidgets::updatePickerInput(
        session,
        "scenario2",
        choices = character(0)
      )
      return()
    }

    # all_scenarios <- selections$scheme_scenarios |>
    #   pull_unique("scenario")

    criteria_cols <- c("start_year", "end_year", "app_version")
    criteria_tbl <- selections$main_scenario |>
      dplyr::select(tidyselect::all_of(criteria_cols))

    comparable_scenarios <- selections$scheme_scenarios |>
      dplyr::setdiff(selections$main_scenario) |>
      dplyr::semi_join(criteria_tbl, criteria_cols) |>
      pull_unique("scenario")

    # disabled_scenarios <- setdiff(all_scenarios, comparable_scenarios)

    shinyWidgets::updatePickerInput(
      session,
      "scenario2",
      choices = comparable_scenarios
      # choicesOpt = list(
      #   disabled = disabled_scenarios,
      #   style = ifelse(
      #     disabled_scenarios,
      #     "color: rgba(119, 119, 119, 0.5);",
      #     ""
      #   )
      # )
    )
  })

  shiny::observe({
    shiny::req(
      input$scenario2,
      selections$main_scenario,
      selections$scheme_scenarios
    )

    comparable_runtimes <- selections$scheme_scenarios |>
      dplyr::filter(.data[["scenario"]] %in% input$scenario2) |>
      dplyr::pull("create_datetime")

    shinyWidgets::updatePickerInput(
      session,
      "scenario2_rt",
      choices = comparable_runtimes
    )

    selections$comparator_scenario <- selections$scheme_scenarios |>
      dplyr::filter(
        .data[["scenario"]] %in% input$scenario2,
        .data[["create_datetime"]] %in% input$scenario2_rt
      )
  })

  # End of selectInput reactive logic ----

  shiny::observe({
    shiny::req(selections$main_scenario, selections$comparator_scenario)

    main <- selections$main_scenario
    comparator <- selections$comparator_scenario

    if (
      all(
        nrow(main) > 0,
        nrow(comparator) > 0,
        main$start_year == comparator$start_year,
        main$end_year == comparator$end_year,
        main$app_version == comparator$app_version
      )
    ) {
      shinyjs::enable("render_plot")
    } else {
      shinyjs::disable("render_plot")
    }
  })

  output$metadata <- DT::renderDT({
    df <- list(selections$main_scenario, selections$comparator_scenario) |>
      purrr::map(possibly_add_outputs_app_link) |>
      purrr::list_rbind()
    error_msg <- paste0(
      "Fewer than 2 scenarios have been selected. ",
      "Please ensure you have selected both scenario names and run times."
    )
    if (nrow(df) < 2) {
      create_dt(tibble::tibble(Message = error_msg))
    } else {
      create_dt(df)
    }
  })

  last_render <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$render_plot, {
    shiny::req(
      nhp_model_runs(),
      input$scenario1,
      input$scenario1_rt,
      input$scenario2,
      input$scenario2_rt
    )
    app_version <- nhp_model_runs() |>
      dplyr::filter(
        .data[["scenario"]] == input$scenario1,
        .data[["create_datetime"]] == input$scenario1_rt
      ) |>
      pull_unique("app_version")

    last_render(list(
      scheme = input$selected_scheme,
      s1 = input$scenario1,
      s1_rt = input$scenario1_rt,
      s2 = input$scenario2,
      s2_rt = input$scenario2_rt,
      version = app_version
    ))
  })

  output$result_text <- shiny::renderUI({
    state <- last_render()
    shiny::req(state)

    text <- glue::glue(
      "You have selected {shiny::tags$strong(state$s1)} ({state$s1_rt}) and ",
      "{shiny::tags$strong(state$s2)} ({state$s2_rt}) from ",
      "{shiny::tags$strong(state$scheme)} (model version ",
      "{shiny::tags$strong(state$version)})"
    )
    shiny::tags$span(shiny::HTML(text))
  })

  shiny::observe({
    warning_text <- c()
    model_runs <- nhp_model_runs()

    # No model runs at all
    if (is.null(model_runs) || nrow(model_runs) == 0) {
      warning_text <- c(
        warning_text,
        bold_red(
          paste0(
            "No Scenarios have met inclusion criteria for your Scheme ",
            "(v3.1+, viewable = TRUE)",
          )
        )
      )
    } else {
      comparable <- get_comparable_scenarios(model_runs, selections$scheme)

      if (nrow(comparable) == 0) {
        txt <- "No comparable scenarios exist for the selected Scheme."
        warning_text <- c(warning_text, bold_red(txt))
      }
    }

    state <- last_render()
    if (!is.null(state)) {
      # detect if selections have changed since last render
      if (
        any(
          state$s1 != input$scenario1,
          state$s1_rt != input$scenario1_rt,
          state$s2 != input$scenario2,
          state$s2_rt != input$scenario2_rt
        )
      ) {
        txt <- "Scenario Selections have changed. Press Render Plots to view."
        warning_text <- c(warning_text, bold_red(txt))
      }
    }

    if (length(warning_text) > 0) {
      output$warning_text <- shiny::renderUI({
        shiny::HTML(paste0(warning_text, collapse = "<br />"))
      })
    } else {
      output$warning_text <- shiny::renderUI(NULL)
    }
  })

  processed_data <- mod_processing_server(
    id = "processing",
    results_metadata_tbl = nhp_model_runs(),
    selections = selections,
    scenario_selections = shiny::reactive(
      list(
        scenario1 = input$scenario1,
        scenario1_rt = input$scenario1_rt,
        scenario2 = input$scenario2,
        scenario2_rt = input$scenario2_rt
      )
    ),
    trigger = shiny::reactive(input$render_plot),
    local_data_flag = FALSE
  )

  mod_summary_bar_server("summary", processed_data)
  mod_los_bar_server("los", processed_data)
  mod_waterfall_server("waterfall", processed_data)
  mod_activity_avoidance_impact_server("activity_avoidance", processed_data)
  mod_efficiencies_impact_server("efficiencies", processed_data)
  mod_p10p90_bar_server("p10p90_bar", processed_data)
  mod_beeswarm_server("beeswarm", processed_data)
  mod_ecdf_server("ecdf", processed_data)
}
