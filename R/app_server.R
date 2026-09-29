app_server <- function(input, output, session) {
  nhp_model_runs <- shiny::reactive({
    allowed_datasets <- get_user_allowed_datasets(session$groups)
    results_metadata_tbl <- get_results_metadata(allowed_datasets)

    # only show non-viewable and dev scenarios to members of nhp_devs group
    if ("nhp_devs" %in% session$groups || is.null(session$groups)) {
      results_metadata_tbl
    } else {
      results_metadata_tbl |>
        dplyr::filter(.data[["viewable"]], .data[["app_version"]] != "dev") |>
        validate_rows("No viewable scenarios are available.")
    }
  })

  all_schemes <- swap_names(yyjsonr::read_json_file(appfile("datasets.json")))
  selections <- shiny::reactiveValues()

  shiny::observeEvent(nhp_model_runs(), {
    runs_tbl <- nhp_model_runs()
    all_schemes <- vctrs::vec_set_intersect(all_schemes, runs_tbl[["dataset"]])
    comparable_scenarios <- runs_tbl |>
      dplyr::mutate(
        n_runs = dplyr::n(),
        .by = c("dataset", "start_year", "end_year", "app_version")
      ) |>
      dplyr::filter(dplyr::if_any("n_runs", \(x) x >= 2)) |>
      dplyr::select(!"n_runs")
    available_schemes <- vctrs::vec_set_intersect(
      all_schemes,
      comparable_scenarios[["dataset"]]
    )
    unavailable_schemes <- vctrs::vec_set_difference(
      all_schemes,
      comparable_scenarios[["dataset"]]
    )
    all_schemes_sorted <- c(available_schemes, unavailable_schemes)
    scheme_unavailable <- !(all_schemes_sorted %in% available_schemes)
    selected_scheme <- shiny::isolate(input$selected_scheme)
    keep <- shiny::isTruthy(selected_scheme) &&
      selected_scheme %in% available_schemes
    shinyWidgets::updatePickerInput(
      session,
      "selected_scheme",
      choices = all_schemes_sorted,
      selected = if (keep) selected_scheme else character(0),
      choicesOpt = list(
        disabled = scheme_unavailable,
        style = ifelse(
          scheme_unavailable,
          "color: rgba(119, 119, 119, 0.5);",
          ""
        )
      )
    )
  })

  # `scheme` and the values below it are pure functions of the inputs, so
  # they're expressed as `reactive()`s rather than `observe()`s writing into
  # `selections`. `main_scenario` and `comp_scenario` are still mirrored into
  # `selections` (see the two bridging `observe()`s below) because
  # `mod_processing_server` reads them as reactiveValues fields.
  scheme <- shiny::reactive(input$selected_scheme)

  scheme_runs_tbl <- shiny::reactive({
    shiny::req(nhp_model_runs(), scheme())
    nhp_model_runs() |>
      dplyr::filter(.data[["dataset"]] %in% scheme())
  })

  scheme_scenarios <- shiny::reactive({
    get_comparable_scenarios(scheme_runs_tbl(), scheme()) |>
      validate_rows("No comparable scenarios are available for this scheme.")
  })

  main_scenario <- shiny::reactive({
    shiny::req(scheme_scenarios(), input$scenario1, input$scenario1_rt)
    scheme_scenarios() |>
      dplyr::filter(
        .data[["scenario"]] %in% input$scenario1,
        .data[["create_datetime"]] %in% input$scenario1_rt
      ) |>
      validate_rows("The main scenario was not available.")
  })

  comp_scenario <- shiny::reactive({
    shiny::req(scheme_scenarios(), input$scenario2, input$scenario2_rt)
    scheme_scenarios() |>
      dplyr::filter(
        .data[["scenario"]] %in% input$scenario2,
        .data[["create_datetime"]] %in% input$scenario2_rt
      ) |>
      validate_rows("The comparison scenario was not found.")
  })

  shiny::observe(selections$main_scenario <- main_scenario())
  shiny::observe(selections$comp_scenario <- comp_scenario())

  shiny::observe({
    shiny::req(scheme_runs_tbl(), scheme_scenarios())
    comparable_scenarios <- scheme_scenarios()
    other_scenarios <- dplyr::setdiff(scheme_runs_tbl(), comparable_scenarios)
    available_scenarios <- pull_unique(comparable_scenarios, "scenario")
    unavailable_scenarios <- pull_unique(other_scenarios, "scenario")
    all_scenarios_sorted <- c(available_scenarios, unavailable_scenarios)
    scenario_unavailable <- !(all_scenarios_sorted %in% available_scenarios)
    shinyWidgets::updatePickerInput(
      session,
      "scenario1",
      choices = all_scenarios_sorted,
      selected = resolve_selection(
        shiny::isolate(input$scenario1),
        available_scenarios,
        auto_max = 2
      ),
      choicesOpt = list(
        disabled = scenario_unavailable,
        style = ifelse(
          scenario_unavailable,
          "color: rgba(119, 119, 119, 0.5);",
          ""
        )
      )
    )
  })

  shiny::observe({
    shiny::req(scheme_scenarios(), input$scenario1)
    available_runtimes <- scheme_scenarios() |>
      dplyr::filter(.data[["scenario"]] %in% input$scenario1) |>
      dplyr::pull("create_datetime")
    shinyWidgets::updatePickerInput(
      session,
      "scenario1_rt",
      choices = available_runtimes,
      selected = resolve_selection(
        shiny::isolate(input$scenario1_rt),
        available_runtimes
      )
    )
  })

  # Set (or cleared) by the `scenario2`/`scenario2_rt` observers whenever a
  # previously-valid selection is knocked out by a `main_scenario()` change;
  # read by `warning_text`.
  scenario2_warning <- shiny::reactiveVal(NULL)
  scenario2_rt_warning <- shiny::reactiveVal(NULL)

  shiny::observe({
    shiny::req(scheme_scenarios(), main_scenario())
    other_scenarios <- scheme_scenarios() |>
      dplyr::setdiff(main_scenario())

    comparable_scenarios <- other_scenarios |>
      filter_compatible_scenarios(main_scenario()) |>
      pull_unique("scenario")
    all_scenarios <- pull_unique(other_scenarios, "scenario")
    unavailable_scenarios <- setdiff(all_scenarios, comparable_scenarios)
    all_scenarios <- c(comparable_scenarios, unavailable_scenarios)
    scenario_unavailable <- !(all_scenarios %in% comparable_scenarios)

    previous_scenario <- shiny::isolate(input$scenario2)
    scenario2_warning(
      if (
        shiny::isTruthy(previous_scenario) &&
          previous_scenario %in% unavailable_scenarios
      ) {
        paste(
          "The previously selected comparator scenario is no longer",
          "compatible with the main scenario and has been deselected.",
          "Please choose a different scenario."
        )
      } else {
        NULL
      }
    )

    shinyWidgets::updatePickerInput(
      session,
      "scenario2",
      choices = all_scenarios,
      selected = resolve_selection(previous_scenario, comparable_scenarios),
      choicesOpt = list(
        disabled = scenario_unavailable,
        style = ifelse(
          scenario_unavailable,
          "color: rgba(119, 119, 119, 0.5);",
          ""
        )
      )
    )
  })

  shiny::observe({
    shiny::req(scheme_scenarios())
    shiny::req(main_scenario())
    shiny::req(input$scenario2)

    other_runtimes <- scheme_scenarios() |>
      dplyr::setdiff(main_scenario()) |>
      dplyr::filter(.data[["scenario"]] %in% input$scenario2)

    # A scenario *name* can be compatible overall (some of its runtimes
    # match `main_scenario()`) while other runtimes of that same name don't
    # — e.g. an older rerun with a different `end_year`. Grey those out here
    # rather than relying solely on the render-button check downstream.
    comparable_runtimes <- other_runtimes |>
      filter_compatible_scenarios(main_scenario()) |>
      dplyr::pull("create_datetime")
    all_runtimes <- dplyr::pull(other_runtimes, "create_datetime")
    unavailable_runtimes <- setdiff(all_runtimes, comparable_runtimes)
    all_runtimes <- c(comparable_runtimes, unavailable_runtimes)
    runtime_unavailable <- !(all_runtimes %in% comparable_runtimes)

    previous_rt <- shiny::isolate(input$scenario2_rt)
    scenario2_rt_warning(
      if (
        shiny::isTruthy(previous_rt) && previous_rt %in% unavailable_runtimes
      ) {
        paste(
          "The previously selected comparator run time is no longer",
          "compatible with the main scenario and has been deselected.",
          "Please choose a different run time."
        )
      } else {
        NULL
      }
    )

    shinyWidgets::updatePickerInput(
      session,
      "scenario2_rt",
      choices = all_runtimes,
      selected = resolve_selection(previous_rt, comparable_runtimes),
      choicesOpt = list(
        disabled = runtime_unavailable,
        style = ifelse(
          runtime_unavailable,
          "color: rgba(119, 119, 119, 0.5);",
          ""
        )
      )
    )
  })

  shiny::observe({
    shiny::req(selections$main_scenario, selections$comp_scenario)

    check_compatible <- filter_compatible_scenarios(
      selections$main_scenario,
      selections$comp_scenario,
      c("dataset", "start_year", "end_year", "app_version")
    )

    if (nrow(check_compatible) == 1) {
      shinyjs::enable("render_plot")
    } else {
      shinyjs::disable("render_plot")
    }
  })

  # The inputs are checked as well as the tables because the observers above
  # `req()` their inputs, so clearing a picker leaves the old table in place.
  scenarios_selected <- shiny::reactive({
    shiny::isTruthy(input$scenario1) &&
      shiny::isTruthy(input$scenario1_rt) &&
      shiny::isTruthy(input$scenario2) &&
      shiny::isTruthy(input$scenario2_rt) &&
      shiny::isTruthy(selections$main_scenario) &&
      shiny::isTruthy(selections$comp_scenario)
  })

  output$metadata <- DT::renderDT({
    if (!scenarios_selected()) {
      hint_msg <- paste0(
        "Select both scenarios and their run times in the sidebar to see ",
        "their metadata here."
      )
      return(create_dt(tibble::tibble(Message = hint_msg)))
    }

    df <- list(selections$main_scenario, selections$comp_scenario) |>
      purrr::map(add_outputs_app_link) |>
      purrr::list_rbind()
    shiny::validate(shiny::need(
      nrow(df) == 2,
      "Metadata could not be found for both selected scenarios."
    ))
    create_dt(df)
  })

  last_render <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$render_plot, {
    shiny::req(
      input$selected_scheme,
      selections$main_scenario,
      input$scenario1,
      input$scenario1_rt,
      input$scenario2,
      input$scenario2_rt
    )
    app_version <- pull_unique(selections$main_scenario, "app_version")

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

  warning_text <- shiny::reactive({
    shiny::req(nhp_model_runs(), scheme())
    text <- NULL
    if (shiny::isTruthy(scheme())) {
      comparable_scenarios <- get_comparable_scenarios(
        scheme_runs_tbl(),
        scheme()
      )
      if (nrow(comparable_scenarios) == 0) {
        txt <- "No comparable scenarios exist for the selected Scheme."
        text <- bold_red(txt)
      }
    }

    if (shiny::isTruthy(scenario2_warning())) {
      text <- c(text, bold_red(scenario2_warning()))
    }
    if (shiny::isTruthy(scenario2_rt_warning())) {
      text <- c(text, bold_red(scenario2_rt_warning()))
    }

    state <- last_render()
    if (!is.null(state)) {
      # `!=` would silently drop a term if a picker was cleared to
      # `character(0)` (`resolve_selection()`'s empty-selection value), since
      # `any()` ignores zero-length results. `!identical()` never has that
      # gap: it's always length-1 and never recycles.
      selections_changed <- !identical(state$s1, input$scenario1) ||
        !identical(state$s1_rt, input$scenario1_rt) ||
        !identical(state$s2, input$scenario2) ||
        !identical(state$s2_rt, input$scenario2_rt)
      if (selections_changed) {
        txt <- "Scenario Selections have changed. Press Render Plots to view."
        text <- c(text, bold_red(txt))
      }
    }

    text
  })

  output$warning_text <- shiny::renderUI({
    text <- warning_text()
    if (length(text) > 0) {
      shiny::HTML(paste0(text, collapse = "<br />"))
    } else {
      NULL
    }
  })

  use_local <- Sys.getenv("NHPSCENARIOCOMP_USE_LOCAL_DATA")
  processed_data <- mod_processing_server(
    id = "processing",
    selections = selections,
    trigger = shiny::reactive(input$render_plot),
    use_local_data = ifelse(nzchar(use_local), as.logical(use_local), FALSE)
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
