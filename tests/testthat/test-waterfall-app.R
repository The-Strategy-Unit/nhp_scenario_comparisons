# Browser-level check that A&E reaches the waterfall dropdown. The full app
# needs Azure for its scenario metadata, so this harness mounts the real
# `mod_processing_server()` and `mod_waterfall` module on the committed
# fixtures instead. Going through `mod_processing_server()` matters: that is
# where the wrong mat table was once chosen, silently dropping A&E.

# Returns a function for `AppDriver$new()`, which runs it in a background R
# process. Its environment holds only `fixture_dir`, so that the test
# environment isn't serialised along with it.
waterfall_harness_app <- function(fixture_dir) {
  app_fn <- function() {
    # shinytest2 turns this into `pkgload::load_all()` in the background,
    # which attaches only exports, so internals are reached via `pkg`
    library(nhpscenarioanalysis)
    pkg <- asNamespace("nhpscenarioanalysis")

    # Seed the process-level lookup cache so nothing is fetched from GitHub
    lookups <- pkg$build_app_lookups(
      full_apm_lookup = readr::read_rds(
        file.path(fixture_dir, "rds", "full_apm_lookup.rds")
      ),
      tpma_lookup = readr::read_rds(
        file.path(fixture_dir, "rds", "tpma_lookup.rds")
      )
    )
    assign("lookups", lookups, envir = pkg$the)

    # With `use_local_data = TRUE`, mod_processing_server() reads rds/*.rds
    # relative to the working directory, matching files by scenario name
    setwd(fixture_dir)

    ui <- bslib::page_fluid(
      shiny::actionButton("render_plot", "Render Plots"),
      pkg$mod_waterfall_ui("waterfall")
    )
    server <- function(input, output, session) {
      scenario <- \(name) {
        tibble::tibble(scenario = name, aggregated_results_path = "unused")
      }
      selections <- shiny::reactiveValues(
        main_scenario = scenario("results1"),
        comp_scenario = scenario("results2")
      )
      processed_data <- pkg$mod_processing_server(
        "processing",
        selections = selections,
        trigger = shiny::reactive(input$render_plot),
        use_local_data = TRUE
      )
      pkg$mod_waterfall_server("waterfall", processed_data)
    }
    shiny::shinyApp(ui, server)
  }
  environment(app_fn) <- list2env(
    list(fixture_dir = fixture_dir),
    parent = globalenv()
  )
  app_fn
}

# selectize keeps only the selected <option> in the DOM, so read its options
selectize_values <- function(app, id) {
  js <- sprintf(
    "Object.keys(document.getElementById('%s').selectize.options)",
    id
  )
  unlist(app$get_js(js))
}

test_that("A&E appears in the waterfall activity type dropdown", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not available")
  fixture_dir <- normalizePath(test_path("test_data"))
  skip_if_not(
    file.exists(file.path(fixture_dir, "rds", "results1.rds")),
    "Fixtures not available"
  )

  app <- shinytest2::AppDriver$new(
    waterfall_harness_app(fixture_dir),
    name = "waterfall-ae",
    load_timeout = 60 * 1000,
    timeout = 30 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("render_plot")
  app$wait_for_idle(timeout = 60 * 1000)

  expect_contains(
    selectize_values(app, "waterfall-filter1"),
    c("Inpatient", "Outpatient", "A&E")
  )

  # Choosing A&E should offer its step_counts measure, and plot without error
  app$set_inputs(`waterfall-filter1` = "A&E")
  app$wait_for_idle()
  expect_identical(selectize_values(app, "waterfall-filter2"), "Arrivals")
  expect_identical(app$get_value(input = "waterfall-filter2"), "Arrivals")
  plot <- app$get_value(output = "waterfall-plot")
  expect_false(is.null(plot[["src"]]))
})
