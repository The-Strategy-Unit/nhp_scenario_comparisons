# Regression tests for the `scenario2` / `scenario2_rt` compatibility
# warnings in `app_server()`: when a `main_scenario()` change (i.e. a new
# scenario1/scenario1_rt) knocks a previously-valid comparator selection out
# of the compatible set, `scenario2_warning()` / `scenario2_rt_warning()`
# should flip from `NULL` to a message, without requiring any Azure access
# (`get_results_metadata()` is mocked).
#
# `get_results_metadata()`'s real columns: dataset, scenario, seed,
# model_runs, start_year, end_year, app_version, create_datetime, viewable,
# run_stage, aggregated_results_path, outputs_app_uri (see R/get_data.R).
# `get_comparable_scenarios()` requires >= 2 rows sharing
# (start_year, end_year, app_version), so every fixture below is built in
# pairs sharing those three columns.

synthetic_metadata <- function(
  scenario,
  end_year,
  create_datetime,
  dataset = "synthetic",
  start_year = 2024,
  app_version = "v3.1"
) {
  n <- length(scenario)
  tibble::tibble(
    dataset = dataset,
    scenario = scenario,
    seed = seq_len(n),
    model_runs = 100,
    start_year = start_year,
    end_year = end_year,
    app_version = app_version,
    create_datetime = create_datetime,
    viewable = TRUE,
    run_stage = "complete",
    aggregated_results_path = paste0("path/", scenario, "/", create_datetime),
    outputs_app_uri = "uri"
  )
}

test_that("changing main_scenario invalidates a stale comparator runtime, without invalidating the comparator scenario name", {
  # "Comp" has two runtimes: one matching each main scenario's start/end year
  # group, so the scenario *name* remains comparable throughout, but the
  # originally-selected runtime does not follow the main scenario across.
  metadata <- synthetic_metadata(
    scenario = c("MainA", "Comp", "MainB", "Comp"),
    end_year = c(2030, 2030, 2031, 2031),
    create_datetime = c("t-mainA", "t-compA", "t-mainB", "t-compB")
  )
  testthat::local_mocked_bindings(
    get_results_metadata = function(...) metadata,
    .package = "nhpscenarioanalysis"
  )

  shiny::testServer(app_server, {
    session$setInputs(selected_scheme = "synthetic")
    session$setInputs(scenario1 = "MainA", scenario1_rt = "t-mainA")
    session$setInputs(scenario2 = "Comp", scenario2_rt = "t-compA")

    testthat::expect_null(scenario2_warning())
    testthat::expect_null(scenario2_rt_warning())

    # Switch the main scenario to the other start/end-year group. "Comp" is
    # still a comparable *name* (its other runtime, t-compB, matches), but
    # the runtime the user had actually picked (t-compA) no longer does.
    session$setInputs(scenario1 = "MainB", scenario1_rt = "t-mainB")

    testthat::expect_null(scenario2_warning())
    testthat::expect_true(nzchar(scenario2_rt_warning()))
  })
})

test_that("changing main_scenario invalidates a stale comparator scenario name", {
  # "CompOnly" has a single runtime, comparable only with "MainA"'s group;
  # switching main to "MainB" invalidates the whole scenario name, not just
  # its runtime.
  metadata <- synthetic_metadata(
    scenario = c("MainA", "CompOnly", "MainB", "Filler"),
    end_year = c(2030, 2030, 2031, 2031),
    create_datetime = c("t-mainA", "t-componly", "t-mainB", "t-filler")
  )
  testthat::local_mocked_bindings(
    get_results_metadata = function(...) metadata,
    .package = "nhpscenarioanalysis"
  )

  shiny::testServer(app_server, {
    session$setInputs(selected_scheme = "synthetic")
    session$setInputs(scenario1 = "MainA", scenario1_rt = "t-mainA")
    session$setInputs(scenario2 = "CompOnly", scenario2_rt = "t-componly")

    testthat::expect_null(scenario2_warning())

    session$setInputs(scenario1 = "MainB", scenario1_rt = "t-mainB")

    testthat::expect_true(nzchar(scenario2_warning()))
  })
})
