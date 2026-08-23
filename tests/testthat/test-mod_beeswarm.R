test_that("filtering works", {
  scenario1_name <- "test1"
  scenario2_name <- "test2"

  # results1, results2, lookups and core_mat_tbl are sourced in helper.R
  beeswarm_data <- prepare_beeswarm_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    core_mat_tbl,
    full_ap_lookup,
    atl_lookup
  ) |>
    expect_no_error()

  expect_contains(colnames(beeswarm_data), "activity_type_label")
  expect_contains(colnames(beeswarm_data), "measure_label")
  atl <- sample(pull_unique(beeswarm_data, "activity_type_label"), 1) |>
    withr::with_seed(seed = 9753)
  input <- list(filter1 = atl)

  filter2_choices <- beeswarm_data |>
    dplyr::filter(.data[["activity_type_label"]] == input$filter1) |>
    pull_unique("measure_label")
  msl <- withr::with_seed(9753, sample(filter2_choices, 1))
  input <- list(filter1 = atl, filter2 = msl)

  filtered_data <- beeswarm_data |>
    dplyr::filter(
      dplyr::if_any("activity_type_label", \(x) x == {{ atl }}),
      dplyr::if_any("measure_label", \(x) x == {{ msl }})
    )
  expect_gt(nrow(filtered_data), 0)

  chart <- create_beeswarm_chart(beeswarm_data, input$filter1, input$filter2) |>
    expect_no_error()
  expect_s3_class(chart, "ggplot")
})
