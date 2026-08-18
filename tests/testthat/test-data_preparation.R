test_that("beeswarm data prep works with reskit demo data", {
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
  expect_shape(beeswarm_data, ncol = 9)
  at <- "activity_type"
  mvbp <- c("model_run", "value", "baseline", "principal")
  xpec_nms <- c(at, "measure", paste0(c(at, "measure"), "_label"), mvbp)
  expect_contains(colnames(beeswarm_data), xpec_nms)
})
