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


test_that("principal pi data prep works with reskit demo data", {
  scenario1_name <- "test1"
  scenario2_name <- "test2"

  # results1, results2, lookups and core_mat_tbl are sourced in helper.R
  principal_pi_data <- prepare_principal_pi_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    full_atp_lookup
  ) |>
    expect_no_error()
  expect_shape(principal_pi_data, ncol = 10)
  at <- "activity_type"
  lu <- c("lower", "upper")
  lubp <- c(lu, "baseline", "principal")
  xpec_nms <- c("measure", paste0(c(at, "pod"), "_label"), lubp)
  expect_contains(colnames(principal_pi_data), xpec_nms)
})


test_that("waterfall data prep works with reskit demo data", {
  scenario1_name <- "test1"
  scenario2_name <- "test2"

  # results1, results2, lookups and core_mat_tbl are sourced in helper.R
  waterfall_data <- prepare_waterfall_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    core_mat_tbl,
    full_ap_lookup,
    tpma_lookup,
    atl_lookup
  ) |>
    expect_no_error()
  expect_shape(waterfall_data, ncol = 10)
  atm <- c("activity_type", "measure")
  xpec_nms <- c(atm, "change_factor", paste0(atm, "_label"))
  expect_contains(colnames(waterfall_data), xpec_nms)
})
