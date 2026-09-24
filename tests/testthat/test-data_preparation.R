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
  expect_shape(waterfall_data, ncol = 9)
  atm <- c("activity_type", "measure")
  xpec_nms <- c(atm, "change_factor", paste0(atm, "_label"))
  expect_contains(colnames(waterfall_data), xpec_nms)
})


test_that("icf data prep works with reskit demo data", {
  scenario1_name <- "test1"
  scenario2_name <- "test2"

  # results1, results2, lookups and core_mat_tbl are sourced in helper.R
  icf_impact_data <- prepare_icf_impact_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    core_mat_tbl,
    cond_ap_lookup,
    tpma_lookup,
    atl_lookup
  ) |>
    expect_no_error()
  expect_shape(icf_impact_data, ncol = 8)
  expect_gt(nrow(icf_impact_data), 0)
  atm <- c("activity_type", "measure")
  xpec_nms <- c(atm, "change_factor", paste0(atm, "_label"))
  expect_contains(colnames(icf_impact_data), xpec_nms)
})


test_that("p10p90 data prep works with reskit demo data", {
  scenario1_name <- "test1"
  scenario2_name <- "test2"

  # results1, results2 and lookup are sourced in helper.R
  principal_pi_data <- prepare_principal_pi_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    full_atp_lookup
  ) |>
    expect_no_error()
  expect_shape(principal_pi_data, ncol = 10)
  expect_gt(nrow(principal_pi_data), 0)
  patm <- c("pod_label", "activity_type_label", "measure")
  xpec_nms <- c(patm, "baseline", "principal", "change", "change_pct")
  expect_contains(colnames(principal_pi_data), c(xpec_nms, "lower", "upper"))
})


test_that("los data prep works with reskit demo data", {
  scenario1_name <- "test1"
  scenario2_name <- "test2"

  # results1, results2 and lookup are sourced in helper.R
  principal_los_data <- prepare_los_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    cond_ap_lookup
  ) |>
    expect_no_error()
  expect_shape(principal_los_data, ncol = 8)
  expect_gt(nrow(principal_los_data), 0)
  patm <- c("pod_label", "los_group", "measure")
  xpec_nms <- c(patm, "baseline", "principal", "change", "change_pct")
  expect_contains(colnames(principal_los_data), xpec_nms)
})


test_that("summary prep works with reskit demo data", {
  scenario1_name <- "test1"
  scenario2_name <- "test2"

  # results1, results2 and lookup are sourced in helper.R
  summary_data <- prepare_summary_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    cond_ap_lookup
  ) |>
    expect_no_error()
  expect_shape(summary_data, ncol = 7)
  expect_gt(nrow(summary_data), 0)
  spal <- c("scenario", "pod_label", "activity_type_label")
  xpec_nms <- c(spal, "baseline", "principal", "change", "change_pct")
  expect_contains(colnames(summary_data), xpec_nms)
})
