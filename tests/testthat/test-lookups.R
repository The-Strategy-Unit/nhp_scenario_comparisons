test_that("lookups have expected columns", {
  at <- "activity_type"
  # full_apm_lookup is sourced in helper.R
  core_ap_names <- c("activity_type", "pod")
  core_ap_names_plus <- c("pod", paste0(core_ap_names, "_label"))

  expect_setequal(colnames(full_ap_lookup), core_ap_names_plus)
  expect_shape(full_ap_lookup, dim = c(14, 3))

  expect_setequal(colnames(cond_ap_lookup), core_ap_names_plus)
  expect_shape(cond_ap_lookup, dim = c(10, 3))

  expect_setequal(colnames(atl_lookup), paste0(at, c("", "_label")))
  expect_shape(atl_lookup, dim = c(3, 2))

  expect_shape(core_mat_tbl, dim = c(6, 2))
  expect_shape(cond_mat_tbl, dim = c(5, 2))
})


test_that("preloaded lookups are as expected", {
  expect_snapshot(full_ap_lookup)
  expect_snapshot(cond_ap_lookup)
  expect_snapshot(core_mat_tbl)
  expect_snapshot(cond_mat_tbl)
  expect_snapshot(atl_lookup)
  expect_snapshot(tpma_lookup)
})
