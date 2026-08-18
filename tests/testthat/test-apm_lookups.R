test_that("lookups have expected columns", {
  # full_apm_lookup is sourced in helper.R
  at <- "activity_type"
  core_ap_names <- c(at, "pod")
  core_ap_names_plus <- c(core_ap_names, paste0(core_ap_names, "_label"))
  full_expected_names <- c(core_ap_names_plus, "measure")
  expect_setequal(colnames(full_apm_lookup), full_expected_names)
  expect_shape(full_apm_lookup, dim = c(27, 5))

  cond_apm_lookup <- get_condensed_apm_lookup(full_apm_lookup)
  expect_setequal(colnames(cond_apm_lookup), full_expected_names)
  expect_shape(cond_apm_lookup, dim = c(18, 5))

  full_ap_lookup <- dplyr::select(full_apm_lookup, !"measure") |>
    dplyr::distinct()
  expect_setequal(colnames(full_ap_lookup), core_ap_names_plus)
  expect_shape(full_ap_lookup, dim = c(14, 4))

  cond_ap_lookup <- dplyr::select(cond_apm_lookup, !"measure") |>
    dplyr::distinct()
  expect_setequal(colnames(cond_ap_lookup), core_ap_names_plus)
  expect_shape(cond_ap_lookup, dim = c(10, 4))

  # matches reskit's get_detailed_pods()
  full_atp_lookup <- dplyr::select(full_ap_lookup, !"activity_type")
  expect_shape(full_atp_lookup, dim = c(14, 3))

  atl_lookup <- full_apm_lookup |>
    dplyr::distinct(dplyr::pick(tidyselect::starts_with(at)))
  expect_setequal(colnames(atl_lookup), paste0(at, c("", "_label")))
  expect_shape(atl_lookup, dim = c(3, 2))

  core_mat_tbl <- full_apm_lookup |>
    dplyr::distinct(dplyr::pick(c("measure", "activity_type")))
  expect_shape(core_mat_tbl, dim = c(6, 2))
})
