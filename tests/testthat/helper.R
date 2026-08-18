test_rds_path <- \(...) testthat::test_path("test_data", "rds", ...)

results1 <- readRDS(test_rds_path("results1.rds"))
results2 <- readRDS(test_rds_path("results2.rds"))

full_apm_lookup <- readRDS(test_rds_path("full_apm_lookup.rds"))
full_ap_lookup <- dplyr::select(full_apm_lookup, !"measure") |>
  dplyr::distinct()
atl_lookup <- full_apm_lookup |>
  dplyr::distinct(dplyr::pick(tidyselect::starts_with("activity_type")))
core_mat_tbl <- full_apm_lookup |>
  dplyr::distinct(dplyr::pick(c("measure", "activity_type")))
