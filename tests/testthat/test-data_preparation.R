test_that("beeswarm data prep works for v3.1", {
  skip_on_ci()

  rds_root <- "rds/v3.1/RXN"
  scenario1_name <- "20240114NDG1V1"
  scenario2_name <- "20241212NDG2V1"
  list_dirs <- purrr::partial(dir, full.names = TRUE, recursive = TRUE)

  results1_file <- list_dirs(file.path(rds_root, scenario1_name))
  results2_file <- list_dirs(file.path(rds_root, scenario2_name))

  results1 <- readr::read_rds(results1_file)
  results2 <- readr::read_rds(results2_file)

  full_apm_lookup <- get_full_apm_lookup()
  full_ap_lookup <- dplyr::select(full_apm_lookup, !"measure") |>
    dplyr::distinct()
  atl_lookup <- full_apm_lookup |>
    dplyr::distinct(dplyr::pick(tidyselect::starts_with("activity_type")))

  core_mat_tbl <- full_apm_lookup |>
    dplyr::distinct(dplyr::pick(c("measure", "activity_type")))

  beeswarm_data <- prepare_beeswarm_data(
    results1,
    results2,
    scenario1_name,
    scenario2_name,
    core_mat_tbl,
    full_ap_lookup,
    atl_lookup
  )
})
