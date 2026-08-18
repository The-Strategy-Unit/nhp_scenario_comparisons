create_results_list <- function(seed) {
  default_tbl <- reskit:::create_demo_default_tbl(seed)
  tretspef_losgroup_tbl <- reskit:::create_demo_tretspef_losgroup_tbl(seed)
  stepcounts_tbl <- reskit:::create_demo_stepcounts_tbl(seed)

  tbl_names <- c("default", "tretspef+los_group", "step_counts")
  list(default_tbl, tretspef_losgroup_tbl, stepcounts_tbl) |>
    rlang::set_names(tbl_names)
}

saveRDS(create_results_list(3456), test_rds_path("results1.rds"))
saveRDS(create_results_list(5678), test_rds_path("results2.rds"))
saveRDS(get_full_apm_lookup(), test_rds_path("full_apm_lookup.rds"))
