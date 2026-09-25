test_rds_path <- \(...) testthat::test_path("test_data", "rds", ...)

results1 <- readRDS(test_rds_path("results1.rds"))
results2 <- readRDS(test_rds_path("results2.rds"))

lookups <- build_app_lookups(
  full_apm_lookup = readRDS(test_rds_path("full_apm_lookup.rds")),
  tpma_lookup = readRDS(test_rds_path("tpma_lookup.rds"))
)
full_ap_lookup <- lookups[["full_ap_lookup"]]
cond_ap_lookup <- lookups[["cond_ap_lookup"]]
atl_lookup <- lookups[["atl_lookup"]]
tpma_lookup <- lookups[["tpma_lookup"]]
# `results$default` has A&E walk-in + ambulance measures, whereas
# `results$step_counts` (change factors) has only A&E "arrivals"
core_mat_tbl <- lookups[["core_mat_tbl"]]
cond_mat_tbl <- lookups[["cond_mat_tbl"]]
