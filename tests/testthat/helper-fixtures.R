# The fixtures are git-ignored (see `rds/` in .gitignore), so tests that need
# them skip rather than fail on a fresh clone or in CI.
read_fixture <- function(file) {
  path <- testthat::test_path("test_data", "rds", file)
  testthat::skip_if_not(file.exists(path), paste("Fixture not available:", file))
  readr::read_rds(path)
}

fixture_lookups <- function() {
  build_app_lookups(
    full_apm_lookup = read_fixture("full_apm_lookup.rds"),
    tpma_lookup = read_fixture("tpma_lookup.rds")
  )
}

fixture_results <- function() {
  list(read_fixture("results1.rds"), read_fixture("results2.rds"))
}

# measure / activity_type pairs as sorted "activity_type:measure" strings, so
# that set comparisons print readable differences on failure
mat_pairs <- function(df) {
  sort(unique(paste(df[["activity_type"]], df[["measure"]], sep = ":")))
}
