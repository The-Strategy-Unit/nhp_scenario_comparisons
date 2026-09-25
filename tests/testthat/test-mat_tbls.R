# `core_mat_tbl` and `cond_mat_tbl` drive the `pmap()` calls in the
# `prepare_*()` functions. Each must match the measure / activity_type pairs
# of the results table its `reskit::compile_*()` function reads, otherwise
# `unnest_*_scenarios_tbl()` silently drops the empty results and an activity
# type vanishes from the plot dropdowns (as A&E once did on the waterfall tab).

# Structure -------------------------------------------------------------------

test_that("build_mat_tbl() gives one row per pair, with only pmap arguments", {
  lookup <- tibble::tribble(
    ~activity_type , ~activity_type_label , ~pod          , ~pod_label , ~measure     ,
    "ip"           , "Inpatient"          , "ip_a"        , "A"        , "admissions" ,
    "ip"           , "Inpatient"          , "ip_b"        , "B"        , "admissions" ,
    "ip"           , "Inpatient"          , "ip_a"        , "A"        , "beddays"    ,
    "aae"          , "A&E"                , "aae_type-01" , "Type 1"   , "walk-in"    ,
    "aae"          , "A&E"                , "aae_type-02" , "Type 2"   , "walk-in"
  )
  out <- build_mat_tbl(lookup)

  # pmap() passes every column as a named argument to the compile function,
  # so any extra column would become an unused-argument error
  expect_setequal(names(out), c("measure", "activity_type"))
  expect_identical(
    mat_pairs(out),
    c("aae:walk-in", "ip:admissions", "ip:beddays")
  )
})

test_that("get_condensed_apm_lookup() collapses A&E to a single arrivals row", {
  full <- read_fixture("full_apm_lookup.rds")
  cond <- get_condensed_apm_lookup(full)

  aae <- dplyr::filter(cond, .data[["activity_type"]] == "aae")
  expect_identical(nrow(aae), 1L)
  expect_identical(aae[["measure"]], "arrivals")
  expect_identical(as.character(aae[["activity_type_label"]]), "A&E")

  non_aae <- \(x) dplyr::filter(x, .data[["activity_type"]] != "aae")
  expect_identical(mat_pairs(non_aae(cond)), mat_pairs(non_aae(full)))
})

test_that("build_app_lookups() returns both mat tables", {
  lookups <- fixture_lookups()
  expect_s3_class(lookups[["core_mat_tbl"]], "tbl_df")
  expect_s3_class(lookups[["cond_mat_tbl"]], "tbl_df")
  expect_false(identical(lookups[["core_mat_tbl"]], lookups[["cond_mat_tbl"]]))
})

# Contracts with the results data ----------------------------------------------

test_that("cond_mat_tbl matches the pairs in results$step_counts", {
  cond_mat_tbl <- fixture_lookups()[["cond_mat_tbl"]]
  for (results in fixture_results()) {
    step_pairs <- mat_pairs(results[["step_counts"]])
    # Pairs in the data but not the table vanish from the change-factor plots
    expect_in(step_pairs, mat_pairs(cond_mat_tbl))
    # Pairs in the table but not the data mean the table is mis-specified
    expect_in(mat_pairs(cond_mat_tbl), step_pairs)
  }
})

test_that("every core_mat_tbl pair has data in results$default", {
  lookups <- fixture_lookups()
  full_apm_lookup <- lookups[["full_apm_lookup"]]
  for (results in fixture_results()) {
    default <- dplyr::distinct(
      results[["default"]],
      dplyr::pick(c("pod", "measure"))
    )
    # The lookup is a whitelist: results$default also holds pairs that are
    # never displayed (e.g. ip "procedures"), so match on pod + measure and
    # test one direction only
    covered <- dplyr::semi_join(
      full_apm_lookup,
      default,
      by = c("pod", "measure")
    )
    expect_setequal(mat_pairs(covered), mat_pairs(lookups[["core_mat_tbl"]]))
    # ...but every pod in the data should be known to the lookup
    expect_in(unique(default[["pod"]]), full_apm_lookup[["pod"]])
  }
})

test_that("full_ap_lookup labels every pod in results$step_counts", {
  full_ap_lookup <- fixture_lookups()[["full_ap_lookup"]]
  for (results in fixture_results()) {
    expect_in(
      unique(results[["step_counts"]][["pod"]]),
      full_ap_lookup[["pod"]]
    )
  }
})

# The prepare_*() outputs -----------------------------------------------------

test_that("prepare_waterfall_data() keeps every step_counts pair, labelled", {
  lookups <- fixture_lookups()
  results <- fixture_results()
  # reskit messages when a measure/activity pair has no step count data, which
  # means a pair is about to be dropped
  expect_no_message(
    out <- prepare_waterfall_data(
      results[[1]],
      results[[2]],
      "s1",
      "s2",
      lookups[["cond_mat_tbl"]],
      lookups[["full_ap_lookup"]],
      lookups[["tpma_lookup"]]
    )
  )
  expect_false(anyNA(out[["activity_type_label"]]))
  expect_setequal(mat_pairs(out), mat_pairs(results[[1]][["step_counts"]]))
  expect_setequal(unique(out[["scenario"]]), c("s1", "s2"))
})

test_that("prepare_tpma_impact_data() labels every activity type", {
  lookups <- fixture_lookups()
  results <- fixture_results()
  out <- suppressMessages(
    prepare_tpma_impact_data(
      results[[1]],
      results[[2]],
      "s1",
      "s2",
      lookups[["cond_mat_tbl"]],
      lookups[["full_ap_lookup"]],
      lookups[["tpma_lookup"]]
    )
  )
  # A pair may legitimately have no TPMA impact, so check a subset only
  expect_in(mat_pairs(out), mat_pairs(results[[1]][["step_counts"]]))
  expect_in("aae", out[["activity_type"]])
  expect_false(anyNA(out[["activity_type_label"]]))
})

test_that("prepare_beeswarm_data() keeps every core_mat_tbl pair, labelled", {
  lookups <- fixture_lookups()
  results <- fixture_results()
  out <- prepare_beeswarm_data(
    results[[1]],
    results[[2]],
    "s1",
    "s2",
    lookups[["core_mat_tbl"]],
    lookups[["full_ap_lookup"]],
    lookups[["atl_lookup"]]
  )
  expect_false(anyNA(out[["activity_type_label"]]))
  expect_setequal(mat_pairs(out), mat_pairs(lookups[["core_mat_tbl"]]))
})

# Scoping ---------------------------------------------------------------------

test_that("mod_processing_server() has no undefined variables", {
  # A variable defined only in the global environment (e.g. by dev/testing.R)
  # is still found by package code under load_all(), masking the bug locally
  # while it fails on deployment
  msgs <- utils::capture.output(
    codetools::checkUsage(mod_processing_server, all = TRUE)
  )
  expect_identical(
    grep("no visible (binding|global function)", msgs, value = TRUE),
    character(0)
  )
})
