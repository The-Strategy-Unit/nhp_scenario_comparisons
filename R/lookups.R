# Process-level cache for the lookup tables. Each is derived from a file
# fetched over the network - the NHP Outputs `golem-config.yml` and reskit's
# TPMA lookup - and is identical for every user, so fetch once per R process
# rather than once per session or once per render.
the <- new.env(parent = emptyenv())

#' Every lookup table the data preparation functions need
#'
#' Built on first use and cached for the lifetime of the R process, so the
#' first caller pays for the two GitHub requests and every later session
#' reuses the result. Deliberately lazy rather than fetched at app start: a
#' session that never renders a plot never triggers the fetch, and a network
#' failure cannot stop the app from loading. Use `reset_app_lookups()` to
#' force a refetch.
#'
#' @returns A named list of lookup tables, plus `core_mat_tbl` and
#'   `cond_mat_tbl`.
#' @keywords internal
#' @noRd
get_app_lookups <- function() {
  if (is.null(the[["lookups"]])) {
    the[["lookups"]] <- build_app_lookups()
  }
  the[["lookups"]]
}

#' Clear the cached lookup tables so that the next call refetches them
#' @keywords internal
#' @noRd
reset_app_lookups <- function() {
  the[["lookups"]] <- NULL
  invisible(NULL)
}

#' Assemble the lookup tables. Call `get_app_lookups()` instead.
#'
#' The two source tables are arguments so that tests can supply fixtures
#' rather than fetching from GitHub.
#' @keywords internal
#' @noRd
build_app_lookups <- function(
  full_apm_lookup = get_full_apm_lookup(),
  tpma_lookup = reskit::get_tpma_label_lookup()
) {
  cond_apm_lookup <- get_condensed_apm_lookup(full_apm_lookup)
  full_ap_lookup <- full_apm_lookup |>
    dplyr::distinct(dplyr::pick(c("activity_type_label", "pod", "pod_label")))
  cond_ap_lookup <- cond_apm_lookup |>
    dplyr::distinct(dplyr::pick(c("activity_type_label", "pod", "pod_label")))
  atl_lookup <- full_apm_lookup |>
    dplyr::distinct(dplyr::pick(c("activity_type", "activity_type_label")))

  list(
    full_apm_lookup = full_apm_lookup,
    full_ap_lookup = full_ap_lookup,
    cond_ap_lookup = cond_ap_lookup,
    atl_lookup = atl_lookup,
    tpma_lookup = tpma_lookup,
    # For `pmap()`ping over `results$default` (A&E: walk-in, ambulance)
    core_mat_tbl = build_mat_tbl(full_apm_lookup),
    # For `pmap()`ping over `results$step_counts` (A&E: arrivals only)
    cond_mat_tbl = build_mat_tbl(cond_apm_lookup)
  )
}


#' One row per measure / activity_type pair, for `pmap()`ping over
#'
#' `pmap()` passes columns by name as arguments, so the result must contain
#' exactly these two columns.
#' @keywords internal
#' @noRd
build_mat_tbl <- function(apm_lookup) {
  dplyr::distinct(apm_lookup, dplyr::pick(c("measure", "activity_type")))
}


#' Read in a lookup table for PoD, activity type and measure compatibility
#' @keywords internal
#' @noRd
get_full_apm_lookup <- function() {
  yaml_data <- possibly_read_pods_lookup()
  msg <- "Unable to read POD lookup file from GitHub"
  azkit::check_that(yaml_data, is_not_null, msg)
  yaml_data |>
    purrr::pluck("default", "pod_measures") |>
    purrr::map(list_to_tbl) |>
    purrr::list_rbind(names_to = "activity_type") |>
    dplyr::mutate(
      dplyr::across("activity_type_label", \(x) sub("s$", "", x)),
      dplyr::across(tidyselect::ends_with("label"), forcats::fct_inorder)
    )
}


#' Read in a lookup table for PoD, activity type and measure compatibility
#' @keywords internal
#' @noRd
get_condensed_apm_lookup <- function(full_apm_lookup) {
  full_apm_lookup |>
    dplyr::filter_out(.data[["activity_type"]] == "aae") |>
    dplyr::add_row(
      activity_type = "aae",
      activity_type_label = "A&E",
      pod = "aae",
      pod_label = "A&E Arrivals",
      measure = "arrivals"
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::ends_with("label"), forcats::fct_inorder)
    )
}


#' Helper function to extract the required data fields from a list (from YAML)
#' @keywords internal
#' @noRd
list_to_tbl <- function(lst) {
  tibble::tibble(
    activity_type_label = lst[["name"]],
    pod = names(lst[["pods"]]),
    pod_label = purrr::map_chr(lst[["pods"]], "name"),
    measure = purrr::map(lst[["pods"]], "measures")
  ) |>
    tidyr::unnest_longer("measure")
}
