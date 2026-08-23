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
