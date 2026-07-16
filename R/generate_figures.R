prepare_all_principal_change_factors <- function(
  r,
  site_codes = list(ip = NULL, op = NULL, aae = NULL) # character vectors
) {
  mitigators_lookup <- read_mitigators()
  atmpo_lookup <- read_atmpo()

  activity_types_long <- list("inpatients", "outpatients", "aae")
  activity_types_short <- list("ip", "op", "aae")

  pods <- purrr::map(
    activity_types_long,
    \(x) {
      atmpo_lookup |>
        dplyr::filter(.data$activity_type == x) |>
        dplyr::pull(.data$pod) |>
        unique()
    }
  ) |>
    purrr::set_names(activity_types_short)

  possibly_prep_principal_change_factors <-
    purrr::possibly(prep_principal_change_factors)

  principal_change_data <- purrr::map2(
    activity_types_short,
    pods,
    \(activity_type, pod) {
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = site_codes,
        mitigators = mitigators_lookup,
        at = activity_type,
        pods = pod
      )
    }
  ) |>
    purrr::set_names(activity_types_short)

  # Scenarios run under v1.0 don't have A&E data when filtering by site, so
  # provide results for whole-scheme level.
  if (r$params$app_version == "v1.0" && !is.null(site_codes[["aae"]])) {
    principal_change_data[["aae"]] <-
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = list(aae = NULL), # provide for whole scheme, not site
        mitigators = mitigators_lookup,
        at = "aae",
        pods = pods[["aae"]]
      )
  }

  principal_change_data
}


read_mitigators <- function(remove_codes = TRUE) {
  mitigators <- "supporting_data/mitigators.json" |>
    jsonlite::read_json(simplifyVector = TRUE) |>
    purrr::simplify() |>
    tibble::enframe("strategy", "mitigator_name")

  if (remove_codes) {
    mitigators <- mitigators |>
      dplyr::mutate(
        mitigator_name = stringr::str_remove(
          .data$mitigator_name,
          " \\(\\w{2}-\\w{2}-\\d{3}\\)"
        )
      )
  }

  mitigators
}

read_atmpo <- function() {
  get_activity_type_pod_measure_options() |>
    dplyr::select("activity_type", "pod", "measure" = "measures") |>
    dplyr::mutate(
      activity_type = dplyr::case_match(
        .data$activity_type,
        "ip" ~ "inpatients",
        "op" ~ "outpatients",
        "aae" ~ "aae"
      )
    )
}
