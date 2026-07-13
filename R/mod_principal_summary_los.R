#This code originated from nhp_outputs
mod_principal_los_pods <- function() {
  get_activity_type_pod_measure_options() |>
    dplyr::filter(.data$activity_type != "aae") |>
    dplyr::distinct(.data$activity_type, .data$pod, .data$pod_name) |>
    dplyr::bind_rows(data.frame(
      activity_type = "aae",
      pod = "aae",
      pod_name = "A&E Attendance"
    )) |>
    dplyr::mutate(dplyr::across("pod_name", forcats::fct_inorder))
}

mod_principal_summary_los_data <- function(r, sites, measure) {
  pods <- mod_principal_los_pods()

  has_tretspef_los <- !is.null(r$results[["tretspef+los_group"]])

  if (has_tretspef_los) {
    los_data <- r$results[["tretspef+los_group"]] |>
      dplyr::select(-"tretspef")
  }

  if (!has_tretspef_los) {
    los_data <- r$results[["los_group"]]
  }

  summary_los <- los_data |>
    dplyr::filter(.data$measure == .env$measure) |>
    trust_site_aggregation(sites) |>
    dplyr::inner_join(pods, by = dplyr::join_by("pod")) |>
    dplyr::mutate(
      change = .data$principal - .data$baseline,
      change_pcnt = .data$change / .data$baseline
    ) |>
    dplyr::select(
      "pod_name",
      "los_group",
      "baseline",
      "principal",
      "change",
      "change_pcnt"
    ) |>
    dplyr::arrange("pod_name", "los_group")

  summary_los[order(summary_los$pod_name, summary_los$los_group), ]
}
