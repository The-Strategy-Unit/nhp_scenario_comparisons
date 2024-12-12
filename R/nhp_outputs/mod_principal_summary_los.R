mod_principal_los_pods <- function() {
  get_activity_type_pod_measure_options() |>
    dplyr::filter(.data$activity_type != "aae") |>
    dplyr::distinct(.data$activity_type, .data$pod, .data$pod_name) |>
    dplyr::bind_rows(data.frame(activity_type = "aae", pod = "aae", pod_name = "A&E Attendance")) |>
    dplyr::mutate(dplyr::across("pod_name", forcats::fct_inorder))
}

mod_principal_summary_los_data <- function(r, sites, measure) {
  pods <- mod_principal_los_pods()

  has_tretspef_los <- !is.null(r$results[["tretspef_raw+los_group"]])

  if (has_tretspef_los) {
    los_data <- r$results[["tretspef_raw+los_group"]] |>
      dplyr::select(-"tretspef_raw")
  }

  if (!has_tretspef_los) {
    los_data<- r$results[["los_group"]]
  }

  summary_los <- los_data |>
    dplyr::filter(.data$measure == .env$measure) |>
    trust_site_aggregation(sites) |>
    dplyr::inner_join(pods, by = "pod") |>
    dplyr::mutate(
      change = .data$principal - .data$baseline,
      change_pcnt = .data$change / .data$baseline
    ) |>
    dplyr::select("pod_name", "los_group", "baseline", "principal", "change", "change_pcnt") |>
    dplyr::arrange("pod_name", "los_group")

  summary_los[order(summary_los$pod_name, summary_los$los_group), ]
}

mod_principal_summary_los_table <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        "principal",
        \(.x) gt_bar(.x, scales::comma_format(1), "#686f73", "#686f73")
      ),
      dplyr::across("change", \(.x) gt_bar(.x, scales::comma_format(1))),
      dplyr::across("change_pcnt", \(.x) gt_bar(.x, scales::percent_format(1)))
    ) |>
    gt::gt(groupname_col = "pod_name") |>
    gt::cols_align(align = "left", columns = "los_group") |>
    gt::cols_label(
      "los_group" = "Length of Stay",
      "baseline" = "Baseline",
      "principal" = "Principal",
      "change" = "Change",
      "change_pcnt" = "Percent Change"
    ) |>
    gt::fmt_integer("baseline") |>
    gt::cols_width(
      .data$principal ~ gt::px(150),
      .data$change ~ gt::px(150),
      .data$change_pcnt ~ gt::px(150)
    ) |>
    gt::cols_align(
      align = "left",
      columns = c("baseline", "principal", "change", "change_pcnt")
    ) |>
    gt_theme()
}
