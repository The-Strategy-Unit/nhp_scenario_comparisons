get_metadata <- function(result_sets, scenario) {
  metadata <- scenario

  metadata |>
    dplyr::mutate(
      create_datetime = .data$create_datetime |>
        lubridate::as_datetime() |>
        format("%Y-%m-%d %H:%M:%S") |>
        as.character(),
      url_app_version = stringr::str_replace(.data$app_version, "\\.", "-"),
      url_stub = glue::glue(
        "https://connect.strategyunitwm.nhs.uk/nhp/{url_app_version}/outputs/?"
      ),
      outputs_link = glue::glue("{url_stub}{outputs_app_uri}")
    ) |>
    dplyr::mutate(
      outputs_app = glue::glue(
        "<a href='{outputs_link}' target='_blank'>Launch</a> \U1F517"
      )
    ) |>
    dplyr::select(
      .data$user,
      .data$dataset,
      .data$scenario,
      .data$seed,
      .data$model_runs,
      .data$start_year,
      .data$end_year,
      .data$app_version,
      .data$create_datetime,
      .data$outputs_app
    )
}
