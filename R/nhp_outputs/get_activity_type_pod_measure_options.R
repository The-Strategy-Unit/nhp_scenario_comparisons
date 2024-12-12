get_activity_type_pod_measure_options <- function() {

  # Read locally instead of get_golem_config("pod_measures") in nhp_outputs
  pod_measures <- yaml::read_yaml(
    "data/golem-config.yml",
    eval.expr = FALSE
  ) |>
    _$default$pod_measures

  pod_measures |>
    purrr::map_dfr(\(.x) {
      .x$pods |>
        purrr::map_dfr(tibble::as_tibble, .id = "pod") |>
        dplyr::transmute(
          activity_type_name = .x$name,
          .data$pod,
          pod_name = .data$name,
          .data$measures
        )
    }, .id = "activity_type")

}
