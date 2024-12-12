info_params_table_expat_repat_adjustment <- function(
    p,
    type = c("expat", "repat_local", "repat_nonlocal")
) {
  df <- local({
    x <- p[[type]]
    if (!is.null(x[["op"]])) {
      x[["op"]] <- list("-" = x[["op"]])
    }
    if (!is.null(x[["aae"]])) {
      x[["aae"]] <- purrr::map(x[["aae"]], \(.x) list(Other = .x))
    }
    return(x)
  })

  shiny::validate(
    shiny::need(df, "No parameters provided")
  )

  df |>
    purrr::map_depth(2, tibble::enframe, "specialty") |>
    purrr::map(dplyr::bind_rows, .id = "pod") |>
    dplyr::bind_rows(.id = "activity_type") |>
    tidyr::unnest_wider("value", names_sep = "") |>
    info_params_fix_data() |>
    # dplyr::relocate("lo", "hi", .after = tidyselect::everything()) |>
    dplyr::select(
      pod,
      activity_type_name,
      specialty_name,
      tidyselect::everything()
    ) |>
    dplyr::rename_with(
      \(.x) dplyr::case_when(
        .x == "value1" ~ "Low",
        .x == "value2" ~ "High",
        TRUE ~ .x
      )
    ) |>
    gt::gt("specialty_name", c("activity_type_name", "pod")) |>
    gt_theme()
}

info_params_fix_data <- function(
    df,
    specs_path = "data/tx-lookup.json",
    mitigators_path = "data/mitigators.json"
) {
  at <- get_activity_type_pod_measure_options() |>
    dplyr::distinct(
      dplyr::across(
        tidyselect::starts_with("activity_type")
      )
    )

  specs <- specs_path |>
    jsonlite::read_json(simplifyVector = TRUE) |>
    dplyr::select(
      "specialty" = "Code",
      "specialty_name" = "Description"
    )

  strategies <- mitigators_path |>
    jsonlite::read_json(simplifyVector = TRUE) |>
    unlist() |>
    tibble::enframe("strategy", "mitigator_name")

  fix_activity_type <- function(df) {
    if (!"activity_type" %in% colnames(df)) {
      return(df)
    }

    df |>
      dplyr::inner_join(at, by = dplyr::join_by("activity_type")) |>
      dplyr::select(-"activity_type")
  }

  fix_specialty <- function(df) {
    if (!"specialty" %in% colnames(df)) {
      return(df)
    }

    df |>
      dplyr::left_join(specs, by = dplyr::join_by("specialty")) |>
      dplyr::mutate(
        dplyr::across(
          "specialty_name",
          \(.x) ifelse(is.na(.x), .data[["specialty"]], .x)
        )
      ) |>
      dplyr::select(-"specialty")
  }

  fix_strategy <- function(df) {
    if (!"strategy" %in% colnames(df)) {
      return(df)
    }

    df |>
      dplyr::left_join(strategies, by = dplyr::join_by("strategy")) |>
      dplyr::select(-"strategy")
  }

  df |>
    fix_activity_type() |>
    fix_specialty() |>
    fix_strategy()
}

info_params_table_activity_avoidance <- function(p) {
  actitvity_avoidance <- p[["activity_avoidance"]]

  # shiny::validate(
  #   shiny::need(actitvity_avoidance, "No parameters provided")
  # )

  time_profiles <- p[["time_profile_mappings"]][["activity_avoidance"]] |>
    purrr::map(unlist) |>
    purrr::map(tibble::enframe, "strategy", "time_profile") |>
    dplyr::bind_rows(.id = "activity_type")

  actitvity_avoidance |>
    purrr::map_depth(2, "interval") |>
    purrr::map(tibble::enframe, "strategy") |>
    dplyr::bind_rows(.id = "activity_type") |>
    tidyr::unnest_wider("value", names_sep = "") |>  # added names_sep otherwise error
    dplyr::left_join(
      time_profiles,
      by = dplyr::join_by("activity_type", "strategy")
    ) |>
    info_params_fix_data() |>
    dplyr::arrange("activity_type_name", "mitigator_name")
  # gt::gt("mitigator_name", "activity_type_name") |>
  # gt_theme()
}

info_params_table_efficiencies <- function(p) {
  efficiencies <- p[["efficiencies"]]

  # shiny::validate(
  #   shiny::need(efficiencies, "No parameters provided")
  # )

  time_profiles <- p[["time_profile_mappings"]][["efficiencies"]] |>
    purrr::map(unlist) |>
    purrr::map(tibble::enframe, "strategy", "time_profile") |>
    dplyr::bind_rows(.id = "activity_type")

  efficiencies |>
    purrr::map_depth(2, "interval") |>
    purrr::map(tibble::enframe, "strategy") |>
    dplyr::bind_rows(.id = "activity_type") |>
    tidyr::unnest_wider("value", names_sep = "") |>  # added names_sep otherwise error
    dplyr::left_join(
      time_profiles,
      by = dplyr::join_by("activity_type", "strategy")
    ) |>
    info_params_fix_data() |>
    dplyr::arrange("activity_type_name", "mitigator_name")
  # gt::gt("mitigator_name", "activity_type_name") |>
  # gt_theme()
}
