#This code originated from nhp_outputs
mod_principal_summary_data <- function(r, sites) {
  pods <- mod_principal_los_pods() # uses same POD lookup as LoS summary

  main_summary <- get_principal_high_level(
    r,
    c("admissions", "attendances", "walk-in", "ambulance"),
    sites
  ) |>
    dplyr::inner_join(pods, by = dplyr::join_by("pod"))

  tele_attendances <- get_principal_high_level(r, "tele_attendances", sites) |>
    dplyr::inner_join(pods, by = dplyr::join_by("pod")) |>
    dplyr::filter(.data$pod_name != "Outpatient Procedure") |>
    dplyr::mutate(
      "pod_name" = stringr::str_replace(
        .data$pod_name,
        "Attendance",
        "Tele-attendance"
      )
    )

  bed_days <- get_principal_high_level(r, "beddays", sites) |>
    dplyr::inner_join(pods, by = dplyr::join_by("pod")) |>
    dplyr::mutate(
      "pod_name" = stringr::str_replace(.data$pod_name, "Admission", "Bed Days")
    )

  dplyr::bind_rows(
    main_summary,
    tele_attendances,
    bed_days
  ) |>
    dplyr::mutate(
      dplyr::across(
        "activity_type",
        ~ dplyr::case_match(
          .data$activity_type,
          "ip" ~ "Inpatient",
          "op" ~ "Outpatient",
          "aae" ~ "A&E"
        )
      ),
      dplyr::across(
        "activity_type",
        # ~ forcats::fct_relevel(.x, "Inpatient", "Outpatient", after = 0)
        ~ factor(.x, levels = c("Inpatient", "Outpatient", "A&E"))
      ),
      measure = dplyr::case_when(
        stringr::str_detect(.data$pod_name, "Admission$") ~ "admission",
        stringr::str_detect(.data$pod_name, "Attendance$") ~ "attendance",
        stringr::str_detect(
          .data$pod_name,
          "Tele-attendance$"
        ) ~ "tele_attendance",
        stringr::str_detect(.data$pod_name, "Procedure$") ~ "procedure",
        stringr::str_detect(.data$pod_name, "Bed Days$") ~ "bed_days"
      ),
      change = .data$principal - .data$baseline,
      change_pcnt = .data$change / .data$baseline
    ) |>
    dplyr::arrange(.data$activity_type, .data$measure, .data$pod_name) |>
    dplyr::select(
      "pod_name",
      "activity_type",
      "baseline",
      "principal",
      "change",
      "change_pcnt"
    )
}
