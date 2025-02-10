mod_principal_detailed_table <- function(data, aggregation, final_year) {
  data |>
    dplyr::mutate(
      dplyr::across("sex", \(.x) ifelse(.x == 1, "Male", "Female")),
      dplyr::across("final", \(.x) gt_bar(.x, scales::comma_format(1), "#686f73", "#686f73")),
      dplyr::across("change", \(.x) gt_bar(.x, scales::comma_format(1))),
      dplyr::across("change_pcnt", \(.x) gt_bar(.x, scales::percent_format(1)))
    ) |>
    gt::gt(groupname_col = "sex") |>
    gt::cols_label(
      agg = dplyr::case_match(
        aggregation,
        "age_group" ~ "Age Group",
        "tretspef" ~ "Treatment Specialty",
        .default = aggregation
      ),
      baseline = "Baseline",
      final = paste0("Final (", final_year, ")"),
      change = "Change",
      change_pcnt = "Percent Change",
    ) |>
    gt::fmt_integer(c("baseline")) |>
    gt::cols_width(
      .data$final ~ gt::px(150),
      .data$change ~ gt::px(150),
      .data$change_pcnt ~ px(150)
    ) |>
    gt::cols_align(
      align = "left",
      columns = c("agg", "final", "change", "change_pcnt")
    ) |>
    gt_theme()
}


