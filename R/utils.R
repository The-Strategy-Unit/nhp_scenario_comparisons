get_comparable_scenarios <- function(model_runs, scheme) {
  model_runs |>
    dplyr::filter(.data[["dataset"]] %in% .env[["scheme"]]) |>
    dplyr::mutate(
      comparable_scenarios = dplyr::n(),
      .by = c("start_year", "end_year", "app_version")
    ) |>
    dplyr::filter(.data[["comparable_scenarios"]] >= 2) |>
    dplyr::select(!"comparable_scenarios")
}

core_chart_theme <- function() {
  ggplot2::theme(
    text = ggplot2::element_text(family = "Segoe UI", size = 12),
    plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
    plot.title.position = "plot",
    legend.text = ggplot2::element_text(face = "bold", hjust = 0.1),
    legend.position = "bottom",
    strip.clip = "off"
  )
}

create_measure_label <- \(x) uppercase_init(sub("dd", "d D", sub("_", "-", x)))

bold_red <- \(x) paste0("<p style='color:red;'><strong>", x, "</strong></p>")

# fmt: skip
create_dt <- function(...) {
  purrr::partial(DT::datatable, rownames = FALSE, escape = FALSE,
    options = list(
      paging = FALSE, searching = FALSE, ordering = FALSE, dom = "t"
    ))(...)
}

swap_names <- function(vec) {
  stopifnot(rlang::is_named(vec))
  rlang::set_names(names(vec), vec)
}

tidy_dttm <- \(x) as.character(sub("Z", "", sub("T", " ", x)))

is_not_null <- \(x) !is.null(x)

pull_unique <- \(df, col) unique(df[[col]])

uppercase_init <- \(x) sub("^([[:alpha:]])(.+)", "\\U\\1\\E\\2", x, perl = TRUE)

error_on_zero_rows <- function(df) {
  if (nrow(df) == 0) {
    stop("Table has zero rows")
  } else {
    df
  }
}

require_rows <- \(x) shiny::req(x, nrow(x) > 0)
