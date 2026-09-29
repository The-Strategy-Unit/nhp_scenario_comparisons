get_comparable_scenarios <- function(model_runs, scheme) {
  model_runs |>
    dplyr::mutate(
      comparable_scenarios = dplyr::n(),
      .by = c("start_year", "end_year", "app_version")
    ) |>
    dplyr::filter(.data[["comparable_scenarios"]] >= 2) |>
    dplyr::select(!"comparable_scenarios")
}


#' Resolve a picker selection
#'
#' Keeps the user's current selection if it is still valid, otherwise
#' auto-selects the first choice when there are at most `auto_max` choices.
#'
#' @param current Currently selected value (usually `shiny::isolate(input$x)`).
#' @param available Character vector of available choices.
#' @param auto_max Maximum number of choices for which auto-selection applies.
#' @return A length-1 character vector, or `character(0)` for no selection.
#' @noRd
resolve_selection <- function(current, available, auto_max = 1) {
  # label columns are often factors; picker values must be plain character so
  # that comparisons against browser input, and against a differently-levelled
  # factor in a newly loaded dataset, behave predictably
  available <- as.character(available)
  current <- as.character(current)

  if (length(current) == 1 && nzchar(current) && current %in% available) {
    current
  } else if (length(available) > 0 && length(available) <= auto_max) {
    available[[1]]
  } else {
    character(0)
  }
}


#' Push a fresh set of choices to a `selectInput`
#'
#' Keeps the current selection when it is still valid, otherwise falls back to
#' the first available choice. Used instead of re-rendering the input via
#' `renderUI()`, which would blank the widget whenever the underlying data
#' changed while the tab was hidden.
#'
#' @param session The (module) session object.
#' @param id Un-namespaced input id.
#' @param choices Character vector of available choices.
#' @return Invisibly, the resolved selection.
#' @noRd
sync_select_input <- function(session, id, choices) {
  choices <- as.character(choices)
  current <- shiny::isolate(session[["input"]][[id]])
  selected <- resolve_selection(current, choices, auto_max = Inf)

  # only freeze when the value is actually changing: freezing unnecessarily
  # blanks dependent outputs until the client round trip completes
  if (!identical(current, selected)) {
    shiny::freezeReactiveValue(session[["input"]], id)
  }

  shiny::updateSelectInput(
    session,
    inputId = id,
    choices = choices,
    selected = if (length(selected) > 0) selected else character(0)
  )

  invisible(selected)
}


#' Lay out module filter inputs side by side
#' @noRd
filter_row <- function(...) {
  shiny::tags$div(style = "display: flex; gap: 15px;", ...)
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

create_measure_label <- \(x) uppercase_init(sub("dd", "d D", gsub("_", "-", x)))

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

error_on_zero_rows <- \(df) if (nrow(df) > 0) df else stop("Table has no rows")

sysfile <- \(...) system.file(..., package = "nhpscenarioanalysis")
appfile <- \(...) sysfile("app", ...)


#' Require a non-empty table, with an explanation if it is empty
#'
#' This function produces `message` in place of the output if `tbl` is empty.
#' The function and its documentation were originally suggested by an LLM.
#' @param tbl A data frame
#' @param message Text to display when `tbl` has no rows.
#' @returns `tbl`, invisibly stopping the calling reactive if it is empty.
#' @noRd
validate_rows <- function(tbl, message) {
  shiny::validate(shiny::need(nrow(tbl) > 0, message))
  tbl
}


#' Wrap long strings to a line length (approximately)
#'
#' A home-made replacement for the core functionality of `stringr::str_wrap()`.
#'
#' Set line length using the `width` argument (confusingly) - this matches the
#' argument name in `stringr::str_wrap()` and `stringi::stri_wrap()` at least.
#'
#' This function will find the nearest appropriate breaks to your desired width;
#' this does however mean that final line lengths may be longer than the
#' specified width, and if a particularly long word is unfortunately placed, the
#' resulting line may be much longer than `width`.
#' The default line length is 72 characters, to increase the chance that all
#' resulting line lengths will be 80 characters at most.
#' Any existing new line characters in a string will not be retained; they will
#' be converted to spaces and treated as potential break points. If you wish to
#' ensure existing new line breaks are kept, replace them with another marker
#' character before wrapping and then restore them afterwards manually.
#'
#' @param vec A character vector
#' @param width integer The desired line length of the wrapped text (default 72)
#' @returns A character vector
str_wrap <- \(vec, width = 72) purrr::map_chr(vec, \(x) wrap_str(x, width))

#' @inheritParams str_wrap
#' @keywords internal
wrap_str <- function(x, width = 72) {
  stopifnot(width >= 1)
  if (is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("\\s+", " ", trimws(x))
  if (!grepl("\\s", x) || nchar(x) <= width) {
    return(x)
  }
  spl <- unlist(strsplit(x, ""))
  spaces <- which(spl == " ")
  lines_expected <- ceiling(length(spl) / width)
  splits_expected <- lines_expected - 1
  targets <- round(seq(splits_expected) * (length(spl) / lines_expected))
  breaks <- purrr::map_int(targets, \(t) spaces[[which.min(abs(spaces - t))]])
  spl[breaks] <- "\n"
  paste0(spl, collapse = "")
}
