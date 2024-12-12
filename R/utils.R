#' Concatenate Scheme Name and Code
#' @param scheme_code Character. A focus scheme's three-character ODS code.
#' @param lookup_path Character. The file path to the CSV lookup of scheme names
#'     and codes.
#' @param as_filestring Logical. Express as a string with punctuation removed,
#'    hyphen-delimited and in lowercase? Used to build filepath.
#' @return Character string.
#' @export
#' @examples \dontrun{construct_scheme_name("XYZ")}
make_scheme_name <- function(
    scheme_code,
    lookup_path = "data/scheme-lookup.csv",
    as_filestring = FALSE
) {

  scheme_string <- readr::read_csv(lookup_path, show_col_types = FALSE) |>
    dplyr::filter(scheme == scheme_code) |>
    dplyr::mutate(
      hosp_site_scheme = glue::glue("{hosp_site} ({scheme})"),
      .keep = "none"
    ) |>
    dplyr::pull()

  if (as_filestring) {
    scheme_string <- scheme_string |>
      stringr::str_remove_all("[:punct:]") |>
      stringr::str_to_lower() |>
      stringr::str_replace_all(" ", "-")
  }

  scheme_string

}
