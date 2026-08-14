#' Read the pods lookup table from a YAML file in the NHP Outputs repo
#' @param file the name of the yaml file to be read in
#' @keywords internal
read_pods_lookup <- function(file = "golem-config.yml") {
  yaml12::parse_yaml(readr::read_lines(get_outputs_gh_url(file)))
}
possibly_read_pods_lookup <- \(...) purrr::possibly(read_pods_lookup)(...)

#' Get the direct URL to a file in the NHP Outputs GitHub repo
#'
#' @param ... Pass the name of the file in via `...`
#' @keywords internal
get_outputs_gh_url <- function(...) {
  purrr::partial(get_su_gh_file, repo = "nhp_outputs", folder = "inst")(...)
}

#' Read in a file from a Strategy Unit GitHub repo
#'
#' @param repo string. The name of the repository in which to find the file
#' @param folder string. The folder where the file is located. Set to `""` to
#'   use the root folder of the repo.
#' @param file string. The name of the file to read in
#' @returns The URL to the raw file contents, to be passed to a reader function
#' @keywords internal
get_su_gh_file <- function(repo, folder, file) {
  su_raw_base_url <- "https://raw.githubusercontent.com/The-Strategy-Unit"
  file.path(su_raw_base_url, repo, "refs", "heads", "main", folder, file)
}
