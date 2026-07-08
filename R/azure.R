get_nhp_result_sets <- function(
  auth_token = azkit::get_auth_token(),
  table_ep = Sys.getenv("AZ_TABLE_EP"),
  runs_table_name = Sys.getenv("AZ_TABLE_NAME"),
  allowed_datasets = get_user_allowed_datasets(NULL)
) {
  ds <- tibble::tibble(dataset = allowed_datasets)
  azkit::read_azure_table(
    table_name = runs_table_name,
    table_endpoint = table_ep,
    token = auth_token,
    filter = "aggregated_results_path ne ''"
  ) |>
    # filter to available datasets for this user
    dplyr::semi_join(ds, by = dplyr::join_by("dataset")) |>
    dplyr::mutate(
      dplyr::across("viewable", as.logical)
    )
}

get_user_allowed_datasets <- function(groups) {
  p <- jsonlite::read_json(
    "supporting_data/datasets.json",
    simplifyVector = TRUE
  ) |>
    names()

  if (!(is.null(groups) || any(c("nhp_devs", "nhp_power_users") %in% groups))) {
    a <- groups |>
      stringr::str_subset("^nhp_(national|icb|provider)_") |>
      stringr::str_remove("^nhp_(national|icb|provider)_")
    intersect(p, a)
  } else {
    p
  }
}

get_container <- function(endpoint, container) {
  token <- azkit::get_auth_token()
  if (is.null(token)) {
    stop(
      "No Azure token found. Please authenticate using AzureAuth::get_azure_token()."
    )
  }

  AzureStor::blob_endpoint(endpoint, token = token) |>
    AzureStor::blob_container(container)
}


#' Unzip, Read and Parse an NHP Results File
#'
#' @param container_results Name of a blob_container/storage_container object
#'     that stores results files.
#' @param file Character. The path to a file in the named `container`.
#'
#' @details Assumes you've connected to the container that holds NHP results.
#'
#' @return A nested list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' container <- azkit::get_container()
#' result_sets <- container |> get_nhp_result_sets()
#' file <- result_sets |> dplyr::slice(1) |> dplyr::pull(file)
#' r <- container |> get_nhp_results(file)
#' }
get_nhp_results <- function(
  container_results = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"),
  blob_url = Sys.getenv("AZ_STORAGE_EP"),
  file
) {
  container <- azkit::get_container(
    container = container_results,
    endpoint = blob_url
  )

  AzureStor::download_blob(container, file, NULL) |>
    memDecompress(type = "gzip") |>
    yyjsonr::read_json_raw(
      yyjsonr::opts_read_json(
        obj_of_arrs_to_df = FALSE
      )
    ) |>
    parse_results() # applies patch logic dependent on app_version in params
}
