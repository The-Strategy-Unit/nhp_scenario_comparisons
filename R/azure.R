#' List All NHP Results Data Files and their Metadata
#'
#' @param container_results Name of the blob_container/storage_container object
#'     that stores results files.
#'
#' @details Assumes you're connecting to the container that holds NHP results.
#'
#' @return A data.frame. As many rows as there are files in `container`. As many
#'    columns as there are metadata elements, plus the file path.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples \dontrun{get_container() |> get_nhp_result_sets()}
get_nhp_result_sets <- function(
    container_results = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS")
) {

  container <- get_container(container_name = container_results)

  container |>
    AzureStor::list_blobs("prod", info = "all", recursive = TRUE) |>
    dplyr::filter(!.data[["isdir"]]) |>
    purrr::pluck("name") |>
    purrr::set_names() |>
    purrr::map(\(name, ...) AzureStor::get_storage_metadata(container, name)) |>
    dplyr::bind_rows(.id = "file") |>
    dplyr::mutate(dplyr::across("viewable", as.logical))

}

#' Connect to an Azure Container
#'
#' @param tenant Character. The tenant ID.
#' @param app_id Character. The app ID.
#' @param ep_uri Character. The endpoint URI.
#' @param container_name Character. The container name. Use `Sys.getenv()` with
#'     `"AZ_STORAGE_CONTAINER_RESULTS"` or `"AZ_STORAGE_CONTAINER_RESULTS"`.
#'
#' @details All arguments default to environmental variables stored in your
#'     .Renviron file. Note that you'll be routed automatically to the browser
#'     for authentication if you don't have a cached token already.
#'
#' @return A blob_container/storage_container object.
#'
#' @export
#'
#' @examples
#' \dontrun{get_container()}
get_container <- function(
    tenant = Sys.getenv("AZ_TENANT_ID"),
    app_id = Sys.getenv("AZ_APP_ID"),
    ep_uri = Sys.getenv("AZ_STORAGE_EP"),
    container_name
) {

  # if the app_id variable is empty, we assume that this is running on an Azure VM,
  # and then we will use Managed Identities for authentication.
  token <- if (app_id != "") {
    AzureAuth::get_azure_token(
      resource = "https://storage.azure.com",
      tenant = Sys.getenv("AZ_TENANT_ID"),
      app = app_id,
      auth_type = "device_code"
    )
  } else {
    AzureAuth::get_managed_token("https://storage.azure.com/") |>
      AzureAuth::extract_jwt()
  }

  ep_uri |>
    AzureStor::blob_endpoint(token = token) |>
    AzureStor::storage_container(container_name)

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
#' container <- get_container()
#' result_sets <- container |> get_nhp_result_sets()
#' file <- result_sets |> dplyr::slice(1) |> dplyr::pull(file)
#' r <- container |> get_nhp_results(file)
#' }
get_nhp_results <- function(
    container_results = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"),
    file
) {

  container <- get_container(container_name = container_results)

  temp_file <- withr::local_tempfile()
  AzureStor::download_blob(container, file, temp_file)

  readBin(temp_file, raw(), n = file.size(temp_file)) |>
    jsonlite::parse_gzjson_raw(simplifyVector = FALSE) |>
    parse_results()  # applies patch logic dependent on app_version in params

}

get_baseline_and_projections <- function(r_trust) {

  r_trust[["results"]][["default"]] |>
    dplyr::group_by(measure, pod, sitetret) |>
    dplyr::summarise(
      baseline = sum(baseline),
      principal = sum(principal),
      lwr_ci = sum(lwr_ci),
      upr_ci = sum(upr_ci)
    )

}

get_stepcounts <- function(r_trust) {
  r_trust[["results"]][["step_counts"]]
}

get_losgroup <- function(r_trust) {

  los_group_is_null <- is.null(r_trust[["results"]][["los_group"]])

  if (los_group_is_null) {
    # tretspef+los_group renamed from tretspef_raw+los_group in v4.0
    r_trust <- r_trust[["results"]][["tretspef+los_group"]]
  } else {
    r_trust <- r_trust[["results"]][["los_group"]]
  }

  r_trust

}
