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
    container_results = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"),
    blob_url = Sys.getenv("AZ_STORAGE_EP"),
    allowed_datasets = get_user_allowed_datasets(NULL)
) {
  
  ds <- tibble::tibble(dataset = allowed_datasets)
  
  container <- get_container(container = container_results,
                             endpoint = blob_url)
  
  metadata_cache <- cachem::cache_disk(".cache")
  get_metadata <- purrr::partial(AzureStor::get_storage_metadata, object = container)
  metadata <- memoise::memoise(get_metadata, cache = metadata_cache)
  
  cat  ("loading result sets filenames\n")
  files <- container |>
    AzureStor::list_blobs("prod", info = "all", recursive = TRUE) |>
    dplyr::filter(!.data[["isdir"]]) |>
    dplyr::filter(!stringr::str_detect(name, "prod/dev")) |> #remove dev runs
    dplyr::mutate(version_number = 
                    as.numeric(
                      stringr::str_replace_all(name,
                                               ".*prod/v([0-9]+\\.[0-9]+)/.*", "\\1")
                    )
    )|> 
    dplyr::filter(version_number >= 3.1) |>  #keep models on v3.1 or later only
    purrr::pluck("name") |>
    purrr::set_names()
  
  cat("getting metadata\n")
  files <- files |>
    purrr::map(metadata, .progress="Initialising..") |>
    dplyr::bind_rows(.id = "file") |>
    # filter to available datasets for this user
    dplyr::semi_join(ds, by = dplyr::join_by("dataset")) |>
    dplyr::mutate(
      dplyr::across("viewable", as.logical)
    )
  cat("returning result sets\n")
  
  files
}

get_user_allowed_datasets <- function(groups) {
  p <- jsonlite::read_json("supporting_data/datasets.json", simplifyVector = TRUE) |>
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

filter_result_sets <- function(result_sets, ds, sc, cd) {
  result_sets |>
    shiny::req() |>
    dplyr::filter(
      .data[["dataset"]] == ds,
      .data[["scenario"]] == sc,
      .data[["create_datetime"]] == cd
    ) |>
    require_rows()
}

get_token <- function(resource) {
  token <- tryCatch(
    {
      AzureAuth::get_managed_token(resource = resource)
    },
    error = function(e) {
      NULL
    }
  )
  
  if (is.null(token)) {
    # list tokens already locally stored
    local_tokens <- AzureAuth::list_azure_tokens()
    if (length(local_tokens) > 0) {
      resources <- purrr::map(local_tokens, "resource")
      # if there are token(s) matching the `resource` argument then return one
      token_index <- match(resource, resources)[1]
      token <- if (!is.na(token_index)) local_tokens[[token_index]] else NULL
    }
  }
  
  token
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
get_container <- function(endpoint, container) {
  token <- azkit::get_auth_token()
  if (is.null(token)) {
    stop("No Azure token found. Please authenticate using AzureAuth::get_azure_token().")
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
#' container <- get_container()
#' result_sets <- container |> get_nhp_result_sets()
#' file <- result_sets |> dplyr::slice(1) |> dplyr::pull(file)
#' r <- container |> get_nhp_results(file)
#' }
get_nhp_results <- function(
    container_results = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"),
    blob_url = Sys.getenv("AZ_STORAGE_EP"),
    file
) {
  
  container <- get_container(container = container_results,
                             endpoint = blob_url)
  
  r <- AzureStor::download_blob(container, file, NULL) |> 
    jsonlite::parse_gzjson_raw(simplifyVector = FALSE) 
  
  
  withr::defer({
    rm(r)
    gc()
  })
  
    parse_results(r)  # applies patch logic dependent on app_version in params
  
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
