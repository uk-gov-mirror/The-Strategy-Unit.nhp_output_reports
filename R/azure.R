#' List All NHP Model Runs and their Metadata from Azure Table Storage
#'
#' @param runs_table Character. Name of the Azure Table Storage table.
#' @param table_ep Character. The endpoint for Azure Table Storage actions.
#' @param auth_token Token. An Azure token for the Table resource.
#' @param entity_query Character. OData query string to pre-filter the table.
#'     Defaults to discard any entities that represent incomplete model runs.
#' @param property_selection Character. OData select string for properties
#'     (comma-separated and no spaces, e.g. `"X,Y,Z"``). Defaults to `NULL`,
#'     meaning retain all.
#'
#' @details
#' This function used to glean metadata stored on blobs in Azure Blob Storage,
#' but now it uses a canonical lookup table in Azure Table Storage.
#'
#' @return A data.frame. Each row is a Azure Storage Table table entity (a model
#'     run) and each column is a property in that table.
#'
#' @export
#'
#' @examples \dontrun{get_nhp_result_sets()}
get_nhp_result_sets <- function(
  runs_table = Sys.getenv("AZ_TABLE_NAME"),
  table_ep = Sys.getenv("AZ_TABLE_EP"),
  auth_token = azkit::get_auth_token(),
  entity_query = "status eq 'complete'",
  property_selection = NULL # defaults to all columns
) {
  runs_table <- azkit::read_azure_table(
    table_name = runs_table,
    table_endpoint = table_ep,
    filter = entity_query,
    select = property_selection,
    token = auth_token
  )

  # Apply changes so the table matches existing expectations of the codebase
  runs_table |>
    dplyr::rename(file = results_json_gz_path) |>
    dplyr::mutate(
      create_datetime = create_datetime |>
        lubridate::as_datetime() |>
        format("%Y%M%d_%H%M%S") # YYYYMMDD_HHMMSS
    )
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
    parse_results() # applies patch logic dependent on app_version in params
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
