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

#' Read and Parse NHP Results Files
#'
#' @param container_results Name of a blob_container/storage_container object
#'     that stores results files.
#' @param results_path Character. The path to a results file (zipped json) or a
#'     results directory (containing parquets) in the named `container`.
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
  results_path
) {
  container <- azkit::get_container(container_results)

  is_json_gz <- tools::file_ext(results_path) == "gz"
  is_parquet <- stringr::str_detect(results_path, "^aggregated-model-results")

  if (is_json_gz) {
    # TODO: replace with azkit
    temp_file <- withr::local_tempfile()
    AzureStor::download_blob(container, results_path, temp_file)

    nhp_results <- readBin(temp_file, raw(), n = file.size(temp_file)) |>
      jsonlite::parse_gzjson_raw(simplifyVector = FALSE) |>
      parse_results() # applies patch logic dependent on app_version in params
  }

  if (is_parquet) {
    params <- azkit::read_azure_json(
      container,
      file.path(results_path, "params.json")
    )

    population_variants <- azkit::read_azure_json(
      container,
      file.path(results_path, "variants.json")
    )

    results <- reskit::read_results_parquet_files(container, results_path)

    nhp_results <- dplyr::lst(params, population_variants, results)
  }

  nhp_results
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
