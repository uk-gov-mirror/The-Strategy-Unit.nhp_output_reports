#' Generate a Report Folder and Populate with Content
#'
#' @param scheme_code Character. Three-digit ODS code.
#' @param site_codes List of three character vectors. To supply your own site
#'   codes, the three list elements should be named `ip`, `op` and `aae` and
#'   contain a vector of site codes (in the form `"XYZ01"`) or `NULL` to mean
#'   'all sites'. Defaults to `NULL`, which fetches the sites automatically from
#'   Azure.
#' @param result_sets A data.frame. Metadata of results files stored on Azure.
#' @param run_stages List of two character elements. The Azure run-stage tags
#'   for the scenarios that you want to fetch results for. Elements should be
#'   named `primary` and `secondary`. Values in thef orm of run-stage values
#'   like `"final_report_ndg2"`. You must supply this argument or
#'   `scenario_files`.
#' @param scenario_files List of two character elements.The path to arbitrary
#'   scenario files on Azure that you want to fetch results for. Elements should
#'   be named `primary` and `secondary` (as per `run_stages`). Values will be
#'   like `"<production folder>/<model version>/<scheme
#'   code>/scenario-ndg1.json.gz"`. You must supply this argument or
#'   `run_stages`. Defaults to `NULL`, which means it's ignored in favour of
#'   `run_stages`.
#' @param template_path Character. Location of the output report template.
#'   Defaults to `NULL`, which fetches the template from SharePoint.
#' @param report_type Character. Option for either "final" for final report OR
#'   "addendum" for addendum report. Defaults to `NULL`.
#'
#' @return Nothing. A populated folder structure in `outputs/`.
#'
#' @noRd
populate_template <- function(
  scheme_code,
  site_codes = NULL,
  result_sets = get_nhp_result_sets(),
  run_stages = NULL,
  scenario_files = NULL,
  template_path = NULL,
  report_type = c("final", "addendum")
) {
  report_type <- match.arg(report_type)

  # Make sure one of run_stages or scenario_files is provided
  if (
    is.null(run_stages) &
      is.null(scenario_files) |
      !is.null(run_stages) & !is.null(scenario_files)
  ) {
    cli::cli_abort(c(
      "!" = "Provide one of {.arg run_stages} or {.arg scenario_files}.",
      "i" = "Set the other to NULL."
    ))
  }

  # Check that scenario_files are for the provided scheme_code by checking for
  # the scheme code in the filepath (excluding the scenario name)
  if (!is.null(scenario_files)) {
    code_matches_path <- scenario_files |>
      purrr::map(
        \(path) {
          path |>
            stringr::str_remove(basename(path)) |>
            stringr::str_detect(scheme_code)
        }
      ) |>
      unlist() |>
      all()

    if (!code_matches_path) {
      cli::cli_abort(c(
        "!" = "{.arg scenario_files} must contain paths for the given {.arg scheme_code}.",
        "i" = "Results for scheme 'XYZ' would be on a path like 'example/example/XYZ/example.json.gz'."
      ))
    }
  }

  # Prepare some meta info
  datetime <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  site_scheme <- make_scheme_name(scheme_code, as_filestring = TRUE)
  output_dir_name <- glue::glue("{datetime}_{site_scheme}_{report_type}")
  output_dir <- file.path("outputs", output_dir_name)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Start the log file
  log_path <- file.path(output_dir, output_dir_name)
  logr::log_open(log_path, logdir = FALSE, show_notes = FALSE, compact = TRUE)

  # Get scheme name
  schemes <- readr::read_csv("data/scheme-lookup.csv", show_col_types = FALSE)
  scheme_name <- dplyr::filter(schemes, scheme == scheme_code) |>
    dplyr::pull(hosp_site)
  logr::log_print(glue::glue("* Scheme: {scheme_code} ({scheme_name})"))
  logr::log_print(glue::glue("* Execution datetime: {datetime}"))
  logr::log_print(glue::glue("* Report type: {report_type}"))

  # Get results files
  if (!is.null(run_stages)) {
    meta <- get_run_metadata(scheme_code, result_sets, run_stages)
    primary_file <- dplyr::pull(meta$metadata_primary, file)
    secondary_file <- dplyr::pull(meta$metadata_secondary, file)
  }
  if (!is.null(scenario_files)) {
    primary_file <- scenario_files[["primary"]]
    secondary_file <- scenario_files[["secondary"]]
  }

  logr::log_print(glue::glue(
    "* Scenario files:\n",
    "- primary variant: {primary_file}\n",
    "- secondary variant: {secondary_file}\n"
  ))

  if (report_type == "addendum") {
    finalreportndg2_file <- get_final_report_result(
      scheme_code,
      result_sets,
      "2"
    )
    finalreportndg1_file <- get_final_report_result(
      scheme_code,
      result_sets,
      "1"
    )

    logr::log_print(glue::glue(
      "- secondary variant: {secondary_file}\n",
      "- final scenario ndg2: {finalreportndg2_file}\n",
      "- final scenario ndg1: {finalreportndg1_file}"
    ))
  }

  # Get sites
  if (is.null(site_codes)) {
    site_codes <- get_sites(meta)
  }
  ip_sites <- if (is.null(site_codes$ip)) {
    "all"
  } else {
    paste(site_codes$ip, collapse = ", ")
  }
  op_sites <- if (is.null(site_codes$op)) {
    "all"
  } else {
    paste(site_codes$op, collapse = ", ")
  }
  aae_sites <- if (is.null(site_codes$aae)) {
    "all"
  } else {
    paste(site_codes$aae, collapse = ", ")
  }
  logr::log_print(glue::glue(
    "* Sites:\n",
    "- Inpatients:  {ip_sites}\n",
    "- Outpatients: {op_sites}\n",
    "- A&E:         {aae_sites}"
  ))

  # Read results data
  logr::log_print(glue::glue("* Fetching results..."))
  r_primary <- get_nhp_results(file = primary_file)
  r_secondary <- get_nhp_results(file = secondary_file)
  if (report_type == "addendum") {
    r_finalreportndg2_file <- get_nhp_results(file = finalreportndg2_file)
    r_finalreportndg1_file <- get_nhp_results(file = finalreportndg1_file)
  }

  # Read Word template
  logr::log_print(glue::glue("* Reading report template..."))
  if (is.null(template_path)) {
    docx <- read_template_docx(report_type = report_type)
  }
  if (!is.null(template_path)) {
    docx <- officer::read_docx(template_path)
  }

  # Generate and write calculated values
  logr::log_print(glue::glue(
    "* Calculating values for insertion to the template..."
  ))
  values_list <- generate_values_list(r_secondary, r_primary, site_codes)
  values_df <- values_list |>
    tibble::enframe(name = "item") |>
    tidyr::unnest(value)
  val_dir <- file.path(output_dir, "values")
  if (report_type == "addendum") {
    values_list_final <- generate_values_list(
      r_finalreportndg1_file,
      r_finalreportndg2_file,
      site_codes
    )
    values_df_final <- values_list_final |>
      tibble::enframe(name = "item") |>
      tidyr::unnest(value)
    val_dir_final <- file.path(output_dir, "values_final")
  }
  if (!dir.exists(val_dir)) {
    dir.create(val_dir)
  }
  values_path <- file.path(val_dir, glue::glue("{output_dir_name}_values.csv"))
  if (report_type == "addendum") {
    if (!dir.exists(val_dir_final)) {
      dir.create(val_dir_final)
    }
    values_path_final <- file.path(
      val_dir_final,
      glue::glue("{output_dir_name}_values.csv")
    )
  }
  logr::log_print(glue::glue(
    "* Writing values to {paste0(values_path, '/...')}"
  ))
  readr::write_csv(values_df, values_path)
  if (report_type == "addendum") {
    readr::write_csv(values_df_final, values_path_final)
  }

  # Generate and write check values
  logr::log_print(glue::glue(
    "* Calculating check values for writing to csv..."
  ))
  check_list <- generate_check_values(
    r_secondary,
    r_primary,
    site_codes,
    char_out = TRUE,
    values_list
  )
  check_list_df <- check_list[-c(14, 15, 16)] |>
    tibble::enframe(name = "item") |>
    tidyr::unnest(value)
  mitigators_df <- check_list[c(14, 15, 16)] |>
    tibble::enframe(name = "item") |>
    tidyr::unnest(value)
  checklist_path <- file.path(
    val_dir,
    glue::glue("{output_dir_name}_checklist.csv")
  )
  mitigators_path <- file.path(
    val_dir,
    glue::glue("{output_dir_name}_mitigators.csv")
  )

  if (report_type == "addendum") {
    check_list_final <- generate_check_values(
      r_finalreportndg1_file,
      r_finalreportndg2_file,
      site_codes,
      char_out = TRUE,
      values_list_final
    )
    check_list_final_df <- check_list_final[-c(14, 15, 16)] |>
      tibble::enframe(name = "item") |>
      tidyr::unnest(value)
    mitigators_final_df <- check_list_final[c(14, 15, 16)] |>
      tibble::enframe(name = "item") |>
      tidyr::unnest(value)
    checklist_final_path <- file.path(
      val_dir_final,
      glue::glue("{output_dir_name}_checklist.csv")
    )
    mitigators_final_path <- file.path(
      val_dir_final,
      glue::glue("{output_dir_name}_mitigators.csv")
    )
  }

  logr::log_print(glue::glue(
    "* Writing checklist values to {paste0(checklist_path, '/...')}"
  ))
  readr::write_csv(check_list_df, checklist_path)
  if (report_type == "addendum") {
    logr::log_print(glue::glue(
      "* Writing checklist values to {paste0(checklist_final_path, '/...')}"
    ))
    readr::write_csv(check_list_final_df, checklist_final_path)
  }
  logr::log_print(glue::glue(
    "* Writing mitigator values to {paste0(mitigators_path, '/...')}"
  ))
  readr::write_csv(mitigators_df, mitigators_path)

  if (report_type == "addendum") {
    logr::log_print(glue::glue(
      "* Writing mitigator values to {paste0(mitigators_final_path, '/...')}"
    ))
    readr::write_csv(mitigators_final_df, mitigators_final_path)
  }

  # Insert calculated values into the Word document's custom properties
  logr::log_print(glue::glue(
    "* Setting values as custom properties in the template..."
  ))
  docx2 <- officer::set_doc_properties(docx, values = values_list)

  # Generate, write and insert plots and tables
  fig_dir <- file.path(output_dir, "figures")
  if (report_type == "addendum") {
    fig_dir_final <- file.path(output_dir, "figures_final")
  }
  if (!dir.exists(fig_dir)) {
    dir.create(fig_dir)
  }
  logr::log_print(glue::glue("* Writing figures to {paste0(fig_dir, '/...')}"))
  if (report_type == "addendum") {
    if (!dir.exists(fig_dir_final)) {
      dir.create(fig_dir_final)
    }
    logr::log_print(glue::glue(
      "* Writing figures to {paste0(fig_dir_final, '/...')}"
    ))
  }

  write_all_figures(r_secondary, r_primary, site_codes, fig_dir)

  if (report_type == "addendum") {
    write_all_figures(
      r_finalreportndg1_file,
      r_finalreportndg2_file,
      site_codes,
      fig_dir_final
    )
  }

  docx3 <- populate_template_with_figures(docx2, fig_dir)

  # Write template (with figures inserted and values in custom properties)
  docx_out_path <- glue::glue(
    "{output_dir}/{datetime}_{site_scheme}_outputs-report_{report_type}_draft.docx"
  )
  logr::log_print(glue::glue(
    "* Writing populated report to {docx_out_path}..."
  ))
  print(docx3, target = docx_out_path)
  logr::log_print(glue::glue("* Done."))
  logr::log_close()
}


get_run_metadata <- function(scheme_code, result_sets, run_stages) {
  scheme_results <- result_sets |> dplyr::filter(dataset == scheme_code)

  metadata_primary <- dplyr::filter(
    scheme_results,
    run_stage == run_stages[["primary"]]
  )
  metadata_secondary <- dplyr::filter(
    scheme_results,
    run_stage == run_stages[["secondary"]]
  )

  dplyr::lst(metadata_secondary, metadata_primary)
}

get_sites <- function(meta) {
  primary_meta <- meta[["metadata_primary"]] # take sites from primary run
  primary_cols <- names(primary_meta)
  sites_cols <- c("sites_aae", "sites_ip", "sites_op")

  if (!all(sites_cols %in% primary_cols)) {
    stop(
      "At least one of sites_aae, sites_ip or sites_op is missing from the ",
      "primary run's metadata.",
      call. = FALSE
    )
  }

  sites_list <- primary_meta |>
    dplyr::select("sites_aae", "sites_ip", "sites_op") |>
    unlist() |>
    as.list()

  # Convert returned values into what's expected by this codebase
  sites_list |>
    purrr::set_names(\(x) stringr::str_remove(x, "sites_")) |> # 'ip' not 'sites_ip'
    purrr::map(\(x) stringr::str_split_1(x, ",")) |> # "X,Y" to c("X", "Y")
    purrr::map(\(x) if (identical(x, "ALL")) NULL else x) # NULL means all sites
}

read_template_docx <- function(
  sharepoint_site = Sys.getenv("SP_SU_SITE"),
  report_type
) {
  if (report_type == "final") {
    template_path <- Sys.getenv("SP_TEMPLATE_PATH")
  }
  if (report_type == "addendum") {
    template_path <- Sys.getenv("SP_TEMPLATE_PATH_ADD")
  }

  site <- Microsoft365R::get_sharepoint_site(sharepoint_site)
  drv <- site$get_drive()
  tmp_docx <- tempfile(fileext = ".docx")
  drv$download_file(template_path, dest = tmp_docx)
  docx <- officer::read_docx(tmp_docx)
  unlink(tmp_docx)
  docx
}

populate_template_with_figures <- function(docx, fig_dir) {
  image_paths <- list.files(fig_dir, pattern = ".png$", full.names = TRUE)

  for (image_path in image_paths) {
    cursor_text <- image_path |>
      basename() |>
      stringr::str_replace("figure_", "[Insert Figure ") |>
      stringr::str_replace(".png", "]")

    img <- png::readPNG(image_path)
    img_dim <- (dim(img) / 300)[1:2] |> setNames(c("height", "width"))

    docx <- docx |>
      insert_figure_on_cursor(
        cursor_text = cursor_text,
        image_path = image_path,
        image_width = img_dim["width"],
        image_height = img_dim["height"]
      )
  }

  docx
}

insert_figure_on_cursor <- function(
  docx = docx,
  cursor_text,
  image_path,
  image_width = NULL,
  image_height = NULL
) {
  cursor_text_unbracketed <- cursor_text |>
    stringr::str_remove("\\[") |>
    stringr::str_remove("\\]")

  cursor_can_reach <- docx |>
    officer::cursor_reach_test(cursor_text_unbracketed)

  if (!cursor_can_reach) {
    stop("Cursor can't reach '", cursor_text, "'", call. = FALSE)
  }

  docx |>
    officer::cursor_reach(cursor_text, fixed = TRUE) |>
    officer::body_add_img(
      image_path,
      width = image_width,
      height = image_height,
      pos = "on"
    )
}

get_final_report_result <- function(scheme_code, results, ndg) {
  final_results <- results |>
    dplyr::filter(dataset == scheme_code) |>
    dplyr::filter(run_stage == paste0("final_report_ndg", ndg))
  file_name <- final_results$file
}
