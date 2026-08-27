# Goal: produce nhp_output_reports charts for:
#   1. Gynae (treatment specialties 502 and 503)
#   2. Paeds (ages 0 to 17)
#   3. Maternity (the POD)
#
# See https://github.com/The-Strategy-Unit/nhp_output_reports/issues/54
#
# General approach: load all functions, then overwrite certain data wrangling
# functions to respond to global options that dictate whether to filter for
# gynae, paeds or maternity. Requires that all steps in this script be run in
# order, since functions are overwritten along the way.

# Set up ----

# Load all existing functions, overwrite later with bespoke versions
purrr::walk(list.files("R", ".R$", , TRUE, TRUE), source)

# Read results/sites as per insert_into_template()
scheme_code <- "REF" # replace with scheme of interest
result_sets <- get_nhp_result_sets()
run_stages <- list(
  primary = "final_report_ndg2",
  secondary = "final_report_ndg3"
)
meta <- get_run_metadata(scheme_code, result_sets, run_stages)
site_codes <- get_sites(scheme_code)
primary_file <- dplyr::pull(meta$metadata_primary, file)
secondary_file <- dplyr::pull(meta$metadata_secondary, file)
r_primary <- get_nhp_results(results_path = primary_file)
r_secondary <- get_nhp_results(results_path = secondary_file)

# Helper functions ----

# Selects a dataset depending on the value of the 'results_type' option, needed
# for paeds/gynae filtering for beeswarms, s-curves and summary tables.
switch_results_data <- function(results) {
  switch(
    getOption("results_type", default = NA_character_),
    "gynae" = results[["results"]][["tretspef"]] |>
      dplyr::filter(
        tretspef %in% c("502", "503"), # gynae-specific treatment specialties
        !purrr::map_lgl(model_runs, is.null) # listcol must contain runs
      ),
    "paeds" = results[["results"]][["age"]] |>
      dplyr::filter(dplyr::between(age, 0, 17)), # inclusive of 0 and 17
    results[["results"]][["default"]] # otherwise original default behaviour
  )
}

# Generate beeswarms and S-curves ----

# Bespoke version of existing function (needed for paeds/gynae)
get_model_run_distribution <- function(r, pod, measure, site_codes) {
  filtered_results <-
    switch_results_data(r) |> # NEW: bespoke paeds/gynae switch
    dplyr::filter(
      .data$pod %in% .env$pod,
      .data$measure %in% .env$measure
    ) |>
    dplyr::select("sitetret", "baseline", "principal", "model_runs")

  if (nrow(filtered_results) == 0) {
    return(NULL)
  }

  filtered_results |>
    dplyr::mutate(
      dplyr::across(
        "model_runs",
        \(.x) purrr::map(.x, tibble::enframe, name = "model_run")
      )
    ) |>
    tidyr::unnest("model_runs") |>
    dplyr::inner_join(get_variants(r), by = "model_run") |>
    trust_site_aggregation(site_codes)
}

# Bespoke version of existing function (needed for maternity)
prepare_all_activity_distribution_plots <- function(r, site_codes) {
  atpmo_lookup <- read_atmpo()

  # NEW: bespoke filter for maternity
  is_maternity <- isTRUE(getOption("maternity"))
  if (is_maternity) {
    atpmo_lookup <- atpmo_lookup |>
      dplyr::filter(stringr::str_detect(pod, "maternity"))
  }

  activity_types <- list("inpatients", "outpatients", "aae")

  pods <- purrr::map(
    activity_types,
    \(x) {
      atpmo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(pod) |>
        unique()
    }
  ) |>
    purrr::map(list) |>
    purrr::set_names(activity_types)

  measures <- purrr::map(
    activity_types,
    \(x) {
      atpmo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(measure) |>
        unique()
    }
  ) |>
    purrr::map(list) |>
    purrr::set_names(activity_types)

  measures[["inpatients"]] <- "beddays" # admissions not shown in report

  possibly_plot_activity_distributions <-
    purrr::possibly(plot_activity_distributions)

  activity_distribution_plots <- purrr::pmap(
    list(activity_types, pods, measures),
    \(activity_type, pod, measure) {
      possibly_plot_activity_distributions(
        data = r,
        site_codes = site_codes,
        activity_type = activity_type,
        pod = pod,
        measure = measure
      )
    }
  ) |>
    purrr::set_names(activity_types)

  # Earlier model runs don't have A&E data when filtering by site, so provide
  # data for whole-scheme level.
  if (r$params$app_version == "v1.0" & !is.null(site_codes[["aae"]])) {
    activity_distribution_plots[["aae"]] <-
      possibly_plot_activity_distributions(
        data = r,
        site_codes = list(aae = NULL), # provide for whole scheme, not site
        activity_type = "aae",
        pod = pods[["aae"]],
        measure = measures[["aae"]]
      )
  }

  activity_distribution_plots |> purrr::list_flatten()
}

# Convenience function to generate plots and (un)set options
plot_activity_dist <- function(
  results = r_primary,
  sites = site_codes,
  results_type = NULL, # set paeds/gynae options
  maternity = NULL # set maternity option
) {
  options(results_type = results_type, maternity = maternity)
  scenario_dataset <- deparse(substitute(results)) # capture name of results object
  on.exit(options(results_type = NULL, maternity = NULL))
  cat(
    "* results object:",
    scenario_dataset,
    "\n* result-type option:",
    getOption("results_type", default = "none"),
    "\n* maternity option:",
    getOption("maternity", default = "none"),
    "\n"
  )
  plots <- prepare_all_activity_distribution_plots(results, sites)
  if (scenario_dataset == "r_secondary") {
    # Only the inpatients ones are needed for figs 9.10 and 9.11 (secondary)
    plots <- plots[c("inpatients_beeswarm_plot", "inpatients_ecdf_plot")]
  }
  plots
}

# Generate primary-scenario beeswarms (9.1, 9.6, 9.8), S-curves (9.2, 9.7, 9.9)
plots_actdist_pri <- plot_activity_dist() # overall
plots_actdist_pri_paeds <- plot_activity_dist(results_type = "paeds")
plots_actdist_pri_gynae <- plot_activity_dist(results_type = "gynae")
plots_actdist_pri_mat <- plot_activity_dist(maternity = TRUE)

# Generate secondary-scenario beeswarm (9.10) and S-curve (9.11)
plots_actdist_sec <- plot_activity_dist(results = r_secondary) # overall
plots_actdist_sec_paeds <- plot_activity_dist(
  results = r_secondary,
  results_type = "paeds"
)
plots_actdist_sec_gynae <- plot_activity_dist(
  results = r_secondary,
  results_type = "gynae"
)
plots_actdist_sec_mat <- plot_activity_dist(
  results = r_secondary,
  maternity = TRUE
)

# Generate summary tables ----

# Bespoke version of existing function
get_principal_high_level <- function(r, measures, sites) {
  switch_results_data(r) |> # NEW: bespoke paeds/gynae switch
    dplyr::filter(.data$measure %in% measures) |>
    dplyr::select("pod", "sitetret", "baseline", "principal") |>
    dplyr::mutate(dplyr::across(
      "pod",
      ~ ifelse(
        stringr::str_starts(.x, "aae"),
        "aae",
        .x
      )
    )) |>
    dplyr::group_by(.data$pod, .data$sitetret) |>
    dplyr::summarise(dplyr::across(where(is.numeric), sum), .groups = "drop") |>
    trust_site_aggregation(sites)
}

# Convenience function to generate plots and (un)set options
make_summary_table <- function(
  results = r_primary,
  sites = site_codes[["ip"]],
  results_type = NULL, # set paeds/gynae options
  maternity = NULL # set maternity option
) {
  options(results_type = results_type, maternity = maternity)
  on.exit(options(results_type = NULL, maternity = NULL))

  cat(
    "* result-type option:",
    getOption("results_type", default = "none"),
    "\n* maternity option:",
    getOption("maternity", default = "none"),
    "\n"
  )

  summary_data <- mod_principal_summary_data(results, sites) |>
    dplyr::filter(activity_type == "Inpatient")

  # NEW: bespoke filter for maternity
  is_maternity <- isTRUE(getOption("maternity"))
  if (is_maternity) {
    summary_data <- summary_data |>
      dplyr::filter(stringr::str_detect(pod_name, "Maternity"))
  }

  summary_data |> mod_principal_summary_table()
}

# Generate summary table (9.3)
summary_table <- make_summary_table() # overall
summary_table_gynae <- make_summary_table(results_type = "gynae")
summary_table_paeds <- make_summary_table(results_type = "paeds")
summary_table_mat <- make_summary_table(maternity = TRUE)

# Generate LoS tables ----

# Bespoke version of existing function
mod_principal_summary_los_data <- function(r, sites, measure) {
  pods <- mod_principal_los_pods()

  has_tretspef_los <- !is.null(r$results[["tretspef+los_group"]])

  # NEW: Bespoke gynae check to use for filtering
  is_gynae <- getOption("results_type", default = "none") == "gynae"

  if (!has_tretspef_los) {
    if (is_gynae) {
      stop("Can't produce an output for gynae, tretspef+los_group is missing.")
    }
    los_data <- r$results[["los_group"]]
  }

  if (has_tretspef_los) {
    los_data <- r$results[["tretspef+los_group"]]
    if (is_gynae) {
      los_data <- los_data |> dplyr::filter(tretspef %in% c("502", "503"))
    }
    los_data <- los_data |> dplyr::select(-"tretspef")
  }

  summary_los <- los_data |>
    dplyr::filter(.data$measure == .env$measure) |>
    trust_site_aggregation(sites) |>
    dplyr::inner_join(pods, by = "pod") |>
    dplyr::mutate(
      change = .data$principal - .data$baseline,
      change_pcnt = .data$change / .data$baseline
    ) |>
    dplyr::select(
      "pod_name",
      "los_group",
      "baseline",
      "principal",
      "change",
      "change_pcnt"
    ) |>
    dplyr::arrange("pod_name", "los_group")

  summary_los[order(summary_los$pod_name, summary_los$los_group), ]
}

# Convenience function to generate plots and (un)set options
make_summary_los_table <- function(
  results = r_primary,
  sites = site_codes[["ip"]],
  results_type = NULL, # set paeds/gynae options
  maternity = NULL # set maternity option
) {
  options(results_type = results_type, maternity = maternity)
  on.exit(options(results_type = NULL, maternity = NULL))

  cat(
    "* result-type option:",
    getOption("results_type", default = "none"),
    "\n* maternity option:",
    getOption("maternity", default = "none"),
    "\n"
  )

  los_data <- mod_principal_summary_los_data(results, sites, "beddays") |>
    dplyr::mutate(
      pod_name = stringr::str_replace(pod_name, "Admission", "Bed Days")
    ) |>
    dplyr::arrange(pod_name, los_group)

  # NEW: bespoke filter for maternity
  is_maternity <- isTRUE(getOption("maternity"))
  if (is_maternity) {
    los_data <- los_data |>
      dplyr::filter(stringr::str_detect(pod_name, "Maternity"))
  }

  los_data |>
    mod_principal_summary_los_table() |>
    gt::tab_options(table.align = "left")
}

# Generate LoS table (9.5)
summary_los_table <- make_summary_los_table() # overall
summary_los_table_gynae <- make_summary_los_table(results_type = "gynae")
summary_los_table_mat <- make_summary_los_table(maternity = TRUE)

# Generate waterfall ----

# Bespoke version of existing function
prepare_all_principal_change_factors <- function(
  r,
  site_codes = list(ip = NULL, op = NULL, aae = NULL) # character vectors
) {
  mitigators_lookup <- read_mitigators()
  atmpo_lookup <- read_atmpo()

  activity_types_long <- list("inpatients", "outpatients", "aae")
  activity_types_short <- list("ip", "op", "aae")

  pods <- purrr::map(
    activity_types_long,
    \(x) {
      atmpo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(pod) |>
        unique()
    }
  ) |>
    purrr::set_names(activity_types_short)

  # NEW: bespoke filter for maternity
  is_maternity <- isTRUE(getOption("maternity"))
  if (is_maternity) {
    pods <- list(ip = "ip_maternity_admission")
    activity_types_long <- list("inpatients")
    activity_types_short <- list("ip")
  }

  possibly_prep_principal_change_factors <-
    purrr::possibly(prep_principal_change_factors)

  principal_change_data <- purrr::map2(
    activity_types_short,
    pods,
    \(activity_type, pod) {
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = site_codes,
        mitigators = mitigators_lookup,
        at = activity_type,
        pods = pod
      )
    }
  ) |>
    purrr::set_names(activity_types_short)

  # Scenarios run under v1.0 don't have A&E data when filtering by site, so
  # provide results for whole-scheme level.
  if (r$params$app_version == "v1.0" & !is.null(site_codes[["aae"]])) {
    principal_change_data[["aae"]] <-
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = list(aae = NULL), # provide for whole scheme, not site
        mitigators = mitigators_lookup,
        at = "aae",
        pods = pods[["aae"]]
      )
  }

  principal_change_data
}

# Convenience function to generate plots and (un)set options
plot_waterfall <- function(
  results = r_primary,
  sites = site_codes,
  maternity = NULL # set maternity option
) {
  options(maternity = maternity)
  on.exit(options(maternity = NULL))

  cat(
    "* maternity option:",
    getOption("maternity", default = "none"),
    "\n"
  )

  prepare_all_principal_change_factors(results, site_codes) |>
    _$ip |> # only need inpatients
    mod_principal_change_factor_effects_summarised("beddays", TRUE) |>
    mod_principal_change_factor_effects_cf_plot()
}

# Generate waterfall chart (9.4)
waterfall <- plot_waterfall() # overall
waterfall_mat <- plot_waterfall(maternity = TRUE)

# Generate individual change-factor charts ----

# Bespoke version of existing function: use the same bespoke version of the
# prepare_all_principal_change_factors() used in the waterfall generation above.

# Convenience function to generate plots and (un)set options
make_icf_charts <- function(
  results = r_primary,
  sites = site_codes,
  maternity = NULL # set maternity option
) {
  options(maternity = maternity)
  on.exit(options(maternity = NULL))

  cat(
    "* maternity option:",
    getOption("maternity", default = "none"),
    "\n"
  )

  icf_plots <- prepare_all_principal_change_factors_plots(results, site_codes)

  # NEW: bespoke filter for maternity
  is_maternity <- isTRUE(getOption("maternity"))
  if (is_maternity) {
    icf_plots <- icf_plots[1:3] # restrict to inpatients plots
  }

  icf_plots
}

# Generate individual change-factor charts (8.2 to 8.6, maternity is only 8.2
# to 8.4, which are the inpatients ones)
plots_icf <- make_icf_charts() # overall
plots_icf_mat <- make_icf_charts(maternity = TRUE)

# Generate S-curve (ECDF) text ----

# The function below uses mod_model_results_distribution_get_data(), which is
# already part of the R/nhp_outputs/ scripts in this repo. That function uses
# get_model_run_distribution(), which we have a gynae/paeds version for in the
# beeswarms/S-curves section above

# Function adapted from a Shiny renderText and reactive in nhp_outputs
get_ecdf_quantiles_data <- function(data) {
  ecdf_fn <- stats::ecdf(data[["value"]])

  # Calculate x values for y-axis quantiles
  probs_pcnts <- c(0.1, 0.9)
  x_quantiles <- stats::quantile(ecdf_fn, probs = probs_pcnts)

  return(x_quantiles)
}

get_ecdf_quantiles <- function(data, site_codes, activity_type, pods, measure) {
  selected_measure <- list(activity_type, pods, measure)

  activity_type_short <-
    switch(activity_type, "inpatients" = "ip", "outpatients" = "op", "aae")
  site_codes <- site_codes[[activity_type_short]]

  aggregated_data <- data |>
    mod_model_results_distribution_get_data(selected_measure, site_codes)

  tibble::enframe(
    get_ecdf_quantiles_data(aggregated_data),
    name = "quant",
    value = "value"
  )
}

atmpo <- read_atmpo()
at <- "inpatients"
m <- "beddays"
pods <- atmpo |>
  dplyr::filter(
    # could also filter for pod == ip_maternity_admission only
    activity_type == at,
    measure == m
  ) |>
  dplyr::pull(pod)

get_ecdf_quantiles(r_primary, site_codes, at, pods, m)
options(results_type = "gynae")
get_ecdf_quantiles(r_primary, site_codes, at, pods, m)
options(results_type = "paeds")
get_ecdf_quantiles(r_primary, site_codes, at, pods, m)
options(results_type = NULL)
