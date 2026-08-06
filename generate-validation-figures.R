### code to create tables for validation report:
## comparison of projections
## comparison of upper CI with principal projection
## comparison of estimated mitigated activity
purrr::walk(list.files("R", ".R$", , TRUE, TRUE), source)

scheme_code = "RGN" # add scheme_code for the scenario here to replace XYZ
# If the scheme has site codes already recorded or if all sites are required then set site_codes=NULL, otherwise set sites manually
scenario_name_1 <- "SOC"
scenario_name_2 <- "OBC"

site_codes = NULL
if (is.null(site_codes)) site_codes <- get_sites(scheme_code)
# site_codes = list( # change each element (each can be NULL to mean 'all')
site_codes = list( # change each element (each can be NULL to mean 'all')
  ip  = c("RGN90","J4I0D"),
  op  = c("RGN90","RGN95"),
  aae = NULL
)

# set up container for getting data via Azkit from Azure
container <- azkit::get_container(Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS")) ##### new code###

result_sets = get_nhp_result_sets()

final_report_ndg1 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="final_report_ndg1")

final_report_ndg2 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="final_report_ndg2")

validation_report_ndg2 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="validation_initial_ndg2")


validation_report_ndg3 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="validation_initial_ndg2")


opening_date_scenario <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="validation_initial_ndg2")


##### Get stepcounts via parquet using azkit where necessary #####

get_model_version <- function(scenario){
  scenario |> dplyr::pull(app_version)
}

model_version <- get_model_version(validation_report_ndg2)


if(model_version=="v5.2"){
  validation_report_ndg2_path <- result_sets |>
    dplyr::filter(dataset==scheme_code) |>
    dplyr::filter(run_stage=="validation_initial_ndg2") |>
    dplyr::pull(aggregated_results_path)

  validation_report_ndg2_azkit_stepcounts <- azkit::read_azure_parquet(
    container,
    paste0(validation_report_ndg2_path,"/step_counts.parquet")
  ) |>
    dplyr::filter(model_run>=1)
}

model_version <- get_model_version(validation_report_ndg3)

if(model_version=="v5.2"){
  validation_report_ndg3_path <- result_sets |>
    dplyr::filter(dataset==scheme_code) |>
    dplyr::filter(run_stage=="validation_initial_ndg2") |>
    dplyr::pull(aggregated_results_path)

  validation_report_ndg3_azkit_stepcounts <- azkit::read_azure_parquet(
    container,
    paste0(validation_report_ndg3_path,"/step_counts.parquet")
  ) |>
    dplyr::filter(model_run>=1)
}


model_version <- get_model_version(opening_date_scenario)


if(model_version=="v5.2"){
  opening_date_scenario_path <- result_sets |>
    dplyr::filter(dataset==scheme_code) |>
    dplyr::filter(run_stage=="validation_initial_ndg2") |>
    dplyr::pull(aggregated_results_path)

  opening_date_scenario_azkit_stepcounts <- azkit::read_azure_parquet(
    container,
    paste0(opening_date_scenario_path,"/step_counts.parquet")
  ) |>
    dplyr::filter(model_run>=1)
}

#### end of extra code#####

selected_results_list <- list(final_report_ndg1,
                              final_report_ndg2,
                              validation_report_ndg2,
                              validation_report_ndg3,
                              opening_date_scenario)


get_final_run_metadata_special <- function(scheme_code, selected_result_set) {

  scheme_results <- selected_result_set |> dplyr::filter(dataset == scheme_code)

  metadata_secondary <- dplyr::filter(scheme_results)
  metadata_primary <- dplyr::filter(scheme_results)

  dplyr::lst(metadata_secondary, metadata_primary)
}

meta <- purrr::map(
  selected_results_list,
  \(result) get_final_run_metadata_special(scheme_code, result)
)

r_final_report_ndg1 <- meta[[1]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _)

r_final_report_ndg2 <- meta[[2]]$metadata_primary |>
  dplyr::pull(file) |>
  get_nhp_results(file = _) #SOC

r_validation_report_ndg2 <- meta[[3]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _) #OBC

r_validation_report_ndg3 <- meta[[4]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _)

r_opening_date_scenario <- meta[[5]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _)

# in CAGR calc, assumes this raises to power of forecast period? Need to account for difference if using opening scenario
# time in years from baseline (23/24) to horizon (41/42 for usual)

get_years <- function(scenario){
  years_to_forecast <- as.numeric(
    scenario[["params"]][["end_year"]] - scenario[["params"]][["start_year"]]
  )
}

fc_period_soc <- get_years(r_final_report_ndg2)
fc_period_obc <- get_years(r_validation_report_ndg2)

get_soc_version <- function(scenario){
  soc_version <- scenario[["params"]][["app_version"]]
  soc_version
}

get_soc_major_version <- function(soc_version){
  soc_major_version <- as.numeric(
    stringr::str_extract(
      soc_version,
      "(?<=v)\\d(?=\\.\\d)" # '4' in e.g. v4.1
    ))
  soc_major_version
}

get_numeric_soc_version <- function(soc_version){
  as.numeric(sub(".", "", soc_version))
}

soc_version <- get_soc_version(r_final_report_ndg2)

soc_major_version <- get_soc_major_version(soc_version)
soc_numeric_version <- get_numeric_soc_version(soc_version)

# get the soc obc data
soc_obc_data <- get_soc_obc(r_final_report_ndg2, r_validation_report_ndg2, site_codes)

# get the soc obc table
soc_obc_table <- get_soc_obc_table(soc_obc_data,soc_numeric_version,scenario_name_1,scenario_name_2)

# get the cagr data
cagr_table <- get_validation_cagr_table(r_final_report_ndg2, r_validation_report_ndg2, site_codes,scenario_name_1,scenario_name_2)

#get the total mitigation table
total_miti_table <- get_total_mitigation_table(r_final_report_ndg2, r_validation_report_ndg2, site_codes,scenario_name_1,scenario_name_2) ###THIS ONE

# get the mitigation data
tpma_impact_table <- get_tpma_impact_table(r_final_report_ndg2, r_validation_report_ndg2, site_codes,scenario_name_1,scenario_name_2)

# get the p90 table
p90_table <-get_p90_table(soc_obc_data,soc_numeric_version,scenario_name_1,scenario_name_2)

# get soc obc obc_opening table for export to excel
save_soc_obc_open_data <- get_soc_obc_open(r_final_report_ndg2, r_validation_report_ndg2, r_opening_date_scenario, site_codes)

# get the bespoke s curve charts
save_bespoke_ecdf_plots <- get_bespoke_ecdf(r_final_report_ndg2, r_validation_report_ndg2, site_codes)

# get the additional risk table
ecdf_vals <- get_bespoke_ecdf_values(r_final_report_ndg2, r_validation_report_ndg2, site_codes)

# get the additional risk table for opening date scenario
ecdf_vals_open <- get_bespoke_ecdf_values(r_final_report_ndg2, r_opening_date_scenario, site_codes)

# get the details of the model runs featured in these outputs
scenarios_used_details <-  tibble::tibble(
  scheme = c(r_final_report_ndg2[["params"]][["dataset"]],r_validation_report_ndg2[["params"]][["dataset"]]),
  scenario_name = c(scenario_name_1,scenario_name_2),
  scenario = c(r_final_report_ndg2[["params"]][["scenario"]],r_validation_report_ndg2[["params"]][["scenario"]]),
  run_stage = c(meta[[2]][[2]][["run_stage"]],meta[[3]][[2]][["run_stage"]]),
  description = c("Final Report NDG2 Scenario", "Validation Report NDG2 Scenario"),
  baseline = c(glue::glue(r_final_report_ndg2[["params"]][["start_year"]],"/",r_final_report_ndg2[["params"]][["start_year"]]+1),
               glue::glue(r_validation_report_ndg2[["params"]][["start_year"]],"/",r_validation_report_ndg2[["params"]][["start_year"]]+1)),
  horizon = c(glue::glue(r_final_report_ndg2[["params"]][["end_year"]],"/",r_final_report_ndg2[["params"]][["end_year"]]+1),
              glue::glue(r_validation_report_ndg2[["params"]][["end_year"]],"/",r_validation_report_ndg2[["params"]][["end_year"]]+1)),
  model_version = c(r_final_report_ndg2[["params"]][["app_version"]],r_validation_report_ndg2[["params"]][["app_version"]])
)


