get_validation_cagr_table <- function(soc_scenario, obc_scenario, site_codes,scenario_name_1,scenario_name_2){

  #### Issue 35 from nhp_output_reports ----

  # where they already exist, use figures from generate_values_list function
  values_list_soc <- data.frame(name = names(generate_values_list(soc_scenario, soc_scenario, site_codes)),
                                soc = unlist(generate_values_list(soc_scenario, soc_scenario, site_codes))) |>
    dplyr::filter(name %in% c("item_01", "item_02", "item_03", "item_04", "item_08", "item_10", "item_13", "item_14","item_16", "item_17", "item_78", "item_79")) |>
    dplyr::mutate(name = dplyr::case_match(
      name,
      "item_01" ~ "dg_ip_adm",
      "item_02" ~ "dg_ip_bd",
      "item_03" ~ "ndg_ip_adm",
      "item_04" ~ "ndg_ip_bd",
      "item_08" ~ "act_av_ip_bd",
      "item_10" ~ "eff_ip_bd",
      "item_13" ~ "act_av_op_app",
      "item_14" ~ "eff_op_app",
      "item_16" ~ "act_av_ae_att",
      "item_17" ~ "eff_ae_att",
      "item_78" ~ "net_gr_ip_adm",
      "item_79" ~ "net_gr_ip_bd"
    ),
    soc = as.numeric(soc))

  values_list_obc <- data.frame(name = names(generate_values_list(obc_scenario, obc_scenario, site_codes)),
                                obc = unlist(generate_values_list(obc_scenario, obc_scenario, site_codes))) |>
    dplyr::filter(name %in% c("item_01", "item_02", "item_03", "item_04", "item_08", "item_10", "item_13", "item_14","item_16", "item_17", "item_78", "item_79")) |>
    dplyr::mutate(name = dplyr::case_match(
      name,
      "item_01" ~ "dg_ip_adm",
      "item_02" ~ "dg_ip_bd",
      "item_03" ~ "ndg_ip_adm",
      "item_04" ~ "ndg_ip_bd",
      "item_08" ~ "act_av_ip_bd",
      "item_10" ~ "eff_ip_bd",
      "item_13" ~ "act_av_op_app",
      "item_14" ~ "eff_op_app",
      "item_16" ~ "act_av_ae_att",
      "item_17" ~ "eff_ae_att",
      "item_78" ~ "net_gr_ip_adm",
      "item_79" ~ "net_gr_ip_bd"
    ),
    obc = as.numeric(obc))

  # values not already created in 'generate_values_list' function
  ### SOC
  # DG & NDG for OP
  baseline <- get_stepcounts(soc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  baseline_adjustment <- get_stepcounts(soc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "baseline_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- get_stepcounts(soc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- get_stepcounts(soc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment <- get_stepcounts(soc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  dg_op <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_soc) - 1) * 100, digits = 2)
  ndg_op <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + non_demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_soc) - 1) * 100, digits = 2)

  # net growth for OP
  birth_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "birth_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  health_status_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "health_status_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  waiting_list_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "waiting_list_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  model_interaction_term <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "model_interaction_term") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_op <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_op <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  net_gr_op <- janitor::round_half_up((((baseline + baseline_adjustment + demographic_adjustment + non_demographic_adjustment + birth_adjustment +
                                           health_status_adjustment + covid_adjustment + waiting_list_adjustment + model_interaction_term +
                                           activity_avoidance_op + efficiencies_op)
                                        / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_soc) - 1) * 100, digits = 2)



  # DG & NDG for A&E
  baseline <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  baseline_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "baseline_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  dg_ae <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_soc) - 1) * 100, digits = 2)
  ndg_ae <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + non_demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_soc) - 1) * 100, digits = 2)

  # net growth A&E
  activity_avoidance_ae <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_ae <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()


  net_gr_ae <- janitor::round_half_up((((baseline + baseline_adjustment + demographic_adjustment + non_demographic_adjustment + birth_adjustment +
                                           health_status_adjustment + covid_adjustment + waiting_list_adjustment + model_interaction_term +
                                           activity_avoidance_ae + efficiencies_ae)
                                        / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_soc) - 1) * 100, digits = 2)


  new_rows <- data.frame(name = c("dg_ae", "ndg_ae", "net_gr_ae", "dg_op", "ndg_op", "net_gr_op"),
                         soc = c(dg_ae, ndg_ae, net_gr_ae, dg_op, ndg_op, net_gr_op))


  values_list_soc <- dplyr::bind_rows(values_list_soc, new_rows)



  ### OBC

  mod_info_downloads_reformat_step_counts <- function(dat) {
    dat |>
      dplyr::summarise(
        .by = -c(.data$model_run, .data$value),
        model_runs = list(.data$value),
        value = purrr::map_dbl(.data$model_runs, mean)
      )
  }


  # DG & NDG for OP
  baseline <- get_stepcounts(obc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  if(get_model_version(validation_report_ndg2)=="v5.2"){
    baseline <- mod_info_downloads_reformat_step_counts(validation_report_ndg2_azkit_stepcounts) |>
      dplyr::filter(activity_type == "op") |>
      dplyr::filter(change_factor == "baseline") |>
      filter_sites_conditionally(site_codes$op) |>
      dplyr::summarise(value = sum(value)) |>
      dplyr::pull()
      }

  baseline_adjustment <- get_stepcounts(obc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "baseline_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- get_stepcounts(obc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- get_stepcounts(obc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment <- get_stepcounts(obc_scenario) |>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  dg_op <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_obc) - 1) * 100, digits = 2)
  ndg_op <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + non_demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_obc) - 1) * 100, digits = 2)

  # net growth for OP
  birth_adjustment <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "birth_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  health_status_adjustment <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "health_status_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  waiting_list_adjustment <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "waiting_list_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  model_interaction_term <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "model_interaction_term") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  activity_avoidance_op <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_op <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "op") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()


  net_gr_op <- janitor::round_half_up((((baseline + baseline_adjustment + demographic_adjustment + non_demographic_adjustment + birth_adjustment +
                                           health_status_adjustment + covid_adjustment + waiting_list_adjustment + model_interaction_term +
                                           activity_avoidance_op + efficiencies_op)
                                        / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_obc) - 1) * 100, digits = 2)


  # DG & NDG for A&E
  baseline <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  if(get_model_version(validation_report_ndg2)=="v5.2"){
    baseline <- mod_info_downloads_reformat_step_counts(validation_report_ndg2_azkit_stepcounts) |>
      dplyr::filter(activity_type == "aae") |>
      dplyr::filter(change_factor == "baseline") |>
      filter_sites_conditionally(site_codes$aae) |>
      dplyr::summarise(value = sum(value)) |>
      dplyr::pull()
      }

  baseline_adjustment <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "baseline_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  demographic_adjustment <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "demographic_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  non_demographic_adjustment <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "non-demographic_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  dg_ae <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_obc) - 1) * 100, digits = 2)
  ndg_ae <- janitor::round_half_up((((baseline + baseline_adjustment + covid_adjustment + non_demographic_adjustment) / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_obc) - 1) * 100, digits = 2)

  # net growth A&E
  activity_avoidance_ae <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "activity_avoidance") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  efficiencies_ae <- get_stepcounts(obc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()


  net_gr_ae <- janitor::round_half_up((((baseline + baseline_adjustment + demographic_adjustment + non_demographic_adjustment + birth_adjustment +
                                           health_status_adjustment + covid_adjustment + waiting_list_adjustment + model_interaction_term +
                                           activity_avoidance_ae + efficiencies_ae)
                                        / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_obc) - 1) * 100, digits = 2)

  new_rows <- data.frame(name = c("dg_ae", "ndg_ae", "net_gr_ae", "dg_op", "ndg_op", "net_gr_op"),
                         obc = c(dg_ae, ndg_ae, net_gr_ae, dg_op, ndg_op, net_gr_op))


  values_list_obc <- dplyr::bind_rows(values_list_obc, new_rows)

 soc_title <- glue::glue("{scenario_name_1} Principal CAGR (%)")
 obc_title <- glue::glue("{scenario_name_2} Principal CAGR (%)")

  ## combine to form table
  tbl_impact <- dplyr::full_join(values_list_soc, values_list_obc) |>
    dplyr::mutate(name = dplyr::case_match(
      name,
      "dg_ip_adm" ~ "Demographic growth - IP admissions",
      "dg_ip_bd" ~ "Demographic growth - IP beddays",
      "ndg_ip_adm" ~ "Non-demographic growth - IP admissions",
      "ndg_ip_bd" ~ "Non-demographic growth - IP beddays",
      "act_av_ip_bd" ~ "Activity avoidance impact - IP beddays",
      "eff_ip_bd" ~ "Efficiency impact - IP beddays",
      "act_av_op_app" ~ "Activity avoidance impact - OP appointments",
      "eff_op_app" ~ "Efficiency impact - OP appointments",
      "act_av_ae_att" ~ "Activity avoidance impact - A&E attendances",
      "eff_ae_att" ~ "Efficiency impact - A&E attendances",
      "net_gr_ip_adm" ~ "Total net growth - IP admissions",
      "net_gr_ip_bd" ~ "total net growth - IP beddays",
      "dg_ae" ~ "Demographic growth - A&E attendances",
      "ndg_ae" ~ "Non-demographic growth - A&E appointments",
      "dg_op" ~ "Demographic growth - OP appointments",
      "ndg_op" ~ "Non-demographic growth - OP appointments",
      "net_gr_ae" ~ "Total net growth - A&E attendances",
      "net_gr_op" ~ "Total net growth - OP appointments"
    )) |>
    gt::gt(rowname_col = "name") |>
    gt::tab_row_group(
      label = "Outpatients",
      rows = c("Demographic growth - OP appointments",
               "Non-demographic growth - OP appointments",
               "Activity avoidance impact - OP appointments",
               "Efficiency impact - OP appointments",
               "Total net growth - OP appointments")
    )|>
    gt::tab_row_group(
      label = "A&E",
      rows = c("Demographic growth - A&E attendances",
               "Non-demographic growth - A&E appointments",
               "Activity avoidance impact - A&E attendances",
               "Efficiency impact - A&E attendances",
               "Total net growth - A&E attendances")
    ) |>
    gt::tab_row_group(
      label = "Inpatients",
      rows = c("Demographic growth - IP admissions",
               "Demographic growth - IP beddays",
               "Non-demographic growth - IP admissions",
               "Non-demographic growth - IP beddays",
               "Activity avoidance impact - IP beddays",
               "Efficiency impact - IP beddays",
               "Total net growth - IP admissions",
               "total net growth - IP beddays")
    ) |>
    gt::tab_source_note(
      source_note = "Note: CAGR calculations use baseline values adjusted for any applied baseline and COVID adjustments."
    ) |>
    gt::cols_label(
       soc = soc_title,
       obc = obc_title
    )  |>
    gt::fmt_percent(
      columns = c(soc, obc),
      scale_values = FALSE
    ) |>
    gt::sub_missing(
      missing_text = "-"
    ) |>
    gt_theme()

}
