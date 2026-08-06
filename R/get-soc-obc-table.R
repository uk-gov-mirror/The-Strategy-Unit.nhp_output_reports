

get_soc_obc_table <- function(soc_obc_data,soc_numeric_version,scenario_name_1,scenario_name_2)
{ #### build table comparing SOC with OBC ---
  tbl_soc_obc <- soc_obc_data |>
    dplyr::arrange(sort) |>
    # dplyr::mutate(baseline_adj.soc = baseline.soc + baseline_adjustment.soc,
    #               baseline_adj.obc = baseline.obc + baseline_adjustment.obc) |>
    dplyr::select(-c(measure, pod, sort, p90, obc_pp_var, obc_p90_var, baseline.soc,
                     baseline.obc, baseline_adjustment.soc, baseline_adjustment.obc,
                     covid_adjustment.soc, covid_adjustment.obc)) |>
    dplyr::relocate(heading) |>
    gt::gt(rowname_col = "heading") |>
    gt::fmt_number(
      columns = c(adj_baseline.soc, principal.soc, adj_baseline.obc, principal.obc),
      decimals = 0
    ) |>
    gt::fmt_percent(
      columns = c(cagr.soc, cagr.obc),
      decimals = 2
    ) |>
    gt::sub_missing(
      missing_text = "-"
    ) |>
    gt::tab_spanner(
      label = scenario_name_1,
    # Code for further change upon confirmation from schemes
    #  label = glue::glue("Scenario ",scenario_name_1),
      columns = c(adj_baseline.soc, principal.soc, cagr.soc)
    ) |>
    gt::tab_spanner(
      label = scenario_name_2,
   # Code for further change upon confirmation from schemes
    #  label = glue::glue("Scenario ",scenario_name_2),
      columns = c(adj_baseline.obc, principal.obc, cagr.obc)
    ) |>
    gt::cols_label(
      adj_baseline.soc = "Adjusted Baseline",
      principal.soc = "Principal",
      cagr.soc = "CAGR (%)",
      adj_baseline.obc = "Adjusted Baseline",
      principal.obc = "Principal",
      cagr.obc = "CAGR (%)"
    ) |>
    gt::tab_style_body(
      style = gt::cell_text(style = "italic"),
      targets = "row",
      values = c("Delivery admissions", "Delivery beddays"),
      extents = c("body", "stub")
    ) |>
    # gt::tab_footnote(
    #   footnote = "Not available in SOC scenario",
    #   locations = gt::cells_stub(
    #     c("Delivery admissions", "Delivery beddays", "SDEC attendances (type 5)")
    #   )
    # ) |>
    gt::tab_footnote(
      footnote = "Deliveries are a subset of maternity.",
      locations = gt::cells_stub(
        c("Delivery admissions", "Delivery beddays")
      )
    )  |>
    # gt::tab_footnote(
    #   footnote = "Activity assumed to be SDEC due to TPMAs. Care should be taken in interpretation as recording of activity is in flux.",
    #   locations = gt::cells_stub(
    #     c("SDEC attendances (type 5)")
    #   )
    # )|>
    gt::tab_source_note(
      source_note = "Note: CAGR is calculated using adjusted baseline values (applied baseline and COVID adjustments),
      so these figures may differ from baseline values shown in other tables."
    ) |>
    gt::opt_footnote_marks(marks = "numbers") |>
    gt_theme()

  # Code awaiting detail on the footnote when received
  #  gt::tab_footnote(
  #   footnote = "Note: In v1 Maternity Growth...",


  # if(soc_numeric_version<2.2){
  #   tbl_soc_obc <-
  #   tbl_soc_obc |>
  #     gt::tab_footnote(
  #       footnote = "Not available in SOC scenario",
  #       locations = gt::cells_stub(
  #         c("Regular Day Attender admissions")
  #       ))
  #     }
  tbl_soc_obc
}
