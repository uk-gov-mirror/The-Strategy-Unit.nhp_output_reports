
get_p90_table <- function(soc_obc,soc_numeric_version,scenario_name_1,scenario_name_2){
# pulls 90% CI from OBC and compares with SOC Principal Projection

tbl_soc_obc_p90 <- soc_obc |>
  dplyr::arrange(sort) |>
  dplyr::select(c(heading, principal.soc, principal.obc, obc_pp_var, p90, obc_p90_var)) |>
  dplyr::relocate(heading) |>
  gt::gt(rowname_col = "heading") |>
  gt::fmt_number(
    decimals = 0
  ) |>

  gt::sub_missing(
    missing_text = "-"
  ) |>

  gt::cols_label(
    principal.soc = glue::glue("{scenario_name_1} Principal"),
    principal.obc = glue::glue("{scenario_name_2} Principal"),
    obc_pp_var = glue::glue("{scenario_name_2} PP variation from {scenario_name_1} PP"),
    p90 = glue::glue("{scenario_name_2} P90"),
    obc_p90_var = glue::glue("{scenario_name_2} P90 variation from {scenario_name_1} PP")
  ) |>
  gt::tab_style_body(
    style = gt::cell_text(style = "italic"),
    targets = "row",
    values = c("Delivery admissions", "Delivery beddays"),
    extents = c("body", "stub")
  ) |>
  # footnote not working
  gt::tab_footnote(
    footnote = "Not available in SOC scenario",
    locations = gt::cells_stub(
      c("Delivery admissions", "Delivery beddays")
    )
  ) |>
  gt::tab_footnote(
    footnote = "Deliveries are a subset of maternity.",
    locations = gt::cells_stub(
      c("Delivery admissions", "Delivery beddays")
      )
    ) |>
  gt::opt_footnote_marks(marks = "numbers") |>
  gt_theme()

# if(soc_numeric_version<2.2){
#   tbl_soc_obc_p90 <-
#     tbl_soc_obc_p90 |>
#     gt::tab_footnote(
#       footnote = "Not available in SOC scenario",
#       locations = gt::cells_stub(
#         c("Regular Day Attender admissions")
#       ))
# }

tbl_soc_obc_p90
}
