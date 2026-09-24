# Save a chart as an Excel chart template (.crtx), read the template back
# and build a new chart from it, and apply the template to an existing chart.

chart_template <- function() {
  require(openxlsx2)
  require(encharter)

  wb <- wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Month = month.abb[1:6],
    Sales = c(120, 135, 128, 160, 175, 190),
    Cost  = c(80, 90, 85, 100, 110, 120)
  ))
  wd <- wb_data(wb)

  styled <- ec("bar")$
    set_chart_title("Sales and cost", bold = TRUE, font_color = "C00000")$
    set_y_axis(grid_lines = TRUE, grid_color = "EEEEEE", format = "#,##0")$
    set_chart_style(fill = "F7F7F7")$
    set_legend_style(pos = "b")$
    add_series(name = "Sales", data = wd, label = "Month", color = "2E4057")$
    add_series(name = "Cost", data = wd, label = "Month", color = "E84855", type = "line",
               marker = "circle", line_type = "dashed", secondary = TRUE)

  template <- tempfile(fileext = ".crtx")
  ec_to_crtx(styled, template)

  # a chart from the template: the series take the template's styling
  from_template <- ec_from_crtx(template)$
    set_chart_title("From the template")$
    add_series(name = "Sales", data = wd, label = "Month")$
    add_series(name = "Cost", data = wd, label = "Month")

  # the template applied to a plain chart
  plain <- ec("line")$
    set_chart_title("Template applied")$
    add_series(name = "Sales", data = wd, label = "Month")$
    add_series(name = "Cost", data = wd, label = "Month")
  plain$apply_crtx(template)

  wb$add_encharter(dims = "E2:L18", graph = styled)
  wb$add_encharter(dims = "E20:L36", graph = from_template)
  wb$add_encharter(dims = "N2:U18", graph = plain)

  if (interactive()) wb$open()
  invisible(wb)
}

chart_template()
