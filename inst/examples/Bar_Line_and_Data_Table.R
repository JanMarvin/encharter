# Bar+line dashboard with a date-formatted X-axis (major every 2 months,
# minor every 1, format "mmm-yy"), Sales on the primary axis (steps of 100,
# format "#,##0") and Growth as a smooth line on the secondary axis
# (5% steps, range -10% to 30%). Data table is shown below the chart and the
# legend is hidden. Chart at E2:M20 on sheet "Dashboard".

bar_line_and_data_table <- function() {
  require(openxlsx2)
  require(encharter)

  data <- data.frame(
    Date   = as.Date("2025-01-01") + seq(0, 330, by = 30),
    Sales  = c(120, 150, 180, 170, 210, 250, 300, 280, 260, 310, 350, 400),
    Growth = c(0.02, 0.05, 0.08, -0.02, 0.10, 0.15, 0.20, -0.05, -0.02, 0.12, 0.10, 0.14)
  )

  wb <- wb_workbook()
  wb$add_worksheet("Dashboard")
  wb$add_data(sheet = "Dashboard", x = data)

  my_chart <- ec(type = "barChart")

  my_chart$add_series(
    name  = "Dashboard!$B$1",
    data  = "Dashboard!$B$2:$B$13",
    label = "Dashboard!$A$2:$A$13",
    color = "4F81BD",
    type  = "barChart"
  )

  my_chart$add_series(
    name      = "Dashboard!$C$1",
    data      = "Dashboard!$C$2:$C$13",
    label     = "Dashboard!$A$2:$A$13",
    type      = "lineChart",
    secondary = TRUE,
    color     = "C0504D",
    marker    = "circle",
    smooth    = TRUE
  )

  my_chart$set_chart_title("Annual Sales & Growth", bold = TRUE, font_size = 16)
  my_chart$set_x_title("Reporting Month")
  my_chart$set_y_title("Revenue (k)")
  my_chart$set_y2_title("Growth Rate (%)")

  my_chart$set_x_axis(
    major      = 2,
    major_time = "months",
    minor      = 1,
    minor_time = "months",
    format     = "mmm-yy"
  )

  my_chart$set_y_axis(major = 100, min = 0, format = "#,##0")
  my_chart$set_y2_axis(major = 0.05, min = -0.10, max = 0.30, format = "0%")

  my_chart$set_data_table(TRUE)
  my_chart$set_legend_style(pos = "none")

  chart_xml <- my_chart$render()
  wb$add_chart_xml(sheet = "Dashboard", xml = chart_xml, dims = "E2:M20")

  if (interactive()) wb_open(wb)
  invisible(wb)
}

bar_line_and_data_table()
