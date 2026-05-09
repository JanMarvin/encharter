# Demonstrates the difference between chart-area styling (set_chart_style:
# the outer ChartSpace, here a light grey EDF2F7 fill with a thick dark
# 2D3748 border) and plot-area styling (set_plot_style: just the inner plot
# rect, white with a thin CBD5E0 border). Single line series. Chart at E2:M20.

chart_and_plot_style <- function() {
  require(openxlsx2)
  require(encharter)

  chart <- ec("lineChart")
  chart$add_series(name = "S1!$B$1", data = "S1!$B$2:$B$5")

  chart$set_chart_style(
    fill       = "EDF2F7",
    line       = "2D3748",
    line_width = 2.3
  )

  chart$set_plot_style(
    fill       = "FFFFFF",
    line       = "CBD5E0",
    line_width = 1
  )

  xml <- as.character(chart$render())

  wb <- wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)$
    add_chart_xml(xml = xml, dims = "E2:M20")

  if (interactive()) wb$open()
  invisible(wb)
}

chart_and_plot_style()
