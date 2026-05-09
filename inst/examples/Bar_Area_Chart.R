# Combo chart: two clustered bars (Product A, Product B) plus an area series
# (Market Trend, green 70AD47) on the secondary axis. Title in Times New Roman
# bold, legend at the bottom, italic X-axis font (Calibri), bold Y-axis font
# (Arial). Chart at E2:M20.

bar_area_chart <- function() {
  require(openxlsx2)
  require(encharter)

  combo_chart <- ec("barChart")

  combo_chart$add_series(
    name     = "Sheet1!$B$1",
    data     = "Sheet1!$B$2:$B$6",
    label    = "Sheet1!$A$2:$A$6",
    color    = "4472C4",
    type     = "barChart",
    grouping = "clustered"
  )

  combo_chart$add_series(
    name     = "Sheet1!$C$1",
    data     = "Sheet1!$C$2:$C$6",
    label    = "Sheet1!$A$2:$A$6",
    color    = "A5A5A5",
    type     = "barChart",
    grouping = "clustered"
  )

  combo_chart$add_series(
    name      = "Sheet1!$D$1",
    data      = "Sheet1!$D$2:$D$6",
    color     = "70AD47",
    type      = "areaChart",
    secondary = TRUE
  )

  combo_chart$set_chart_title("Inventory vs Market Trend",
                              font_name = "Times New Roman", bold = TRUE)
  combo_chart$set_legend_style(pos = "b", font_size = 10)
  combo_chart$set_x_title("Months")
  combo_chart$set_y_title("Values")
  combo_chart$set_y2_title("Also Values")
  combo_chart$set_x_axis(bold = FALSE, italic = TRUE,
                         font_name = "Calibri", font_size = 12)
  combo_chart$set_y_axis(bold = TRUE, italic = FALSE,
                         font_name = "Arial", font_size = 12)

  chart_data <- data.frame(
    Month        = c("Jan", "Feb", "Mar", "Apr", "May"),
    Product_A    = c(45, 52, 30, 48, 60),
    Product_B    = c(25, 30, 45, 40, 35),
    Market_Trend = c(80, 85, 90, 100, 110)
  )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = chart_data) |>
    wb_add_encharter(dims = "E2:M20", graph = combo_chart)

  if (interactive()) wb$open()
  invisible(wb)
}

bar_area_chart()
