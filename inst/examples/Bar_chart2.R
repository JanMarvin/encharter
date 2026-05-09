# Combo chart with an areaChart base type and clustered bars on top, plus
# another area series on the secondary axis. Demonstrates set_chart_style()
# (light gray border) and set_plot_style() (yellow plot fill). Title bold,
# legend at bottom, separate axis-title text. Chart at E2:M20.

bar_chart2 <- function() {
  require(openxlsx2)
  require(encharter)

  combo_chart <- ec("areaChart")

  combo_chart$
    add_series(
      name  = "Sheet1!$B$1",
      data  = "Sheet1!$B$2:$B$6",
      label = "Sheet1!$A$2:$A$6",
      color = "4472C4",
      type  = "barChart"
    )$
    add_series(
      name  = "Sheet1!$C$1",
      data  = "Sheet1!$C$2:$C$6",
      label = "Sheet1!$A$2:$A$6",
      color = "A5A5A5",
      type  = "barChart"
    )

  combo_chart$add_series(
    name      = "Sheet1!$D$1",
    data      = "Sheet1!$D$2:$D$6",
    color     = "70AD47",
    type      = "areaChart",
    secondary = TRUE
  )

  combo_chart$
    set_chart_title("Inventory vs Market Trend", bold = TRUE, font_size = 14)$
    set_legend_style(pos = "b", font_size = 10)$
    set_x_title("Months")$
    set_y_title("Inventory Level")$
    set_y2_title("Market Trend Index")$
    set_chart_style(line = wb_color(hex = "#D9D9D9"), line_width = 1)$
    set_plot_style(fill = wb_color("yellow"))

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

bar_chart2()
