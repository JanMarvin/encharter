rm(list = ls())

library(openxlsx2)
library(encharter)


# 1. Initialize with Area Chart as the default base
combo_chart <- ec("areaChart")

# 2. Add two series for the "Clustered" effect
# These use 'barChart' which renders as Columns by default in OOXML XML
combo_chart$
  add_series(
    header = "Sheet1!$B$1",
    data   = "Sheet1!$B$2:$B$6",
    cat    = "Sheet1!$A$2:$A$6",
    color  = "4472C4",
    type   = "barChart"
  )$
  add_series(
    header = "Sheet1!$C$1",
    data   = "Sheet1!$C$2:$C$6",
    cat    = "Sheet1!$A$2:$A$6",
    color  = "A5A5A5",
    type   = "barChart"
  )

# 3. Add an Area Chart series as an overlay on the Secondary Axis
combo_chart$add_series(
  header    = "Sheet1!$D$1",
  data      = "Sheet1!$D$2:$D$6",
  color     = "70AD47",
  type      = "areaChart",
  secondary = TRUE
)

# 4. Final Polish - Using the refactored styling methods
combo_chart$
  set_chart_title("Inventory vs Market Trend", bold = TRUE, sz = 14)$
  set_legend_style(pos = "b", font_size = 10)$
  set_x_title("Months")$
  set_y_title("Inventory Level")$
  set_y2_title("Market Trend Index")$
  # Add some area styling for a professional look
  set_chart_style(line = wb_color(hex = "#D9D9D9"), line_width = 1)$
  set_plot_style(fill = wb_color("yellow"))

# 5. Create the Dataset and Workbook
chart_data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May"),
  Product_A = c(45, 52, 30, 48, 60),
  Product_B = c(25, 30, 45, 40, 35),
  Market_Trend = c(80, 85, 90, 100, 110)
)

wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = chart_data)

# 6. Add the chart to the workbook
# Note: Ensure your wb_add_chart wrapper passes the rendered XML to the drawing
wb <- wb |>
  wb_add_encharter(dims = "E2:M20", graph = combo_chart)

# Open in spreadsheet software
wb$open()
