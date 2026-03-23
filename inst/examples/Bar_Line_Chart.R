rm(list = ls())

library(openxlsx2)
library(encharter)

# 1. Create the dataset
sales_data <- data.frame(
  Month = month.abb,
  Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
  Sales = c(12000, 11500, 13000, 12500, 14000, 13500, 13200, 12600, 14400, 18000, 21600, 24000)
)

# 2. Setup the Chart Object
# Note: Ensure the sheet name in the string exactly matches the wb_add_worksheet name
my_chart <- ec("barChart") # Base is Bar for Volume
my_chart$
  set_chart_title("Sales vs Volume")$
  set_legend_style(pos = "b", sz = 10)$
  # Primary Axis: Volume (Bars)
  add_series(
    header = "Sheet1!$B$1",
    data   = "Sheet1!$B$2:$B$13",
    cat    = "Sheet1!$A$2:$A$13",
    color  = "4472C4",
    type   = "barChart"
  )$
  # Secondary Axis: Sales (Line)
  add_series(
    header = "Sheet1!$C$1",
    data   = "Sheet1!$C$2:$C$13",
    color  = "ED7D31",
    type   = "lineChart",
    line_width = 2.5,
    line_type  = "dashed",
    secondary = TRUE
  )

# 3. Build and Save
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = sales_data) |>
  openxlsx2::wb_add_encharter(dims = "E2:M20", graph = my_chart) # Pass my_chart, not chart

wb$open()

# wb_save(wb, "Sales_Performance.xlsx")
