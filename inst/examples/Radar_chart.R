rm(list = ls())

library(openxlsx2)
library(encharter)

# 1. Create Radar Data
# Categories (Metrics) will become the circular 'spokes'
skill_data <- data.frame(
  Metric = c("Speed", "Reliability", "Comfort", "Safety", "Efficiency"),
  Model_X = c(90, 60, 85, 70, 50),
  Model_Y = c(60, 90, 50, 85, 80)
)

# 2. Setup a Standard Radar Chart
# Note: Line width and markers work just like in Line charts
radar_std <- ec()
radar_std$
  set_chart_title("Standard Radar: Model Comparison")$
  set_legend_style(pos = "b")$
  add_series(
    name = "Model X",
    label  = "Sheet1!$A$2:$A$6",
    data   = "Sheet1!$B$2:$B$6",
    color  = "4472C4",
    line_width = 2,
    marker = "circle",
    type = "radarChart"
  )$
  add_series(
    name = "Model Y",
    label  = "Sheet1!$A$2:$A$6",
    data   = "Sheet1!$C$2:$C$6",
    color  = "ED7D31",
    line_width = 2,
    marker = "square",
    type = "radarChart"
  )

# 3. Setup a Filled Radar Chart
# We set filled = TRUE in the first series to trigger <c:radarStyle val="filled"/>
radar_filled <- ec("radarChart")
radar_filled$
  set_chart_title("Filled Radar: Area View")$
  add_series(
    name = "Model X",
    label  = "Sheet1!$A$2:$A$6",
    data   = "Sheet1!$B$2:$B$6",
    color  = "4472C4",
    filled = TRUE
  )$
  add_series(
    name = "Model Y",
    label  = "Sheet1!$A$2:$A$6",
    data   = "Sheet1!$C$2:$C$6",
    color  = "ED7D31",
    filled = TRUE
  )

# 4. Build and Open
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = skill_data) |>
  # Add both charts to compare styles
  openxlsx2::wb_add_encharter(dims = "E2:L20", graph = radar_std) |>
  openxlsx2::wb_add_encharter(dims = "E22:L40", graph = radar_filled)

wb$open()
