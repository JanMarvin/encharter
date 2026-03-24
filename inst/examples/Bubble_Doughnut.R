rm(list = ls())

library(openxlsx2)
library(encharter)


# 1. Create dummy data
chart_data <- data.frame(
  Category = c("Project A", "Project B", "Project C", "Project D"),
  Investment = c(10, 40, 25, 55),
  Profit = c(15, 35, 10, 60),
  Risk_Size = c(5, 20, 15, 10)
)

# 2. Setup Workbook
wb <- wb_workbook() |>
  wb_add_worksheet("Data") |>
  wb_add_data(x = chart_data)

# --- DOUGHNUT CHART ---
dn_chart <- ec("doughnutChart")
dn_chart$set_chart_title("Budget Allocation")$set_pie_options(hole_size = 60)
dn_chart$add_series(
  header = "Data!$B$1",
  cat    = "Data!$A$2:$A$5",
  data   = "Data!$B$2:$B$5"
)

# --- BUBBLE CHART ---
bb_chart <- ec("bubbleChart")
bb_chart$set_chart_title("Investment Analysis")
bb_chart$set_x_title("Investment")$set_y_title("Profit")
bb_chart$add_series(
  header = "Investment vs Profit",
  cat    = "Data!$A$2:$A$5",
  data   = "Data!$C$2:$C$5",
  z_data = "Data!$D$2:$D$5"
)

# 3. Add Charts to Workbook
# We add "dummy" charts first so openxlsx2 creates the XML structure we can overwrite
wb <- wb |>
  openxlsx2::wb_add_encharter(dims = "B2:K12", graph = dn_chart) |>
  openxlsx2::wb_add_encharter(dims = "B13:K25", graph = bb_chart)

# 5. Save and Open
wb$open()
# shell.exec("ComplexCharts.xlsx") # Uncomment to open immediately
