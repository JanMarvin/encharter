library(openxlsx2)
library(encharter)

# 1. Setup Data
sales_data <- data.frame(
  Month = month.abb,
  Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
  Revenue = c(120, 115, 130, 125, 140, 135, 132, 126, 144, 180, 216, 240),
  Growth = c(0.02, 0.05, 0.08, -0.02, 0.10, 0.15, 0.20, -0.05, -0.02, 0.12, 0.10, 0.14)
)

project_data <- data.frame(
  Category = c("Project A", "Project B", "Project C", "Project D"),
  Investment = c(10, 40, 25, 55),
  Profit = c(15, 35, 10, 60),
  Risk = c(5, 20, 15, 10)
)

# 2. Create Workbook and add data
wb <- wb_workbook() |>
  wb_add_worksheet("Standard") |>
  wb_add_worksheet("Extended") |>
  wb_add_data(sheet = "Standard", x = sales_data) |>
  wb_add_data(sheet = "Standard", x = project_data, start_col = 6) |>
  wb_add_data(sheet = "Extended", x = sales_data)

# --- SHEET 1: STANDARD CHARTS ---

# A. Combo Bar & Line with Secondary Axis
combo_chart <- ec("barplot")$ # Using the new alias
  set_chart_title("Sales Volume vs Growth", bold = TRUE)$
  add_series(
    name = "Standard!$B$1", data = "Standard!$B$2:$B$13",
    label = "Standard!$A$2:$A$13", color = "4472C4"
  )$
  add_series(
    name = "Standard!$D$1", data = "Standard!$D$2:$D$13",
    type = "lineChart", secondary = TRUE, color = "C0504D", marker = "circle"
  )$
  set_legend_style(pos = "bottom")

# B. Doughnut Chart
dn_chart <- ec("doughnut")$
  set_chart_title("Investment Distribution")$
  set_pie_options(hole_size = 65)$
  add_series(
    name = "Standard!$G$1", data = "Standard!$G$2:$G$5",
    label = "Standard!$F$2:$F$5"
  )

# C. Bubble Chart
bb_chart <- ec("scatter")$
  set_chart_title("Risk vs Profit Analysis")$
  add_series(
    name = "Projects",
    label = "Standard!$G$2:$G$5",   # X = Investment
    data = "Standard!$H$2:$H$5",  # Y = Profit
    weight = "Standard!$I$2:$I$5" # Z = Risk
  )

# --- SHEET 2: EXTENDED CHARTS (ChartEx) ---

# D. Box Plot (Box & Whisker)
box_plot <- ec("boxplot")$ # Using the new alias
  set_chart_title("Revenue Distribution (Inclusive)")$
  add_series(
    name = "Revenue",
    data = "Extended!$C$2:$C$13",
    statistics = "inclusive"
  )

# E. Waterfall Chart
wf_chart <- ec("waterfall")$
  set_chart_title("Monthly Volume Shifts")$
  add_series(
    name = "Extended!$B$1",
    data = "Extended!$B$2:$B$10"
  )

# 3. Add all charts to Workbook
wb <- wb |>
  openxlsx2::wb_add_encharter(sheet = "Standard", dims = "A15:E30", graph = combo_chart) |>
  openxlsx2::wb_add_encharter(sheet = "Standard", dims = "G15:K30", graph = dn_chart) |>
  openxlsx2::wb_add_encharter(sheet = "Standard", dims = "L15:P30", graph = bb_chart) |>
  openxlsx2::wb_add_encharter(sheet = "Extended", dims = "E2:M18",  graph = box_plot) |>
  openxlsx2::wb_add_encharter(sheet = "Extended", dims = "E20:M36", graph = wf_chart)

# 4. Open or Save
if (interactive()) wb$open()
