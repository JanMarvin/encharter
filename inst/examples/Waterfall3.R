rm(list = ls())

library(openxlsx2)
library(encharter)

# Create data with dates
wf_dates <- data.frame(
  Date = seq(as.Date("2024-01-01"), by = "month", length.out = 6),
  Change = c(1000, -200, 150, -300, 400, 1050)
)

# Initialize the chart
my_wf <- ChartEx$new()

# 1. Skinning the Series (Using the core color logic)
my_wf$add_series(
  header = "Data2!$B$1",
  data   = "Data2!$B$2:$B$7",
  cat    = "Data2!$A$2:$A$7",
  fill_color   = wb_color(theme = "5"), # Correctly maps to accent2
  line_color   = "000000",              # Solid black border
  type         = "waterfall"
)

# 2. Skinning the X-Axis (Category Axis)
my_wf$set_x_axis(
  sz = 11,
  font_name = "Segoe UI",
  bold = TRUE,
  color = wb_color(hex = "#444444"),
  major_tick = "out",   # Uses the new <cx:majorTickMarks type="out" />
  gap_width = 1.5       # Controls space between waterfall bars
)

# 3. Skinning the Y-Axis (Value Axis)
my_wf$set_y_axis(
  sz = 10,
  font_name = "Segoe UI",
  italic = TRUE,
  axis = "y",
  grid_color = wb_color(hex = "#D9D9D9"), # Subtle gridlines using render_color_core
  major_gridlines = TRUE,
  min = 0                # Forces baseline at 0
)

# 4. Skinning the Legend & Title
my_wf$set_chart_title("2024 Performance", sz = 14, bold = TRUE)$
  set_legend_style(pos = "b", sz = 10)$
  set_plot_style(
    fill       = wb_color("white"),
    line       = wb_color("black"),
    line_width = 1
  )

wb <- wb_workbook()$
  add_worksheet("Data2")$
  add_data(x = wf_dates)
wb <- wb_add_chartx(wb, sheet = "Data2", chart_obj = my_wf)

wb$open()
