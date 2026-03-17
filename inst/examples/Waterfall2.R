rm(list = ls())

library(openxlsx2)
library(encharter)

# Create data with dates
wf_dates <- data.frame(
  Date = seq(as.Date("2024-01-01"), by = "month", length.out = 6),
  Change = c(1000, -200, 150, -300, 400, 1050)
)

# In R, these are Date objects. When added to the workbook:
wb <- wb_workbook()$
  add_worksheet("MonthlyFlow")$
  add_data(x = wf_dates)

my_wf <- ec("waterfall")
my_wf$set_chart_title("2024 Financial Performance")$
  set_x_axis(format = "YYYY-MM-DD",
             major_tick = "out",
             minor_tick = "cross")$
  set_y_axis(grid_color = wb_color(theme = "3"),   # Subtle theme-based grid
             gridline = "dot"
  )

# When you map it in ChartEx:
my_wf$add_series(
  header = "'MonthlyFlow'!$B$1",
  data   = "'MonthlyFlow'!$B$2:$B$7",
  cat    = "'MonthlyFlow'!$A$2:$A$7", # Dates are here
  type         = "waterfall",
  gap_width = 0
)

wb <- wb_add_encharter(wb, sheet = "MonthlyFlow", graph = my_wf)

wb$open()
