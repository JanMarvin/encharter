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

my_wf <- ChartEx$new()
my_wf$set_chart_title("2024 Financial Performance")$
  set_x_numfmt("YYYY-MM-DD")$
  set_axis_params(axis = "x",
                  gap_width = 0,
                  major_tick = "out",
                  minor_tick = "cross")$
  set_axis_params(axis = "y",
                  grid_color = wb_color(theme = "3"),   # Subtle theme-based grid
                  grid_dash = "dot"
  )

# When you map it in ChartEx:
my_wf$add_series(
  header_range = "'MonthlyFlow'!$B$1",
  data_range   = "'MonthlyFlow'!$B$2:$B$7",
  cat_range    = "'MonthlyFlow'!$A$2:$A$7", # Dates are here
  type         = "waterfall"
)

wb <- wb_add_chartx(wb, sheet = "MonthlyFlow", chart_obj = my_wf)

wb$open()
