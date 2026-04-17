stock_data <- data.frame(
  Date = seq(as.Date("2020-01-21"), as.Date("2020-02-04"), by = "day"),
  High = c(42.30, 42.70, 42.97, 41.53, 42.03, 42.78, 43.08, 43.78, 44.11, 44.98, 45.09, 45.45, 45.23, 45.62, 45.31),
  Low  = c(40.29, 41.37, 41.00, 40.65, 41.33, 42.11, 42.73, 43.28, 43.86, 44.36, 44.75, 45.03, 44.86, 45.21, 44.91),
  Close = c(41.23, 42.21, 41.29, 41.03, 41.86, 42.53, 42.99, 43.62, 44.03, 44.75, 45.02, 45.33, 44.98, 45.48, 45.03)
)


wb <- wb_workbook()$add_worksheet("Sheet1")$add_data(x = stock_data)

df <- wb_data(wb)

library(encharter)

stock_chart <- ec("stockChart")

# Add the 'High' series
stock_chart$add_series(
  data = df,
  label = Date,
  name = High,
  show_line = FALSE, marker = "circle"
)

# Add the 'Low' series
stock_chart$add_series(
  data = df,
  label = Date,
  name = Low,
  show_line = FALSE
)

# Add the 'Close' series
stock_chart$add_series(
  data = df,
  label = Date,
  name = Close,
  show_line = FALSE
)

stock_chart$set_x_axis(
  base_time = "days"
)

stock_chart

# Mimic stock behavior using High-Low lines
# In the Chart class, hi_low_lines is a logical field
stock_chart$high_low_lines <- TRUE

# Optional: Add drop lines if you want vertical lines to the X-axis
stock_chart$drop_lines <- FALSE

stock_chart$up_down_bars <- TRUE

# 4. Integrate with openxlsx2 (standard workflow)
wb$add_encharter(sheet = "Sheet1", graph = stock_chart)

wb$open()
