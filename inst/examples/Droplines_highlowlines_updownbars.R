library(openxlsx2)
# library(encharter) # Assuming your Chart class is loaded

# 1. Prepare some sample data
# High-Low Lines and Up-Down Bars are most effective with 2+ series
df <- data.frame(
  Day   = paste("Day", 1:5),
  Open  = c(100, 110, 105, 120, 115),
  Close = c(115, 105, 125, 110, 130)
)

# 2. Create the Chart Object
ch <- Chart$new(type = "lineChart")

# Add the "Open" series
ch$add_series(
  header = "Sheet1!$B$1",
  data   = "Sheet1!$B$2:$B$6",
  cat    = "Sheet1!$A$2:$A$6",
  color  = "4F81BD" # Blue
)

# Add the "Close" series
ch$add_series(
  header = "Sheet1!$C$1",
  data   = "Sheet1!$C$2:$C$6",
  color  = "C0504D" # Red
)

# 3. Enable the new features we just added to the render_series_node
ch$drop_lines     <- TRUE   # Lines from points to the X-axis
ch$high_low_lines <- TRUE   # Vertical lines between the two series at each point
ch$up_down_bars   <- TRUE   # Shaded bars between the two series

# 4. Finalize the Workbook
wb <- wb_workbook() %>%
  wb_add_worksheet("Sheet1") %>%
  wb_add_data(x = df) %>%
  # Dimensions define where the chart sits in the Excel sheet
  wb_add_chart(dims = "E2:M20", chart_obj = ch)

# 5. Open to inspect
wb_open(wb)
