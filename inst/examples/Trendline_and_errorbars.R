library(openxlsx2)
library(encharter) # Assuming your updated Chart class is here

# 1. Setup Data
df <- data.frame(
  Month = month.abb[1:6],
  Revenue = c(100, 120, 110, 150, 140, 170)
)

# 2. Initialize Standard Chart (NOT ChartEx)
ch <- Chart$new(type = "barChart")

# 3. Add Series with Error Bars and Trendline

ch$add_series(
  header = "Monthly Revenue",
  data = "Sheet1!$B$2:$B$7",
  cat = "Sheet1!$A$2:$A$7",
  type = "barChart",
  error_bars = list(
    type = "percentage",
    value = 10,
    color = "404040"
  ),
  trendline = list(
    type = "linear",
    color = "FF0000",
    show_r2 = FALSE
  )
)

# 4. Finalize Workbook
wb <- wb_workbook()  |>
  wb_add_worksheet("Sheet1")  |>
  wb_add_data(x = df)  |>
  wb_add_chart(dims = "D2:L20", chart_obj = ch)

wb_open(wb)
