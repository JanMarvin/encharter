library(openxlsx2)

df_hist <- data.frame(Value = rnorm(100, 50, 10))

ce <- ec("clusteredColumn")
ce$add_series(
  header = "Distribution",
  data = "Sheet1!$A$2:$A$101",
  binning = list(
    binSize = 10, # or binCount = 10
    intervalClosed = "left",
    underflow = 20,
    overflow = 80
  )
)

wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = df_hist) |>
  wb_add_encharter(dims = "C2:J20", graph = ce)

if (interactive()) wb$open()
