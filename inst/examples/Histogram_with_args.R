# Histogram via clusteredColumn (ChartEx) on 100 normally distributed values.
# Bin specification: binSize 10, left-closed intervals, with explicit underflow
# at 20 and overflow at 80 (binCount can be used instead of binSize).
# Chart at C2:J20.

histogram_with_args <- function() {
  require(openxlsx2)
  require(encharter)

  df_hist <- data.frame(Value = rnorm(100, 50, 10))

  ce <- ec("clusteredColumn")
  ce$add_series(
    name = "Distribution",
    data = "Sheet1!$A$2:$A$101",
    binning = list(
      binSize        = 10,
      intervalClosed = "left",
      underflow      = 20,
      overflow       = 80
    )
  )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = df_hist) |>
    wb_add_encharter(dims = "C2:J20", graph = ce)

  if (interactive()) wb$open()
  invisible(wb)
}

histogram_with_args()
