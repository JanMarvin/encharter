# Five-row financial bridge: gross revenue -> COGS -> opex -> tax ->
# other income -> net income, with row 5 (Net Income) marked as a subtotal
# via subtotals = 5.

waterfall <- function() {
  require(openxlsx2)
  require(encharter)

  waterfall_df <- data.frame(
    Category = c("Gross Revenue", "COGS", "Operating Exp", "Tax",
                 "Other Income", "Net Income"),
    Amount   = c(1200, -450, -300, -100, 50, 400)
  )

  wb <- wb_workbook()$
    add_worksheet("Data2")$
    add_data(x = waterfall_df)

  my_wf <- ec("waterfall")
  my_wf$set_chart_title("2024 Financial Performance")

  my_wf$add_series(
    name      = "Data2!$B$1",
    data      = "Data2!$B$2:$B$7",
    label     = "Data2!$A$2:$A$7",
    type      = "waterfall",
    subtotals = 5
  )

  wb <- wb_add_encharter(wb, sheet = "Data2", dims = "D2:L20", graph = my_wf)

  if (interactive()) wb$open()
  invisible(wb)
}

waterfall()
