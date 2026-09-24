# Round trip: build a bar + line combo chart, save the workbook, load it
# again, read the chart back with ec_load(), append a month to the data and
# point the chart at the longer range. The loaded chart is placed next to the
# original so both can be compared.

load_and_extend <- function() {
  require(openxlsx2)
  require(encharter)

  sales <- data.frame(
    Month  = month.abb[1:6],
    Volume = c(1200, 1150, 1300, 1250, 1400, 1350),
    Sales  = c(12000, 11500, 13000, 12500, 14000, 13500)
  )

  wb <- wb_workbook()$add_worksheet("Sheet1")$add_data(x = sales)
  wd <- wb_data(wb)

  chart <- ec("bar")$
    set_chart_title("Sales vs Volume")$
    set_legend_style(pos = "b", font_size = 10)$
    set_y_axis(format = "#,##0")$
    add_series(name = "Volume", data = wd, label = "Month", color = "4472C4")$
    add_series(name = "Sales", data = wd, label = "Month", color = "ED7D31",
               type = "line", secondary = TRUE, line_width = 2.5, line_type = "dashed")$
    set_y2_title("Sales")

  wb$add_encharter(dims = "E2:M20", graph = chart)

  tmp <- tempfile(fileext = ".xlsx")
  wb$save(tmp)

  # --- later, in another session ---
  wb <- wb_load(tmp)

  wb$add_data(x = data.frame(Month = "Jul", Volume = 1100, Sales = 13200),
              dims = "A8", col_names = FALSE)

  chart <- ec_load(wb)
  print(chart)

  chart$update_series(data = wb_data(wb), label = "Month")
  chart$set_chart_title("Sales vs Volume (Jan-Jul)")

  wb$add_encharter(dims = "E22:M40", graph = chart)

  # preview without a spreadsheet application
  plot(chart)

  if (interactive()) wb$open()
  invisible(wb)
}

load_and_extend()
