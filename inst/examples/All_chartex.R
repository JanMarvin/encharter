# Tour of the extended (ChartEx) chart types: waterfall, clusteredColumn
# (histogram-style), funnel, paretoLine, sunburst, treemap with custom labels,
# box-whisker with multi-series styling, and a regionMap (also re-used on a
# dedicated chart sheet). Built against mtcars and a small country dataset.

all_chartex <- function() {
  require(openxlsx2)
  require(encharter)

  my_wf <- ec("waterfall")
  my_wf$set_chart_title("Waterfall")$
    add_series("Data!$A$1", "Data!$A$2:$A$10", subtotals = FALSE)

  my_hist <- ec("clusteredColumn")
  my_hist$set_chart_title("Histogram")$
    add_series("Data!$B$1", "Data!$B$2:$B$30")

  my_funnel <- ec("funnel")
  my_funnel$set_chart_title("Sales Funnel")$
    add_series("Data!$C$1", "Data!$C$2:$C$6")

  my_pl <- ec("paretoLine")
  my_pl$set_chart_title("Pareto Line")$
    add_series("Data!$B$1", "Data!$B$2:$B$30")$
    set_y_axis(grid_lines = "dashed")

  my_sb <- ec("sunburst")
  my_sb$set_chart_title("Sunburst")$
    add_series("Data!$C$1", "Data!$C$2:$C$30", "Data!$A$2:$B$30",
               line_color = wb_color("white"))

  my_tm <- ec("treemap")
  my_tm$set_chart_title("treemap")$
    add_series("Data!$C$1", "Data!$C$2:$C$30", "Data!$B$2:$B$30")$
    set_data_label_style(
      show_val  = TRUE,
      pos       = "outEnd",
      font_size = 10,
      bold      = TRUE,
      color     = wb_color("white"),
      format    = "#,##0.0"
    )

  my_bw <- ec("boxWhisker")
  my_bw$
    set_chart_title("MPG Distribution",
                    font_size = 16, font_name = "Arial", bold = TRUE)$
    set_x_title("by Cylinder", font_size = 12, italic = TRUE)$
    set_y_title("Miles per Gallon", font_size = 12, font_name = "Calibri")$
    set_x_axis(font_size = 10, font_name = "Arial", bold = TRUE)$
    set_y_axis(font_size = 12, font_name = "Times New Roman",
               italic = TRUE, color = "000000", format = "0.0")$
    add_series(
      name       = '"Super Duper MPG"',
      data       = "Data!$A$2:$A$33",
      label      = "Data!$B$2:$B$33",
      color      = wb_color("magenta"),
      line_color = wb_color("black")
    )$
    add_series(
      name       = "Data!$C$1",
      data       = "Data!$C$2:$C$33",
      label      = "Data!$B$2:$B$33",
      color      = wb_color(hex = "FFA500"),
      line_color = wb_color("black")
    )$
    set_legend_style(
      pos       = "r",
      font_size = 15,
      bold      = TRUE,
      color     = wb_color(theme = "4")
    )

  wb <- wb_workbook()$add_worksheet("Data")$add_data(x = mtcars)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "A2:G12",   graph = my_wf)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "A13:G24",  graph = my_hist)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "H2:N12",   graph = my_funnel)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "H13:N24",  graph = my_bw)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "O2:U12",   graph = my_pl)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "V2:AB12",  graph = my_tm)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "O13:U24",  graph = my_sb)

  map_data <- data.frame(
    Country = c("United States", "Canada", "Mexico", "Brazil", "United Kingdom",
                "Germany", "France", "China", "Japan", "Australia", "India"),
    Sales_Volume = c(850, 420, 300, 510, 600, 720, 580, 950, 640, 310, 880),
    Growth_Rate  = c(0.05, 0.02, 0.08, 0.12, 0.03, 0.04, 0.01, 0.15, 0.02, 0.06, 0.18)
  )

  my_rm <- ec("regionMap")
  my_rm$set_chart_title("Region Map")$
    add_series(name  = "'Region Map'!$B$1",
               data  = "'Region Map'!$B$2:$B$12",
               label = "'Region Map'!$A$2:$A$12")

  wb$add_worksheet("Region Map")$add_data(x = map_data)
  wb <- wb_add_encharter(wb, sheet = "Data", dims = "V13:AB24", graph = my_rm)

  wb$add_chartsheet("WorldMap")
  wb <- wb_add_encharter(wb, sheet = "WorldMap", graph = my_rm)

  if (interactive()) wb$open()
  invisible(wb)
}

all_chartex()
