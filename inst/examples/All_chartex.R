rm(list = ls())

library(openxlsx2)
library(encharter)


# waterfall
my_wf <- ChartEx$new()
my_wf$set_chart_title("Waterfall")$
  add_series("Data!$A$1", "Data!$A$2:$A$10", type = "waterfall", subtotals = FALSE)

# histogram
my_hist <- ChartEx$new()
my_hist$set_chart_title("Histogram")$
  add_series("Data!$B$1", "Data!$B$2:$B$30", type = "clusteredColumn")

# funnel
my_funnel <- ChartEx$new()
my_funnel$set_chart_title("Sales Funnel")$
  add_series("Data!$C$1", "Data!$C$2:$C$6", type = "funnel")

# paretoLine
my_pl <- ChartEx$new()
my_pl$set_chart_title("Pareto Line")$
  add_series("Data!$B$1", "Data!$B$2:$B$30", type = "paretoLine")

# sunburst
my_sb <- ChartEx$new()
my_sb$set_chart_title("Sunburst")$
  add_series("Data!$C$1", "Data!$C$2:$C$30", "Data!$B$2:$B$30", type = "sunburst")

# treemap
my_tm <- ChartEx$new()
my_tm$set_chart_title("treemap")$
  add_series("Data!$C$1", "Data!$C$2:$C$30", "Data!$B$2:$B$30", type = "treemap")$
  set_data_label_style(
    show = TRUE,
    pos = "outEnd",       # or "ctr", "inEnd", "inBase"
    sz = 10,
    bold = TRUE,
    color = wb_color("white"),    # White labels if you have dark bars
    numfmt = "#,##0.0"    # Custom precision
  )

# box whisker plot
my_bw <- ChartEx$new()
my_bw$
  set_chart_title("MPG Distribution", sz = 16, font_name = "Arial", bold = TRUE)$
  set_x_title("by Cylinder", sz = 12, italic = TRUE)$
  set_y_title("Miles per Gallon", sz = 12, font_name = "Calibri")$
  set_x_axis(sz = 10, font_name = "Arial", bold = TRUE)$
  set_y_axis(sz = 12, font_name = "Times New Roman", italic = TRUE, color = "000000", numfmt = "0.0")$
  add_series(
    header = '"Super Duper MPG"!',
    data   = "Data!$A$2:$A$33",
    cat    = "Data!$B$2:$B$33",
    fill_color=wb_color("magenta"), line_color=wb_color("black"), type = "boxWhisker")$
  add_series(
    header = "Data!$C$1",
    data   = "Data!$C$2:$C$33",
    cat    = "Data!$B$2:$B$33",
    fill_color=wb_color(hex = "FFA500"), line_color=wb_color("black"), type = "boxWhisker")$
  set_legend_style(
    pos = "r",
    sz = 15,
    bold = TRUE,
    color = wb_color(theme = "5")
  )

wb <- wb_workbook()$add_worksheet("Data")$add_data(x = mtcars)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "A2:G12", chart_obj = my_wf)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "A13:G24", chart_obj = my_hist)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "H2:N12", chart_obj = my_funnel)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "H13:N24", chart_obj = my_bw)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "A25:G36", chart_obj = my_pl)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "H25:N36", chart_obj = my_sb)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "A37:G48", chart_obj = my_tm)


map_data <- data.frame(
  Country = c("United States", "Canada", "Mexico", "Brazil", "United Kingdom",
              "Germany", "France", "China", "Japan", "Australia", "India"),
  Sales_Volume = c(850, 420, 300, 510, 600, 720, 580, 950, 640, 310, 880),
  Growth_Rate = c(0.05, 0.02, 0.08, 0.12, 0.03, 0.04, 0.01, 0.15, 0.02, 0.06, 0.18)
)


# regionMap
## something is missing
my_rm <- ChartEx$new()
my_rm$set_chart_title("Region Map")$
  add_series(header = "'Region Map'!$B$1", data = "'Region Map'!$B$2:$B$12",
             cat = "'Region Map'!$A$2:$A$12", type = "regionMap")

wb$add_worksheet("Region Map")$add_data(x = map_data)
wb <- wb_add_chartx(wb, sheet = "Data", dims = "H37:N48", chart_obj = my_rm)

wb$add_chartsheet("WorldMap")
wb <- wb_add_chartx(wb, sheet = "WorldMap", chart_obj = my_rm)

if (interactive()) wb$open()
