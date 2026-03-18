rm(list = ls())

library(openxlsx2)
library(encharter)


# waterfall
my_wf <- ec("waterfall")
my_wf$set_chart_title("Waterfall")$
  add_series("Data!$A$1", "Data!$A$2:$A$10", subtotals = FALSE)

# histogram
my_hist <- ec("clusteredColumn")
my_hist$set_chart_title("Histogram")$
  add_series("Data!$B$1", "Data!$B$2:$B$30")

# funnel
my_funnel <- ec("funnel")
my_funnel$set_chart_title("Sales Funnel")$
  add_series("Data!$C$1", "Data!$C$2:$C$6")

# paretoLine
my_pl <- ec("paretoLine")
my_pl$set_chart_title("Pareto Line")$
  add_series("Data!$B$1", "Data!$B$2:$B$30")$
  set_y_axis(gridlines = "dashed")

# sunburst
my_sb <- ec("sunburst")
my_sb$set_chart_title("Sunburst")$
  add_series("Data!$C$1", "Data!$C$2:$C$30", "Data!$A$2:$B$30", line_color = wb_color("white"))


# treemap
my_tm <- ec("treemap")
my_tm$set_chart_title("treemap")$
  add_series("Data!$C$1", "Data!$C$2:$C$30", "Data!$B$2:$B$30")$
  set_data_label_style(
    show = TRUE,
    pos = "outEnd",       # or "ctr", "inEnd", "inBase"
    sz = 10,
    bold = TRUE,
    color = wb_color("white"),    # White labels if you have dark bars
    format = "#,##0.0"    # Custom precision
  )

# box whisker plot
my_bw <- ec("boxWhisker")
my_bw$
  set_chart_title("MPG Distribution", sz = 16, name = "Arial", bold = TRUE)$
  set_x_title("by Cylinder", sz = 12, italic = TRUE)$
  set_y_title("Miles per Gallon", sz = 12, name = "Calibri")$
  set_x_axis(sz = 10, name = "Arial", bold = TRUE)$
  set_y_axis(sz = 12, name = "Times New Roman", italic = TRUE, color = "000000", format = "0.0")$
  add_series(
    header = '"Super Duper MPG"', # dont use strings with a "!"
    data   = "Data!$A$2:$A$33",
    cat    = "Data!$B$2:$B$33",
    color  = wb_color("magenta"), line_color = wb_color("black"))$
  add_series(
    header = "Data!$C$1",
    data   = "Data!$C$2:$C$33",
    cat    = "Data!$B$2:$B$33",
    color  = wb_color(hex = "FFA500"), line_color = wb_color("black"))$
  set_legend_style(
    pos   = "r",
    sz    = 15,
    bold  = TRUE,
    color = wb_color(theme = "4")
  )

wb <- wb_workbook()$add_worksheet("Data")$add_data(x = mtcars)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "A2:G12", graph = my_wf)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "A13:G24", graph = my_hist)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "H2:N12", graph = my_funnel)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "H13:N24", graph = my_bw)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "O2:U12", graph = my_pl)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "V2:AB12", graph = my_tm)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "O13:U24", graph = my_sb)


map_data <- data.frame(
  Country = c("United States", "Canada", "Mexico", "Brazil", "United Kingdom",
              "Germany", "France", "China", "Japan", "Australia", "India"),
  Sales_Volume = c(850, 420, 300, 510, 600, 720, 580, 950, 640, 310, 880),
  Growth_Rate = c(0.05, 0.02, 0.08, 0.12, 0.03, 0.04, 0.01, 0.15, 0.02, 0.06, 0.18)
)


# regionMap
## something is missing
my_rm <- ec("regionMap")
my_rm$set_chart_title("Region Map")$
  add_series(header = "'Region Map'!$B$1", data = "'Region Map'!$B$2:$B$12",
             cat = "'Region Map'!$A$2:$A$12")

wb$add_worksheet("Region Map")$add_data(x = map_data)
wb <- wb_add_encharter(wb, sheet = "Data", dims = "V13:AB24", graph = my_rm)

wb$add_chartsheet("WorldMap")
wb <- wb_add_encharter(wb, sheet = "WorldMap", graph = my_rm)

if (interactive()) wb$open()
