# The reference charts whose rendering was compared with Excel; used by
# the snapshot tests. Each function returns the workbook holding the data
# and the list of charts.

reference_standard <- function() {

  df <- data.frame(
    Month = month.abb[1:8],
    A = c(5, 3, 8, 12, 6, 4, 9, 2),
    B = c(2, 4, 1, 6, 3, 5, 7, 1),
    N = c(5, -3, 8, 12, -6, 4, 9, 2),
    M = c(2, 4, -1, 6, 3, -5, 7, 1),
    D = as.Date("2024-01-01") + 0:7 * 7
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = df)$add_worksheet("Charts")
  wd <- openxlsx2::wb_data(wb, sheet = "Data")

  charts <- list(
    ec("area")$set_chart_title("01 Stacked area")$
      add_series(name = A, data = wd, label = Month, grouping = "stacked")$
      add_series(name = B, data = wd, label = Month, grouping = "stacked", color = "70AD47"),
    ec("area")$set_chart_title("02 Stacked area, negatives")$
      add_series(name = N, data = wd, label = Month, grouping = "stacked")$
      add_series(name = M, data = wd, label = Month, grouping = "stacked", color = "70AD47"),
    ec("line")$set_chart_title("03 Log axis, poly trend")$
      add_series(name = A, data = wd, label = Month, trendline = list(type = "poly", order = 3, color = "000000", forward = 1))$
      set_y_axis(log_base = 10, min = 1),
    ec("line")$set_chart_title("04 Date axis, auto major")$
      add_series(name = A, data = wd, label = D, marker = "circle")$
      set_x_axis(format = "dd.mm.yyyy", base_time = "days"),
    ec("line")$set_chart_title("05 Date axis, stacked %")$
      add_series(name = A, data = wd, label = D, grouping = "percentStacked")$
      add_series(name = B, data = wd, label = D, grouping = "percentStacked", color = "ED7D31")$
      set_x_axis(format = "dd.mm.yyyy", base_time = "days")$set_legend_style(pos = "t"),
    ec("bar")$set_chart_title("06 100% stacked bars, reversed")$
      add_series(name = A, data = wd, label = Month, grouping = "percentStacked")$
      add_series(name = B, data = wd, label = Month, grouping = "percentStacked", color = "A5A5A5")$
      set_x_axis(rev = TRUE)$set_y_axis(grid_lines = "dash", major_tick = "out"),
    ec("bar")$set_chart_title("07 Stacked %, negatives")$
      add_series(name = N, data = wd, label = Month, grouping = "percentStacked")$
      add_series(name = M, data = wd, label = Month, grouping = "percentStacked", color = "A5A5A5"),
    ec("scatter")$set_chart_title("08 Scatter smooth, secondary x")$
      add_series(name = A, data = wd, label = B, marker = "diamond", show_line = FALSE, color = "C00000")$
      add_series(name = B, data = wd, label = A, secondary = "x", smooth = TRUE, marker = "square", color = "7030A0")$
      set_x_title("B")$set_x2_title("A (top)"),
    ec("radar")$set_chart_title("09 Radar")$
      add_series(name = A, data = wd, label = Month, marker = "circle")$
      add_series(name = B, data = wd, label = Month, color = "ED7D31", filled = TRUE),
    ec("bar")$set_chart_title("10 Negative bars, labels low")$
      add_series(name = N, data = wd, label = Month)$
      add_series(name = M, data = wd, label = Month, color = "ED7D31")$
      set_x_axis(label_pos = "low", rotation = -45)$set_data_label_style(show_val = TRUE),
    ec("bar")$set_chart_title("11 Horizontal stacked, negatives")$
      add_series(name = N, data = wd, label = Month, dir = "bar", grouping = "stacked")$
      add_series(name = M, data = wd, label = Month, dir = "bar", grouping = "stacked", color = "ED7D31"),
    ec("line")$set_chart_title("12 Smooth + markers + trends")$
      add_series(name = N, data = wd, label = Month, smooth = TRUE, marker = "circle", trendline = list(type = "linear"))$
      add_series(name = M, data = wd, label = Month, color = "ED7D31", trendline = list(type = "movingAvg", period = 3)),
    ec("bar")$set_chart_title("13 Combo, secondary y")$
      add_series(name = A, data = wd, label = Month, gap_width = 80, overlap = -10)$
      add_series(name = B, data = wd, label = Month, type = "line", secondary = TRUE, color = "ED7D31",
                 line_type = "dashed", line_width = 2.5, marker = "diamond",
                 error_bars = list(type = "percentage", value = 10))$
      set_y2_title("B")$set_legend_style(pos = "b")$set_data_label_style(show_val = TRUE),
    ec("pie")$set_chart_title("14 Pie")$set_pie_options(rotation = 45, expansion = 10)$
      set_data_label_style(show_percent = TRUE, show_cat = TRUE, show_val = FALSE)$
      add_series(name = A, data = wd, label = Month, color = c("C00000", "4472C4", "70AD47", "FFC000", "7030A0", "00B0F0", "A5A5A5", "5B9BD5")),
    ec("doughnut")$set_chart_title("15 Doughnut")$set_pie_options(hole_size = 50, rotation = 90)$
      add_series(name = A, data = wd, label = Month)$set_legend_style(pos = "b"),
    ec("bubble")$set_chart_title("16 Bubble")$set_bubble_options(scale = 60)$
      add_series(name = A, data = wd, label = B, weight = A)
  )
  list(wb = wb, charts = charts)
}

reference_chartex <- function() {
  set.seed(1)

  steps <- data.frame(
    Item  = c("Start", "Sales", "Refunds", "Fees", "Bonus", "Mid", "Costs", "Tax", "End"),
    Value = c(300, 120, -80, -40, 60, 360, -200, -50, 110),
    Day   = as.Date("2024-01-01") + 0:8
  )
  groups <- data.frame(
    Group = rep(c("Alpha", "Beta", "Gamma"), each = 12),
    X     = round(c(rnorm(12, 20, 4), rnorm(12, 35, 8), c(rnorm(11, 50, 6), 95)), 1),
    Y     = round(c(rnorm(12, 25, 5), rnorm(12, 30, 5), rnorm(12, 40, 10)), 1)
  )
  values <- data.frame(Value = round(c(rnorm(60, 50, 12), 3, 97), 1))
  tree <- data.frame(
    Region  = c("North", "North", "North", "South", "South", "East", "East", "East", "West"),
    Country = c("DE", "DE", "DK", "IT", "ES", "PL", "PL", "CZ", "FR"),
    City    = c("Berlin", "Hamburg", "Odense", "Rome", "Madrid", "Warsaw", "Krakow", "Prague", "Paris"),
    Sales   = c(120, 80, 30, 90, 70, 60, 25, 40, 150)
  )
  funnel <- data.frame(
    Stage = c("Visits", "Signups", "Trials", "Paid", "Renewed"),
    Count = c(1000, 620, 340, 150, 90)
  )

  wb <- openxlsx2::wb_workbook()$
    add_worksheet("Data")$add_data(x = steps)$
    add_data(x = groups, dims = "E1")$
    add_data(x = values, dims = "I1")$
    add_data(x = tree, dims = "K1")$
    add_data(x = funnel, dims = "P1")$
    add_worksheet("Charts")

  charts <- list(
    ec("waterfall")$set_chart_title("01 Waterfall, subtotals, labels")$
      add_series(name = "Data!$B$1", data = "Data!$B$2:$B$10", label = "Data!$A$2:$A$10", subtotals = c(0, 5, 8))$
      set_data_label_style(show_val = TRUE),
    ec("waterfall")$set_chart_title("02 Waterfall, dates, no connectors")$
      add_series(name = "Data!$B$1", data = "Data!$B$2:$B$10", label = "Data!$C$2:$C$10", gap_width = 100,
                 visibility = list(connectorLines = FALSE))$set_legend_style(pos = "b"),
    ec("boxWhisker")$set_chart_title("03 Box, one series, inclusive")$
      add_series(name = "Data!$F$1", data = "Data!$F$2:$F$37", label = "Data!$E$2:$E$37", statistics = "inclusive"),
    ec("boxWhisker")$set_chart_title("04 Box, two series, exclusive, mean")$
      add_series(name = "Data!$F$1", data = "Data!$F$2:$F$37", label = "Data!$E$2:$E$37", statistics = "exclusive",
                 visibility = list(meanMarker = TRUE, meanLine = TRUE, nonoutliers = FALSE))$
      add_series(name = "Data!$G$1", data = "Data!$G$2:$G$37", label = "Data!$E$2:$E$37", statistics = "exclusive",
                 visibility = list(meanMarker = TRUE, meanLine = TRUE, nonoutliers = FALSE))$
      set_legend_style(pos = "r"),
    ec("boxWhisker")$set_chart_title("05 Box, no labels")$
      add_series(name = "Data!$I$1", data = "Data!$I$2:$I$63"),
    ec("clusteredColumn")$set_chart_title("06 Histogram, automatic bins")$
      add_series(name = "Data!$I$1", data = "Data!$I$2:$I$63"),
    ec("clusteredColumn")$set_chart_title("07 Histogram, size 10, under/overflow")$
      add_series(name = "Data!$I$1", data = "Data!$I$2:$I$63",
                 binning = list(binSize = 10, underflow = 30, overflow = 70))$set_data_label_style(show_val = TRUE),
    ec("clusteredColumn")$set_chart_title("08 Histogram, 5 bins, left closed")$
      add_series(name = "Data!$I$1", data = "Data!$I$2:$I$63", binning = list(binCount = 5, intervalClosed = "l")),
    ec("paretoLine")$set_chart_title("09 Pareto, size 15")$
      add_series(name = "Data!$I$1", data = "Data!$I$2:$I$63", binning = list(binSize = 15)),
    ec("funnel")$set_chart_title("10 Funnel")$
      add_series(name = "Data!$Q$1", data = "Data!$Q$2:$Q$6", label = "Data!$P$2:$P$6"),
    ec("treemap")$set_chart_title("11 Treemap, 3 levels, banner")$
      add_series(name = "Data!$N$1", data = "Data!$N$2:$N$10", label = "Data!$K$2:$M$10", parent_label = "banner"),
    ec("treemap")$set_chart_title("12 Treemap, 2 levels, overlapping, values")$
      add_series(name = "Data!$N$1", data = "Data!$N$2:$N$10", label = "Data!$L$2:$M$10", parent_label = "overlapping")$
      set_data_label_style(show_val = TRUE, show_cat = TRUE),
    ec("sunburst")$set_chart_title("13 Sunburst, 3 levels")$
      add_series(name = "Data!$N$1", data = "Data!$N$2:$N$10", label = "Data!$K$2:$M$10"),
    ec("sunburst")$set_chart_title("14 Sunburst, 2 levels, no legend")$
      add_series(name = "Data!$N$1", data = "Data!$N$2:$N$10", label = "Data!$K$2:$L$10")$set_legend_style(pos = "none"),
    ec("waterfall")$set_chart_title("15 Waterfall, negatives only")$
      add_series(name = "Data!$B$1", data = "Data!$B$4:$B$5", label = "Data!$A$4:$A$5"),
    ec("funnel")$set_chart_title("16 Funnel, labels")$
      add_series(name = "Data!$Q$1", data = "Data!$Q$2:$Q$6", label = "Data!$P$2:$P$6")$
      set_data_label_style(show_val = TRUE, show_cat = TRUE)
  )
  list(wb = wb, charts = charts)
}

reference_threed <- function() {

  stock <- data.frame(
    Date  = seq(as.Date("2020-01-21"), by = "day", length.out = 15),
    Open  = c(40.8, 41.3, 42.2, 41.3, 41.0, 41.9, 42.5, 43.0, 43.6, 44.0, 44.8, 45.0, 45.3, 45.0, 45.5),
    High  = c(42.30, 42.70, 42.97, 41.53, 42.03, 42.78, 43.08, 43.78, 44.11, 44.98, 45.09, 45.45, 45.23, 45.62, 45.31),
    Low   = c(40.29, 41.37, 41.00, 40.65, 41.33, 42.11, 42.73, 43.28, 43.86, 44.36, 44.75, 45.03, 44.86, 45.21, 44.91),
    Close = c(41.23, 42.21, 41.29, 41.03, 41.86, 42.53, 42.99, 43.62, 44.03, 44.75, 45.02, 45.33, 44.98, 45.48, 45.03)
  )
  regions <- data.frame(
    Region = c("North", "South", "East", "West", "Online", "Partner", "Export", "Other"),
    Sales  = c(48000, 31000, 26500, 22000, 9500, 4200, 2600, 1200),
    Costs  = c(31000, 24000, 19000, 17500, 8000, 3900, 2400, 1100)
  )
  months <- data.frame(
    Month   = month.abb[1:6],
    Revenue = c(1210, 1150, 1330, 1280, 1420, 1390),
    Target  = c(1200, 1200, 1300, 1300, 1400, 1400),
    Cost    = c(900, 950, 1000, 980, 1100, 1050)
  )
  surface <- data.frame(
    Y = c("Y1", "Y2", "Y3", "Y4", "Y5"),
    X1 = c(10, 20, 30, 20, 10), X2 = c(20, 40, 60, 40, 20), X3 = c(30, 60, 90, 60, 30),
    X4 = c(20, 40, 60, 40, 20), X5 = c(10, 20, 30, 20, 10)
  )

  wb <- openxlsx2::wb_workbook()$
    add_worksheet("Data")$add_data(x = stock)$
    add_data(x = regions, dims = "G1")$
    add_data(x = months, dims = "K1")$
    add_data(x = surface, dims = "P1")$
    add_worksheet("Charts")
  st <- openxlsx2::wb_data(wb, sheet = "Data", dims = "A1:E16")
  reg <- openxlsx2::wb_data(wb, sheet = "Data", dims = "G1:I9")
  mon <- openxlsx2::wb_data(wb, sheet = "Data", dims = "K1:N7")

  hlc <- function(title) {
    ch <- ec("stockChart")$set_chart_title(title)
    ch$add_series(data = st, label = Date, name = High, show_line = FALSE)
    ch$add_series(data = st, label = Date, name = Low, show_line = FALSE)
    ch$add_series(data = st, label = Date, name = Close, show_line = FALSE, marker = "dash", marker_size = 7)
    ch$set_x_axis(base_time = "days")
    ch
  }
  ohlc <- function(title) {
    ch <- ec("stockChart")$set_chart_title(title)
    ch$add_series(data = st, label = Date, name = Open, show_line = FALSE)
    ch$add_series(data = st, label = Date, name = High, show_line = FALSE)
    ch$add_series(data = st, label = Date, name = Low, show_line = FALSE)
    ch$add_series(data = st, label = Date, name = Close, show_line = FALSE)
    ch$set_x_axis(base_time = "days")
    ch
  }
  surf <- function(type, title) {
    ch <- ec(type)$set_chart_title(title)
    for (i in 2:6) ch$add_series(name = paste0("Data!$P$", i), data = paste0("Data!$Q$", i, ":$U$", i), label = "Data!$Q$1:$U$1", type = type)
    ch
  }

  charts <- list(
    {
      ch <- hlc("01 Stock HLC, high-low lines")
      ch$high_low_lines <- TRUE
      ch
    },
    {
      ch <- ohlc("02 Stock OHLC, up/down bars")
      ch$high_low_lines <- TRUE
      ch$up_down_bars <- TRUE
      ch
    },
    hlc("03 Stock HLC, nothing on"),
    ec("pieOfPie")$add_series(name = Sales, data = reg, label = Region)$
      set_of_pie_options(split_type = "pos", split_pos = 4, second_size = 65)$
      set_chart_title("04 Pie of pie, pos 4, size 65")$set_data_label_style(show_val = FALSE, show_percent = TRUE, format = "0.0%")$set_legend_style(pos = "b"),
    ec("pieOfPie")$add_series(name = Sales, data = reg, label = Region)$
      set_of_pie_options(split_type = "val", split_pos = 10000)$
      set_chart_title("05 Pie of pie, val < 10000, defaults")$set_legend_style(pos = "b"),
    ec("barOfPie")$add_series(name = Sales, data = reg, label = Region)$
      set_of_pie_options(split_type = "percent", split_pos = 10)$
      set_chart_title("06 Bar of pie, percent < 10")$set_data_label_style(show_val = TRUE, show_cat = TRUE)$set_legend_style(pos = "b"),
    ec("barOfPie")$add_series(name = Sales, data = reg, label = Region)$
      set_of_pie_options(split_type = "cust", split_pos = c(1, 3, 5, 7), second_size = 100)$
      set_chart_title("07 Bar of pie, custom 1,3,5,7, size 100")$set_legend_style(pos = "r"),
    ec("bar3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31")$
      set_chart_title("08 3D column, clustered, defaults"),
    ec("bar3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4", grouping = "standard")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31", grouping = "standard")$
      add_series(name = Cost, data = mon, label = Month, color = "A5A5A5", grouping = "standard")$
      set_chart_title("09 3D column, standard (series in depth)"),
    ec("bar3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4", grouping = "stacked", dir = "bar")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31", grouping = "stacked", dir = "bar")$
      set_3d_options(rot_x = 20, rot_y = 30, right_angle_axes = FALSE, perspective = 30)$
      set_chart_title("10 3D bar, stacked, rot 20/30, perspective 30"),
    ec("bar3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4", gap_width = 80)$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31", gap_width = 80)$
      set_3d_options(shape = "cylinder", gap_depth = 50)$
      set_chart_title("11 3D column, cylinder, gap 80/50")$set_data_label_style(show_val = TRUE),
    ec("line3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31")$
      set_chart_title("12 3D line, defaults"),
    ec("area3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "5B9BD5")$
      add_series(name = Target, data = mon, label = Month, color = "FFC000")$
      set_chart_title("13 3D area, defaults"),
    ec("pie3DChart")$add_series(name = Sales, data = reg, label = Region)$
      set_3d_options(rot_x = 40, h_percent = 60)$
      set_chart_title("14 3D pie, rot_x 40, h 60")$set_data_label_style(show_val = FALSE, show_percent = TRUE)$set_legend_style(pos = "r"),
    ec("pie3DChart")$add_series(name = Sales, data = reg, label = Region)$
      set_chart_title("15 3D pie, defaults")$set_legend_style(pos = "b"),
    surf("surfaceChart", "16 Surface (contour)"),
    surf("surface3DChart", "17 Surface 3D, defaults"),
    {
      ch <- surf("surface3DChart", "18 Surface 3D, wireframe, rot 30/60")
      for (i in seq_along(ch$series_data)) ch$series_data[[i]]$filled <- TRUE
      ch$set_3d_options(rot_x = 30, rot_y = 60)
      ch
    },
    ec("area3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "5B9BD5", grouping = "stacked")$
      add_series(name = Target, data = mon, label = Month, color = "FFC000", grouping = "stacked")$
      set_chart_title("19 3D area, stacked, rot 10/10")$set_3d_options(rot_x = 10, rot_y = 10),
    ec("bar3DChart")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4", grouping = "percentStacked")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31", grouping = "percentStacked")$
      set_3d_options(shape = "pyramid")$set_chart_title("20 3D column, percent stacked, pyramid")
  )
  list(wb = wb, charts = charts)
}

reference_options <- function() {

  months <- data.frame(Month = month.abb[1:8], Revenue = c(1210, 1150, 1330, 1280, 1420, 1390, 1500, 1610),
                       Target = c(1200, 1200, 1300, 1300, 1400, 1400, 1450, 1550), Cost = c(900, 950, 1000, 980, 1100, 1050, 1120, 1200))
  bubbles <- data.frame(X = c(1, 2, 3, 4, 5), Y = c(20, 35, 28, 50, 42), Size = c(5, 12, 8, 20, 15))
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = months)$add_data(x = bubbles, dims = "F1")$add_worksheet("Charts")
  mon <- openxlsx2::wb_data(wb, sheet = "Data", dims = "A1:D9")
  bub <- openxlsx2::wb_data(wb, sheet = "Data", dims = "F1:H6")

  charts <- list(
    ec("line")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4", marker = "circle")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31")$
      set_y_axis(cross_between = "midCat", grid_lines = TRUE)$set_x_axis(major_tick = "out")$
      set_chart_title("01 Line, axis crosses midCat"),
    ec("area")$add_series(name = Revenue, data = mon, label = Month, color = "5B9BD5")$
      set_y_axis(cross_between = "midCat")$set_chart_title("02 Area, midCat"),
    ec("bar")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31")$
      set_chart_title("03 Data table")$set_legend_style(pos = "none")$set_data_table(TRUE),
    ec("line")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      add_series(name = Cost, data = mon, label = Month, color = "A5A5A5")$
      set_chart_title("04 Data table with legend")$set_legend_style(pos = "b")$set_data_table(TRUE),
    ec("bar")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      set_data_label_style(show_val = TRUE, show_legend_key = TRUE, pos = "outEnd")$
      set_chart_title("05 Labels with legend key")$set_legend_style(pos = "b"),
    ec("bubble")$add_series(name = Y, data = bub, label = X, weight = Size, color = "4472C4")$
      set_data_label_style(show_val = FALSE, show_bubble_size = TRUE)$
      set_chart_title("06 Bubble size labels"),
    ec("scatter")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4", marker = "circle", show_line = FALSE,
                             trendline = list(type = "linear", intercept = 1000, show_eq = TRUE, forward = 2))$
      set_chart_title("07 Trend, intercept 1000, forward 2"),
    ec("line")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4",
                          trendline = list(type = "poly", order = 3, show_eq = TRUE, show_r2 = TRUE))$
      add_series(name = Cost, data = mon, label = Month, color = "A5A5A5", trendline = list(type = "exp", intercept = 800))$
      set_chart_title("08 Poly 3 and exp trend"),
    ec("bar")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      add_series(name = Target, data = mon, label = Month, color = "ED7D31")$
      set_legend_style(pos = "r", overlay = TRUE)$set_chart_title("09 Legend overlay right"),
    ec("line")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      set_legend_style(pos = "t", overlay = TRUE)$set_chart_title("10 Legend overlay top"),
    ec("bar")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4",
                         error_bars = list(type = "stdErr", direction = "both"))$
      add_series(name = Cost, data = mon, label = Month, color = "A5A5A5", error_bars = list(type = "percentage", value = 10, direction = "minus", color = "FF0000"))$
      set_chart_title("11 Error bars stdErr, percent minus"),
    ec("line")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4", marker = "diamond", marker_size = 9, line_type = "dash", line_width = 2.5)$
      add_series(name = Target, data = mon, label = Month, color = "70AD47", marker = "triangle", marker_size = 7, line_type = "sysDot", smooth = TRUE)$
      add_series(name = Cost, data = mon, label = Month, color = "7030A0", marker = "x", marker_size = 8, line_type = "lgDashDot")$
      set_chart_title("12 Line types, markers, smooth"),
    ec("bar")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      set_y_axis(disp_units = "thousands", format = "0.0", min = 0, max = 2000, major = 250, minor = 50, minor_tick = "out", minor_grid_lines = TRUE, log_base = NULL)$
      set_x_axis(label_pos = "high", rotation = -30)$set_chart_title("13 Display units, minor grid, labels high"),
    ec("line")$add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
      set_y_axis(log_base = 10, min = 100, max = 10000, rev = TRUE)$set_x_axis(crosses = "max")$
      set_chart_title("14 Log axis reversed, x crosses max"),
    ec("pie")$add_series(name = Revenue, data = mon, label = Month)$set_pie_options(rotation = 90, expansion = 10)$
      set_data_label_style(show_percent = TRUE, show_cat = TRUE, pos = "outEnd")$set_chart_title("15 Pie, angle 90, explosion 10, outEnd"),
    ec("doughnut")$add_series(name = Revenue, data = mon, label = Month)$add_series(name = Target, data = mon, label = Month)$
      set_pie_options(hole_size = 40)$set_chart_title("16 Doughnut, two rings, hole 40")$set_legend_style(pos = "r")
  )
  list(wb = wb, charts = charts)
}
