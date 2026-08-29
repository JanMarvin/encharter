# nolint start: object_usage_linter.
# Showcase of the chart types and options added in 0.11: one workbook, one
# sheet per feature. Covers the previously missing EG_PlotAreaChoice types
# (ofPieChart as Pie of Pie and Bar of Pie, bar3DChart, line3DChart,
# pie3DChart, area3DChart, surface3DChart) plus the new niche options
# (tick_lbl_skip/tick_mark_skip, disp_units, extended data labels with a
# number format, error bar direction).

new_chart_types <- function() {
  require(openxlsx2)
  require(encharter)

  regions <- data.frame(
    Region = c("North", "South", "East", "West", "Online", "Partner", "Export", "Other"),
    Sales  = c(48000, 31000, 26500, 22000, 9500, 4200, 2600, 1200),
    Costs  = c(31000, 24000, 19000, 17500, 8000, 3900, 2400, 1100)
  )
  months <- data.frame(
    Month   = month.abb,
    Revenue = c(1210000, 1150000, 1330000, 1280000, 1420000, 1390000,
                1160000, 1080000, 1250000, 1540000, 1830000, 2050000),
    Target  = c(1200000, 1200000, 1300000, 1300000, 1400000, 1400000,
                1200000, 1200000, 1300000, 1500000, 1800000, 2000000)
  )

  wb <- wb_workbook() |>
    wb_add_worksheet("data") |>
    wb_add_data(x = regions, dims = "A1") |>
    wb_add_data(x = months, dims = "F1")

  dat <- wb_data(wb, sheet = "data", dims = "A1:C9")
  mon <- wb_data(wb, sheet = "data", dims = "F1:H13")

  # --- Pie of Pie -----------------------------------------------------------
  # The four smallest regions are split into the secondary pie
  # (split_type "pos" takes the last n points).
  pie_of_pie <- ec("pieOfPie")$
    add_series(name = Sales, data = dat, label = Region)$
    set_of_pie_options(split_type = "pos", split_pos = 4, second_size = 65)$
    set_chart_title("Sales by Region (Pie of Pie)")$
    set_data_label_style(show_val = FALSE, show_percent = TRUE, format = "0.0%")$
    set_legend_style(pos = "b")

  # --- Bar of Pie with a custom split ---------------------------------------
  # "barOfPie" preselects the bar subtype; split_type "cust" moves explicit
  # 0-based point indices into the secondary bar.
  bar_of_pie <- ec("barOfPie")$
    add_series(name = Sales, data = dat, label = Region)$
    set_of_pie_options(split_type = "cust", split_pos = c(4, 5, 6, 7))$
    set_chart_title("Sales by Region (Bar of Pie, custom split)")$
    set_legend_style(pos = "b")

  # --- 3D column chart ------------------------------------------------------
  # Cylinder shape, custom rotation, and a gap between the series rows.
  bar3d <- ec("bar3DChart")$
    add_series(name = Sales, data = dat, label = Region, color = "4472C4", gap_width = 120)$
    add_series(name = Costs, data = dat, label = Region, color = "ED7D31", gap_width = 120)$
    set_3d_options(rot_x = 20, rot_y = 30, shape = "cylinder", gap_depth = 80)$
    set_chart_title("Sales vs Costs (3D cylinders)")

  # --- 3D pie ---------------------------------------------------------------
  pie3d <- ec("pie3d")$
    add_series(name = Sales, data = dat, label = Region)$
    set_3d_options(rot_x = 40, h_percent = 60)$
    set_chart_title("Sales by Region (3D)")$
    set_legend_style(pos = "r")

  # --- 3D line and 3D area --------------------------------------------------
  line3d <- ec("line3d")$
    add_series(name = Revenue, data = mon, label = Month, color = "4472C4")$
    add_series(name = Target, data = mon, label = Month, color = "A5A5A5")$
    set_3d_options(rot_x = 25, rot_y = 15, gap_depth = 150)$
    set_chart_title("Revenue vs Target (3D lines)")

  area3d <- ec("area3d")$
    add_series(name = Revenue, data = mon, label = Month, color = "5B9BD5")$
    add_series(name = Target, data = mon, label = Month, color = "FFC000")$
    set_chart_title("Revenue vs Target (3D areas)")

  # --- 3D surface -----------------------------------------------------------
  # The 2D surfaceChart renders the top-down contour view; surface3DChart is
  # the actual 3D surface. h_percent/depth_percent stretch the box.
  surface3d <- ec("surface3d")$
    add_series(name = Sales, data = dat, label = Region)$
    add_series(name = Costs, data = dat, label = Region)$
    set_3d_options(rot_x = 20, rot_y = 40, h_percent = 100, depth_percent = 150)$
    set_chart_title("Sales/Costs surface")

  # --- New niche axis options on a plain 2D chart ---------------------------
  # Display units turn 1,210,000 into 1,210 with a "thousands" hint; every
  # second category label and every third tick mark is drawn; error bars only
  # point upwards (direction was previously ignored).
  niche <- ec("line")$
    add_series(name = Revenue, data = mon, label = Month, color = "steelblue",
               marker = "circle", marker_size = 6,
               error_bars = list(type = "percentage", value = 5, direction = "plus"))$
    set_x_axis(tick_lbl_skip = 2, tick_mark_skip = 3, major_tick = "out")$
    set_y_axis(disp_units = "thousands", format = "#,##0", grid_lines = TRUE)$
    set_chart_title("Revenue (display units, tick skips, plus-only error bars)")

  wb <- wb |>
    wb_add_worksheet("PieOfPie") |>
    wb_add_encharter(dims = "B2:J20", graph = pie_of_pie) |>
    wb_add_worksheet("BarOfPie") |>
    wb_add_encharter(dims = "B2:J20", graph = bar_of_pie) |>
    wb_add_worksheet("Bar3D") |>
    wb_add_encharter(dims = "B2:L22", graph = bar3d) |>
    wb_add_worksheet("Pie3D") |>
    wb_add_encharter(dims = "B2:J20", graph = pie3d) |>
    wb_add_worksheet("Line3D") |>
    wb_add_encharter(dims = "B2:L22", graph = line3d) |>
    wb_add_worksheet("Area3D") |>
    wb_add_encharter(dims = "B2:L22", graph = area3d) |>
    wb_add_worksheet("Surface3D") |>
    wb_add_encharter(dims = "B2:L22", graph = surface3d) |>
    wb_add_worksheet("NicheAxis") |>
    wb_add_encharter(dims = "B2:L20", graph = niche)

  if (interactive()) wb$open()
  invisible(wb)
}

new_chart_types()
# nolint end
