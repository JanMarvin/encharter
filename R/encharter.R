# The 'Super' routing vectors

#' @noRd
ENCHARTER_STANDARD <- c(
  "barChart", "lineChart", "areaChart", "scatterChart",
  "pieChart", "doughnutChart", "radarChart", "bubbleChart",
  "stockChart", "surfaceChart"
)

#' @noRd
ENCHARTER_EXTENDED <- c(
  "waterfall", "sunburst", "treemap", "regionMap", "clusteredColumn", "funnel",
  "paretoLine", "boxWhisker"
)

#' Create an Encharter Chart
#'
#' @description
#' Factory function that initialises an R6 chart object. Returns a \code{Chart}
#' object for standard OOXML chart types (bar, line, scatter, …) or a
#' \code{ChartEx} object for modern extended chart types (waterfall, treemap, …).
#'
#' @param type A character string specifying the chart type. Common R-style
#'   aliases are accepted (see Details).
#'
#' @details
#' \strong{Supported Chart Types:}
#' \itemize{
#'   \item \bold{Bar/Column:} \code{"barChart"}, \code{"barplot"},
#'     \code{"hist"}, \code{"histogram"}
#'   \item \bold{Line/Area:} \code{"lineChart"}, \code{"line"},
#'     \code{"areaChart"}, \code{"area"}
#'   \item \bold{Scatter:} \code{"scatterChart"}, \code{"scatter"},
#'     \code{"point"}
#'   \item \bold{Pie/Doughnut:} \code{"pieChart"}, \code{"pie"},
#'     \code{"doughnutChart"}, \code{"doughnut"}
#'   \item \bold{Extended (ChartEx):} \code{"waterfall"}, \code{"treemap"},
#'     \code{"sunburst"}, \code{"regionMap"},
#'     \code{"boxWhisker"} / \code{"boxplot"}, \code{"funnel"}
#' }
#'
#' \strong{Bar vs Column direction:}
#' For bar/column charts, orientation is set via the \code{dir} argument in
#' \code{$add_series()}: \code{"col"} (vertical, default) or \code{"bar"}
#' (horizontal).
#'
#' @return An R6 object of class \code{Chart} or \code{ChartEx}.
#'
#' @examples
#' # Standard line chart
#' ec("lineChart")
#'
#' # Extended waterfall chart
#' ec("waterfall")
#'
#' # R-style alias
#' ec("barplot")
#'
#' @export
encharter <- function(type = "lineChart") {

  type <- normalize_encharter_type(type)
  match.arg(as.character(type), choices = c(ENCHARTER_STANDARD, ENCHARTER_EXTENDED))

  if (type %in% ENCHARTER_EXTENDED) {
    ec <- ChartEx$new(type = type)
  }

  if (type %in% ENCHARTER_STANDARD) {
    ec <- Chart$new(type = type)
  }

  ec
}

#' @rdname encharter
#' @export
ec <- encharter

#' Encharter Base R6 Class
#'
#' @description
#' Abstract base class inherited by \code{Chart} and \code{ChartEx}. Holds all
#' shared fields (palette, titles, axis params, legend/label settings) and the
#' shared private helpers (\code{render_color_core}, \code{render_color},
#' \code{set_axis_params}, \code{validate_input}).
#'
#' Users should not instantiate \code{EncharterBase} directly; use
#' \code{\link{encharter}()} instead.
#'
#' @useDynLib encharter, .registration=TRUE
#' @import R6
#' @importFrom openxlsx2 wb_color dims_to_dataframe read_xml fmt_txt
#  some XML functions from openxlsx2 are used but not imported because of name
#  clashes
EncharterBase <- R6::R6Class(
  "EncharterBase",
  public = list(
    #' @field xml The raw xml2 object containing the chart space.
    xml = NULL,
    #' @field series_data A list containing all added data series and their styles.
    series_data = list(),
    #' @field type The default chart type for the object (e.g., \code{"lineChart"}).
    type = NULL,
    #' @field palette A character vector of six-digit hex colors used for series
    #'   when no explicit color is supplied. Defaults to the standard Office theme
    #'   palette.
    palette = c("4472C4", "ED7D31", "A5A5A5", "FFC000", "5B9BD5", "70AD47"),

    #' @field chart_title Named list with elements \code{text} (character) and
    #'   \code{style} (list of font/fill/line options) for the main chart title.
    chart_title = list(text = NULL, style = list()),
    #' @field x_title Named list with elements \code{text} and \code{style} for
    #'   the primary X-axis title.
    x_title  = list(text = NULL, style = list()),
    #' @field y_title Named list with elements \code{text} and \code{style} for
    #'   the primary Y-axis title.
    y_title  = list(text = NULL, style = list()),

    #' @field chart_style Named list controlling the outer chart area:
    #'   \code{fill} (hex), \code{line} (hex), \code{line_width} (numeric).
    chart_style = list(fill = "FFFFFF", line = NULL, line_width = 1),
    #' @field plot_style Named list controlling the inner plot area:
    #'   \code{fill} (hex), \code{line} (hex), \code{line_width} (numeric).
    plot_style  = list(fill = NULL, line = NULL, line_width = 1),

    #' @field label_params Named list of global data label defaults:
    #'   \code{show_val}, \code{show_cat}, \code{show_legend_key} (logicals),
    #'   \code{pos} (character), \code{style} (list).
    label_params  = list(show_val = FALSE, show_cat = FALSE, show_legend_key = FALSE, pos = "t", style = list()),
    #' @field legend_params Named list of legend defaults:
    #'   \code{pos} (character), \code{overlay} ("0"/"1"), \code{style} (list).
    legend_params = list(pos = "r", overlay = "0", style = list()),

    #' @field axis_params Named list with one entry per axis (\code{x}, \code{x2},
    #'   \code{y}, \code{y2}). Each entry is a named list of scaling, formatting,
    #'   and style parameters. Modified via \code{$set_x_axis()}, etc.
    axis_params = list(
      x  = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rotation =  NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      x2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rotation =  NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      y  = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rotation =  NULL, grid_color = "D9D9D9", gridlines = TRUE,  minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      y2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rotation =  NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo")
    ),

    #' @description Set the chart's main title.
    #' @param text Title string. Accepts a plain character or an
    #'   \code{openxlsx2::fmt_txt()} object for rich-text formatting.
    #' @param font_size Numeric font size in points (e.g. \code{14}).
    #' @param font_name Font typeface name (e.g. \code{"Arial"}).
    #' @param font_color Six-digit hex color for the title text (e.g.
    #'   \code{"FF0000"} for red).
    #' @param bold Logical; \code{TRUE} renders the title in bold.
    #' @param italic Logical; \code{TRUE} renders the title in italics.
    #' @param fill Six-digit hex color for the title background box.
    #' @param line Six-digit hex color for the title border.
    #' @param line_width Numeric border width in points.
    #' @examples
    #' ec("line")$set_chart_title("Monthly Sales", font_size = 14, bold = TRUE)
    set_chart_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL, bold = NULL, italic = NULL, fill = NULL, line = NULL, line_width = NULL) {
      if (!inherits(text, "fmt_txt")) text <- private$sanitize_xml(text)
      self$chart_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
      invisible(self)
    },

    #' @description Set the primary X-axis title.
    #' @param text Title string.
    #' @param font_size Numeric font size in points.
    #' @param font_name Font typeface name.
    #' @param font_color Six-digit hex color for the title text.
    #' @param bold Logical.
    #' @param italic Logical.
    #' @param fill Six-digit hex color for the title background box.
    #' @param line Six-digit hex color for the title border.
    #' @param line_width Numeric border width in points.
    #' @examples
    #' ec("line")$set_x_title("Month", font_color = "888888", italic = TRUE)
    set_x_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL, bold = NULL, italic = NULL, fill = NULL, line = NULL, line_width = NULL) {
      if (!inherits(text, "fmt_txt")) text <- private$sanitize_xml(text)
      self$x_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
      invisible(self)
    },

    #' @description Set the primary Y-axis title.
    #' @param text Title string.
    #' @param font_size Numeric font size in points.
    #' @param font_name Font typeface name.
    #' @param font_color Six-digit hex color for the title text.
    #' @param bold Logical.
    #' @param italic Logical.
    #' @param fill Six-digit hex color for the title background box.
    #' @param line Six-digit hex color for the title border.
    #' @param line_width Numeric border width in points.
    #' @examples
    #' ec("line")$set_y_title("Revenue (USD)", bold = TRUE)
    set_y_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL, bold = NULL, italic = NULL, fill = NULL, line = NULL, line_width = NULL) {
      if (!inherits(text, "fmt_txt")) text <- private$sanitize_xml(text)
      self$y_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
      invisible(self)
    },

    #' @description Set primary X-axis scaling, tick marks, and label formatting.
    #' @param min,max Numeric axis limits.
    #' @param major,minor Numeric major/minor unit intervals. For date axes, unit
    #'   is set by \code{major_time}/\code{minor_time}.
    #' @param major_time,minor_time Time unit for major/minor steps on date axes:
    #'   \code{"days"}, \code{"months"}, or \code{"years"}.
    #' @param base_time Base time unit for date axes: \code{"days"},
    #'   \code{"months"}, or \code{"years"}.
    #' @param major_tick,minor_tick Tick mark style: \code{"cross"},
    #'   \code{"in"}, \code{"out"}, or \code{"none"}.
    #' @param format Number or date format string (e.g. \code{"#,##0"},
    #'   \code{"yyyy-mm-dd"}).
    #' @param log_base Numeric base for logarithmic scaling (e.g. \code{10}).
    #' @param color Six-digit hex color for the axis line.
    #' @param font_color Six-digit hex color for axis tick labels. Defaults to
    #'   \code{color} when not set.
    #' @param font_size Numeric label font size in points.
    #' @param font_name Font typeface name for tick labels.
    #' @param bold,italic Logical font style for tick labels.
    #' @param rotation Numeric label rotation in degrees.
    #' @param grid_color,minor_grid_color Six-digit hex colors for major/minor
    #'   gridlines.
    #' @param gridlines,minor_gridlines Show gridlines. \code{TRUE}/\code{FALSE}
    #'   to toggle; or a dash style string (\code{"dash"}, \code{"dot"},
    #'   \code{"dashDot"}, etc.) to show styled lines.
    #' @param line_width,grid_width,minor_grid_width Numeric widths in points for
    #'   the axis line, major gridlines, and minor gridlines respectively.
    #' @param cross_between Where the value axis crosses: \code{"between"}
    #'   (default, between categories) or \code{"midCat"} (through categories).
    #' @param crosses Where this axis crosses its perpendicular axis:
    #'   \code{"autoZero"} (default), \code{"min"}, or \code{"max"}.
    #' @param crosses_at Numeric axis value at which to cross. Overrides
    #'   \code{crosses} when supplied.
    #' @param label_pos Tick label position: \code{"nextTo"} (default),
    #'   \code{"high"}, \code{"low"}, or \code{"none"}.
    #' @examples
    #' ec("line")$set_x_axis(
    #'   min = 0, max = 12,
    #'   major_tick = "out",
    #'   gridlines  = TRUE,
    #'   font_color = "666666",
    #'   rotation   = -45
    #' )
    set_x_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                          major_time = NULL, minor_time = NULL, base_time = NULL,
                          major_tick = NULL, minor_tick = NULL,
                          format = NULL, log_base = NULL, color = NULL,
                          font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                          font_color = NULL, rotation =  NULL,
                          grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL, cross_between = NULL,
                          line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                          crosses = NULL, crosses_at = NULL, label_pos = NULL) {
        private$set_axis_params(
          "x",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rotation = rotation, grid_color = grid_color, gridlines = gridlines,
          minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos
        )
    },

    #' @description Set primary Y-axis scaling, tick marks, and label formatting.
    #' @param min,max Numeric axis limits.
    #' @param major,minor Numeric major/minor unit intervals.
    #' @param major_time,minor_time Time unit for date axes: \code{"days"},
    #'   \code{"months"}, or \code{"years"}.
    #' @param base_time Base time unit for date axes.
    #' @param major_tick,minor_tick Tick mark style: \code{"cross"},
    #'   \code{"in"}, \code{"out"}, or \code{"none"}.
    #' @param format Number format string.
    #' @param log_base Numeric base for logarithmic scaling.
    #' @param color Six-digit hex color for the axis line.
    #' @param font_color Six-digit hex color for axis tick labels.
    #' @param font_size Numeric label font size in points.
    #' @param font_name Font typeface name.
    #' @param bold,italic Logical font style.
    #' @param rotation Numeric label rotation in degrees.
    #' @param grid_color,minor_grid_color Hex colors for major/minor gridlines.
    #' @param gridlines,minor_gridlines \code{TRUE}/\code{FALSE} or a dash style
    #'   string.
    #' @param line_width,grid_width,minor_grid_width Numeric widths in points.
    #' @param cross_between \code{"between"} or \code{"midCat"}.
    #' @param crosses \code{"autoZero"}, \code{"min"}, or \code{"max"}.
    #' @param crosses_at Numeric crossing value; overrides \code{crosses}.
    #' @param label_pos \code{"nextTo"}, \code{"high"}, \code{"low"}, or
    #'   \code{"none"}.
    #' @examples
    #' ec("bar")$set_y_axis(
    #'   min        = 0,
    #'   max        = 1000,
    #'   major      = 200,
    #'   format     = "#,##0",
    #'   gridlines  = TRUE,
    #'   grid_color = "DDDDDD"
    #' )
    set_y_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                          major_time = NULL, minor_time = NULL, base_time = NULL,
                          major_tick = NULL, minor_tick = NULL,
                          format = NULL, log_base = NULL, color = NULL,
                          font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                          font_color = NULL, rotation =  NULL,
                          grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL, cross_between = NULL,
                          line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                          crosses = NULL, crosses_at = NULL, label_pos = NULL) {
        private$set_axis_params(
          "y",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rotation = rotation, grid_color = grid_color, gridlines = gridlines,
          minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos
        )
    },

    #' @description Configure global data label defaults for all series.
    #'
    #' Per-series overrides can be set via the \code{show_val}/\code{show_cat}
    #' arguments in \code{$add_series()}.
    #'
    #' @param show_val Logical; show the data point value. Default \code{TRUE}.
    #' @param show_cat Logical; show the category name. Default \code{FALSE}.
    #' @param show_legend_key Logical; show the series color swatch next to each
    #'   label. Default \code{FALSE}.
    #' @param pos Label position: \code{"t"} (top, default), \code{"b"}
    #'   (bottom), \code{"l"}, \code{"r"}, \code{"ctr"}, \code{"inEnd"},
    #'   \code{"outEnd"}, \code{"bestFit"}.
    #' @param ... Additional font style arguments passed to the label text
    #'   properties (e.g. \code{font_size}, \code{font_color}, \code{bold}).
    #' @examples
    #' ec("bar")$set_data_label_style(show_val = TRUE, pos = "outEnd", font_size = 9)
    set_data_label_style = function(show_val = TRUE, show_cat = FALSE, show_legend_key = FALSE, pos = "t", ...) {
      pos <- normalize_encharter_string(pos)
      pos <- private$validate_input(pos, c("t", "b", "l", "r", "ctr", "inEnd", "outEnd", "bestFit", "none"), "pos")
      self$label_params <- list(show_val = show_val, show_cat = show_cat, show_legend_key = show_legend_key, pos = pos, style = list(...))
      invisible(self)
    },

    #' @description Configure the chart legend.
    #' @param pos Legend position: \code{"t"}, \code{"b"}, \code{"l"},
    #'   \code{"r"} (default), or \code{"none"} to hide.
    #' @param align Legend alignment relative to the chart: \code{"ctr"}
    #'   (default), \code{"min"}, or \code{"max"}.
    #' @param overlay Logical; if \code{TRUE} the legend overlaps the plot area.
    #' @param font_size Numeric font size in points.
    #' @param font_name Font typeface name.
    #' @param bold,italic Logical font style.
    #' @param color Six-digit hex color for the legend text.
    #' @examples
    #' ec("line")$set_legend_style(pos = "b", font_size = 9)
    set_legend_style = function(pos = "t", align = "ctr", overlay = FALSE, font_size = NULL, font_name = NULL, bold = NULL, italic = NULL, color = NULL) {
      pos <- normalize_encharter_string(pos)
      align <- normalize_encharter_string(align)
      self$legend_params <- list(pos = pos, align = align, overlay = ifelse(overlay, "1", "0"),
                                 style = list(font_size = font_size, font_name = font_name, bold = bold, italic = italic, color = color))
      invisible(self)
    },

    #' @description Style the outer chart area (background and border).
    #' @param fill Six-digit hex color for the chart background.
    #'   Default \code{"FFFFFF"}.
    #' @param line Six-digit hex color for the chart border. \code{NULL} for no
    #'   border.
    #' @param line_width Numeric border width in points. Default \code{1}.
    #' @examples
    #' ec("bar")$set_chart_style(fill = "F5F5F5", line = "CCCCCC", line_width = 0.5)
    set_chart_style = function(fill = "FFFFFF", line = NULL, line_width = 1) {
      self$chart_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    },

    #' @description Style the inner plot area (background and border).
    #' @param fill Six-digit hex color for the plot area background.
    #'   \code{NULL} for transparent.
    #' @param line Six-digit hex color for the plot area border.
    #' @param line_width Numeric border width in points. Default \code{1}.
    #' @examples
    #' ec("line")$set_plot_style(fill = "FAFAFA")
    set_plot_style = function(fill = NULL, line = NULL, line_width = 1) {
      self$plot_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    },

    #' @description Print a summary of the chart object.
    #' @examples
    #' ec("line")
    print = function() {
      nSeries <- length(self$series_data)

      cat("An encharter object\n")
      cat("Number of Series:", nSeries, "\n")

      if (nSeries > 0) {
        cat(rep("-", 30), "\n", sep = "")

        for (i in seq_len(nSeries)) {
          s <- self$series_data[[i]]

          is_secondary <- s$sec_type %in% c("x", "y", "xy")
          axis_hint <- if (is_secondary) " [Secondary Axis]" else ""

          s_type <- if (!is.null(s$type)) s$type else self$type
          # series_data stores the name under 'header'
          s_name <- if (!is.null(s$header)) s$header else paste("Series", i)

          cat(sprintf("Series %d: %s %s\n", i, s_name, axis_hint))
          cat(sprintf("  - Type: %s\n", s_type))

          if (!is.null(s$data)) {
            cat(sprintf("  - Data: [%s]\n", s$data))
          }

          if (!is.null(s$cat)) {
            cat(sprintf("  - Cat:  [%s]\n", s$cat))
          }

          cat(rep("-", 30), "\n", sep = "")
        }
      }

      invisible(self)
    }
  ),
  private = list(

    # Internal helper: validate and merge axis parameters into self$axis_params.
    # 'which' must be one of "x", "y", "x2", "y2".
    # All other arguments mirror the public set_*_axis() signatures exactly.
    set_axis_params = function(which, min, max, major, minor,
                               major_time, minor_time, base_time,
                               major_tick, minor_tick,
                               format, log_base, color,
                               font_name, font_size, bold, italic,
                               font_color, rotation,
                               grid_color, gridlines,
                               minor_grid_color, minor_gridlines, cross_between,
                               line_width, grid_width, minor_grid_width,
                               crosses, crosses_at, label_pos) {

      crosses    <- private$validate_input(crosses,    c("autoZero", "min", "max"), "crosses")
      label_pos  <- private$validate_input(label_pos,  c("nextTo", "high", "low", "none"), "label_pos")
      major_tick <- private$validate_input(major_tick, c("cross", "in", "out", "none"), "major_tick")
      minor_tick <- private$validate_input(minor_tick, c("cross", "in", "out", "none"), "minor_tick")

      DASH_TYPES <- c("solid", "dash", "dot", "dashDot", "lgDash",
                      "lgDashDot", "sysDash", "sysDot", "dashed", "dotted")
      if (is.character(gridlines))       private$validate_input(gridlines,       DASH_TYPES, "gridlines")
      if (is.character(minor_gridlines)) private$validate_input(minor_gridlines, DASH_TYPES, "minor_gridlines")

      params <- list(
        min = min, max = max, major = major, minor = minor,
        major_time = major_time, minor_time = minor_time, base_time = base_time,
        major_tick = major_tick, minor_tick = minor_tick,
        format = format, log_base = log_base, color = color,
        font_name = font_name, font_size = font_size, bold = bold, italic = italic,
        font_color = font_color, rotation = rotation,
        grid_color = grid_color, gridlines = gridlines,
        minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines,
        cross_between = cross_between,
        line_width = line_width, grid_width = grid_width, minor_grid_width = minor_grid_width,
        crosses = crosses, crosses_at = crosses_at, label_pos = label_pos
      )

      self$axis_params[[which]] <- modifyList(
        self$axis_params[[which]],
        Filter(Negate(is.null), params)
      )
      invisible(self)
    },

    # Core color renderer. Writes the appropriate DrawingML color child node
    # (<a:srgbClr>, <a:schemeClr>) directly into `target_node`.
    # When wrap = TRUE, first inserts an <a:solidFill> wrapper and writes into
    # that instead, which is the correct structure for text run properties.
    render_color_core = function(target_node, color_val, wrap = FALSE) {
      # Guard: treat NULL and zero-length as no-op
      if (is.null(color_val) || length(color_val) == 0) return()

      node <- if (wrap) xml_add_child(target_node, "a:solidFill") else target_node

      # "auto" -> accent1 scheme color
      if (length(color_val) == 1 && tolower(as.character(color_val)) == "auto") {
        xml_add_child(node, "a:schemeClr", val = "accent1")
        return()
      }

      type <- names(color_val)

      # wbColour objects (from openxlsx2::wb_color())
      if (inherits(color_val, "wbColour")) {
        if (!is.null(type) && type == "auto") {
          xml_add_child(node, "a:schemeClr", val = "accent1")
          return()
        }

        if (!is.null(type) && type == "theme") {
          theme_map <- c(
            "bg1", "tx1", "bg2", "tx2",
            "accent1", "accent2", "accent3", "accent4", "accent5", "accent6",
            "hlink", "folHlink", "phClr",
            "dk1", "lt1", "dk2", "lt2"
          )
          if (as.character(color_val) %in% theme_map) {
            val_name <- as.character(color_val)
          } else {
            theme_idx <- as.integer(color_val)
            val_name <- theme_map[as.numeric(theme_idx) + 1]
          }
          xml_add_child(node, "a:schemeClr", val = val_name)
          return()
        }

        hex <- if (!is.null(type) && type == "rgb") as.character(color_val) else as.character(color_val[1])
      } else {
        hex <- as.character(color_val[1])
      }

      # RGB hex path
      clean <- toupper(gsub("^#", "", hex))

      alpha_val <- NULL
      if (nchar(clean) == 8) {
        aa_hex <- substr(clean, 1, 2)
        aa_dec <- as.numeric(paste0("0x", aa_hex))
        alpha_val <- as.integer(round((aa_dec / 255) * 100000))
        clean <- substr(clean, 3, 8)
      }

      if (nchar(clean) != 6) clean <- "000000"

      color_node <- xml_add_child(node, "a:srgbClr", val = clean)
      if (!is.null(alpha_val)) {
        xml_add_child(color_node, "a:alpha", val = as.character(alpha_val))
      }
    },

    # Convenience wrapper: adds <a:solidFill> to parent_node then delegates to
    # render_color_core. Returns silently for NULL or "auto" (no fill emitted).
    render_color = function(parent_node, color_val) {
      if (is.null(color_val) || identical(color_val, "auto")) return()
      private$render_color_core(
        xml_add_child(parent_node, "a:solidFill"),
        color_val
      )
    },

    sanitize_xml = function(text) {
      if (is.null(text) || !is.character(text)) return(text)
      # Standard XML entities replacement
      text <- gsub("&", "&amp;", text, fixed = TRUE)
      text <- gsub("<", "&lt;", text, fixed = TRUE)
      text <- gsub(">", "&gt;", text, fixed = TRUE)
      text <- gsub("\"", "&quot;", text, fixed = TRUE)
      text <- gsub("'", "&apos;", text, fixed = TRUE)
      text
    },

    # Input validator. Returns choices[1] for NULL input, or the matched choice.
    # Throws an informative error for unrecognised values.
    validate_input = function(val, choices, arg_name = "Argument") {
      if (is.null(val)) return(choices[1])

      res <- try(match.arg(val, choices), silent = TRUE)

      if (inherits(res, "try-error")) {
        stop(sprintf("'%s' must be one of: %s", arg_name, paste(choices, collapse = ", ")), call. = FALSE)
      }
      res
    }
  )
)
