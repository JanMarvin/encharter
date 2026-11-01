# Input validation helpers.
#
# The numeric bounds and enumerations below are taken from the simple types in
# ECMA-376 (5th edition) dml-chart.xsd. Violating them produces files that
# Excel repairs or refuses to load, so they are rejected at input time.

# ST_TextFontSize allows 100 to 400000 hundredths of a point, ST_LineWidth
# 0 to 20116800 EMU (1584 points)
#' @noRd
MAX_FONT_SIZE <- 4000

#' @noRd
MAX_LINE_WIDTH <- 1584

# series types whose series element (CT_BarSer, CT_LineSer, CT_AreaSer,
# CT_ScatterSer, CT_BubbleSer) has trendline and errBars children
#' @noRd
TRENDLINE_TYPES <- c("barChart", "bar3DChart", "lineChart", "line3DChart", "areaChart",
                     "area3DChart", "scatterChart", "bubbleChart", "stockChart")

# font sizes are written in hundredths of a point (ST_TextFontSize, 100 to 400000)
#' @noRd
check_font_size <- function(val, name = "font_size") {
  check_num(val, name, min = 1, max = MAX_FONT_SIZE)
  if (!is.null(val) && abs(val * 100 - round(val * 100)) > 1e-8) {
    stop(sprintf("'%s' must be a multiple of 0.01 points", name), call. = FALSE)
  }
  invisible(val)
}

#' @noRd
num_bounds_txt <- function(min, max, exclusive_min) {
  if (is.infinite(max)) {
    sprintf("%s %s", if (exclusive_min) "greater than" else "at least", format(min))
  } else {
    sprintf("between %s and %s", format(min), format(max))
  }
}

#' @noRd
is_num_scalar <- function(val) {
  is.numeric(val) && length(val) == 1L && is.finite(val)
}

#' @noRd
check_num <- function(val, name, min = -Inf, max = Inf, integer = FALSE,
                      exclusive_min = FALSE) {
  if (is.null(val)) return(invisible(NULL))
  if (!is_num_scalar(val)) {
    stop(sprintf("'%s' must be a single finite numeric value", name), call. = FALSE)
  }
  if (integer && val != trunc(val)) {
    stop(sprintf("'%s' must be a whole number", name), call. = FALSE)
  }
  too_small <- if (exclusive_min) val <= min else val < min
  if (too_small || val > max) {
    stop(sprintf("'%s' must be %s (got %s)", name,
                 num_bounds_txt(min, max, exclusive_min), format(val)), call. = FALSE)
  }
  invisible(val)
}

# Like validate_input(), but NULL passes through as NULL instead of
# defaulting to the first choice.
#' @noRd
check_choice <- function(val, choices, name) {
  if (is.null(val)) return(NULL)
  if (!is.character(val) || length(val) != 1L || !val %in% choices) {
    stop(sprintf("'%s' must be one of: %s", name, paste(choices, collapse = ", ")), call. = FALSE)
  }
  val
}

#' @noRd
check_bool <- function(val, name) {
  if (is.null(val)) return(invisible(NULL))
  if (!is.logical(val) || length(val) != 1L || is.na(val)) {
    stop(sprintf("'%s' must be TRUE or FALSE", name), call. = FALSE)
  }
  invisible(val)
}

# Converts a single color token: "auto"/"none" pass through lowercased,
# 6/8-digit hex is uppercased; everything else is delegated to
# openxlsx2::wb_color(), which resolves R color names and errors on garbage.
# A fully opaque ARGB result is reduced to 6-digit RGB so the rendered XML is
# identical to a plain hex input.
#' @noRd
convert_color_token <- function(x, name) {
  if (is.na(x)) {
    stop(sprintf("'%s' contains NA", name), call. = FALSE)
  }
  if (tolower(x) %in% c("auto", "none")) return(tolower(x))
  clean <- sub("^#", "", x)
  if (grepl("^[0-9A-Fa-f]{6}$", clean) || grepl("^[0-9A-Fa-f]{8}$", clean)) {
    return(toupper(clean))
  }
  hex <- tryCatch(
    as.character(openxlsx2::wb_color(x)),
    error = function(e) {
      stop(sprintf(
        "'%s' contains an invalid color '%s'. Use a 6/8 digit hex value, an R color name, \"auto\", \"none\", or wb_color().",
        name, x
      ), call. = FALSE)
    }
  )
  sub("^FF(?=[0-9A-F]{6}$)", "", hex, perl = TRUE)
}

# Validates (and normalizes) color input. Accepts NULL, wbColour objects,
# "auto"/"none", 6- or 8-digit hex (optionally prefixed with '#'), and R
# color names (resolved via openxlsx2::wb_color()). Returns the possibly
# converted value; errors on anything else that would otherwise be written
# as black silently.
#' @noRd
check_color <- function(val, name) {
  if (is.null(val) || length(val) == 0) return(val)
  if (inherits(val, "wbColour")) return(check_wb_color(val, name))
  if (!is.character(val)) {
    stop(sprintf("'%s' must be a character color or a wb_color() object", name), call. = FALSE)
  }
  vapply(val, convert_color_token, character(1), name = name, USE.NAMES = FALSE)
}

# wb_color() objects are written as srgbClr or schemeClr. Theme colors map to
# the 17 scheme color names, indexed colors have no DrawingML equivalent.
#' @noRd
check_wb_color <- function(val, name, allow_auto = TRUE) {
  type <- names(val)[1]
  if (identical(type, "rgb")) return(val)
  if (identical(type, "auto") && allow_auto) return(val)
  if (identical(type, "theme")) {
    theme <- suppressWarnings(as.numeric(val[["theme"]]))
    scheme <- c("bg1", "tx1", "bg2", "tx2", "accent1", "accent2", "accent3", "accent4", "accent5",
                "accent6", "hlink", "folHlink", "phClr", "dk1", "lt1", "dk2", "lt2")
    if (val[["theme"]] %in% scheme || (!is.na(theme) && theme %in% 0:16)) return(val)
    stop(sprintf("'%s' has the theme color %s, theme colors must be from 0 to 16", name, val[["theme"]]), call. = FALSE)
  }
  stop(sprintf("'%s' must be a hex color, R color name or wb_color() with hex, name or theme%s",
               name, if (allow_auto) " or auto" else ""), call. = FALSE)
}

#' @noRd
check_format <- function(val, name = "format") {
  if (is.null(val)) return(invisible(NULL))
  if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
    stop(sprintf("'%s' must be a non-empty format string", name), call. = FALSE)
  }
  invisible(val)
}

# ST_BuiltInUnit
#' @noRd
ST_BUILT_IN_UNIT <- c(
  "hundreds", "thousands", "tenThousands", "hundredThousands", "millions",
  "tenMillions", "hundredMillions", "billions", "trillions"
)

# disp_units accepts a built-in unit name or a positive number (custUnit)
#' @noRd
check_disp_units <- function(val) {
  if (is.null(val)) return(invisible(NULL))
  if (is.numeric(val)) {
    check_num(val, "disp_units", min = 0, exclusive_min = TRUE)
  } else {
    check_choice(val, ST_BUILT_IN_UNIT, "disp_units")
  }
  invisible(val)
}

#' @noRd
check_trendline <- function(tl) {
  if (!is.list(tl)) return(invisible(NULL))
  check_choice(tl$type, c("exp", "linear", "log", "movingAvg", "poly", "power"), "trendline$type")
  if (identical(tl$type, "poly") && is.null(tl$order)) {
    stop("'trendline$order' is required for polynomial trendlines", call. = FALSE)
  }
  if (identical(tl$type, "movingAvg") && is.null(tl$period)) {
    stop("'trendline$period' is required for moving average trendlines", call. = FALSE)
  }
  check_num(tl$order, "trendline$order", min = 2, max = 6, integer = TRUE)
  check_num(tl$period, "trendline$period", min = 2, max = 255, integer = TRUE)
  check_num(tl$forward, "trendline$forward", min = 0)
  check_num(tl$backward, "trendline$backward", min = 0)
  check_num(tl$intercept, "trendline$intercept")
  invisible(NULL)
}

#' @noRd
check_error_bars <- function(eb) {
  if (!is.list(eb)) return(invisible(NULL))
  check_choice(eb$type, c("fixedVal", "percentage", "stdDev", "stdErr", "cust"), "error_bars$type")
  if (identical(eb$type, "cust")) {
    stop("custom ('cust') error bars are not supported", call. = FALSE)
  }
  check_num(eb$value, "error_bars$value", min = 0)
  check_choice(eb$direction, c("both", "plus", "minus"), "error_bars$direction")
  check_choice(eb$axis, c("x", "y"), "error_bars$axis")
  invisible(NULL)
}

# Shared numeric/enumeration checks for axis parameters. Returns the axis
# color arguments possibly converted (R color names -> hex).
#' @noRd
check_axis_params <- function(p) {
  check_num(p$min, "min")
  check_num(p$max, "max")
  if (!is.null(p$min) && !is.null(p$max) && p$max <= p$min) {
    stop("axis 'max' must be greater than 'min'", call. = FALSE)
  }
  check_num(p$major, "major", min = 0, exclusive_min = TRUE)
  check_num(p$minor, "minor", min = 0, exclusive_min = TRUE)
  check_choice(p$major_time, c("days", "months", "years"), "major_time")
  check_choice(p$minor_time, c("days", "months", "years"), "minor_time")
  check_choice(p$base_time,  c("days", "months", "years"), "base_time")
  check_num(p$log_base, "log_base", min = 2, max = 1000)
  check_num(p$rotation, "rotation", min = -90, max = 90)
  check_font_size(p$font_size)
  check_num(p$line_width, "line_width", min = 0, max = MAX_LINE_WIDTH)
  check_num(p$grid_width, "grid_width", min = 0, max = MAX_LINE_WIDTH)
  check_num(p$minor_grid_width, "minor_grid_width", min = 0, max = MAX_LINE_WIDTH)
  check_num(p$crosses_at, "crosses_at")
  check_num(p$tick_lbl_skip,  "tick_lbl_skip",  min = 1, integer = TRUE)
  check_num(p$tick_mark_skip, "tick_mark_skip", min = 1, integer = TRUE)
  check_choice(p$cross_between, c("between", "midCat"), "cross_between")
  check_format(p$format)
  check_bool(p$rev, "rev")
  check_disp_units(p$disp_units)
  invisible(NULL)
}
