ENCHARTER_PLOT_TYPES <- c(
  "barChart", "lineChart", "areaChart", "scatterChart",
  "pieChart", "doughnutChart", "radarChart", "bubbleChart"
)

# Office theme colors, used for wb_color(theme = ...) and "auto"
ENCHARTER_THEME_HEX <- c(
  bg1 = "FFFFFF", tx1 = "000000", bg2 = "E7E6E6", tx2 = "44546A",
  accent1 = "4472C4", accent2 = "ED7D31", accent3 = "A5A5A5",
  accent4 = "FFC000", accent5 = "5B9BD5", accent6 = "70AD47",
  hlink = "0563C1", folHlink = "954F72", phClr = "000000",
  dk1 = "000000", lt1 = "FFFFFF", dk2 = "44546A", lt2 = "E7E6E6"
)

# theme colors of the workbook being plotted; plot() sets them from the
# workbook's theme part and restores the Office defaults afterwards
plot_state <- new.env(parent = emptyenv())
plot_state$theme <- ENCHARTER_THEME_HEX

plot_set_theme <- function(wb) {
  theme <- ENCHARTER_THEME_HEX
  xml <- if (!is.null(wb)) wb$theme else NULL
  if (is.character(xml) && length(xml) == 1 && nzchar(xml)) {
    scheme <- xml_find_first(read_xml(xml), ".//a:clrScheme")
    if (!is_missing(scheme)) {
      for (nm in c("dk1", "lt1", "dk2", "lt2", paste0("accent", 1:6), "hlink", "folHlink")) {
        node <- xml_find_first(scheme, paste0("./a:", nm))
        if (is_missing(node)) next
        srgb <- xml_find_first(node, "./a:srgbClr")
        sys <- xml_find_first(node, "./a:sysClr")
        hex <- if (!is_missing(srgb)) xml_attr(srgb, "val") else if (!is_missing(sys)) xml_attr(sys, "lastClr") else ""
        if (nzchar(hex)) theme[[nm]] <- toupper(hex)
      }
      theme[["tx1"]] <- theme[["dk1"]]
      theme[["bg1"]] <- theme[["lt1"]]
      theme[["tx2"]] <- theme[["dk2"]]
      theme[["bg2"]] <- theme[["lt2"]]
    }
  }
  plot_state$theme <- theme
}

# Converts an encharter color (hex, AARRGGBB hex, "auto", "none", wbColour)
# to an R color string; NA for no fill.
plot_color <- function(x, default = "#000000") {
  if (is.null(x) || length(x) == 0) return(default)
  if (inherits(x, "wbColour")) {
    type <- names(x)
    if (identical(type, "theme")) {
      val <- as.character(x)
      if (!val %in% names(plot_state$theme)) val <- names(plot_state$theme)[as.integer(val) + 1]
      lum_mod <- attr(x, "lumMod")
      lum_off <- attr(x, "lumOff")
      x <- plot_state$theme[[val]]
      if (!is.null(lum_mod) || !is.null(lum_off)) {
        # DrawingML lumMod/lumOff act on the HSL luminance
        rgb <- grDevices::col2rgb(paste0("#", x)) / 255
        mx <- max(rgb)
        mn <- min(rgb)
        l <- (mx + mn) / 2
        sat <- if (mx == mn) 0 else (mx - mn) / (1 - abs(2 * l - 1))
        h <- if (mx == mn) 0 else if (mx == rgb[1]) ((rgb[2] - rgb[3]) / (mx - mn)) %% 6 else if (mx == rgb[2]) (rgb[3] - rgb[1]) / (mx - mn) + 2 else (rgb[1] - rgb[2]) / (mx - mn) + 4
        l <- min(1, max(0, l * (lum_mod %||% 1) + (lum_off %||% 0)))
        c1 <- (1 - abs(2 * l - 1)) * sat
        x1 <- c1 * (1 - abs(h %% 2 - 1))
        base <- switch(floor(h) + 1, c(c1, x1, 0), c(x1, c1, 0), c(0, c1, x1), c(0, x1, c1), c(x1, 0, c1), c(c1, 0, x1))
        x <- toupper(substr(grDevices::rgb(base[1] + l - c1 / 2, base[2] + l - c1 / 2, base[3] + l - c1 / 2), 2, 7))
      }
    } else if (identical(type, "auto")) {
      x <- "auto"
    } else {
      x <- as.character(x[1])
      x <- sub("^FF(?=[0-9A-Fa-f]{6}$)", "", x, perl = TRUE)
    }
  }
  x <- as.character(x[1])
  if (is.na(x)) return(default)
  if (tolower(x) == "auto") return(paste0("#", plot_state$theme[["accent1"]]))
  if (tolower(x) == "none") return(NA_character_)
  hex <- toupper(sub("^#", "", x))
  if (nchar(hex) == 8) return(paste0("#", substr(hex, 3, 8), substr(hex, 1, 2)))
  if (nchar(hex) == 6) return(paste0("#", hex))
  default
}

# Automatic point colors: the six accents, then the same accents
# with the brightness variations of the default color style
plot_auto_color <- function(i, palette) {
  if (i <= length(palette)) return(plot_color(palette[i], "#4472C4"))
  accents <- plot_state$theme[paste0("accent", 1:6)]
  base <- grDevices::col2rgb(paste0("#", accents[(i - 1) %% 6 + 1])) / 255
  cycle <- (i - 1) %/% 6
  mod <- c(1, 0.6, 0.8, 0.8, 0.6, 0.5)[min(cycle + 1, 6)]
  off <- c(0, 0, 0.2, 0, 0.4, 0)[min(cycle + 1, 6)]
  rgb <- pmin(1, base * mod + off)
  grDevices::rgb(rgb[1], rgb[2], rgb[3])
}

plot_lty <- function(type) {
  if (is.null(type)) return("solid")
  switch(type,
    dash = , dashed = "dashed",
    dot = , dotted = , sysDot = "dotted",
    dashDot = "dotdash",
    lgDash = "longdash",
    lgDashDot = "twodash",
    sysDash = "22",
    "solid"
  )
}

plot_pch <- function(symbol) {
  switch(symbol %||% "none",
    circle = 21, square = 22, diamond = 23, triangle = 24,
    x = 4, plus = 3, star = 8, dash = 45, dot = 20, NA_integer_
  )
}

plot_gpar_text <- function(style, default_size, default_col = "#000000") {
  face <- if (isTRUE(style$bold) && isTRUE(style$italic)) "bold.italic"
    else if (isTRUE(style$bold)) "bold"
    else if (isTRUE(style$italic)) "italic"
    else "plain"
  # theme font placeholders such as "+mn-lt" are not font families
  family <- style$font_name %||% ""
  if (startsWith(family, "+")) family <- ""
  grid::gpar(
    fontsize = style$font_size %||% default_size,
    fontface = face,
    col = plot_color(style$font_color %||% style$color, default_col),
    fontfamily = family
  )
}

# Formats numbers roughly the way a spreadsheet does for a handful of common format
# codes; everything else falls back to "General".
plot_format <- function(x, format = NULL) {
  if (inherits(x, c("Date", "POSIXt"))) {
    # without a format the cell's short date is shown, which follows the
    # system locale
    if (is.null(format)) return(format(x, "%x"))
    fmt <- tolower(format)
    fmt <- gsub("yyyy", "%Y", fmt)
    fmt <- gsub("yy", "%y", fmt)
    fmt <- gsub("mmmm", "%B", fmt)
    fmt <- gsub("mmm", "%b", fmt)
    fmt <- gsub("mm", "%m", fmt)
    fmt <- gsub("dd", "%d", fmt)
    fmt <- gsub("hh", "%H", fmt)
    fmt <- gsub("ss", "%S", fmt)
    fmt <- gsub("%m:%m", "%M:%M", fmt, fixed = TRUE)
    fmt <- gsub("%H:%m", "%H:%M", fmt, fixed = TRUE)
    fmt <- gsub("\\\\", "", fmt)
    return(format(x, fmt))
  }
  if (is.character(x)) return(x)
  fmt <- format %||% "General"
  fmt <- sub(";.*$", "", fmt)
  is_pct <- grepl("%", fmt)
  if (is_pct) x <- x * 100
  big <- if (grepl(",", fmt)) "," else ""
  dec <- regmatches(fmt, regexpr("\\.0+", fmt))
  digits <- if (length(dec)) nchar(dec) - 1L else 0L
  if (fmt == "General") {
    out <- formatC(x, digits = 10, format = "fg", big.mark = big)
    has_dec <- grepl(".", out, fixed = TRUE)
    out[has_dec] <- sub("\\.?0+$", "", out[has_dec])
  } else if (grepl("[0#]", fmt)) {
    out <- formatC(x, format = "f", digits = digits, big.mark = big)
  } else {
    out <- format(x, trim = TRUE)
  }
  out <- trimws(out)
  if (is_pct) out <- paste0(out, "%")
  out[is.na(x)] <- ""
  out
}

# The automatic value axis. The minimum is zero for all-positive data
# unless the values sit in the upper sixth of their magnitude, the ends are
# padded by 5% of the range, and the major unit is the power of ten below
# the padded range, divided by 2, 5 or 10 when the range covers less than
# 5, 2 or 1 of those units. Checked against Excel output for a dozen charts.
# `pad = FALSE` is used for percent and radar axes, which end exactly at the
# data maximum.
plot_scale <- function(lo, hi, params = list(), pad = TRUE) {
  if (!is.null(params$log_base)) {
    base <- params$log_base
    lo <- max(lo, .Machine$double.eps)
    mn <- params$min %||% base^floor(log(lo, base))
    mx <- params$max %||% base^ceiling(log(hi, base))
    return(list(min = mn, max = mx, major = params$major %||% base, log = base))
  }
  if (!is.finite(lo) || !is.finite(hi)) {
    lo <- 0
    hi <- 1
  }
  if (lo == hi) {
    lo <- if (lo > 0) 0 else lo - 1
    hi <- if (hi > 0) hi else 0
  }
  if (lo > 0 && (hi - lo) > hi / 6) lo <- 0
  if (hi < 0 && (hi - lo) > abs(lo) / 6) hi <- 0
  rng <- hi - lo
  hi_pad <- params$max %||% if (hi > 0 && pad) hi + 0.05 * rng else hi
  lo_pad <- params$min %||% if (lo < 0 && pad) lo - 0.05 * rng else lo
  major <- params$major
  if (is.null(major)) {
    span <- hi_pad - lo_pad
    mag <- 10^floor(log10(span))
    ratio <- span / mag
    major <- if (ratio <= 1) mag / 10 else if (ratio < 2) mag / 5 else if (ratio < 5) mag / 2 else mag
  }
  mn <- params$min %||% (floor(lo_pad / major) * major)
  mx <- params$max %||% (ceiling(hi_pad / major) * major)
  if (mx <= mn) mx <- mn + major
  list(min = mn, max = mx, major = major, log = NULL)
}

plot_ticks <- function(scale) {
  if (!is.null(scale$log)) {
    return(scale$log^seq(floor(log(scale$min, scale$log)), ceiling(log(scale$max, scale$log))))
  }
  t <- seq(scale$min, scale$max, by = scale$major)
  if (abs(t[length(t)] - scale$max) > 1e-9 * scale$major) t <- c(t, scale$max)
  t
}

# Reads a range such as "'Sheet 1'!$B$2:$B$7" from the workbook.
plot_read_ref <- function(wb, ref, levels = FALSE) {
  if (is.null(wb) || is.null(ref) || !grepl("!", ref)) return(NULL)
  sheet <- gsub("^'|'$", "", sub("!.*$", "", ref))
  sheet <- gsub("''", "'", sheet)
  dims <- gsub("\\$", "", sub("^.*!", "", ref))
  if (!sheet %in% wb$get_sheet_names(escape = TRUE)) return(NULL)
  df <- openxlsx2::wb_to_df(wb, sheet = sheet, dims = dims, col_names = FALSE)
  if (isTRUE(levels) && nrow(df) >= ncol(df) && ncol(df) > 1) {
    # a multi-level category range: one column per level, outer level first
    return(as.data.frame(lapply(df, function(col) as.character(col)), stringsAsFactors = FALSE))
  }
  vals <- if (nrow(df) >= ncol(df)) df[[1]] else unlist(df[1, ], use.names = FALSE)
  if (is.factor(vals)) vals <- as.character(vals)
  vals
}

# Resolves values and names of every series
plot_collect <- function(chart, wb) {
  out <- vector("list", length(chart$series_data))
  for (i in seq_along(chart$series_data)) {
    s <- chart$series_data[[i]]
    y <- s$data_cache %||% plot_read_ref(wb, s$data)
    x <- s$cat_cache %||% plot_read_ref(wb, s$label, levels = TRUE)
    cat_levels <- NULL
    if (is.data.frame(x)) {
      cat_levels <- x
      x <- x[[ncol(x)]]
    }
    z <- s$z_cache %||% plot_read_ref(wb, s$weight)
    if (is.null(y)) {
      stop(sprintf("series %d has no values; add it from wb_data() or pass 'wb'", i), call. = FALSE)
    }
    y <- suppressWarnings(as.numeric(y))
    if (!is.null(z)) z <- suppressWarnings(as.numeric(z))
    name <- s$name_cache %||% s$name
    if (!is.null(name) && grepl("!.+", name)) {
      cell <- plot_read_ref(wb, name)
      name <- if (is.null(cell)) paste("Series", i) else as.character(cell[1])
    }
    if (is.null(name) || is.na(name)) name <- paste("Series", i)
    if (is.null(x)) x <- seq_along(y)
    if (length(x) < length(y)) x <- c(x, rep(NA, length(y) - length(x)))
    s$values <- y
    s$cats <- x
    s$cat_levels <- cat_levels
    s$sizes <- z
    s$label_text <- xml_unescape(name)
    out[[i]] <- s
  }
  out
}

# Legend: returns its size in points and a drawing function. A horizontal
# legend wider than `max_w` is broken into several rows.
plot_legend <- function(entries, params, style, max_w = Inf) {
  pos <- params$pos %||% "r"
  if (pos == "tr") pos <- "r"
  gp <- plot_gpar_text(style, if (length(style)) 10 else 9)
  n <- length(entries)
  key_w <- 18
  pad <- 6
  widths <- vapply(entries, function(e) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(e$label, gp = gp)), "points", valueOnly = TRUE)
  }, numeric(1))
  item_w <- widths + key_w + 2 * pad
  line_h <- gp$fontsize * 1.4
  horizontal <- pos %in% c("t", "b")

  # rows: horizontal legends fill each row up to max_w, vertical ones use one
  # entry per row
  rows <- list()
  if (horizontal) {
    current <- integer()
    used <- 0
    for (k in seq_len(n)) {
      if (length(current) && used + item_w[k] > max_w) {
        rows[[length(rows) + 1]] <- current
        current <- integer()
        used <- 0
      }
      current <- c(current, k)
      used <- used + item_w[k]
    }
    rows[[length(rows) + 1]] <- current
    size <- c(w = max(vapply(rows, function(r) sum(item_w[r]), numeric(1))), h = length(rows) * line_h + pad)
  } else {
    rows <- as.list(seq_len(n))
    size <- c(w = max(item_w), h = n * line_h + pad)
  }

  draw <- function() {
    y <- size[["h"]] - pad / 2
    for (r in rows) {
      cy <- y - line_h / 2
      y <- y - line_h
      x <- if (horizontal) (size[["w"]] - sum(item_w[r])) / 2 + pad else pad
      for (k in r) {
        e <- entries[[k]]
        if (e$kind == "line") {
          grid::grid.lines(x = grid::unit(c(x, x + key_w - 4), "points"), y = grid::unit(c(cy, cy), "points"),
                           gp = grid::gpar(col = e$col, lwd = e$lwd, lty = e$lty))
          if (!is.na(e$pch)) {
            grid::grid.points(x = grid::unit(x + (key_w - 4) / 2, "points"), y = grid::unit(cy, "points"),
                              pch = e$pch, size = grid::unit(e$cex / 0.75, "points"),
                              gp = grid::gpar(col = e$mcol, fill = e$mfill, lwd = 1))
          }
        } else {
          grid::grid.rect(x = grid::unit(x + 4, "points"), y = grid::unit(cy, "points"),
                          width = grid::unit(8, "points"), height = grid::unit(8, "points"),
                          just = c("center", "center"), gp = grid::gpar(fill = e$col, col = NA))
        }
        grid::grid.text(e$label, x = grid::unit(x + key_w, "points"), y = grid::unit(cy, "points"),
                        just = c("left", "center"), gp = gp)
        x <- x + item_w[k]
      }
    }
  }
  list(pos = pos, size = size, draw = draw)
}

# Title text broken into lines that fit `width` points, the way titles wrap
# a title that is wider than the chart
plot_title_lines <- function(title, default_size, width) {
  gp <- plot_gpar_text(title$style, default_size)
  out <- character()
  for (para in strsplit(plot_title_text(title), "\n", fixed = TRUE)[[1]]) {
    words <- strsplit(para, " ", fixed = TRUE)[[1]]
    line <- ""
    for (w in words) {
      trial <- if (nzchar(line)) paste(line, w) else w
      wide <- grid::convertWidth(grid::grobWidth(grid::textGrob(trial, gp = gp)), "points", valueOnly = TRUE) > width
      if (wide && nzchar(line)) {
        out <- c(out, line)
        line <- w
      } else {
        line <- trial
      }
    }
    out <- c(out, line)
  }
  out
}

plot_title_height <- function(title, default_size, width = Inf) {
  if (is.null(title$text)) return(0)
  gp <- plot_gpar_text(title$style, default_size)
  gp$fontsize * 1.2 * length(plot_title_lines(title, default_size, width)) + 8
}

plot_title_text <- function(title) {
  txt <- title$text
  if (inherits(txt, "fmt_txt")) return(as.character(txt))
  xml_unescape(as.character(txt))
}

# Draws the chart title in the current viewport (a strip at the top)
plot_draw_title <- function(title, default_size, width = Inf) {
  if (is.null(title$text)) return(invisible())
  grid::grid.text(paste(plot_title_lines(title, default_size, width), collapse = "\n"),
                  gp = plot_gpar_text(title$style, default_size, "#000000"))
}

plot_axis_gp_line <- function(p) {
  grid::gpar(col = plot_color(p$color, "#000000"), lwd = (p$line_width %||% 1) * 96 / 72)
}

plot_grid_gp <- function(p, minor = FALSE) {
  key <- if (minor) "minor_" else ""
  style <- p[[paste0(key, "grid_lines")]]
  grid::gpar(
    col = plot_color(p[[paste0(key, "grid_color")]], if (minor) "#F2F2F2" else "#D9D9D9"),
    lwd = (p[[paste0(key, "grid_width")]] %||% if (minor) 0.5 else 1) * 96 / 72,
    lty = if (is.character(style)) plot_lty(style) else "solid"
  )
}

# Marker for a line/scatter point
plot_draw_markers <- function(x, y, m, series_col) {
  pch <- plot_pch(m$symbol)
  if (is.na(pch)) return(invisible())
  fill <- plot_color(m$fill, series_col)
  line <- plot_color(m$line$color, series_col)
  # grid draws a symbol at 3/4 of `size`; the marker size is the diameter in points
  grid::grid.points(x, y, pch = pch, size = grid::unit((m$size %||% 5) / 0.75, "points"),
                    gp = grid::gpar(col = line, fill = fill, lwd = (m$line$width %||% 0.75) * 96 / 72),
                    default.units = "native")
}

plot_trend_curve <- function(x, y, tl, shift = 0) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok] + shift
  y <- y[ok]
  if (length(x) < 2) return(NULL)
  type <- tl$type %||% "linear"
  xs <- seq(min(x) - (tl$backward %||% 0), max(x) + (tl$forward %||% 0), length.out = 100)
  new <- data.frame(x = xs)
  fit <- switch(type,
    linear = stats::predict(stats::lm(y ~ x), new),
    poly = stats::predict(stats::lm(y ~ stats::poly(x, min(tl$order %||% 2, length(x) - 1), raw = TRUE)), new),
    exp = exp(stats::predict(stats::lm(log(y) ~ x), new)),
    log = stats::predict(stats::lm(y ~ log(x)), new),
    power = exp(stats::predict(stats::lm(log(y) ~ log(x)), new)),
    movingAvg = {
      p <- tl$period %||% 2
      xs <- x[p:length(x)]
      vapply(p:length(y), function(i) mean(y[(i - p + 1):i]), numeric(1))
    },
    NULL
  )
  if (is.null(fit)) return(NULL)
  list(x = xs - shift, y = as.numeric(fit))
}

# Legend text of a trendline
plot_trend_name <- function(tl, series_name) {
  if (!is.null(tl$name)) return(tl$name)
  kind <- switch(tl$type %||% "linear",
    linear = "Linear", poly = "Poly.", exp = "Expon.", log = "Log.", power = "Power",
    movingAvg = sprintf("%d per. Mov. Avg.", tl$period %||% 2), "Linear"
  )
  sprintf("%s (%s)", kind, series_name)
}

# Equation text of a trendline, with 4 decimals
plot_trend_equation <- function(x, y, tl) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 2) return(NULL)
  num <- function(v) formatC(v, format = "f", digits = 4)
  term <- function(coef, txt, first = FALSE) {
    sign <- if (coef < 0) "- " else if (first) "" else "+ "
    paste0(sign, num(abs(coef)), txt)
  }
  type <- tl$type %||% "linear"
  fit <- switch(type,
    linear = stats::lm(y ~ x),
    poly = stats::lm(y ~ stats::poly(x, min(tl$order %||% 2, length(x) - 1), raw = TRUE)),
    exp = if (all(y > 0)) stats::lm(log(y) ~ x),
    log = if (all(x > 0)) stats::lm(y ~ log(x)),
    power = if (all(x > 0 & y > 0)) stats::lm(log(y) ~ log(x)),
    NULL
  )
  if (is.null(fit)) return(NULL)
  b <- stats::coef(fit)
  eq <- switch(type,
    linear = paste("y =", term(b[2], "x", TRUE), term(b[1], "")),
    poly = {
      k <- length(b) - 1
      parts <- character()
      for (p in k:1) {
        power <- if (p == 1) "x" else paste0("x", intToUtf8(c(0xB2, 0xB3, 0x2074, 0x2075, 0x2076)[p - 1]))
        parts <- c(parts, term(b[p + 1], power, p == k))
      }
      paste("y =", paste(c(parts, term(b[1], "")), collapse = " "))
    },
    exp = paste0("y = ", num(exp(b[1])), "e", term(b[2], "x", TRUE)),
    log = paste("y =", term(b[2], "ln(x)", TRUE), term(b[1], "")),
    power = paste0("y = ", num(exp(b[1])), "x^", num(b[2]))
  )
  r2 <- summary(fit)$r.squared
  list(eq = eq, r2 = paste0("R\u00b2 = ", num(r2)))
}

plot_error_extent <- function(y, eb) {
  type <- eb$type %||% "fixedVal"
  val <- eb$value %||% 5
  switch(type,
    fixedVal = rep(val, length(y)),
    percentage = abs(y) * val / 100,
    stdDev = rep(stats::sd(y, na.rm = TRUE) * val, length(y)),
    stdErr = rep(stats::sd(y, na.rm = TRUE) / sqrt(sum(!is.na(y))), length(y)),
    rep(val, length(y))
  )
}

# Draws error bars around points at (x, y) in native units
plot_draw_error_bars <- function(x, y, s, horizontal = FALSE) {
  eb <- s$error_bars
  if (!is.list(eb)) return(invisible())
  ext <- plot_error_extent(y, eb)
  if (identical(eb$type, "stdDev")) y <- rep(mean(y, na.rm = TRUE), length(y))
  dir <- eb$direction %||% "both"
  up <- if (dir %in% c("both", "plus")) y + ext else y
  lo <- if (dir %in% c("both", "minus")) y - ext else y
  gp <- grid::gpar(col = plot_color(eb$color, "#000000"), lwd = 1)
  cap <- grid::unit(c(-3, 3), "points")
  for (i in seq_along(x)) {
    if (!is.finite(x[i]) || !is.finite(y[i])) next
    if (horizontal) {
      grid::grid.lines(grid::unit(c(lo[i], up[i]), "native"), grid::unit(c(x[i], x[i]), "native"), gp = gp)
      for (v in c(lo[i], up[i])) {
        grid::grid.lines(grid::unit(c(v, v), "native"), grid::unit(c(x[i], x[i]), "native") + cap, gp = gp)
      }
    } else {
      grid::grid.lines(grid::unit(c(x[i], x[i]), "native"), grid::unit(c(lo[i], up[i]), "native"), gp = gp)
      for (v in c(lo[i], up[i])) {
        grid::grid.lines(grid::unit(c(x[i], x[i]), "native") + cap, grid::unit(c(v, v), "native"), gp = gp)
      }
    }
  }
}

# Data label text for a point
plot_label_text <- function(lp, cat, val, pct = NULL, name = NULL, sep = ", ") {
  parts <- character()
  if (isTRUE(lp$show_ser_name) && !is.null(name)) parts <- c(parts, name)
  if (isTRUE(lp$show_cat)) parts <- c(parts, plot_format(cat))
  if (isTRUE(lp$show_val)) parts <- c(parts, plot_format(val, lp$format))
  if (isTRUE(lp$show_percent) && !is.null(pct)) parts <- c(parts, plot_format(pct, lp$format %||% "0%"))
  paste(parts, collapse = sep)
}

plot_labels_on <- function(lp) {
  isTRUE(lp$show_val) || isTRUE(lp$show_cat) || isTRUE(lp$show_percent) || isTRUE(lp$show_ser_name)
}

# ---------------------------------------------------------------------------
# Cartesian charts: bar, line, area, scatter, bubble
# ---------------------------------------------------------------------------

plot_cartesian <- function(chart, series) {
  is_xy <- any(vapply(series, function(s) s$type %in% c("scatterChart", "bubbleChart"), logical(1)))
  horizontal <- any(vapply(series, function(s) s$type == "barChart" && identical(s$dir, "bar"), logical(1)))
  lp <- chart$label_params
  label_gp <- plot_gpar_text(lp$style, 9, "#000000")

  # ---- categories / x values ----
  if (is_xy) {
    cats <- NULL
    x_of <- function(s) {
      x <- suppressWarnings(as.numeric(s$cats))
      if (all(is.na(x))) x <- seq_along(s$values)
      x
    }
  } else {
    cats <- series[[1]]$cats
    n_cat <- max(vapply(series, function(s) length(s$values), integer(1)))
    if (length(cats) < n_cat) cats <- c(cats, rep(NA, n_cat - length(cats)))
    x_of <- function(s) seq_along(s$values) - 0.5
  }
  # a trendline extrapolated forwards adds empty categories to the axis
  n_slots <- length(cats)
  for (s in series) {
    if (is.list(s$trendline) && s$type != "scatterChart") n_slots <- max(n_slots, length(cats) + (s$trendline$forward %||% 0))
  }
  if (!is_xy && n_slots > length(cats)) cats <- c(cats, rep(NA, n_slots - length(cats)))

  # A date axis places the points by date in date_unit units (days, months or
  # years, as set or as inferred from the spacing of the dates). Each unit
  # is one slot; points and bars sit in the middle of their slot.
  is_date <- !is_xy && inherits(cats, c("Date", "POSIXt")) && !horizontal
  if (is_date) {
    px_date <- chart$axis_params$x
    dates <- as.Date(cats)
    date_unit <- px_date$base_time
    if (is.null(date_unit)) {
      gaps <- diff(sort(unique(as.numeric(dates))))
      med <- if (length(gaps)) stats::median(gaps) else 1
      date_unit <- if (med >= 365) "years" else if (med >= 28) "months" else "days"
    }
    serial_date <- function(v) as.Date(v, origin = "1899-12-30")
    d_min <- if (!is.null(px_date$min)) serial_date(px_date$min) else min(dates, na.rm = TRUE)
    d_max <- if (!is.null(px_date$max)) serial_date(px_date$max) else max(dates, na.rm = TRUE)
    origin <- switch(date_unit,
      days = d_min,
      months = as.Date(format(d_min, "%Y-%m-01")),
      years = as.Date(format(d_min, "%Y-01-01"))
    )
    to_units <- function(d) {
      d <- as.Date(d)
      y <- as.integer(format(d, "%Y"))
      m <- as.integer(format(d, "%m"))
      switch(date_unit,
        days = as.numeric(d) - as.numeric(origin),
        months = (y * 12 + m) - (as.integer(format(origin, "%Y")) * 12 + as.integer(format(origin, "%m"))) +
          (as.integer(format(d, "%d")) - 1) / 31,
        years = y - as.integer(format(origin, "%Y")) + (as.integer(format(d, "%j")) - 1) / 365
      )
    }
    from_units <- function(u) {
      switch(date_unit,
        days = origin + u,
        months = {
          m0 <- as.integer(format(origin, "%Y")) * 12 + as.integer(format(origin, "%m")) - 1 + u
          as.Date(sprintf("%d-%02d-01", m0 %/% 12, m0 %% 12 + 1))
        },
        years = as.Date(sprintf("%d-01-01", as.integer(format(origin, "%Y")) + u))
      )
    }
    unit_in_base <- function(unit) {
      if (is.null(unit) || unit == date_unit) return(1)
      days <- c(days = 1, months = 30.4375, years = 365.25)
      days[[unit]] / days[[date_unit]]
    }
    date_span <- floor(to_units(d_max)) + 1
    x_of <- function(s) {
      u <- suppressWarnings(floor(to_units(s$cats)) + 0.5)
      length(u) <- length(s$values)
      u
    }
  }

  # ---- value ranges per axis (stacking taken into account) ----
  axis_range <- function(sel) {
    if (!length(sel)) return(c(NA, NA))
    lo <- Inf
    hi <- -Inf
    for (grp in split(sel, vapply(sel, function(s) paste(s$type, s$grouping), character(1)))) {
      if (grp[[1]]$grouping %in% c("stacked", "percentStacked") && grp[[1]]$type %in% c("barChart", "areaChart", "lineChart")) {
        mat <- do.call(rbind, lapply(grp, function(s) {
          v <- s$values
          length(v) <- length(cats)
          v
        }))
        mat[is.na(mat)] <- 0
        if (grp[[1]]$grouping == "percentStacked") {
          tot <- colSums(abs(mat))
          mat <- sweep(mat, 2, ifelse(tot == 0, 1, tot), "/")
        }
        if (grp[[1]]$type == "barChart") {
          pos <- colSums(pmax(mat, 0))
          neg <- colSums(pmin(mat, 0))
          lo <- min(lo, neg)
          hi <- max(hi, pos)
        } else {
          cum <- apply(mat, 2, cumsum)
          lo <- min(lo, cum, 0)
          hi <- max(hi, cum, 0)
        }
      } else {
        for (s in grp) {
          v <- s$values
          if (is.list(s$error_bars)) {
            ext <- plot_error_extent(v, s$error_bars)
            v <- c(v, v + ext, v - ext)
          }
          lo <- min(lo, v, na.rm = TRUE)
          hi <- max(hi, v, na.rm = TRUE)
        }
      }
    }
    if (any(vapply(sel, function(s) s$type %in% c("barChart", "areaChart"), logical(1)))) {
      lo <- min(lo, 0)
      hi <- max(hi, 0)
    }
    c(lo, hi)
  }
  prim <- Filter(function(s) !s$sec_type %in% c("y", "xy"), series)
  sec  <- Filter(function(s)  s$sec_type %in% c("y", "xy"), series)
  pct_prim <- length(prim) && all(vapply(prim, function(s) identical(s$grouping, "percentStacked"), logical(1)))
  pct_sec  <- length(sec)  && all(vapply(sec,  function(s) identical(s$grouping, "percentStacked"), logical(1)))
  r1 <- axis_range(prim)
  y1 <- plot_scale(r1[1], r1[2], chart$axis_params$y, pad = !pct_prim)
  y2 <- NULL
  if (length(sec) || !is.null(chart$y2_title$text)) {
    r2 <- if (length(sec)) axis_range(sec) else r1
    y2 <- plot_scale(r2[1], r2[2], chart$axis_params$y2, pad = !pct_sec)
  }
  on_x2 <- vapply(series, function(s) s$sec_type %in% c("x", "xy"), logical(1))
  x2 <- NULL
  if (is_xy) {
    xs <- unlist(lapply(series[!on_x2], x_of))
    if (!length(xs)) xs <- unlist(lapply(series, x_of))
    xa <- plot_scale(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE), chart$axis_params$x)
    if (any(on_x2) || !is.null(chart$x2_title$text)) {
      xs2 <- if (any(on_x2)) unlist(lapply(series[on_x2], x_of)) else xs
      x2 <- plot_scale(min(xs2, na.rm = TRUE), max(xs2, na.rm = TRUE), chart$axis_params$x2)
    }
  } else if (is_date) {
    step <- chart$axis_params$x$major
    if (!is.null(step)) {
      step <- step * unit_in_base(chart$axis_params$x$major_time)
    } else {
      candidates <- switch(date_unit,
        days = c(1, 2, 3, 4, 5, 7, 10, 14, 21, 28, 61, 91, 182, 365, 730),
        months = c(1, 2, 3, 4, 6, 12, 24, 60, 120),
        years = c(1, 2, 5, 10, 20, 50, 100)
      )
      for (cand in candidates) {
        if (date_span / cand <= 25) {
          step <- cand
          break
        }
      }
      if (is.null(step)) step <- candidates[length(candidates)] * ceiling(date_span / 25 / candidates[length(candidates)])
    }
    xa <- list(min = 0, max = date_span, major = step, log = NULL)
  } else {
    xa <- list(min = 0, max = n_slots, major = 1, log = NULL)
  }
  px <- chart$axis_params$x
  px2 <- chart$axis_params$x2
  py <- chart$axis_params$y
  py2 <- chart$axis_params$y2

  # transform for log axes: native coordinates are log-scaled
  tr <- function(v, sc) if (is.null(sc$log)) v else log(v, sc$log)
  lim <- function(sc, p = NULL) {
    l <- sort(tr(c(sc$min, sc$max), sc))
    if (isTRUE(p$rev)) rev(l) else l
  }

  # ---- tick labels ----
  # OOXML rotation is clockwise, grid rotation counter-clockwise; Excel
  # writes rot="-60000000" (outside the valid range) for automatic rotation
  rot_auto <- is.null(px$rotation) || abs(px$rotation) > 90
  rot_x <- if (rot_auto) 0 else -px$rotation
  if (is_xy) {
    x_ticks <- plot_ticks(xa)
    x_labels <- plot_format(x_ticks, px$format)
  } else if (is_date) {
    x_ticks <- seq(0, date_span, by = xa$major)
    x_labels <- plot_format(from_units(x_ticks), px$format)
    # date labels turn upright when they do not fit side by side
    gp_tmp <- plot_gpar_text(px, 10)
    lab_w <- max(vapply(x_labels, function(l) grid::convertWidth(grid::grobWidth(grid::textGrob(l, gp = gp_tmp)), "points", valueOnly = TRUE), numeric(1)))
    avail <- grid::convertWidth(grid::unit(1, "npc"), "points", valueOnly = TRUE) / max(1, length(x_ticks))
    if (rot_auto && lab_w + 4 > avail) rot_x <- 90
  } else {
    x_ticks <- seq_along(cats) - 0.5
    x_labels <- plot_format(cats, px$format)
    x_labels[is.na(cats)] <- ""
  }
  x_skip <- px$tick_lbl_skip
  if (!is_xy && !is_date && is.null(x_skip) && !horizontal && rot_x == 0) {
    gp_tmp <- plot_gpar_text(px, 10)
    lab_w <- max(vapply(x_labels, function(l) grid::convertWidth(grid::grobWidth(grid::textGrob(l, gp = gp_tmp)), "points", valueOnly = TRUE), numeric(1)))
    avail <- grid::convertWidth(grid::unit(1, "npc"), "points", valueOnly = TRUE) / max(1, length(cats))
    if (lab_w + 4 > avail) x_skip <- ceiling((lab_w + 4) / avail)
  }
  if (!is_xy && !is.null(x_skip) && x_skip > 1) {
    keep <- seq(1, length(x_labels), by = x_skip)
    x_labels[-keep] <- ""
  }
  disp_divisor <- function(p) {
    du <- p$disp_units
    if (is.null(du)) return(1)
    if (is.numeric(du)) return(du)
    c(hundreds = 1e2, thousands = 1e3, tenThousands = 1e4, hundredThousands = 1e5, millions = 1e6,
      tenMillions = 1e7, hundredMillions = 1e8, billions = 1e9, trillions = 1e12)[[du]]
  }
  y_ticks <- plot_ticks(y1)
  y_labels <- plot_format(y_ticks / disp_divisor(py), py$format %||% if (pct_prim) "0%" else NULL)
  y2_ticks <- if (is.null(y2)) NULL else plot_ticks(y2)
  y2_labels <- if (is.null(y2)) NULL else plot_format(y2_ticks / disp_divisor(py2), py2$format %||% if (pct_sec) "0%" else NULL)
  x2_ticks <- if (is.null(x2)) NULL else plot_ticks(x2)
  x2_labels <- if (is.null(x2)) NULL else plot_format(x2_ticks, px2$format)
  if (identical(px$label_pos, "none")) x_labels <- rep("", length(x_labels))
  if (identical(py$label_pos, "none")) y_labels <- rep("", length(y_labels))

  x_gp <- plot_gpar_text(px, 10, "#000000")
  x2_gp <- plot_gpar_text(px2, 10, "#000000")
  y_gp <- plot_gpar_text(py, 10, "#000000")
  y2_gp <- plot_gpar_text(py2, 10, "#000000")

  text_w <- function(labels, gp) {
    if (!length(labels)) return(0)
    max(vapply(labels, function(l) grid::convertWidth(grid::grobWidth(grid::textGrob(l, gp = gp)), "points", valueOnly = TRUE), numeric(1)))
  }
  text_h <- function(gp) grid::convertHeight(grid::grobHeight(grid::textGrob("Xg", gp = gp)), "points", valueOnly = TRUE)

  # the primary value axis sits at the right edge when the categories are
  # reversed or it is set to cross at the maximum, but not both
  y_side <- if (xor(isTRUE(px$rev), identical(py$crosses, "max"))) "right" else "left"

  # ---- margins around the plot area ----
  if (horizontal) {
    left_w  <- text_w(x_labels, x_gp) + 8
    bottom_h <- text_h(y_gp) + 8
    right_w <- if (is.null(y2)) 4 else 4
    top_h <- if (is.null(y2)) 4 else text_h(y2_gp) + 8
  } else {
    y_w <- text_w(y_labels, y_gp) + 8
    y2_w <- if (is.null(y2)) 0 else text_w(y2_labels, y2_gp) + 8
    left_w <- if (y_side == "left") y_w else 4
    right_w <- max(4, if (y_side == "right") y_w else 0) + y2_w
    lab_h <- text_h(x_gp)
    lab_w <- text_w(x_labels, x_gp)
    bottom_h <- if (rot_x != 0) abs(sin(rot_x * pi / 180)) * lab_w + abs(cos(rot_x * pi / 180)) * lab_h + 8 else lab_h + 8
    outer_levels <- if (!is_xy && !is_date && is.data.frame(series[[1]]$cat_levels)) ncol(series[[1]]$cat_levels) - 1 else 0
    bottom_h <- bottom_h + outer_levels * (lab_h + 6)
    top_h <- if (is.null(x2)) 4 else text_h(x2_gp) + 8
  }
  xt_h <- plot_title_height(chart$x_title, 10)
  x2t_h <- if (is.null(x2)) 0 else plot_title_height(chart$x2_title, 10)
  yt_w <- plot_title_height(chart$y_title, 10)
  y2t_w <- plot_title_height(chart$y2_title, 10)
  if (horizontal) {
    left_w <- left_w + xt_h
    bottom_h <- bottom_h + yt_w
  } else {
    bottom_h <- bottom_h + xt_h
    left_w <- left_w + yt_w
    right_w <- right_w + y2t_w
    top_h <- top_h + x2t_h
  }

  # value axis crossing: where the category axis line sits
  cross_val <- function(sc, p, default_max = FALSE) {
    if (!is.null(p$crosses_at)) return(tr(p$crosses_at, sc))
    crosses <- p$crosses %||% if (default_max) "max" else "autoZero"
    l <- lim(sc)
    if (crosses == "min") return(l[1])
    if (crosses == "max") return(l[2])
    if (!is.null(sc$log)) return(l[1])
    min(max(0, l[1]), l[2])
  }

  # ---- viewport for the plot area ----
  outer <- grid::viewport(layout = grid::grid.layout(
    3, 3,
    widths = grid::unit(c(left_w, 1, right_w), c("points", "null", "points")),
    heights = grid::unit(c(top_h, 1, bottom_h), c("points", "null", "points"))
  ))
  grid::pushViewport(outer)
  xlim <- if (horizontal) lim(y1, py) else lim(xa, px)
  ylim <- if (horizontal) lim(xa, px) else lim(y1, py)
  plot_vp <- grid::viewport(layout.pos.row = 2, layout.pos.col = 2, xscale = xlim, yscale = ylim, name = "plot")
  grid::pushViewport(plot_vp)

  # plot area fill
  ps <- chart$plot_style
  if (!is.null(ps$fill) || !is.null(ps$line)) {
    grid::grid.rect(gp = grid::gpar(fill = plot_color(ps$fill, NA), col = plot_color(ps$line, NA), lwd = (ps$line_width %||% 1) * 96 / 72))
  }

  # coordinates: cx = category/x direction, cy = value direction
  at <- function(cx, cy) if (horizontal) list(x = cy, y = cx) else list(x = cx, y = cy)

  # ---- gridlines ----
  draw_grid <- function(sc, p, along_value) {
    for (minor in c(FALSE, TRUE)) {
      style <- p[[if (minor) "minor_grid_lines" else "grid_lines"]]
      if (is.null(style) || isFALSE(style)) next
      if (minor) {
        step <- p$minor %||% (sc$major / 5)
        ticks <- seq(sc$min, sc$max, by = step)
      } else {
        ticks <- plot_ticks(sc)
      }
      gp <- plot_grid_gp(p, minor)
      for (t in tr(ticks, sc)) {
        if (along_value == !horizontal) {
          grid::grid.lines(x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(t, t), "native"), gp = gp)
        } else {
          grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(c(0, 1), "npc"), gp = gp)
        }
      }
    }
  }
  draw_grid(y1, py, TRUE)
  if (!is_xy) {
    if (!isFALSE(px$grid_lines) && !is.null(px$grid_lines)) {
      gp <- plot_grid_gp(px)
      for (t in if (is_date) x_ticks else 0:length(cats)) {
        if (horizontal) grid::grid.lines(x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(t, t), "native"), gp = gp)
        else grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(c(0, 1), "npc"), gp = gp)
      }
    }
  } else {
    draw_grid(xa, px, FALSE)
  }

  # ---- series ----
  order_of <- c(areaChart = 1, barChart = 2, lineChart = 3, scatterChart = 4, bubbleChart = 4)
  bar_groups <- Filter(function(s) s$type == "barChart", series)
  bar_key <- vapply(bar_groups, function(s) paste(s$sec_type %in% c("y", "xy"), s$grouping), character(1))
  labels_pending <- list()

  scale_of <- function(s) if (s$sec_type %in% c("y", "xy") && !is.null(y2)) y2 else y1

  y2lim <- if (is.null(y2)) NULL else lim(y2, py2)
  x2lim <- if (is.null(x2)) NULL else lim(x2, px2)
  push_scale <- function(s, clip = TRUE) {
    sec_y <- s$sec_type %in% c("y", "xy") && !is.null(y2)
    sec_x <- s$sec_type %in% c("x", "xy") && !is.null(x2)
    if (horizontal) {
      xsc <- if (sec_y) y2lim else xlim
      ysc <- if (sec_x) x2lim else ylim
    } else {
      xsc <- if (sec_x) x2lim else xlim
      ysc <- if (sec_y) y2lim else ylim
    }
    grid::pushViewport(grid::viewport(xscale = xsc, yscale = ysc, clip = if (clip) "on" else "inherit"))
  }

  # trendlines are fitted against 1, 2, ... on category axes
  trend_shift <- if (is_xy || is_date) 0 else 0.5
  trend_labels <- list()
  stack_tops <- list()
  for (s in series[order(order_of[vapply(series, function(s) s$type, character(1))])]) {
    sc <- scale_of(s)
    push_scale(s)
    col <- plot_color(s$line$color, "#4472C4")
    v <- s$values
    xs <- x_of(s)
    stacked <- s$grouping %in% c("stacked", "percentStacked") && s$type %in% c("barChart", "areaChart", "lineChart")
    key <- paste(s$type, s$sec_type %in% c("y", "xy"))
    base <- rep(0, length(v))
    if (stacked) {
      if (is.null(stack_tops[[key]])) stack_tops[[key]] <- list(pos = rep(0, length(cats)), neg = rep(0, length(cats)))
      tops <- stack_tops[[key]]
      vv <- v
      if (s$grouping == "percentStacked") {
        same_group <- Filter(function(o) paste(o$type, o$sec_type %in% c("y", "xy")) == key, series)
        tot <- Reduce(`+`, lapply(same_group, function(o) {
          a <- abs(o$values)
          length(a) <- length(cats)
          a[is.na(a)] <- 0
          a
        }))
        vv <- v / tot[seq_along(v)]
        vv[!is.finite(vv)] <- 0
      }
      vv[is.na(vv)] <- 0
      idx <- seq_along(vv)
      if (s$type == "barChart") {
        base <- ifelse(vv >= 0, tops$pos[idx], tops$neg[idx])
        top <- base + vv
        tops$pos[idx] <- ifelse(vv >= 0, top, tops$pos[idx])
        tops$neg[idx] <- ifelse(vv < 0, top, tops$neg[idx])
      } else {
        base <- tops$pos[idx]
        top <- base + vv
        tops$pos[idx] <- top
      }
      stack_tops[[key]] <- tops
      v <- top
    }
    yv <- tr(v, sc)
    if (s$type == "barChart") {
      # every series has its own slot in the cluster; stacked series only
      # sit on top of each other when the overlap says so (overlap = 100)
      k_all <- bar_groups[bar_key == paste(s$sec_type %in% c("y", "xy"), s$grouping)]
      k <- length(k_all)
      j <- which(vapply(k_all, function(o) identical(o, s), logical(1)))
      gap <- (s$gap_width %||% 150) / 100
      ov <- (s$overlap %||% 0) / 100
      w <- 1 / (k - (k - 1) * ov + gap)
      left <- xs - 0.5 + gap / 2 * w + (j - 1) * (1 - ov) * w
      b <- tr(pmax(base, sc$min), sc)
      if (!is.null(sc$log)) b <- tr(sc$min, sc)
      fill <- col
      for (i in seq_along(v)) {
        if (!is.finite(yv[i])) next
        pt <- if (horizontal) list(x = grid::unit(min(b[i], yv[i]), "native"), y = grid::unit(left[i], "native"),
                                   width = grid::unit(abs(yv[i] - b[i]), "native"), height = grid::unit(w, "native"))
              else list(x = grid::unit(left[i], "native"), y = grid::unit(min(b[i], yv[i]), "native"),
                        width = grid::unit(w, "native"), height = grid::unit(abs(yv[i] - b[i]), "native"))
        # render() writes invertIfNegative only when it is TRUE and never
        # val="0"; Excel treats the missing element as TRUE and draws negative
        # bars white with a black outline, so that is what the file shows
        inverted <- v[i] < 0
        grid::grid.rect(x = pt$x, y = pt$y, width = pt$width, height = pt$height, just = c("left", "bottom"),
                        gp = grid::gpar(fill = if (inverted) "#FFFFFF" else fill, col = if (inverted) "#000000" else NA, lwd = 1))
      }
      centers <- left + w / 2
      plot_draw_error_bars(if (horizontal) centers else centers, yv, s, horizontal = horizontal)
      if (plot_labels_on(lp)) {
        for (i in seq_along(v)) {
          if (!is.finite(yv[i])) next
          pos <- lp$pos %||% "t"
          txt <- plot_label_text(lp, cats[i], s$values[i], name = s$label_text)
          outward <- if (v[i] >= 0) c("center", "bottom") else c("center", "top")
          inward  <- if (v[i] >= 0) c("center", "top") else c("center", "bottom")
          if (pos %in% c("t", "outEnd")) {
            yy <- yv[i]
            just <- outward
          } else if (pos %in% c("b", "inBase")) {
            yy <- b[i]
            just <- outward
          } else if (pos == "inEnd") {
            yy <- yv[i]
            just <- inward
          } else {
            yy <- (yv[i] + b[i]) / 2
            just <- c("center", "center")
          }
          if (horizontal) {
            hj <- c(if (just[2] == "bottom") "left" else if (just[2] == "top") "right" else "center", "center")
            labels_pending[[length(labels_pending) + 1]] <- list(x = yy, y = centers[i], txt = txt, just = hj)
          } else {
            labels_pending[[length(labels_pending) + 1]] <- list(x = centers[i], y = yy, txt = txt, just = just)
          }
        }
      }
      if (is.list(s$trendline)) {
        tc <- plot_trend_curve(xs, s$values, s$trendline, shift = trend_shift)
        if (!is.null(tc)) {
          grid::grid.lines(tc$x, tr(tc$y, sc), default.units = "native",
            gp = grid::gpar(col = plot_color(s$trendline$color, col), lwd = 1.5 * 96 / 72))
          trend_labels[[length(trend_labels) + 1]] <- list(s = s, tc = tc, xs = xs)
        }
      }
    } else if (s$type == "areaChart") {
      ok <- is.finite(yv)
      bx <- tr(if (is.null(sc$log)) base else pmax(base, sc$min), sc)
      poly_x <- c(xs[ok], rev(xs[ok]))
      poly_y <- c(yv[ok], rev(bx[ok]))
      a <- at(poly_x, poly_y)
      grid::grid.polygon(a$x, a$y, default.units = "native", gp = grid::gpar(fill = col, col = NA))
    } else {
      # lineChart, scatterChart, bubbleChart
      if (s$type == "bubbleChart") {
        z <- s$sizes %||% abs(s$values)
        zmax <- max(unlist(lapply(series, function(o) o$sizes %||% abs(o$values))), na.rm = TRUE)
        maxd <- 0.25 * (chart$bubble_scale %||% 100) / 100
        d <- if (identical(chart$size_represents, "w")) maxd * abs(z) / zmax else maxd * sqrt(abs(z) / zmax)
        for (i in seq_along(v)) {
          if (!is.finite(yv[i]) || !is.finite(xs[i]) || is.na(d[i])) next
          if (z[i] < 0 && !isTRUE(chart$show_neg_bubbles)) next
          grid::grid.circle(x = grid::unit(xs[i], "native"), y = grid::unit(yv[i], "native"),
                            r = grid::unit(d[i] / 2, "snpc"),
                            gp = grid::gpar(fill = plot_auto_color(i, chart$palette), col = NA))
        }
      } else {
        if (!isFALSE(s$line$show)) {
          gp <- grid::gpar(col = col, lwd = (s$line$width %||% 1) * 96 / 72, lty = plot_lty(s$line$type), lineend = "round")
          ok <- is.finite(yv) & is.finite(xs)
          runs <- if (identical(chart$disp_blanks_as, "span")) list(which(ok)) else split(which(ok), cumsum(!ok)[ok])
          for (r in runs) {
            if (length(r) < 2) next
            a <- at(xs[r], yv[r])
            if (isTRUE(s$smooth)) grid::grid.xspline(a$x, a$y, shape = -0.5, open = TRUE, default.units = "native", gp = gp)
            else grid::grid.lines(a$x, a$y, default.units = "native", gp = gp)
          }
        }
        m <- s$marker
        if (s$type == "scatterChart" && (is.null(m$symbol) || m$symbol == "none")) m$symbol <- "circle"
        ok <- is.finite(yv) & is.finite(xs)
        a <- at(xs[ok], yv[ok])
        plot_draw_markers(a$x, a$y, m, col)
        if (is.list(s$error_bars) && identical(s$error_bars$axis, "x")) plot_draw_error_bars(yv, xs, s, horizontal = !horizontal)
        else plot_draw_error_bars(xs, yv, s, horizontal = horizontal)
      }
      if (plot_labels_on(lp)) {
        for (i in seq_along(v)) {
          if (!is.finite(yv[i]) || !is.finite(xs[i])) next
          txt <- plot_label_text(lp, if (is_xy) xs[i] else cats[i], s$values[i], name = s$label_text)
          pos <- lp$pos %||% "t"
          just <- switch(pos, b = c("center", "top"), l = c("right", "center"), r = c("left", "center"), ctr = c("center", "center"), c("center", "bottom"))
          off <- switch(pos, b = c(0, -4), l = c(-4, 0), r = c(4, 0), ctr = c(0, 0), c(0, 4))
          labels_pending[[length(labels_pending) + 1]] <- list(x = xs[i], y = yv[i], txt = txt, just = just, off = off)
        }
      }
      if (is.list(s$trendline)) {
        tc <- plot_trend_curve(xs, s$values, s$trendline, shift = trend_shift)
        if (!is.null(tc)) {
          a <- at(tc$x, tr(tc$y, sc))
          grid::grid.lines(a$x, a$y, default.units = "native",
            gp = grid::gpar(col = plot_color(s$trendline$color, col), lwd = 1.5 * 96 / 72))
          trend_labels[[length(trend_labels) + 1]] <- list(s = s, tc = tc, xs = xs)
        }
      }
    }
    labels_pending <- lapply(labels_pending, function(l) {
      if (is.null(l$sec)) l$sec <- s$sec_type
      l
    })
    grid::upViewport()
  }

  # equation and R-squared are shown unless they were switched off
  for (tl in trend_labels) {
    show_eq <- !isFALSE(tl$s$trendline$show_eq)
    show_r2 <- !isFALSE(tl$s$trendline$show_r2)
    if (!show_eq && !show_r2) next
    txt <- plot_trend_equation(tl$xs + trend_shift, tl$s$values, tl$s$trendline)
    if (is.null(txt)) next
    lines <- c(if (show_eq) txt$eq, if (show_r2) txt$r2)
    # the label sits above the end of the trendline; when that end
    # leaves the plot area the label goes to the top left corner instead
    sc <- scale_of(tl$s)
    n_pts <- length(tl$tc$x)
    end_y <- tr(tl$tc$y[n_pts], sc)
    l <- lim(sc)
    push_scale(tl$s, clip = FALSE)
    if (is.finite(end_y) && end_y >= min(l) && end_y <= max(l) && tl$tc$x[n_pts] < lim(xa, px)[2]) {
      a <- at(tl$tc$x[n_pts], end_y)
      grid::grid.text(paste(lines, collapse = "\n"), x = grid::unit(a$x, "native"),
                      y = grid::unit(a$y, "native") + grid::unit(6, "points"), just = c("right", "bottom"),
                      gp = grid::gpar(fontsize = 9, col = "#000000"))
    } else {
      grid::grid.text(paste(lines, collapse = "\n"), x = grid::unit(0, "npc"), y = grid::unit(1, "npc") + grid::unit(4, "points"),
                      just = c("left", "bottom"), gp = grid::gpar(fontsize = 9, col = "#000000"))
    }
    grid::upViewport()
  }

  line_series <- Filter(function(s) s$type == "lineChart", series)
  if (length(line_series) && (isTRUE(chart$drop_lines) || isTRUE(chart$high_low_lines))) {
    push_scale(line_series[[1]])
    sc <- scale_of(line_series[[1]])
    gp <- grid::gpar(col = "#000000", lwd = 0.75 * 96 / 72)
    xs <- x_of(line_series[[1]])
    mat <- do.call(rbind, lapply(line_series, function(s) {
      v <- s$values
      length(v) <- length(xs)
      v
    }))
    for (i in seq_along(xs)) {
      col_v <- mat[, i]
      if (all(is.na(col_v))) next
      if (isTRUE(chart$drop_lines)) {
        for (v in col_v[!is.na(col_v)]) {
          a <- at(c(xs[i], xs[i]), tr(c(max(sc$min, min(0, v)), v), sc))
          grid::grid.lines(a$x, a$y, default.units = "native", gp = gp)
        }
      }
      if (isTRUE(chart$high_low_lines)) {
        a <- at(c(xs[i], xs[i]), tr(range(col_v, na.rm = TRUE), sc))
        grid::grid.lines(a$x, a$y, default.units = "native", gp = gp)
      }
    }
    grid::upViewport()
  }

  for (l in labels_pending) {
    off <- l$off %||% c(0, if (l$just[2] == "bottom") 3 else if (l$just[2] == "top") -3 else 0)
    if (horizontal && is.null(l$off)) off <- c(if (l$just[1] == "left") 3 else if (l$just[1] == "right") -3 else 0, 0)
    push_scale(list(sec_type = l$sec), clip = FALSE)
    grid::grid.text(l$txt, x = grid::unit(l$x, "native") + grid::unit(off[1], "points"),
                    y = grid::unit(l$y, "native") + grid::unit(off[2], "points"), just = l$just, gp = label_gp)
    grid::upViewport()
  }

  # ---- axes ----
  # Tick marks: Excel treats a missing majorTickMark/minorTickMark as
  # "cross"; encharter only writes them when set. Minor ticks sit at the
  # minor unit (major / 5 by default), on log axes at 2..9 times a decade.
  tick_ends <- function(style, len, outward) {
    switch(style,
      none = NULL,
      out = c(0, outward * len),
      "in" = c(0, -outward * len),
      c(-len, len)
    )
  }
  minor_positions <- function(sc, p) {
    if (!is.null(sc$log)) {
      dec <- floor(log(sc$min, sc$log)):ceiling(log(sc$max, sc$log))
      pos <- as.vector(outer(2:(sc$log - 1), sc$log^dec))
      return(pos[pos > sc$min & pos < sc$max])
    }
    step <- p$minor %||% (sc$major / 5)
    pos <- seq(sc$min, sc$max, by = step)
    pos[abs((pos - sc$min) %% sc$major) > 1e-9 * sc$major & abs(((pos - sc$min) %% sc$major) - sc$major) > 1e-9 * sc$major]
  }

  x_cross <- cross_val(y1, px)
  y_cross <- if (is_xy) cross_val(xa, py) else lim(xa)[1]
  x_line_gp <- plot_axis_gp_line(px)
  y_line_gp <- plot_axis_gp_line(py)

  # category / x axis
  x_label_pos <- px$label_pos %||% "nextTo"
  x_major <- tick_ends(px$major_tick %||% "cross", 4, -1)
  if (horizontal) {
    grid::grid.lines(x = grid::unit(c(x_cross, x_cross), "native"), y = grid::unit(c(0, 1), "npc"), gp = x_line_gp)
    lab_x <- switch(x_label_pos, low = grid::unit(0, "npc"), high = grid::unit(1, "npc"), grid::unit(x_cross, "native"))
    for (i in seq_along(x_ticks)) {
      grid::grid.text(x_labels[i], x = lab_x - grid::unit(6, "points"), y = grid::unit(x_ticks[i], "native"), just = c("right", "center"), rot = rot_x, gp = x_gp)
    }
    if (!is.null(x_major)) {
      tks <- if (is_xy) tr(x_ticks, xa) else 0:length(cats)
      if (!is_xy && !is.null(px$tick_mark_skip)) tks <- tks[seq(1, length(tks), by = px$tick_mark_skip)]
      for (t in tks) grid::grid.lines(x = grid::unit(x_cross, "native") + grid::unit(x_major, "points"), y = grid::unit(c(t, t), "native"), gp = x_line_gp)
    }
    if (is_xy) {
      x_minor <- tick_ends(px$minor_tick %||% "cross", 2, -1)
      if (!is.null(x_minor)) for (t in tr(minor_positions(xa, px), xa)) grid::grid.lines(x = grid::unit(x_cross, "native") + grid::unit(x_minor, "points"), y = grid::unit(c(t, t), "native"), gp = x_line_gp)
    } else {
      # minor ticks of a category axis sit at the category centers
      x_minor <- tick_ends(px$minor_tick %||% "cross", 3, -1)
      if (!is.null(x_minor)) for (t in seq_along(cats) - 0.5) grid::grid.lines(x = grid::unit(x_cross, "native") + grid::unit(x_minor, "points"), y = grid::unit(c(t, t), "native"), gp = x_line_gp)
    }
  } else {
    grid::grid.lines(x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(x_cross, x_cross), "native"), gp = x_line_gp)
    lab_y <- switch(x_label_pos, low = grid::unit(0, "npc"), high = grid::unit(1, "npc"), grid::unit(x_cross, "native"))
    just <- if (rot_x > 0) c("right", "top") else if (rot_x < 0) c("left", "top") else c("center", "top")
    gap <- if (rot_x != 0) 10 else 6
    for (i in seq_along(x_ticks)) {
      grid::grid.text(x_labels[i], x = grid::unit(tr(x_ticks[i], xa), "native"), y = lab_y - grid::unit(gap, "points"), just = just, rot = rot_x, gp = x_gp)
    }
    if (!is.null(x_major)) {
      tks <- if (is_xy) tr(x_ticks, xa) else if (is_date) x_ticks else 0:n_slots
      if (!is_xy && !is.null(px$tick_mark_skip)) tks <- tks[seq(1, length(tks), by = px$tick_mark_skip)]
      for (t in tks) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(x_cross, "native") + grid::unit(x_major, "points"), gp = x_line_gp)
    }
    if (is_xy) {
      x_minor <- tick_ends(px$minor_tick %||% "cross", 2, -1)
      if (!is.null(x_minor)) for (t in tr(minor_positions(xa, px), xa)) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(x_cross, "native") + grid::unit(x_minor, "points"), gp = x_line_gp)
    } else if (!is_date) {
      # minor ticks of a category axis sit at the category centers
      x_minor <- tick_ends(px$minor_tick %||% "cross", 3, -1)
      if (!is.null(x_minor)) for (t in seq_len(n_slots) - 0.5) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(x_cross, "native") + grid::unit(x_minor, "points"), gp = x_line_gp)
    }
    if (is_date && !is.null(px$minor)) {
      x_minor <- tick_ends(px$minor_tick %||% "cross", 2, -1)
      minor_step <- px$minor * unit_in_base(px$minor_time)
      if (!is.null(x_minor)) for (t in seq(0, date_span, by = minor_step)) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(x_cross, "native") + grid::unit(x_minor, "points"), gp = x_line_gp)
    }
    # outer levels of multi-level categories: one row per level below the
    # inner labels, each group centered over its span with separator lines
    if (outer_levels > 0) {
      lv <- series[[1]]$cat_levels
      row_h <- text_h(x_gp) + 6
      for (level in rev(seq_len(outer_levels))) {
        depth <- outer_levels - level + 1
        labels <- lv[[level]]
        labels[is.na(labels)] <- ""
        starts <- which(nzchar(labels))
        if (!length(starts) || starts[1] != 1) starts <- c(1, starts)
        ends <- c(starts[-1] - 1, nrow(lv))
        y_row <- lab_y - grid::unit(6 + depth * row_h, "points")
        for (g in seq_along(starts)) {
          grid::grid.text(labels[starts[g]], x = grid::unit((starts[g] - 1 + ends[g]) / 2, "native"),
                          y = y_row, just = c("center", "top"), gp = x_gp)
        }
        for (b in c(starts - 1, nrow(lv))) {
          grid::grid.lines(x = grid::unit(c(b, b), "native"), y = lab_y - grid::unit(c(0, 6 + depth * row_h + text_h(x_gp)), "points"), gp = x_line_gp)
        }
      }
    }
  }

  # value axes. `side` is where the axis sits; for the primary axis of a
  # scatter chart the line is at the crossing value instead of the edge.
  draw_val_axis <- function(sc, p, ticks, labels, gp, side, line_gp, at_cross = FALSE) {
    outward <- if (side %in% c("right", "top")) 1 else -1
    major <- tick_ends(p$major_tick %||% "cross", 4, outward)
    minor <- tick_ends(p$minor_tick %||% "cross", 2, outward)
    minors <- tr(minor_positions(sc, p), sc)
    if (horizontal) {
      yy <- if (side == "top") grid::unit(1, "npc") else grid::unit(0, "npc")
      grid::grid.lines(x = grid::unit(c(0, 1), "npc"), y = yy, gp = line_gp)
      for (i in seq_along(ticks)) {
        t <- tr(ticks[i], sc)
        if (!is.null(major)) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = yy + grid::unit(major, "points"), gp = line_gp)
        grid::grid.text(labels[i], x = grid::unit(t, "native"), y = yy + grid::unit(6 * outward, "points"),
                        just = c("center", if (outward > 0) "bottom" else "top"), gp = gp)
      }
      if (!is.null(minor)) for (t in minors) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = yy + grid::unit(minor, "points"), gp = line_gp)
    } else {
      xx <- if (at_cross) grid::unit(y_cross, "native") else if (side == "right") grid::unit(1, "npc") else grid::unit(0, "npc")
      grid::grid.lines(x = xx, y = grid::unit(c(0, 1), "npc"), gp = line_gp)
      for (i in seq_along(ticks)) {
        t <- tr(ticks[i], sc)
        if (!is.null(major)) grid::grid.lines(x = xx + grid::unit(major, "points"), y = grid::unit(c(t, t), "native"), gp = line_gp)
        grid::grid.text(labels[i], x = xx + grid::unit(6 * outward, "points"), y = grid::unit(t, "native"),
                        just = c(if (outward > 0) "left" else "right", "center"), gp = gp)
      }
      if (!is.null(minor)) for (t in minors) grid::grid.lines(x = xx + grid::unit(minor, "points"), y = grid::unit(c(t, t), "native"), gp = line_gp)
    }
  }
  draw_val_axis(y1, py, y_ticks, y_labels, y_gp, if (horizontal) "bottom" else y_side, y_line_gp, at_cross = is_xy)
  if (!is.null(y2)) {
    push_scale(list(sec_type = "y"), clip = FALSE)
    draw_val_axis(y2, py2, y2_ticks, y2_labels, y2_gp, if (horizontal) "top" else "right", plot_axis_gp_line(py2))
    grid::upViewport()
  }
  if (!is.null(x2) && !horizontal) {
    push_scale(list(sec_type = "x"), clip = FALSE)
    x2_line_gp <- plot_axis_gp_line(px2)
    x2_major <- tick_ends(px2$major_tick %||% "cross", 4, 1)
    grid::grid.lines(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1, "npc"), gp = x2_line_gp)
    for (i in seq_along(x2_ticks)) {
      t <- tr(x2_ticks[i], x2)
      if (!is.null(x2_major)) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(1, "npc") + grid::unit(x2_major, "points"), gp = x2_line_gp)
      grid::grid.text(x2_labels[i], x = grid::unit(t, "native"), y = grid::unit(1, "npc") + grid::unit(6, "points"), just = c("center", "bottom"), gp = x2_gp)
    }
    x2_minor <- tick_ends(px2$minor_tick %||% "cross", 2, 1)
    if (!is.null(x2_minor) && is_xy) for (t in tr(minor_positions(x2, px2), x2)) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(1, "npc") + grid::unit(x2_minor, "points"), gp = x2_line_gp)
    grid::upViewport()
  }
  grid::upViewport()

  # ---- axis titles ----
  if (!is.null(chart$x_title$text)) {
    if (horizontal) {
      grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
      grid::grid.text(plot_title_text(chart$x_title), x = grid::unit(xt_h / 2, "points"), rot = 90, gp = plot_gpar_text(chart$x_title$style, 10, "#000000"))
    } else {
      grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 2))
      grid::grid.text(plot_title_text(chart$x_title), y = grid::unit(xt_h / 2, "points"), gp = plot_gpar_text(chart$x_title$style, 10, "#000000"))
    }
    grid::upViewport()
  }
  if (!is.null(chart$y_title$text)) {
    if (horizontal) {
      grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 2))
      grid::grid.text(plot_title_text(chart$y_title), y = grid::unit(yt_w / 2, "points"), gp = plot_gpar_text(chart$y_title$style, 10, "#000000"))
    } else {
      grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
      grid::grid.text(plot_title_text(chart$y_title), x = grid::unit(yt_w / 2, "points"), rot = 90, gp = plot_gpar_text(chart$y_title$style, 10, "#000000"))
    }
    grid::upViewport()
  }
  if (!is.null(x2) && !is.null(chart$x2_title$text) && !horizontal) {
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    grid::grid.text(plot_title_text(chart$x2_title), y = grid::unit(1, "npc") - grid::unit(x2t_h / 2, "points"), gp = plot_gpar_text(chart$x2_title$style, 10, "#000000"))
    grid::upViewport()
  }
  if (!is.null(chart$y2_title$text) && !horizontal) {
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 3))
    grid::grid.text(plot_title_text(chart$y2_title), x = grid::unit(1, "npc") - grid::unit(y2t_w / 2, "points"), rot = 90, gp = plot_gpar_text(chart$y2_title$style, 10, "#000000"))
    grid::upViewport()
  }
  grid::upViewport()

  invisible()
}

# ---------------------------------------------------------------------------
# Pie and doughnut
# ---------------------------------------------------------------------------

plot_pie <- function(chart, series) {
  s <- series[[1]]
  v <- abs(s$values)
  v[is.na(v)] <- 0
  n <- length(v)
  pal <- chart$palette
  if (length(s$line$color) > 1) pal <- s$line$color
  cols <- vapply(seq_len(n), function(i) plot_auto_color(i, pal), character(1))
  total <- sum(v)
  start <- (chart$first_slice_ang %||% 0) * pi / 180
  hole <- if (chart$type == "doughnutChart") (chart$hole_size %||% 75) / 100 else 0
  expl <- (chart$expansion %||% 0) / 100
  lp <- chart$label_params

  grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc")))
  r_out <- 0.42 / (1 + expl)
  a0 <- start
  for (i in seq_len(n)) {
    if (total == 0) break
    sweep <- 2 * pi * v[i] / total
    a1 <- a0 + sweep
    ang <- seq(a0, a1, length.out = max(2, ceiling(sweep * 60)))
    mid <- (a0 + a1) / 2
    cx <- 0.5 + expl * r_out * sin(mid)
    cy <- 0.5 + expl * r_out * cos(mid)
    px <- c(cx + r_out * sin(ang), cx + hole * r_out * sin(rev(ang)))
    py <- c(cy + r_out * cos(ang), cy + hole * r_out * cos(rev(ang)))
    grid::grid.polygon(px, py, gp = grid::gpar(fill = cols[i], col = "#FFFFFF", lwd = 1))
    if (plot_labels_on(lp) && v[i] > 0) {
      rr <- r_out * (if (hole > 0) (1 + hole) / 2 else 0.65)
      grid::grid.text(plot_label_text(lp, s$cats[i], s$values[i], pct = v[i] / total, name = s$label_text, sep = "\n"),
                      x = cx + rr * sin(mid), y = cy + rr * cos(mid), gp = plot_gpar_text(lp$style, 9, "#000000"))
    }
    a0 <- a1
  }
  grid::upViewport()

  invisible()
}

# ---------------------------------------------------------------------------
# Radar
# ---------------------------------------------------------------------------

plot_radar <- function(chart, series) {
  cats <- series[[1]]$cats
  n <- length(cats)
  all_v <- unlist(lapply(series, function(s) s$values))
  sc <- plot_scale(min(0, all_v, na.rm = TRUE), max(all_v, na.rm = TRUE), chart$axis_params$y, pad = FALSE)
  ticks <- plot_ticks(sc)
  py <- chart$axis_params$y
  px <- chart$axis_params$x
  gp_lab <- plot_gpar_text(px, 10, "#000000")

  grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc")))
  r_max <- 0.38
  ang <- 2 * pi * (seq_len(n) - 1) / n
  rad <- function(val) r_max * (val - sc$min) / (sc$max - sc$min)
  ggp <- plot_grid_gp(py)
  for (t in ticks) {
    grid::grid.polygon(0.5 + rad(t) * sin(ang), 0.5 + rad(t) * cos(ang), gp = grid::gpar(col = ggp$col, fill = NA, lwd = ggp$lwd))
  }
  # the value axis runs along every spoke: black, with crossing tick marks
  # at the major and minor units (Excel's default for missing tick marks)
  spoke_gp <- plot_axis_gp_line(py)
  major_len <- 0.008
  minor_len <- 0.005
  minor_ticks <- seq(sc$min, sc$max, by = py$minor %||% (sc$major / 5))
  for (k in seq_len(n)) {
    sx <- sin(ang[k])
    sy <- cos(ang[k])
    grid::grid.lines(c(0.5, 0.5 + r_max * sx), c(0.5, 0.5 + r_max * sy), gp = spoke_gp)
    if (!identical(py$major_tick, "none")) {
      for (t in ticks) grid::grid.lines(0.5 + rad(t) * sx + c(-1, 1) * major_len * sy, 0.5 + rad(t) * sy - c(-1, 1) * major_len * sx, gp = spoke_gp)
    }
    if (!identical(py$minor_tick, "none")) {
      for (t in minor_ticks) grid::grid.lines(0.5 + rad(t) * sx + c(-1, 1) * minor_len * sy, 0.5 + rad(t) * sy - c(-1, 1) * minor_len * sx, gp = spoke_gp)
    }
    grid::grid.text(plot_format(cats[k], px$format), x = 0.5 + (r_max + 0.05) * sx, y = 0.5 + (r_max + 0.05) * sy, gp = gp_lab)
  }
  filled <- isTRUE(series[[1]]$filled)
  if (!identical(py$label_pos, "none")) {
    for (t in ticks) grid::grid.text(plot_format(t, py$format), x = 0.5 - 0.01, y = 0.5 + rad(t), just = c("right", "center"), gp = plot_gpar_text(py, 10, "#000000"))
  }
  for (s in series) {
    col <- plot_color(s$line$color, "#4472C4")
    v <- s$values
    v[is.na(v)] <- sc$min
    x <- 0.5 + rad(v) * sin(ang[seq_along(v)])
    y <- 0.5 + rad(v) * cos(ang[seq_along(v)])
    if (filled) {
      grid::grid.polygon(x, y, gp = grid::gpar(fill = grDevices::adjustcolor(col, 0.5), col = col, lwd = 1.5))
    } else {
      grid::grid.polygon(x, y, gp = grid::gpar(fill = NA, col = col, lwd = 2.25 * 96 / 72, lty = plot_lty(s$line$type)))
      plot_draw_markers(grid::unit(x, "npc"), grid::unit(y, "npc"), s$marker, col)
    }
  }
  grid::upViewport()

  invisible()
}

# ---------------------------------------------------------------------------
# plot.Chart()
# ---------------------------------------------------------------------------

#' Plot a chart
#'
#' @description
#' Draws a `Chart` or `ChartEx` object on the current graphics device with
#' grid, approximating what a spreadsheet application shows for it.
#'
#' Supported are bar/column (clustered, stacked, percent stacked, horizontal),
#' line, area, scatter, bubble, pie, doughnut and radar charts with titles,
#' primary and secondary axes, gridlines, legend, markers, line styles, data
#' labels, trendlines and error bars, and the extended types waterfall,
#' box-and-whisker, histogram, Pareto, funnel, treemap and sunburst. 3D,
#' stock, surface, pie-of-pie and region map charts are not drawn.
#'
#' @details
#' The values come from the series caches (present when a series was added
#' from `wb_data()`) or are read from `wb`. Series added with plain range
#' strings and all `ChartEx` objects need `wb`.
#'
#' Axis scaling follows the rules for automatic axes; fonts, spacing and
#' the exact placement of labels differ from the original. Number formats are
#' approximated for common codes (`0`, `0.00`, `#,##0`, `0%`, date formats).
#'
#' @param x A `Chart` or `ChartEx` object.
#' @param wb Optional `wbWorkbook` the series reference.
#' @param newpage Call `grid::grid.newpage()` first. Default `TRUE`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
#'   Month = month.abb[1:6], Sales = c(120, 135, 128, 160, 175, 190)
#' ))
#' chart <- ec("line")$set_chart_title("Sales")$
#'   add_series(name = Sales, data = openxlsx2::wb_data(wb), label = Month, marker = "circle")
#' plot(chart)
#' @export
plot.Chart <- function(x, wb = NULL, newpage = TRUE, ...) {
  chart <- x
  if (!inherits(chart, "Chart")) {
    stop("Only standard charts (class 'Chart') can be plotted.", call. = FALSE)
  }
  if (length(chart$series_data) == 0) stop("The chart has no series.", call. = FALSE)
  plot_set_theme(wb)
  on.exit(plot_set_theme(NULL), add = TRUE)
  types <- unique(vapply(chart$series_data, function(s) s$type, character(1)))
  bad <- setdiff(types, ENCHARTER_PLOT_TYPES)
  if (length(bad)) {
    stop("Chart type not supported by plot(): ", paste(bad, collapse = ", "), call. = FALSE)
  }
  if (!is.null(wb) && !inherits(wb, "wbWorkbook")) stop("'wb' must be a wbWorkbook object.", call. = FALSE)

  series <- plot_collect(chart, wb)

  if (isTRUE(newpage)) grid::grid.newpage()
  depth0 <- length(grid::current.vpPath())
  on.exit({
    extra <- length(grid::current.vpPath()) - depth0
    if (extra > 0) grid::upViewport(extra)
  }, add = TRUE)
  cs <- chart$chart_style
  grid::grid.rect(gp = grid::gpar(fill = plot_color(cs$fill, "#FFFFFF"), col = plot_color(cs$line, "#D9D9D9"),
                                  lwd = (cs$line_width %||% 1) * 96 / 72))

  chart_w <- grid::convertWidth(grid::unit(1, "npc"), "points", valueOnly = TRUE) - 16
  title_h <- plot_title_height(chart$chart_title, 14, chart_w)

  draw_body <- function() {
    if (types[1] %in% c("pieChart", "doughnutChart")) plot_pie(chart, series)
    else if (types[1] == "radarChart") plot_radar(chart, series)
    else plot_cartesian(chart, series)
  }

  legend <- NULL
  l_pos <- chart$legend_params$pos %||% "r"
  if (l_pos != "none") {
    entries <- if (types[1] %in% c("pieChart", "doughnutChart")) {
      s <- series[[1]]
      pal <- if (length(s$line$color) > 1) s$line$color else chart$palette
      lapply(seq_along(s$values), function(i) {
        list(kind = "rect", label = plot_format(s$cats[i]), col = plot_auto_color(i, pal))
      })
    } else {
      entries <- list()
      for (s in series) {
        col <- plot_color(s$line$color, "#4472C4")
        if (s$type %in% c("lineChart", "scatterChart") || (s$type == "radarChart" && !isTRUE(series[[1]]$filled))) {
          m <- s$marker
          if (s$type == "scatterChart" && (is.null(m$symbol) || m$symbol == "none")) m$symbol <- "circle"
          entries[[length(entries) + 1]] <- list(
            kind = "line", label = s$label_text, col = if (isFALSE(s$line$show)) NA else col,
            lwd = (s$line$width %||% 1) * 96 / 72, lty = plot_lty(s$line$type),
            pch = plot_pch(m$symbol), cex = m$size %||% 5,
            mcol = plot_color(m$line$color, col), mfill = plot_color(m$fill, col))
        } else {
          entries[[length(entries) + 1]] <- list(kind = "rect", label = s$label_text, col = col)
        }
        if (is.list(s$trendline) && !types[1] %in% c("pieChart", "doughnutChart", "radarChart")) {
          entries[[length(entries) + 1]] <- list(
            kind = "line", label = plot_trend_name(s$trendline, s$label_text),
            col = plot_color(s$trendline$color, col), lwd = 1.5 * 96 / 72, lty = "solid", pch = NA_integer_)
        }
      }
      entries
    }
    if (types[1] == "bubbleChart") {
      s <- series[[1]]
      entries <- lapply(seq_along(s$values), function(i) {
        list(kind = "rect", label = plot_format(s$cats[i]), col = plot_auto_color(i, chart$palette))
      })
    }
    stacked <- all(vapply(series, function(s) {
      s$grouping %in% c("stacked", "percentStacked") && s$type %in% c("barChart", "areaChart", "lineChart")
    }, logical(1)))
    if (stacked && l_pos %in% c("l", "r", "tr")) entries <- rev(entries)
    legend <- plot_legend(entries, chart$legend_params, chart$legend_params$style, max_w = chart_w)
  }

  pad <- 8
  lw <- if (!is.null(legend) && legend$pos %in% c("l", "r")) legend$size[["w"]] + pad else 0
  lh <- if (!is.null(legend) && legend$pos %in% c("t", "b")) legend$size[["h"]] + pad else 0
  layout <- grid::grid.layout(
    5, 3,
    widths  = grid::unit(c(pad + if (identical(legend$pos, "l")) lw else 0, 1, pad + if (identical(legend$pos, "r")) lw else 0), c("points", "null", "points")),
    heights = grid::unit(c(pad + title_h, if (identical(legend$pos, "t")) lh else 0, 1, if (identical(legend$pos, "b")) lh else 0, pad), c("points", "points", "null", "points", "points"))
  )
  grid::pushViewport(grid::viewport(layout = layout))

  if (title_h > 0) {
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1:3))
    grid::pushViewport(grid::viewport(y = grid::unit(0, "npc"), height = grid::unit(title_h, "points"), just = "bottom"))
    plot_draw_title(chart$chart_title, 14, chart_w)
    grid::upViewport(2)
  }

  grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 2))
  grid::pushViewport(grid::viewport(width = grid::unit(1, "npc") - grid::unit(2 * pad, "points"),
                                    height = grid::unit(1, "npc") - grid::unit(pad, "points")))
  draw_body()
  grid::upViewport(2)

  if (!is.null(legend)) {
    row <- switch(legend$pos, t = 2, b = 4, 3)
    col <- switch(legend$pos, l = 1, r = 3, 1:3)
    grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
    grid::pushViewport(grid::viewport(
      x = if (legend$pos == "r") grid::unit(1, "npc") - grid::unit(pad, "points") else if (legend$pos == "l") grid::unit(pad, "points") else grid::unit(0.5, "npc"),
      y = grid::unit(0.5, "npc"),
      width = grid::unit(legend$size[["w"]], "points"), height = grid::unit(legend$size[["h"]], "points"),
      just = if (legend$pos == "r") "right" else if (legend$pos == "l") "left" else "center"))
    legend$draw()
    grid::upViewport(2)
  }
  grid::upViewport()
  invisible(chart)
}
