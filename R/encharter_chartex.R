#' ChartEx R6 Class for Extended Excel Charts
#'
#' @description
#' An R6 class to create and manipulate Excel Extended Charts (ChartEx),
#' including Waterfall, Sunburst, Treemap, and Region Maps, which are not
#' supported by standard Office Open XML chart types.
#'
#' @details
#' This class uses `xml2` to manipulate the underlying XML structure and
#' integrates with `openxlsx2` for workbook generation.
#'
#' @import R6
#' @import xml2
#' @import openxlsx2
#' @export
ChartEx <- R6::R6Class(
  "ChartEx",
  public = list(
    #' @field xml The raw XML object for the chart space.
    xml = NULL,
    #' @field series_data A list containing all added series definitions.
    series_data = list(),
    #' @field chart_title The title text.
    chart_title = NULL,
    #' @field chart_title_style List of styling parameters for the title.
    chart_title_style = list(),
    #' @field x_title The X-axis title text.
    x_title = NULL,
    #' @field x_title_style List of styling parameters for the X-axis title.
    x_title_style = list(),
    #' @field y_title The Y-axis title text.
    y_title = NULL,
    #' @field y_title_style List of styling parameters for the Y-axis title.
    y_title_style = list(),
    #' @field x_axis_style Style parameters for X-axis labels.
    x_axis_style = list(),
    #' @field y_axis_style Style parameters for Y-axis labels.
    y_axis_style = list(),
    #' @field x_numfmt Number format for X-axis.
    x_numfmt = NULL,
    #' @field y_numfmt Number format for Y-axis.
    y_numfmt = NULL,
    #' @field x_axis_params Parameters for X-axis scaling/gridlines.
    x_axis_params = list(),
    #' @field y_axis_params Parameters for Y-axis scaling/gridlines.
    y_axis_params = list(),
    #' @field legend_params Parameters for legend positioning and style.
    legend_params = list(),
    #' @field data_label_params Parameters for data labels.
    data_label_params = list(),
    #' @field chart_area_style Styling for the outer chart area.
    chart_area_style = list(),
    #' @field plot_area_style Styling for the inner plot area.
    plot_area_style = list(),

    #' @description Create a new ChartEx object.
    initialize = function() {
      self$xml <- xml2::read_xml(
        '<cx:chartSpace xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                        xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                        xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex">
           <cx:chartData/><cx:chart><cx:title pos="t" align="ctr" overlay="0"/><cx:plotArea><cx:plotAreaRegion/>
           <cx:axis id="0"><cx:catScaling gapWidth="0.5"/><cx:tickLabels/></cx:axis>
           <cx:axis id="1"><cx:valScaling/><cx:majorGridlines/><cx:tickLabels/></cx:axis>
           </cx:plotArea><cx:legend pos="t" align="ctr" overlay="0"/></cx:chart></cx:chartSpace>'
      )
      self$legend_params <- list(pos = "t", align = "ctr", overlay = "0", style = list())
      self$data_label_params <- list(show = FALSE)
      self$x_axis_params <- list()
      self$y_axis_params <- list()
    },

    #' @description Set chart area styling.
    set_chart_style = function(fill = NULL, line = NULL, line_width = 1) {
      self$chart_area_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    },

    #' @description Set plot area styling.
    set_plot_style = function(fill = NULL, line = NULL, line_width = 1) {
      self$plot_area_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    },

    #' @description Set the chart title and style.
    set_chart_title = function(text, ...) {
      self$chart_title <- text
      self$chart_title_style <- list(...)
      invisible(self)
    },

    #' @description Set the X-axis title and style.
    set_x_title = function(text, ...) {
      self$x_title <- text
      self$x_title_style <- list(...)
      invisible(self)
    },

    #' @description Set the Y-axis title and style.
    set_y_title = function(text, ...) {
      self$y_title <- text
      self$y_title_style <- list(...)
      invisible(self)
    },

    #' @description Set legend properties.
    set_legend_style = function(pos = "t", align = "ctr", overlay = FALSE, font_size = NULL, font_name = NULL, bold = NULL, italic = NULL, color = NULL) {
      self$legend_params <- list(pos = pos, align = align, overlay = ifelse(overlay, "1", "0"),
                                 style = list(sz = font_size, font = font_name, b = bold, i = italic, color = color))
      invisible(self)
    },

    #' @description Set data label properties.
    set_data_label_style = function(show = TRUE, pos = "outEnd", font_size = NULL, font_name = NULL, bold = NULL, italic = NULL, color = NULL, numfmt = NULL) {
      self$data_label_params <- list(show = show, pos = pos,
                                     style = list(sz = font_size, font = font_name, b = bold, i = italic, color = color),
                                     numfmt = numfmt)
      invisible(self)
    },

    #' @description Set X-axis tick label style.
    set_x_axis_style = function(...) { self$x_axis_style <- list(...); invisible(self) },

    #' @description Set Y-axis tick label style.
    set_y_axis_style = function(...) { self$y_axis_style <- list(...); invisible(self) },

    #' @description Set X-axis number format.
    set_x_numfmt = function(f) { self$x_numfmt <- f; invisible(self) },

    #' @description Set Y-axis number format.
    set_y_numfmt = function(f) { self$y_numfmt <- f; invisible(self) },

    #' @description Set specific axis parameters (min, max, etc).
    set_axis_params = function(axis = "x", ...) {
      if (axis == "x") self$x_axis_params <- list(...) else self$y_axis_params <- list(...)
      invisible(self)
    },

    #' @description Add a data series to the chart.
    add_series = function(header_range, data_range, cat_range = NULL, type = "waterfall", fill_color = "auto", line_color = NULL, line_width = 1) {
      fix_quote = function(x) {
        if (is.null(x)) return(NULL)
        if (grepl("!", x) && !grepl("^'", x)) {
          parts <- strsplit(x, "!", fixed = TRUE)[[1]]
          return(paste0("'", parts[1], "'!", parts[2]))
        }
        return(x)
      }
      self$series_data[[length(self$series_data) + 1]] <- list(
        header = fix_quote(header_range), data = fix_quote(data_range), cat = fix_quote(cat_range),
        type = type, fill_color = fill_color, line_color = line_color, line_width = line_width
      )
      invisible(self)
    },

    #' @description Render the internal XML for writing to a file.
    render = function(id_start = 1) {
      chart_data_node <- xml2::xml_find_first(self$xml, "//cx:chartData")
      plot_area_node <- xml2::xml_find_first(self$xml, "//cx:plotArea")
      plot_region_node <- xml2::xml_find_first(self$xml, "//cx:plotAreaRegion")

      xml2::xml_children(chart_data_node) |> xml2::xml_remove()
      xml2::xml_find_all(plot_region_node, "cx:series") |> xml2::xml_remove()

      # 1. Plot Area Background (plotSurface)
      xml2::xml_find_all(plot_region_node, "cx:plotSurface") |> xml2::xml_remove()
      if (length(self$plot_area_style) > 0) {
        surf <- xml2::xml_add_child(plot_region_node, "cx:plotSurface", .where = 0)
        spPr <- xml2::xml_add_child(surf, "cx:spPr")
        if (!is.null(self$plot_area_style$fill)) private$render_color(spPr, self$plot_area_style$fill)
        if (!is.null(self$plot_area_style$line)) {
          ln <- xml2::xml_add_child(spPr, "a:ln", w = as.character(round(self$plot_area_style$line_width * 12700)))
          private$render_color(ln, self$plot_area_style$line)
        }
      }

      head_attrs <- character(); body_attrs <- character()
      v_idx <- id_start; is_hierarchical <- FALSE

      for (i in seq_along(self$series_data)) {
        s <- self$series_data[[i]]
        if (s$type %in% c("sunburst", "treemap")) is_hierarchical <- TRUE

        h_id <- paste0("_xlchart.v1.", v_idx); v_idx <- v_idx + 1
        nf_id <- paste0("_xlchart.v1.", v_idx); v_idx <- v_idx + 1
        c_id <- paste0("_xlchart.v1.", v_idx); v_idx <- v_idx + 1
        d_id <- paste0("_xlchart.v1.", v_idx); v_idx <- v_idx + 1

        dat <- xml2::xml_add_child(chart_data_node, "cx:data", id = as.character(i - 1))
        if (!is.null(s$cat)) {
          cat_node <- xml2::xml_add_child(dat, "cx:strDim", type = "cat")
          xml2::xml_add_child(cat_node, "cx:f", c_id); xml2::xml_add_child(cat_node, "cx:nf", nf_id)
        }

        dim_type <- if (s$type == "regionMap") "colorVal" else if (s$type %in% c("sunburst", "treemap")) "size" else "val"
        num_dim <- xml2::xml_add_child(dat, "cx:numDim", type = dim_type)
        xml2::xml_add_child(num_dim, "cx:f", d_id); xml2::xml_add_child(num_dim, "cx:nf", nf_id)

        ser <- xml2::xml_add_child(plot_region_node, "cx:series", layoutId = s$type, uniqueId = openxlsx2:::st_guid())
        xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(ser, "cx:tx"), "cx:txData"), "cx:f", h_id)

        if ((length(s$fill_color) == 1 && s$fill_color != "auto") || !is.null(s$line_color)) {
          spPr_ser <- xml2::xml_add_child(ser, "cx:spPr")
          if (length(s$fill_color) == 1 && s$fill_color != "auto") private$render_color(spPr_ser, s$fill_color)
          if (!is.null(s$line_color)) private$render_color(xml2::xml_add_child(spPr_ser, "a:ln", w = as.character(round(s$line_width * 12700))), s$line_color)
        }

        if (isTRUE(self$data_label_params$show)) {
          dlbls <- xml2::xml_add_child(ser, "cx:dataLabels", pos = self$data_label_params$pos %||% "outEnd")
          if (!is.null(self$data_label_params$numfmt)) xml2::xml_add_child(dlbls, "cx:numFmt", formatCode = self$data_label_params$numfmt, sourceLinked = "0")
          if (any(!vapply(self$data_label_params$style, is.null, logical(1)))) private$apply_label_style(dlbls, self$data_label_params$style)
          xml2::xml_add_child(dlbls, "cx:visibility", seriesName="0", categoryName="0", value="1")
        }

        xml2::xml_add_child(ser, "cx:dataId", val = as.character(i - 1))

        lpr <- xml2::xml_add_child(ser, "cx:layoutPr")
        if (s$type == "waterfall") {
          coords <- dims_to_rowcol(gsub(".*!", "", s$data), as_integer = TRUE)
          last_idx <- max(length(coords$row), length(coords$col)) - 1
          subtotals <- xml2::xml_add_child(lpr, "cx:subtotals")
          xml2::xml_add_child(subtotals, "cx:idx", val = "0")
          xml2::xml_add_child(subtotals, "cx:idx", val = as.character(last_idx))
        }

        head_attrs[h_id] <- s$header; body_attrs[d_id] <- s$data; body_attrs[nf_id] <- s$data
        if (!is.null(s$cat)) body_attrs[c_id] <- s$cat
      }

      # 2. Legends & Titles
      legend_node <- xml2::xml_find_first(self$xml, "//cx:legend")
      l_pos <- self$legend_params$pos %||% "t"
      if (l_pos == "none") { xml2::xml_remove(legend_node) } else {
        xml2::xml_set_attr(legend_node, "pos", l_pos)
        xml2::xml_set_attr(legend_node, "align", self$legend_params$align %||% "ctr")
        xml2::xml_set_attr(legend_node, "overlay", self$legend_params$overlay %||% "0")
        if (any(!vapply(self$legend_params$style, is.null, logical(1)))) private$apply_legend_text_style(legend_node, self$legend_params$style)
      }

      if (!is.null(self$chart_title)) private$add_rich_text(xml2::xml_find_first(self$xml, "//cx:chart/cx:title"), self$chart_title, self$chart_title_style)

      # 3. Axes
      if (!is_hierarchical) {
        axes <- xml2::xml_find_all(self$xml, "//cx:axis")
        private$apply_axis_full(axes[[1]], self$x_axis_params, self$x_title, self$x_title_style, self$x_axis_style, self$x_numfmt, TRUE)
        private$apply_axis_full(axes[[2]], self$y_axis_params, self$y_title, self$y_title_style, self$y_axis_style, self$y_numfmt, FALSE)
      }

      # 4. Chart Area Styling
      xml2::xml_find_all(self$xml, "/cx:chartSpace/cx:spPr") |> xml2::xml_remove()
      if (length(self$chart_area_style) > 0) {
        spPr_chart <- xml2::xml_add_child(self$xml, "cx:spPr")
        if (!is.null(self$chart_area_style$fill)) private$render_color(spPr_chart, self$chart_area_style$fill)
        if (!is.null(self$chart_area_style$line)) {
          ln_chart <- xml2::xml_add_child(spPr_chart, "a:ln", w = as.character(round(self$chart_area_style$line_width * 12700)))
          private$render_color(ln_chart, self$chart_area_style$line)
        }
      }

      out <- self$xml
      attr(out, "head") <- head_attrs
      attr(out, "body") <- body_attrs
      return(out)
    }
  ),
  private = list(
    apply_label_style = function(node, s) {
      txPr <- xml2::xml_add_child(node, "cx:txPr")
      xml2::xml_add_child(txPr, "a:bodyPr", lIns="0", tIns="0", rIns="0", bIns="0")
      xml2::xml_add_child(txPr, "a:lstStyle")
      p <- xml2::xml_add_child(txPr, "a:p")
      pPr <- xml2::xml_add_child(p, "a:pPr")
      set_run_attrs <- function(n, st) {
        if (!is.null(st$sz)) xml2::xml_set_attr(n, "sz", as.character(st$sz * 100))
        if (!is.null(st$b)) xml2::xml_set_attr(n, "b", if(isTRUE(st$b)) "1" else "0")
        if (!is.null(st$i)) xml2::xml_set_attr(n, "i", if(isTRUE(st$i)) "1" else "0")
      }
      defRPr <- xml2::xml_add_child(pPr, "a:defRPr")
      set_run_attrs(defRPr, s)
      if (!is.null(s$color)) private$render_color(defRPr, s$color)
      endRPr <- xml2::xml_add_child(p, "a:endParaRPr")
      set_run_attrs(endRPr, s)
      if (!is.null(s$color)) private$render_color(endRPr, s$color)
      if (!is.null(s$font)) xml2::xml_add_child(endRPr, "a:latin", typeface = s$font)
    },
    apply_axis_full = function(node, p, title, title_style, axis_style, numfmt, is_x) {
      if (length(node) == 0) return()
      axis_id <- xml2::xml_attr(node, "id")
      xml2::xml_children(node) |> xml2::xml_remove()
      xml2::xml_set_attr(node, "id", axis_id)

      scaling_tag <- if(is_x) "cx:catScaling" else "cx:valScaling"
      scaling <- xml2::xml_add_child(node, scaling_tag)
      if (is_x && !is.null(p$gap_width)) xml2::xml_set_attr(scaling, "gapWidth", as.character(p$gap_width))
      if (!is_x) {
        if (!is.null(p$min)) xml2::xml_set_attr(scaling, "min", as.character(p$min))
        if (!is.null(p$max)) xml2::xml_set_attr(scaling, "max", as.character(p$max))
      }
      if (!is.null(title)) private$add_rich_text(xml2::xml_add_child(node, "cx:title"), title, title_style)
      if (!is.null(p$grid_color) || (!is_x && identical(p$major_gridlines, TRUE))) {
        grid <- xml2::xml_add_child(node, "cx:majorGridlines")
        if (!is.null(p$grid_color)) {
          spPr <- xml2::xml_add_child(grid, "cx:spPr")
          ln <- xml2::xml_add_child(spPr, "a:ln", w = "9525")
          private$render_color_core(ln, p$grid_color, wrap = TRUE)
        }
      }
      if (!is.null(p$major_tick)) xml2::xml_add_child(node, "cx:majorTickMarks", type = p$major_tick)
      if (!is.null(p$minor_tick)) xml2::xml_add_child(node, "cx:minorTickMarks", type = p$minor_tick)
      xml2::xml_add_child(node, "cx:tickLabels")
      if (!is.null(numfmt)) xml2::xml_add_child(node, "cx:numFmt", formatCode = numfmt, sourceLinked = "0")
      if (length(axis_style) > 0) private$apply_axis_style(node, axis_style)
    },
    apply_legend_text_style = function(node, s) {
      xml2::xml_find_all(node, "cx:txPr") |> xml2::xml_remove()
      txPr <- xml2::xml_add_child(node, "cx:txPr")
      xml2::xml_add_child(txPr, "a:bodyPr", lIns="0", tIns="0", rIns="0", bIns="0", anchor="ctr", anchorCtr="1")
      xml2::xml_add_child(txPr, "a:lstStyle")
      p <- xml2::xml_add_child(txPr, "a:p")
      pPr <- xml2::xml_add_child(p, "a:pPr", algn="ctr")
      defRPr <- xml2::xml_add_child(pPr, "a:defRPr")
      if (!is.null(s$sz)) xml2::xml_set_attr(defRPr, "sz", as.character(s$sz * 100))
      if (!is.null(s$b)) xml2::xml_set_attr(defRPr, "b", if(isTRUE(s$b)) "1" else "0")
      if (!is.null(s$i)) xml2::xml_set_attr(defRPr, "i", if(isTRUE(s$i)) "1" else "0")
      endRPr <- xml2::xml_add_child(p, "a:endParaRPr")
      if (!is.null(s$sz)) xml2::xml_set_attr(endRPr, "sz", as.character(s$sz * 100))
      if (!is.null(s$b)) xml2::xml_set_attr(endRPr, "b", if(isTRUE(s$b)) "1" else "0")
      if (!is.null(s$i)) xml2::xml_set_attr(endRPr, "i", if(isTRUE(s$i)) "1" else "0")
      if (!is.null(s$color)) private$render_color(endRPr, s$color)
      if (!is.null(s$font)) xml2::xml_add_child(endRPr, "a:latin", typeface = s$font)
    },
    render_color = function(parent_node, color_val) {
      if (is.null(color_val) || identical(color_val, "auto")) return()
      fill_node <- xml2::xml_add_child(parent_node, "a:solidFill")
      private$render_color_core(fill_node, color_val)
    },
    render_color_core = function(target_node, color_val, wrap = FALSE) {
      if (is.null(color_val) || identical(color_val, "auto")) return()
      dest_node <- if(wrap) xml2::xml_add_child(target_node, "a:solidFill") else target_node
      if (inherits(color_val, "wbColour")) {
        attrs <- attributes(color_val)
        if (!is.null(attrs$theme)) {
          theme_map <- c("bg1", "tx1", "bg2", "tx2", "accent1", "accent2", "accent3", "accent4", "accent5", "accent6", "hlink", "folHlink")
          clr_node <- xml2::xml_add_child(dest_node, "a:schemeClr", val = theme_map[as.numeric(attrs$theme) + 1])
          if (!is.null(attrs$tint)) {
            tint <- as.numeric(attrs$tint); lum <- if(tint > 0) (1-tint)*100000 else (1+tint)*100000
            xml2::xml_add_child(clr_node, "a:lumMod", val = as.character(round(lum)))
            if (tint > 0) xml2::xml_add_child(clr_node, "a:lumOff", val = as.character(round(tint*100000)))
          }
          return()
        }
        if (!is.null(attrs$rgb)) {
          xml2::xml_add_child(dest_node, "a:srgbClr", val = toupper(gsub("^FF", "", attrs$rgb)))
          return()
        }
      }
      hex <- toupper(gsub("#", "", as.character(color_val)))
      if (nchar(hex) == 8) hex <- substr(hex, 3, 8)
      if (grepl("^[0-9A-F]{6}$", hex)) xml2::xml_add_child(dest_node, "a:srgbClr", val = hex)
    },
    add_rich_text = function(parent, text, s) {
      xml2::xml_children(parent) |> xml2::xml_remove()
      if (!is.null(s$fill) || !is.null(s$line)) {
        spPr <- xml2::xml_add_child(parent, "cx:spPr")
        if (!is.null(s$fill)) private$render_color(spPr, s$fill)
        if (!is.null(s$line)) {
          ln <- xml2::xml_add_child(spPr, "a:ln", w = "12700")
          private$render_color_core(ln, s$line)
        }
      }
      tx <- xml2::xml_add_child(xml2::xml_add_child(parent, "cx:tx"), "cx:rich")
      xml2::xml_add_child(tx, "a:bodyPr"); xml2::xml_add_child(tx, "a:lstStyle")
      p <- xml2::xml_add_child(tx, "a:p"); r <- xml2::xml_add_child(p, "a:r")
      rPr <- xml2::xml_add_child(r, "a:rPr")
      if (!is.null(s$sz)) xml2::xml_set_attr(rPr, "sz", as.character(s$sz*100))
      if (!is.null(s$b)) xml2::xml_set_attr(rPr, "b", ifelse(isTRUE(s$b), "1", "0"))
      if (!is.null(s$color)) private$render_color_core(rPr, s$color)
      if (!is.null(s$font)) xml2::xml_add_child(rPr, "a:latin", typeface = s$font)
      xml2::xml_add_child(r, "a:t", text)
    },
    apply_axis_style = function(node, style) {
      pr <- xml2::xml_add_child(node, "cx:txPr")
      xml2::xml_add_child(pr, "a:bodyPr"); xml2::xml_add_child(pr, "a:lstStyle")
      pPr <- xml2::xml_add_child(xml2::xml_add_child(pr, "a:p"), "a:pPr")
      defRPr <- xml2::xml_add_child(pPr, "a:defRPr")
      if (!is.null(style$sz)) xml2::xml_set_attr(defRPr, "sz", as.character(style$sz*100))
      if (!is.null(style$b)) xml2::xml_set_attr(defRPr, "b", ifelse(isTRUE(style$b), "1", "0"))
      if (!is.null(style$i)) xml2::xml_set_attr(defRPr, "i", ifelse(isTRUE(style$i), "1", "0"))
      if (!is.null(style$color)) private$render_color(defRPr, style$color)
      if (!is.null(style$font)) xml2::xml_add_child(defRPr, "a:latin", typeface = style$font)
    }
  )
)

#' Internal Helper: Null Coalescing Operator
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Add ChartEx to a Workbook
#'
#' @param wb A wbWorkbook object.
#' @param sheet The sheet name or index.
#' @param dims The cell range (e.g., "A1:G10") where the chart should be placed.
#' @param chart_obj An instance of the ChartEx R6 class.
#' @return The modified wbWorkbook object.
#' @export
wb_add_chartx <- function(wb, sheet = current_sheet(), dims = "E2", chart_obj) {
  wb <- wb$clone(deep = TRUE)
  target_sheet <- wb$validate_sheet(sheet)

  # 1. Find the highest ID to prevent collisions
  existing_names <- wb$get_named_regions()$name
  chart_ids <- grep("^_xlchart\\.v1\\.", existing_names, value = TRUE)
  id_base <- 1L
  if (length(chart_ids) > 0) {
    id_nums <- as.integer(gsub("_xlchart\\.v1\\.", "", chart_ids))
    id_base <- max(id_nums, na.rm = TRUE) + 1L
  }

  chart_xml_rendered <- chart_obj$render(id_start = id_base)

  # 2. Add Drawing to the TARGET sheet
  drw_rel_id <- wb$worksheets[[target_sheet]]$relships$drawing
  next_rid <- if (length(drw_rel_id)) {
    openxlsx2:::get_next_id(wb$drawings_rels[[drw_rel_id]])
  } else { "rId1" }

  drawing_xml <- sprintf("<xdr:wsDr xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><xdr:absoluteAnchor><xdr:pos x=\"0\" y=\"0\"/><xdr:ext cx=\"4572000\" cy=\"2926080\"/><xdr:graphicFrame macro=\"\"><xdr:nvGraphicFramePr><xdr:cNvPr id=\"2\" name=\"Chart 1\"/><xdr:cNvGraphicFramePr/></xdr:nvGraphicFramePr><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"4572000\" cy=\"2926080\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"http://schemas.microsoft.com/office/drawing/2014/chartex\"><cx:chart xmlns:cx=\"http://schemas.microsoft.com/office/drawing/2014/chartex\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"%s\"/></a:graphicData></a:graphic></xdr:graphicFrame><xdr:clientData/></xdr:absoluteAnchor></xdr:wsDr>", next_rid)

  wb$add_drawing(sheet = target_sheet, dims = dims, xml = drawing_xml)

  # 3. Handle Relationships & Chart XML
  chart_idx <- length(wb$charts$chartEx) + 1L
  rel_xml <- sprintf("<Relationship Id=\"%s\" Type=\"http://schemas.microsoft.com/office/2014/relationships/chartEx\" Target=\"../charts/chartEx%s.xml\"/>", next_rid, chart_idx)
  drw_id <- wb$worksheets[[target_sheet]]$relships$drawing
  wb$drawings_rels[[drw_id]] <- if (all(wb$drawings_rels[[drw_id]] == "")) rel_xml else c(wb$drawings_rels[[drw_id]], rel_xml)

  wb$charts <- rbind(wb$charts, data.frame(
    chart = "", colors = openxlsx2:::colors1_xml, style = openxlsx2:::styleplot_xml,
    rels = "", chartEx = as.character(chart_xml_rendered),
    relsEx = sprintf("<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId2\" Type=\"http://schemas.microsoft.com/office/2011/relationships/chartColorStyle\" Target=\"colors%s.xml\"/><Relationship Id=\"rId1\" Type=\"http://schemas.microsoft.com/office/2011/relationships/chartStyle\" Target=\"style%s.xml\"/></Relationships>", chart_idx, chart_idx),
    stringsAsFactors = FALSE
  ))

  # 4. Add Named Regions to the DATA sheet
  h_at <- attr(chart_xml_rendered, "head")
  b_at <- attr(chart_xml_rendered, "body")
  all_refs <- c(h_at, b_at)

  for (i in seq_along(all_refs)) {
    ref <- all_refs[i]
    if (is.na(ref) || ref == "") next
    sheet_part <- gsub("^'?(.*?)'?!.*$", "\\1", ref)
    range_part <- gsub("^.*!", "", ref)
    wb$add_named_region(sheet = sheet_part, dims = range_part, name = names(all_refs)[i], hidden = "1")
  }

  return(wb)
}
