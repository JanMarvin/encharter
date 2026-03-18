# The 'Super' routing vectors
ENCHARTER_STANDARD <- c(
  "barChart", "lineChart", "areaChart", "scatterChart",
  "pieChart", "doughnutChart", "radarChart", "bubbleChart", "surfaceChart"
)

ENCHARTER_EXTENDED <- c(
  "waterfall", "sunburst", "treemap", "regionMap", "clusteredColumn", "funnel",
  "paretoLine", "boxWhisker"
)

#' Create an Encharter Chart (The Factory)
#' @param type A character string specifying the chart type
#' @export
encharter <- function(type = "lineChart") {

  match.arg(as.character(type), choices = c(ENCHARTER_STANDARD, ENCHARTER_EXTENDED))

  if (type %in% ENCHARTER_EXTENDED) {
    # Returns the ChartEx child
    ec <- ChartEx$new(type = type)
  }

  if (type %in% ENCHARTER_STANDARD) {
    # Returns the Chart child
    ec <- Chart$new(type = type)
  }

  ec
}

#' Alias for encharter()
#' @rdname encharter
#' @export
ec <- encharter
