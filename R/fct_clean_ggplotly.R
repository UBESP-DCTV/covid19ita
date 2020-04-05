#' Clean version of ggplotly
#'
#' @param p a ggplot object.
#' @param width Width of the plot in pixels (optional, defaults to automatic sizing).
#' @param height Height of the plot in pixels (optional, defaults to automatic sizing).
#' @param tooltip a character vector specifying which aesthetic mappings to show in the tooltip. The default, "all", means show all the aesthetic mappings (including the unofficial "text" aesthetic). The order of variables here will also control the order they appear. For example, use tooltip = c("y", "x", "colour") if you want y first, x second, and colour last.
#' @param dynamicTicks should plotly.js dynamically generate axis tick labels? Dynamic ticks are useful for updating ticks in response to zoom/pan interactions; however, they can not always reproduce labels as they would appear in the static ggplot2 image.
#' @param layerData data from which layer should be returned?
#' @param originalData should the "original" or "scaled" data be returned?
#' @param source a character string of length 1. Match the value of this string with the source argument in event_data() to retrieve the event data corresponding to a specific plot (shiny apps can have multiple plots).
#' @param ... arguments passed onto methods.
#'
#' @export
clean_ggplotly <- function(
  p = ggplot2::last_plot(),
  width = NULL,
  height = NULL,
  tooltip = "all",
  dynamicTicks = FALSE,
  layerData = 1,
  originalData = TRUE,
  source = "A",
  ...
) {
  plotly::ggplotly(
    p = p,
    width = width,
    height = height,
    tooltip = tooltip,
    dynamicTicks = dynamicTicks,
    layerData = layerData,
    originalData = originalData,
    source = source,
    ... = ...
  ) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d"
      )
    )

}
