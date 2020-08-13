#' Create a Heatmap from data
#'
#' @param plotData tibble. typically meanPlotData()
#' @param input :: list. Uses:
#' - standardizationFeatures NULL | character vector.
#' - aesX :: string.
#' - aesColor :: string.
#' - aesFacetCol :: string.
#' - aesFacetRow :: string.
#' - heatLabSize :: numeric. Size of axis labels
#' - heatColor :: string. Color palette
#'   One of c("viridis", "magma", "plasma", "inferno", "cividis")
#' - heatLogScale :: logical.
#'
#' @return ggplot. Heatmap
#' @export
createHeatmap <- function(plotData, input) {
  # Log scale, name of y-axis and percent format for standardized data
  if (input$heatLogScale) {
    plotData$value <- log1p(plotData$value)
    if (!is.null(input$standardizationFeatures) || input$standardizeWithinTechnicalReplicate) {
      fillName   <- "log1 apmount [ Mol % ]"
    } else {
      fillName  <- "log1p amount [ \u00b5M ]"
    }
  } else {
    if (!is.null(input$standardizationFeatures) || input$standardizeWithinTechnicalReplicate) {
      fillName   <- "amount [ Mol % ]"
    } else {
      fillName   <- "amount [ \u00b5M ]"
    }
  }
  
  plt <- ggplot(plotData) +
    aes(
      x = factor(!!sym(input$aesX)),
      y = factor(!!sym(input$aesColor)),
      fill = value
    ) +
    geom_raster() +
    mainTheme() +
    theme(
      axis.text.y      = element_text(size = input$heatLabSize, colour = "black"),
      plot.background  = element_blank(),
      panel.grid       = element_blank(),
      panel.background = element_rect(colour = NA, fill = "grey80")
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_viridis_c(option = input$heatColor) +
    labs(
      y    = input$aesColor,
      x    = input$aesX,
      fill = fillName
    )
  
  if (input$aesFacetCol != "" & input$aesFacetRow != "") {
    plt <- plt +
      facet_grid(
        rows   = vars(!!sym(input$aesFacetRow)),
        cols   = vars(!!sym(input$aesFacetCol)),
        scales = "free"
      )
  } else {
    if (input$aesFacetCol != "") {
      plt <- plt +
        facet_wrap(facets = vars(!!sym(input$aesFacetCol)), scales = "free")
    }
    if (input$aesFacetRow != "") {
      plt <- plt +
        facet_wrap(facets = vars(!!sym(input$aesFacetRow)), scales = "free")
    }
  }
  plt
}
