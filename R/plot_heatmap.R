#' Create a Heatmap from data
#'
#' @param data tibble. typically plotData()
#' @param standardizationFeatures NULL | character vector.
#' @param aesX string.
#' @param aesColor string.
#' @param aesFacetCol string.
#' @param aesFacetRow string.
#' @param heatLabSize numeric. Size of axis labels
#' @param heatColor string. Color palette
#' One of c("viridis", "magma", "plasma", "inferno", "cividis")
#'
#' @return ggplot. Heatmap
#' @export
createHeatmap <- function(data,
                          standardizationFeatures   = NULL,
                          aesX                      = "class",
                          aesColor                  = "sample",
                          aesFacetCol               = "",
                          aesFacetRow               = "",
                          heatLabSize               = 9,
                          heatColor                 = "viridis") {
  if (!is.null(standardizationFeatures)) { # TODO check other standardization
    fillName <- "amount [ Mol % ]"
  } else {
    fillName <- "amount [ \u00b5M ]"
  }
  
  plt <- ggplot(data) +
    aes(
      x = factor(!!sym(aesX)),
      y = factor(!!sym(aesColor)),
      fill = value
    ) +
    geom_raster() +
    mainTheme() +
    theme(
      axis.text.y      = element_text(size = heatLabSize, colour = "black"),
      plot.background  = element_blank(),
      panel.grid       = element_blank(),
      panel.background = element_rect(colour = NA, fill = "grey80")
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_viridis_c(option = heatColor) +
    labs(
      y    = aesColor,
      x    = aesX,
      fill = fillName
    )
  
  if (aesFacetCol != "" & aesFacetRow != "") {
    plt <- plt +
      facet_grid(
        rows   = vars(!!sym(aesFacetRow)),
        cols   = vars(!!sym(aesFacetCol)),
        scales = "free"
      )
  } else {
    if (aesFacetCol != "") {
      plt <- plt +
        facet_wrap(facets = vars(!!sym(aesFacetCol)), scales = "free")
    }
    if (aesFacetRow != "") {
      plt <- plt +
        facet_wrap(facets = vars(!!sym(aesFacetRow)), scales = "free")
    }
  }
  plt
}
