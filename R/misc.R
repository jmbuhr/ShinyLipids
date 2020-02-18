#' Create color scale
#'
#' @importFrom grDevices colorRampPalette
#' @param colorCount integer
#'
#' @return A list with scale_color_ and scale_fill_
mainScale <- function(colorCount) {
  getPalette <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))
  list(
    scale_fill_manual(values  = getPalette(colorCount)),
    scale_color_manual(values = getPalette(colorCount))
  )
}

#' Test that all samples have more than one replicate
#' 
#' Used before \code{doAllPairwiseComparisons}. 
#'
#' @param data tibble. 
#' @param aesX string. Feature on the x-axis.
#' @param aesColor string. Feature mapped to the color.
#'
#' @return boolean.
testAllMoreThanOneReplicate <- function(data, aesX, aesColor) {
  data %>%
    group_by(!!sym(aesX), !!sym(aesColor)) %>%
    count() %>%
    pull(n) %>% {
      all(. > 1)
    }
}

#' Convex hull for PCA plots
#'
#' Borrowed from https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#'
#' @param mapping aesthetic mapping
#' @param data data
#' @param geom geometric element
#' @param position default = "identity"
#' @param na.rm remove NAs
#' @param show.legend show legend
#' @param inherit.aes inherit aesthetics
#' @param ... passed to layer
#'
#' @return a state for ggplot
stat_chull <- function(mapping     = NULL,
                       data        = NULL,
                       geom        = "polygon",
                       position    = "identity",
                       na.rm       = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
  StatChull <- ggproto(
    "StatChull",
    Stat,
    compute_group = function(data, scales) {
      data[chull(data$x, data$y), , drop = FALSE]
    },
    required_aes = c("x", "y")
  )
  layer(
    stat        = StatChull,
    data        = data,
    mapping     = mapping,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, ...)
  )
}

#' Make a date
#'
#' @param col character vector.
#' 
#' @return date vector.
#' @export
parseDate <- function(col) {
  as.Date(col, format = "%y%m%d")
}
