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


#' Make a date
#'
#' @param col character vector.
#' 
#' @return date vector.
#' @export
parseDate <- function(col) {
  as.Date(col, format = "%y%m%d")
}
