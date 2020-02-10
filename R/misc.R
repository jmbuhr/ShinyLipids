#' @importFrom stats qt
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import RSQLite
#' @importFrom rlang .data
#' @importFrom grDevices chull
#' @importFrom graphics title
#' @importFrom stats p.adjust pairwise.t.test sd
#' @importFrom utils data
#' 
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(
  c(".",
    "CI_lower", "CI_upper", "N", "PC1", "PC2", "SD", "SEM", "category", "class_order",
    "datasets", "date_extraction", "date_measured", "date_sample", "date_upload", "db", 
    "func_cat", "lipid", "oh", "p.value", "pairwise", "sample_identifier", "sample_replicate",
    "sample_replicate_technical", "value")
)

# helper functions ####

# Suppress warning that not all factor levels for lipid class order are used:
quiet_fct_relevel <- purrr::quietly(forcats::fct_relevel)

possiblyQt <- possibly(stats::qt, otherwise = NA_real_)

# Returns a function that takes an interger and creates a color palette
getPalette <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))

#' Create color scale
#'
#' @param colorCount integer
#'
#' @return A list with scale_color_ and scale_fill_
mainScale <- function(colorCount) {
  list(
    scale_fill_manual(values  = getPalette(colorCount)),
    scale_color_manual(values = getPalette(colorCount))
  )
}

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
#' @param col A character vector 
#' (Column in a dataframe)
#' @return A Date vector
#' @export
parseDate <- function(col) {
  as.Date(col, format = "%y%m%d")
}
