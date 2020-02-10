# TODO preserve width of single bars while not shifting points to others bars
# TODO reduce cyclomatic complexity of createMainPlot

#' Title
#'
#' @param plotData tibble. Data for the plot, pass it from reactive plotData()
#' @param meanPlotData tibble. Data of means, pass from reactive meanPlotData()
#' @param pairwiseComparisons tibble. Pairwise t-tests from pairwiseComparisons()
#' @param rangeX numeric vector | NULL. vector with min and max X
#' @param rangeY numeric vector | NULL.  vector with min and max Y
#' @param aesX string.
#' @param aesColor string.
#' @param aesFacetCol string.
#' @param aesFacetRow string.
#' @param mainPlotAdditionalOptions character vector. Options:
#' list("points", "bars", "mean", "values", "ind_values", "log", 
#' "N", "label", "swap", "free_y", "signif")
#' @param errorbarType string. "None" | "SD" | "SEM" | "CI"
#' @param summariseTechnicalReplicates boolean.
#' @param standardizationFeatures character vector | NULL.
#' @param standardizeWithinTechnicalReplicate boolean.
#'
#' @return ggplot object
#' @export
createMainPlot <- function(plotData,
                           meanPlotData,
                           pairwiseComparisons,
                           rangeX                              = NULL,
                           rangeY                              = NULL,
                           aesX                                = "class",
                           aesColor                            = "sample",
                           aesFacetCol                         = "",
                           aesFacetRow                         = "",
                           mainPlotAdditionalOptions           = list("points", "bars"),
                           errorbarType                        = "None",
                           summariseTechnicalReplicates        = TRUE,
                           standardizationFeatures             = NULL,
                           standardizeWithinTechnicalReplicate = TRUE) {
  
  if ("length" %in% names(plotData)) {
    plotData <- plotData %>%
      ungroup() %>%
      mutate(length = factor(length)) %>%
      group_by(length)
    
    meanPlotData <- meanPlotData %>%
      ungroup() %>%
      mutate(length = factor(length)) %>%
      group_by(length)
  }
  
  plt <- ggplot(plotData, aes(x = !!sym(aesX), y = value))
  
  # add color/fill if requested
  # number of colors needed, if any
  if (aesColor != "") {
    colorCount <- plotData[[aesColor]] %>%
      unique() %>% 
      length()
    
    plt <- plt +
      aes(
        color = factor(!!sym(aesColor)),
        fill  = factor(!!sym(aesColor)))
  } else {
    colorCount <- 0
  }
  
  plt <- plt +
    mainTheme +
    mainScale(colorCount) +
    guides(
      color = guide_legend(ncol = 12,
                           nrow = as.integer(colorCount / 12) + 1,
                           title = aesColor),
      fill = guide_legend(ncol = 12, # usefull with way too many colors
                          nrow = as.integer(colorCount / 12) + 1,
                          title = aesColor
      )
    )
  
  # Add bars
  if ("bars" %in% mainPlotAdditionalOptions) {
    plt <- plt +
      geom_col(
        data     = meanPlotData,
        position = position_dodge2(width = 0.9)
      )
  }
  
  # Add points
  if ("points" %in% mainPlotAdditionalOptions) {
    plt <- plt +
      geom_point(
        position    = position_dodge(width = 0.9),
        pch         = 21,
        alpha       = 1,
        color       = "black",
        show.legend = FALSE
      )
  }
  
  # Error bars and mean
  if (errorbarType != "None") {
    plt <- plt +
      geom_errorbar(
        data = meanPlotData,
        position = position_dodge2(width = 0.2, padding = 0.8),
        aes(ymin = switch(
          errorbarType,
          "SD"   = value - SD,
          "SEM"  = value - SEM,
          "CI"   = CI_lower
        ),
        ymax = switch(
          errorbarType,
          "SD"  = value + SD,
          "SEM" = value + SEM,
          "CI"  = CI_upper
        )),
        alpha = .8,
        color = "black"
      )
  }
  
  # Hightlight Mean
  if ("mean" %in% mainPlotAdditionalOptions) {
    plt <- plt +
      geom_errorbar(
        data = meanPlotData,
        aes(ymin = value, ymax = value),
        position = position_dodge2(width = 0.9),
        size = 1.2
      )
  }
  
  # facetting
  if (aesFacetCol != "" | aesFacetRow != "") {
    facet_col <- vars(!!sym(aesFacetCol))
    facet_row <- vars(!!sym(aesFacetRow))
    
    if (aesFacetCol == "") {
      facet_col <- NULL
    }
    if (aesFacetRow == "") {
      facet_row <- NULL
    }
    
    plt <- plt +
      facet_grid(
        cols   = facet_col,
        rows   = facet_row,
        scales = if_else("free_y" %in% mainPlotAdditionalOptions, "free", "free_x"),
        space  = "free_x"
      )
  }
  
  # Display value of means as text
  if ("values" %in% mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        data = meanPlotData,
        aes(label = round(value, 2)),
        vjust     = 0,
        color     = "black",
        position  = position_dodge(width = 0.9)
      )
  }
  
  # Display value of points as text
  if ("ind_values" %in% mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        aes(label = round(value, 2)),
        vjust    = 0,
        color    = "black",
        position = position_dodge(width = 0.9)
      )
  }
  
  # Label points
  if ("label" %in% mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        aes(label = !!sym(
          ifelse(
            summariseTechnicalReplicates,
            "sample_replicate",
            "sample_replicate_technical"
          )
        )),
        vjust    = 0,
        hjust    = 0,
        color    = "black",
        position = position_dodge(width = 0.9)
      )
  }
  
  # Show N
  if ("N" %in% mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        data = meanPlotData,
        aes(y = 0, label = N),
        vjust = 1.2,
        hjust = 0.5,
        color = "grey10",
        position = position_dodge(width = 0.9)
      )
  }
  
  # Log scale, name of y-axis and percent format for standardized data
  if ("log" %in% mainPlotAdditionalOptions) {
    if (!is.null(standardizationFeatures) || standardizeWithinTechnicalReplicate) {
      yAxisName   <- "amount [ Mol % ], log1p scale"
      yAxisLabels <- scales::percent_format(scale = 1, accuracy = NULL)
      yAxisTransformation  <- "log1p"
    } else {
      yAxisName   <- "amount [ \u00b5M ], log1p scale"
      yAxisLabels <- waiver()
      yAxisTransformation  <- "log1p"
    }
  } else {
    if (!is.null(standardizationFeatures) || standardizeWithinTechnicalReplicate) {
      yAxisName   <- "amount [ Mol % ]"
      yAxisLabels <- scales::percent_format(scale = 1, accuracy = NULL)
      yAxisTransformation  <- "identity"
    } else {
      yAxisName   <- "amount [ \u00b5M ]"
      yAxisLabels <- scales::number_format()
      yAxisTransformation  <- "identity"
    }
  }
  
  plt <- plt +
    scale_y_continuous(
      name   = yAxisName,
      labels = yAxisLabels,
      trans  = yAxisTransformation
    )
  
  # Zooming
  plt <-
    plt + coord_cartesian(xlim = rangeX, ylim = rangeY)
  
  # Swap X and Y
  if ("swap" %in% mainPlotAdditionalOptions) {
    validate(
      need(
        !("log" %in% mainPlotAdditionalOptions),
        "Swapped X and Y Axis are currently not supported for a logarithmic Y-Axis"
      )
    )
    plt <- plt +
      coord_flip()
  }
  
  # Highlite significant hits
  if ("signif" %in% mainPlotAdditionalOptions) {
    signif <- filter(pairwiseComparisons, p.value <= 0.05) %>%
      distinct(!!sym(aesX))
    if (nrow(signif) > 0) {
      plt <- plt +
        geom_text(
          data = signif,
          aes(!!sym(aesX), Inf, label = "*", vjust = 1, hjust = 0.5),
          inherit.aes = F,
          size        = 10
        )
    }
  }
  plt
}
