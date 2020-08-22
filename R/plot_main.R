# TODO preserve width of single bars while not shifting points to others bars
# TODO reduce cyclomatic complexity of createMainPlot

#' Creat the main plot
#' 
#' Uses ggplot2.
#'
#' @param plotData :: tibble. Data for the plot, pass it from reactive plotData()
#' @param meanPlotData :: tibble. Data of means, pass from reactive meanPlotData()
#' @param pairwiseComparisons :: tibble. Pairwise t-tests from pairwiseComparisons()
#' @param input :: list. Input list from shiny ui. Uses
#' - aesX :: string.
#' - aesColor :: string.
#' - aesFacetCol :: string.
#' - aesFacetRow :: string.
#' - mainPlotAdditionalOptions :: character vector. Options:
#' list("points", "bars", "mean", "values", "ind_values", "log", 
#' "N", "label", "swap", "free_y", "signif")
#' - errorbarType :: string. "None" | "SD" | "SEM" | "CI"
#' - summariseTechnicalReplicates :: boolean.
#' - standardizationFeatures :: character vector | NULL.
#' - standardizeWithinTechnicalReplicate :: boolean.
#'
#' @return :: ggplot object
#' @export
createMainPlot <- function(plotData,
                           meanPlotData,
                           pairwiseComparisons,
                           input) {
  
  if ("length" %in% names(plotData)) {
    plotData <- plotData %>%
      mutate(length = factor(length))
    
    meanPlotData <- meanPlotData %>%
      mutate(length = factor(length))
  }
  
  plt <- ggplot(plotData, aes(x = !!sym(input$aesX), y = value)) %>% 
    mainPlotAddColors(input$aesColor, plotData) %>% 
    mainPlotAddBars(input$mainPlotAdditionalOptions, meanPlotData) %>% 
    mainPlotAddPoints(input$mainPlotAdditionalOptions) %>% 
    mainPlotAddErrorBars(input$errorbarType, meanPlotData) %>% 
    mainPlotAddMeans(input$mainPlotAdditionalOptions, meanPlotData) %>% 
    mainPlotAddFacets(input$aesFacetRow, input$aesFacetCol, input$mainPlotAdditionalOptions) %>% 
    mainPlotAddValues(input$mainPlotAdditionalOptions, meanPlotData) %>% 
    mainPlotAddPointValues(input$mainPlotAdditionalOptions) %>% 
    mainPlotLabelPoints(input$mainPlotAdditionalOptions, input$summariseTechnicalReplicates)
  
  
  # Show N
  if ("N" %in% input$mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        data = meanPlotData,
        aes(y = -0.99, label = N),
        vjust = 1,
        hjust = 0.5,
        color = "grey10",
        position = position_dodge(width = 0.9)
      )
  }
  
  # Log scale, name of y-axis and percent format for standardized data
  if ("log" %in% input$mainPlotAdditionalOptions) {
    if (!is.null(input$standardizationFeatures) || input$standardizeWithinTechnicalReplicate) {
      yAxisName   <- "amount [ Mol % ], log1p scale"
      yAxisLabels <- scales::percent_format(scale = 1, accuracy = NULL)
      yAxisTransformation  <- "log1p"
    } else {
      yAxisName   <- "amount [ \u00b5M ], log1p scale"
      yAxisLabels <- waiver()
      yAxisTransformation  <- "log1p"
    }
  } else {
    if (!is.null(input$standardizationFeatures) || input$standardizeWithinTechnicalReplicate) {
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
      expand = expansion(mult = c(
        if_else("N" %in% input$mainPlotAdditionalOptions, 0.05, 0), 0.05)),
      name   = yAxisName,
      labels = yAxisLabels,
      trans  = yAxisTransformation)
  
  # Swap X and Y
  if ("swap" %in% input$mainPlotAdditionalOptions) {
    validate(
      need(
        !("log" %in% input$mainPlotAdditionalOptions),
        "Swapped X and Y Axis are currently not supported for a logarithmic Y-Axis"
      )
    )
    plt <- plt +
      coord_flip()
  }
  
  # Highlite significant hits
  if ("signif" %in% input$mainPlotAdditionalOptions) {
    signif <- filter(pairwiseComparisons, p.value <= 0.05) %>%
      distinct(!!sym(input$aesX))
    if (nrow(signif) > 0) {
      plt <- plt +
        geom_text(
          data = signif,
          aes(!!sym(input$aesX), Inf, label = "*", vjust = 1, hjust = 0.5),
          inherit.aes = F,
          size        = 10
        )
    }
  }
  return(plt)
}


# subfunctions ------------------------------------------------------------
mainPlotAddColors <- function(plt, aesColor, plotData) {
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
  
  plt +
    mainTheme() +
    mainScale(colorCount) +
    guides(
      color = guide_legend(ncol = 12,
                           nrow = as.integer(colorCount / 12) + 1,
                           title = aesColor),
      fill = guide_legend(ncol = 12, # useful for way too many colors
                          nrow = as.integer(colorCount / 12) + 1,
                          title = aesColor
      )
    )
}

  
mainPlotAddBars <- function(plt, mainPlotAdditionalOptions, meanPlotData) {
  if ("bars" %in% mainPlotAdditionalOptions) {
    plt +
      geom_col(
        data     = meanPlotData,
        position = position_dodge2(width = 0.9)
      )
  } else plt
}


mainPlotAddPoints <- function(plt, mainPlotAdditionalOptions) {
  if ("points" %in% mainPlotAdditionalOptions) {
    plt +
      geom_point(
        position    = position_dodge(width = 0.9),
        pch         = 21,
        alpha       = 1,
        color       = "black",
        show.legend = FALSE
      )
  } else plt
}

mainPlotAddErrorBars <- function(plt, errorbarType, meanPlotData) {
  if (errorbarType != "None") {
    plt +
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
  } else plt
}

mainPlotAddMeans <- function(plt, mainPlotAdditionalOptions, meanPlotData) {
  if ("mean" %in% mainPlotAdditionalOptions) {
    plt +
      geom_errorbar(
        data = meanPlotData,
        aes(ymin = value, ymax = value),
        position = position_dodge2(width = 0.9),
        size = 1.2
      )
  } else plt
}

mainPlotAddFacets <- function(plt, aesFacetRow, aesFacetCol, mainPlotAdditionalOptions) {
  if (aesFacetCol != "" | aesFacetRow != "") {
    facet_col <- vars(!!sym(aesFacetCol))
    facet_row <- vars(!!sym(aesFacetRow))
    
    if (aesFacetCol == "") {
      facet_col <- NULL
    }
    if (aesFacetRow == "") {
      facet_row <- NULL
    }
    
    plt +
      facet_grid(
        cols   = facet_col,
        rows   = facet_row,
        scales = if_else("free_y" %in% mainPlotAdditionalOptions, "free", "free_x"),
        space  = "free_x"
      )
  } else plt
}

mainPlotAddValues <- function(plt, mainPlotAdditionalOptions, meanPlotData) {
  if ("values" %in% mainPlotAdditionalOptions) {
    plt +
      geom_text(
        data = meanPlotData,
        aes(label = round(value, 2)),
        vjust     = 0,
        color     = "black",
        position  = position_dodge(width = 0.9)
      )
  } else plt
}

mainPlotAddPointValues <- function(plt, mainPlotAdditionalOptions) {
  if ("ind_values" %in% mainPlotAdditionalOptions) {
    plt +
      geom_text(
        aes(label = round(value, 2)),
        vjust    = 0,
        color    = "black",
        position = position_dodge(width = 0.9)
      )
  } else plt
}

mainPlotLabelPoints <- function(plt, mainPlotAdditionalOptions, summariseTechnicalReplicates) {
  if ("label" %in% mainPlotAdditionalOptions) {
    plt +
      geom_text(aes(label = !!sym(ifelse(
        summariseTechnicalReplicates,
        "sample_replicate",
        "sample_replicate_technical")
      )
      ),
      vjust    = 0,
      hjust    = 0,
      color    = "black",
      position = position_dodge(width = 0.9)
      )
  } else plt
}
