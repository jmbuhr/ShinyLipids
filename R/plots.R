#' Create a Heatmap from data
#'
#' @param data a tibble, typically plotData()
#' @param input list of inputs from shinys app_ui.R 
#'
#' @return a heatmap as a ggplot object
#' @export
createHeatmap <- function(data, input) {
  if (!is.null(input$standardizationFeatures)) {
    fillName <- "amount [ Mol % ]"
  } else {
    fillName <- "amount [ \u00b5M ]"
  }
  
  plt <- ggplot(data) +
    aes(
      x = factor(!!sym(input$aesX)),
      y = factor(!!sym(input$aesColor)),
      fill = value
    ) +
    geom_raster() +
    mainTheme +
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
  
  # facetting
  if (input$aesFacetCol != "" & input$aesFacetRow != "") {
    plt <- plt +
      facet_grid(
        rows   = vars(!!sym(input$aesFacetCol)),
        cols   = vars(!!sym(input$aesFacetRow)),
        scales = "free"
      )
  }
  if (input$aesFacetCol != "" & input$aesFacetRow == "") {
    plt <- plt +
      facet_wrap(facets = vars(!!sym(input$aesFacetCol)), scales = "free")
  }
  plt
}


# TODO preserve width of single bars while not shifting points to others bars

#' Title
#'
#' @param plotData data for the plot, pass it from reactive plotData()
#' @param meanPlotData data of means, pass from reactive meanPlotData()
#' @param pairwiseComparisons a tibble of pairwise t-tests from pairwiseComparisons()
#' @param rangeX vector with min and max X
#' @param rangeY vector with min and max Y
#' @param input list of inputs from shiny UI
#'
#' @return a ggplot object
#' @export
createMainPlot <- function(plotData, meanPlotData, pairwiseComparisons,
                           rangeX, rangeY, input) {
  
  if ("length" %in% names(plotData)) {
    plotData <-
      plotData %>%
      ungroup() %>%
      mutate(length = factor(length)) %>%
      group_by(length)
    meanPlotData <-
      meanPlotData %>%
      ungroup() %>%
      mutate(length = factor(length)) %>%
      group_by(length)
  }
  
  # basic plot object
  plt <- plotData %>%
    ggplot()
  
  # main plot definition
  plt <- plt +
    aes(
      x = !!sym(input$aesX),
      y = value
    )
  
  # add color/fill if requested
  # number of colors needed, if any
  if (input$aesColor != "") {
    colorCount <-
      plotData[, input$aesColor] %>%
      unique() %>%
      as_vector() %>%
      length()
    
    plt <- plt +
      aes(
        color = factor(!!sym(input$aesColor)),
        fill  = factor(!!sym(input$aesColor))
      )
  } else {
    colorCount <- 0
  }
  
  # Add bars
  if ("bars" %in% input$mainPlotAdditionalOptions) {
    plt <- plt +
      geom_col(
        data     = meanPlotData,
        position = position_dodge2(width = 0.9)
      )
  }
  
  # Add points
  if ("points" %in% input$mainPlotAdditionalOptions) {
    plt <- plt +
      geom_point(
        position    = position_dodge(width = 0.9),
        pch         = 21,
        alpha       = 1,
        color       = "black",
        show.legend = F
      )
  }
  
  # Error bars and mean
  if (input$errorbarType != "None") {
    plt <- plt +
      geom_errorbar(
        data = meanPlotData,
        position = position_dodge2(width = 0.2, padding = 0.8),
        aes(ymin = switch(
          input$errorbarType,
          "SD"   = value - SD,
          "SEM"  = value - SEM,
          "CI"   = CI_lower
        ),
        ymax = switch(
          input$errorbarType,
          "SD"  = value + SD,
          "SEM" = value + SEM,
          "CI"  = CI_upper
        )),
        alpha = .8,
        color = "black"
      )
  }
  
  # Hightlight Mean
  if ("mean" %in% input$mainPlotAdditionalOptions) {
    plt <- plt +
      geom_errorbar(
        data = meanPlotData,
        aes(ymin = value, ymax = value),
        position = position_dodge2(width = 0.9),
        # color = "black",
        size = 1.2
      )
  }
  
  # facetting
  if (input$aesFacetCol != "" | input$aesFacetRow != "") {
    facet_col <- vars(!!sym(input$aesFacetCol))
    facet_row <- vars(!!sym(input$aesFacetRow))
    
    if (input$aesFacetCol == "") {
      facet_col <- NULL
    }
    if (input$aesFacetRow == "") {
      facet_row <- NULL
    }
    
    plt <- plt +
      facet_grid(
        cols   = facet_col,
        rows   = facet_row,
        scales = if_else("free_y" %in% input$mainPlotAdditionalOptions, "free", "free_x"),
        space  = "free_x"
      )
  }
  
  # Display value of means as text
  if ("values" %in% input$mainPlotAdditionalOptions) {
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
  if ("ind_values" %in% input$mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        aes(label = round(value, 2)),
        vjust    = 0,
        color    = "black",
        position = position_dodge(width = 0.9)
      )
  }
  
  # Label points
  if ("label" %in% input$mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        aes(label = !!sym(
          ifelse(
            input$summariseTechnicalReplicates,
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
  if ("N" %in% input$mainPlotAdditionalOptions) {
    plt <- plt +
      geom_text(
        data = meanPlotData,
        aes(y = -Inf, label = N),
        vjust = -1,
        hjust = 0.5,
        color = "grey10",
        position = position_dodge(width = 0.9)
      )
  }
  
  # add theme and scale (defined in global.R) includes titles and formatting
  plt <- plt +
    mainTheme +
    mainScale(colorCount) +
    guides(
      color = guide_legend(
        ncol = 12,
        nrow = as.integer(colorCount / 12) + 1,
        title = input$aesColor
      ),
      # usefull if way to many values of color
      fill = guide_legend(
        ncol = 12,
        nrow = as.integer(colorCount / 12) + 1,
        title = input$aesColor
      )
    )
  
  # Log scale, name of y-axis and percent format for standardized data
  if ("log" %in% input$mainPlotAdditionalOptions) {
    if ( !is.null(input$standardizationFeatures) || input$standardizeWithinTechnicalReplicate) {
      yAxisName   <- "amount [ Mol % ], log1p scale"
      yAxisLabels <- scales::percent_format(scale = 1, accuracy = NULL)
      yAxisTransformation  <- "log1p"
    } else {
      yAxisName   <- "amount [ \u00b5M ], log1p scale"
      yAxisLabels <- waiver()
      yAxisTransformation  <- "log1p"
    }
  } else {
    if ( !is.null(input$standardizationFeatures) || input$standardizeWithinTechnicalReplicate) {
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
  
  plt
}

