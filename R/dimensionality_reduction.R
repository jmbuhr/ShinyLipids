#' Create wide data
#' 
#' Create data in wide format for dimensionality reduction workflows
#' with tidymodels.
#'
#' @param plotData :: tibble.
#' @param input :: list. Uses:
#' - aesX :: String 
#'
#' @return :: tibble.
#' @export
createWideData <- function(plotData,
                           input) {
  plotData %>%
    pivot_wider(names_from = input$aesX,
                values_from = value)
}

pcaScaleIf <- function(rec, doIt = TRUE) {
  if (doIt) step_scale(rec, all_predictors())
  else rec
}

pcaCenterIf <- function(rec, doIt = TRUE) {
  if (doIt) step_center(rec, all_predictors())
  else rec
}

#' Create prepped pca recipe
#'
#' @param wideData :: tibble.
#' @param input :: list. Uses:
#' - summariseTechnicalReplicates :: logical
#' - aesColor
#' - pcaScale
#' - pcaCenter
#' - pcaNumberPrincipalComponents
#'
#' @return :: prepped recipe
#' @export
createPcaPrep <- function(wideData, input) {
  selectReplicate <- ifelse(input$summariseTechnicalReplicates,
                            "sample_replicate",
                            "sample_replicate_technical")
  
  recipe(~., data = wideData) %>% 
    update_role(!!sym(selectReplicate),
                !!sym(input$aesColor),
                new_role = "id") %>% 
    pcaScaleIf(input$pcaScale) %>% 
    pcaCenterIf(input$pcaCenter) %>% 
    step_pca(all_predictors(), id = "pca") %>%
    prep()
}


createPcaLoadingsPlot <- function(pcaTidy, input) {
  pcaTidy %>%
    filter(component %in% paste0("PC", 1:input$pcaNumberPrincipalComponents)) %>%
    group_by(component) %>%
    top_n(10, abs(value)) %>%
    ungroup() %>%
    mutate(terms = tidytext::reorder_within(terms, abs(value), component)) %>%
    ggplot(aes(abs(value), terms, fill = value > 0)) +
    geom_col() +
    facet_wrap(~component, scales = "free_y") +
    tidytext::scale_y_reordered() +
    labs(
      x = "Absolute value of contribution",
      y = NULL, fill = "Positive?"
    )
}




createPcaScoresPlot <- function(pcaJuice, pcaTidy, input) {
  plt <- pcaJuice %>%
    ggplot(aes(PC1, PC2, fill = !!sym(input$aesColor)))
  
  if (input$drawPcaConvexHull) {
    splitData <- split(pcaJuice, pcaJuice[[input$aesColor]])
    combinedData <- map_dfr(splitData, ~ .x[chull(.x$PC1, .x$PC2), ])
    plt <- plt +
      geom_polygon(data = combinedData,  
                   alpha = 1 / 2, show.legend = FALSE)
  }
  
  if (input$pcaVectors) {
    pcaTidyWide <- filter(pcaTidy, component %in% paste0("PC", 1:2)) %>% 
      pivot_wider(names_from = component, values_from = value)
    
    # Scaling factor for original data dimension vectors in principal component space
    scaler <- min(max(abs(pcaJuice[, "PC1"])) / max(abs(pcaTidyWide[, "PC1"])),
                max(abs(pcaJuice[, "PC2"])) / max(abs(pcaTidyWide[, "PC2"])))
    pcaTidyWide[, c("PC1", "PC2")] <- pcaTidyWide[, c("PC1", "PC2")] * scaler * 0.8
     
    plt <- plt +
      geom_segment(
        data = pcaTidyWide,
        aes(x     = 0,
            y     = 0,
            xend  = PC1,
            yend  = PC2,
            group = terms
        ),
        inherit.aes = FALSE,
        alpha = .2
      ) +
      # ggrepel::geom_label_repel(
      geom_text(
        data = pcaTidyWide,
        aes(
          x = PC1,
          y = PC2,
          label = terms
        ),
        inherit.aes = FALSE,
        alpha = .3,
        show.legend = FALSE
      )
  }
  
  plt <- plt +
    geom_point(
      pch = 21,
      alpha = 1,
      size = input$pcaPointSize / 2
    ) +
    mainTheme() +
    mainScale(colorCount = n_distinct(pcaJuice[[input$aesColor]]))
  
  if (input$pcaLabels) {
    sampleSelect <- if_else(input$summariseTechnicalReplicates,
                            "sample_replicate",
                            "sample_replicate_technical")
    plt <- plt +
      geom_text(aes(label = !!sym(sampleSelect)))
  }
  
  plt
}


pcaPlotPercentVariation <- function(pcaPrep, input) {
  sdev <- pcaPrep$steps[[3]]$res$sdev
  sdev <- sdev[1:input$pcaNumberPrincipalComponents]
  percentVariation <- sdev^2 / sum(sdev^2)
  
  tibble(
    component = fct_inorder(paste0("PC", 1:length(sdev))),
    percentVar = percentVariation ## use cumsum() to find cumulative, if you prefer
  ) %>%
  ggplot(aes(component, percentVar)) +
    geom_col() +
    geom_text(aes(label = scales::percent(percentVar)),
              vjust = 1.3, color = "white",
              size = 5,
              fontface = "bold") +
    labs(y = "% explained variation", x = NULL) +
    mainTheme() +
    scale_y_continuous(labels = scales::percent_format())
}
