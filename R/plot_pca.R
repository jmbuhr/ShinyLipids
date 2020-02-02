createPcaData <- function(plotData, summariseTecRep, aesX) {
  selectReplicate <- ifelse(summariseTecRep,
                            "sample_replicate",
                            "sample_replicate_technical")
  
  plotData %>%
    ungroup() %>%
    select(
      !!sym(selectReplicate),
      !!sym(aesX), value
    ) %>%
    spread(key = aesX, value = "value") %>%
    data.frame(row.names = TRUE) %>%
    as.matrix() 
}


createPcaResult <- function(pcaData,
                            pcaMethod,
                            pcaNumberPrincipalComponents,
                            pcaCenter,
                            pcaScalingMethod,
                            pcaCrossValidationMethod) {
  pcaMethods::pca(
    pcaData,
    method = if_else(any(is.na(pcaData)), "nipals", pcaMethod),
    nPcs   = pcaNumberPrincipalComponents,
    center = pcaCenter,
    scale  = pcaScalingMethod,
    cv     = pcaCrossValidationMethod
  )
}

getPcaSampleNames <- function(plotData, summariseTechnicalReplicates) {
  if (summariseTechnicalReplicates) {
    plotData %>%
      ungroup() %>%
      select(sample, sample_replicate) %>%
      distinct() %>%
      mutate(
        sample           = as.character(sample),
        sample_replicate = as.character(sample_replicate)
      )
  } else {
    plotData %>%
      ungroup() %>%
      select(sample, sample_replicate_technical) %>%
      distinct() %>%
      mutate(
        sample                     = as.character(sample),
        sample_replicate_technical = as.character(sample_replicate_technical)
      )
  }
}


pcaScaleLoadings <- function(pcaObject,
                             pcaSampleNames,
                             aesX,
                             summariseTechnicalReplicates) {
  loadings <- pcaObject@loadings %>% as_tibble(rownames = aesX)
  scores <- pcaObject@scores %>%
    as_tibble(
      rownames = if_else(
        summariseTechnicalReplicates,
        "sample_replicate",
        "sample_replicate_technical"
      )
    ) %>%
    left_join(pcaSampleNames,
              by = if_else(
                summariseTechnicalReplicates,
                "sample_replicate",
                "sample_replicate_technical"
              )
    )
  # Scaling factor for original data dimension vectors in principal component space
  scaler <- min(
      max(abs(scores[, "PC1"])) / max(abs(loadings[, "PC1"])),
      max(abs(scores[, "PC2"])) / max(abs(loadings[, "PC2"])))
  loadings[, c("PC1", "PC2")] <- loadings[, c("PC1", "PC2")] * scaler * 0.8
  loadings
}

createPcaScoresPlot <- function(pcaData,
                                pcaObject,
                                pcaSampleNames,
                                scaledLoadings,
                                aesX,
                                summariseTechnicalReplicates,
                                drawPcaConvexHull,
                                pcaPointSize,
                                pcaLabels,
                                pcaVectors) {
  colorCount <- nrow(pcaData)
  scores <- pcaObject@scores %>%
    as_tibble(
      rownames = if_else(
        summariseTechnicalReplicates,
        "sample_replicate",
        "sample_replicate_technical"
      )
    ) %>%
    left_join(
      pcaSampleNames,
      by = if_else(
        summariseTechnicalReplicates,
        "sample_replicate",
        "sample_replicate_technical"
      )
    )
  
  scores$sample <- factor(scores$sample)
  
  plt <- scores %>%
    ggplot(aes(PC1, PC2, fill = sample))
  
  if (drawPcaConvexHull) {
    plt <- plt +
      stat_chull(alpha = .15, show.legend = FALSE)
  }
  
  plt <- plt +
    geom_point(
      pch = 21,
      alpha = 1,
      size = pcaPointSize / 2
    ) +
    mainTheme +
    mainScale(colorCount = colorCount)
  
  if (pcaLabels) {
    plt <- plt +
      ggrepel::geom_text_repel(aes(label = !!sym(
        ifelse(
          summariseTechnicalReplicates,
          "sample_replicate",
          "sample_replicate_technical"
        )
      )), show.legend = FALSE)
  }
  
  # Add scaled orginal vectors as arrows
  if (pcaVectors) {
    plt <- plt +
      geom_segment(
        data = scaledLoadings,
        aes(
          x = 0,
          y = 0,
          xend = PC1,
          yend = PC2,
          group = !!sym(aesX)
        ),
        inherit.aes = FALSE,
        arrow = arrow(),
        alpha = .3
      ) +
      ggrepel::geom_label_repel(
        data = scaledLoadings,
        aes(
          x = PC1,
          y = PC2,
          label = !!sym(aesX)
        ),
        inherit.aes = FALSE,
        alpha = .3,
        show.legend = FALSE
      )
  }
  plt
}


createPcaLoadingsPlot <- function(pcaObject, aesX, pcaPointSize) {
  loadings <-
    pcaObject@loadings %>%
    as_tibble(rownames = aesX)
  
  loadings %>%
    ggplot(aes(PC1, PC2)) +
    geom_point(pch = 19, size = pcaPointSize / 3) +
    mainTheme +
    ggrepel::geom_text_repel(aes(label = !!sym(aesX)),
                             show.legend = FALSE
    )
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
      data[chull(data$x, data$y),, drop = FALSE]
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


