#' @import shiny
app_server <- function(input, output, session) {
  databaseConnection <- golem::get_golem_options("db")
  # Metadata / Datasets -------------------------------------------------------------------------------------------
  
  # Reading in table of datasets
  metaData <- reactive({
    collectMetaData(databaseConnection)
  })
  
  # Rendering datasets as a table to send to UI
  output$metaDataTable <- DT::renderDT({
    req(metaData())
    
    if (input$showFullMeta == TRUE) {
      meta <- metaData()
    } else {
      meta <-
        metaData()[c( "id", "title", "date_upload", "status", "sample_from")]
    }
    meta
  },
  server = FALSE, selection = list(mode = "single", selected = 1),
  options = list(
    orderClasses   = TRUE,
    pageLength     = 10,
    order          = list(0, "desc"),
    scrollX        = TRUE,
    deferRender    = TRUE,
    scrollY        = 500,
    scrollCollapse = TRUE
  )
  )
  
  ## Update SelectInput for datasets based on sets loaded and row selected
  # select clicked row in table
  observe({
    choices <- list(metaData()$id)
    names(choices) <- paste(metaData()$id, "-", metaData()$title)
    selection <- input$metaDataTable_rows_selected
    if (!is.null(selection)) {
      updateSelectInput(session,
                        "ID",
                        choices  = choices,
                        selected = choices[[selection]]
      )
    }
  })
  
  # Data ------------------------------------------------------------------------------------------------------------

  # * Reading in raw data based on dataset selected -----------------------------------------------------------------
  rawData <- reactive({
    # Only runs if a dataset is selected
    validate(need(input$ID, "Please select a dataset first."))
    query <- createQueryForID(input$ID)
    collectRawData(databaseConnection, query, lipidClassOrder = collectLipidClassOrder(databaseConnection))
  })
  
  output$debug <- renderText({
    cat(input$lipidClassOrder)
  })
  
  # * mainData from rawData -------------------------------------------------------------------------
  # filtering, then standardization
  mainData <- reactive({
    req(rawData())
    
    rawData() %>%
      standardizeWithinTechnicalReplicatesIf(input$standardizeWithinTechnicalReplicate) %>% 
      filterRawDataFor(input) %>% 
      standardizeRawDataWithin(baselineSample          = input$baselineSample,
                               standardizationFeatures = input$standardizationFeatures)
  })
  
  # Updating filtering options by dataset --------------------------------------------------------
  observe({
    tribble(
      ~ inputName,                    ~ choiceColumn,               ~ selectedChoice,
      "samplesToSelect",              "sample",                     NULL,
      "baselineSample",               "sample",                     ""  ,
      "samplesToRemove",              "sample",                     NULL,
      "replicatesToSelect",           "sample_replicate",           NULL,
      "replicatesToRemove",           "sample_replicate",           NULL,
      "technicalReplicatesToRemove",  "sample_replicate_technical", NULL,
      "categoryToSelect",             "category",                   NULL,
      "functionalCategoryToSelect",   "func_cat",                   NULL,
      "lipidClassToSelect",           "class",                      NULL,
      "quickClassForProfile",         "class",                      ""
    ) %>% 
      pwalk(updateAllSelectizeInputs, rawData(), session)
  
    tribble(
      ~ inputName,     ~ choiceColumn, 
      "filter_length", "length",
      "filter_db",     "db",
      "filter_oh",     "oh"
    ) %>% 
      pwalk(updateAllRangeInputs, rawData(), session)
  })
  
  observe({
    if (is.null(input$samplesToSelect)) {
      updateSelectizeInput(session,
                           "samplesToRemove",
                           choices = levels(rawData()$sample)
      )
    }
    if (!is.null(input$samplesToSelect)) {
      updateSelectizeInput(session,
                           "samplesToRemove",
                           choices = unname(input$samplesToSelect)
      )
    }
  })
  

# Update inputs based on selected default quickoption ---------------------
  
  observeEvent(input$quickClassForProfile, {
    if (input$quickClassForProfile != "") {
      updateSelectInput(session, "aesFacetCol", selected = "")
      updateSelectInput(session, "aesFacetRow", selected = "")
      updateSelectizeInput(session, "standardizationFeatures", selected = c("class", "sample_replicate"))
      updateSelectInput(session, "aesX", selected = "lipid")
      updateSelectizeInput(session, "lipidClassToSelect", selected = unname(input$quickClassForProfile) )
    }
  })
  
  observeEvent(input$class_profile, {
    updateSelectInput(session, "aesFacetCol", selected = "")
    updateSelectizeInput(session, "standardizationFeatures", selected = "")
    updateSelectInput(session, "aesX", selected = "class")
    updateSelectizeInput(session, "lipidClassToSelect", selected = "")
  })
  
  # Displaying main Dataset as a table ----------------------------------------------------------------------------
  
  # Rendering selected dataset as a table to send to UI
  output$mainDataTable <- DT::renderDT({
    mainData()
  },
  filter = "none",
  rownames = FALSE,
  options  = list(
    orderClasses   = TRUE,
    pageLength     = 10,
    order          = list(0, "desc"),
    scrollX        = TRUE,
    deferRender    = TRUE,
    scrollCollapse = TRUE
  )
  )
  
  # plotData from mainData based on sidebar inputs ---------------------------------------------------------------
  
  # with apropriate summarize functions based on selecte plot type, standards and aes
  plotData <- reactive({
    req(mainData())
    # Validations, friendly error messages
    validate(
      need(
        !(
          input$summariseTechnicalReplicates &
            (
              input$aesColor    == "sample_replicate_technical" |
                input$aesX      == "sample_replicate_technical" |
                # input$aesY    == "sample_replicate_technical" |
                input$aesFacetCol == "sample_replicate_technical" |
                input$aesFacetRow == "sample_replicate_technical"
            )
        ),
        "You are currently averaging over technical replicates (see the samples tab in the sidebar)
                  and thus can't use this feature in your plots."
      ),
      need(
        input$aesX != "",
        "Please select a feature to display on the x-axis"
      )
    )
    
    mainData() %>%
      createPlotData(input)
  })
  
  
  # meanPlotData for bars/averages ----------------------------------------------------------------------------------
  
  meanPlotData <- reactive({
    req(plotData())
    plotData() %>%
      summarisePlotData()
  })
  
  
  # Pairwise Comparisons --------------------------------------------------------------------------------------------
  
  pairwiseComparisons <- reactive({
    # req(plotData())
    validate(
      need(
        input$aesColor == "sample",
        "To compare between samples, chose sample as the color"
      ),
      need(
        input$aesX %in% c("class", "category"),
        "Comparisons are only supported for class or category on the x axis"
      ),
      need(
        length(unique(plotData()$sample)) > 1,
        "You need at least 2 samples to compare them"
      ),
      need(
        testAllMoreThanOneReplicate(plotData(), input$aesX, input$aesColor),
        "You need more than 1 replicate per sample for everything visible in the plot"
      )
    )
    doAllPairwiseComparisons(plotData(), input$aesX)
  })
  
  output$pairwiseComparisonsTable <- DT::renderDT({
    pairwiseComparisons()
  })
  
  # Main Plot output ------------------------------------------------------------------------------------------------
  
  # Ranges for zooming by clicking on the plot
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$mainPlotDoubleClick, {
    brush <- input$mainPlotBrush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # * Plot Object ----------------------------------------------------------------------------------------
  mainPlot <- reactive({
    req(plotData())
    req(meanPlotData())
    createMainPlot(plotData(), meanPlotData(),
                   rangeX = ranges$x,
                   rangeY = ranges$y,
                   input  = input)
  })
  
  # ** Plot Render --------------------------------------------------------------------------------------------
  output$mainPlot <- renderPlot({
    mainPlot()
  })
  
  # meanPlotDataTable -----------------------------------------------------------------------------------------------
  output$meanPlotDataTable <- DT::renderDT({
    req(meanPlotData())
    
    meanPlotData() %>%
    select(
      Average_value = value,
      !!sym(input$aesX),
      everything()
    )
  },
  filter = "none",
  rownames = FALSE,
  options = list(
    orderClasses   = TRUE,
    pageLength     = 10,
    order          = list(0, "desc"),
    scrollX        = TRUE,
    deferRender    = TRUE,
    scrollCollapse = TRUE
  )
  )
  
  
  # Heatmap ---------------------------------------------------------------------------------------------------------
  # * Plot Object ---------------------------------------------------------------------------------------------------
  heatmapPlot <- reactive({
    # dataframe
    # df <- plotData()
    createHeatmap(data = meanPlotData(), input = input)
  })
  
  
  # * Plot Render ---------------------------------------------------------------------------------------------------
  output$heatPlot <- renderPlot({
    heatmapPlot()
  })
  
  # PCA -------------------------------------------------------------------------------------------------------------
  
  # ** Updating pca-options --------------------------------------------------------------------------------------------------
  # update nPCs, they should not exceed the dimensions of the data
  observe({
    req(pcaData())
    updateSliderInput(session,
                      "pcaNumberPrincipalComponents",
                      max = min(dim(pcaData())))
  })
  
  # * pcaData -------------------------------------------------------------------------------------------------------
  pcaData <- reactive({
    req(plotData())
    validate(
      need(
        input$aesColor == "sample",
        "To perform a PCA, please set color to sample in the mappings"
      ),
      need(
        (
          input$aesX != "sample" &
            input$aesX != "sample_replicate" &
            input$aesX != "sample_replicate_technical"
        ),
        "To perform a PCA, please select a feature other than sample as your x-axis in the mappings"
      ),
      need(
        input$aesFacetCol == "",
        "To perform a PCA, please remove any facetting in the mappings"
      ),
      need(
        input$aesFacetRow == "",
        "To perform a PCA, please remove any facetting in the mappings"
      ),
      need(
        length(unique(plotData()[[input$aesX]])) > 1,
        "Not enough datapoints to perform PCA"
      )
    )
    plotData() %>%
      ungroup() %>%
      select(
        !!sym(
          ifelse(
            input$summariseTechnicalReplicates,
            "sample_replicate",
            "sample_replicate_technical"
          )
        ),
        !!sym(input$aesX), value
      ) %>%
      spread(key = input$aesX, value = "value") %>%
      data.frame(row.names = TRUE) %>%
      as.matrix()
  })
  
  # * pcaObject -----------------------------------------------------------------------------------------------------
  pcaObject <- reactive({
    m <- pcaData()
    # returns pcaRes object
    pcaMethods::pca(
      m,
      method = if_else(any(is.na(m)), "nipals", input$pca_method),
      nPcs   = input$pcaNumberPrincipalComponents,
      center = input$pca_center,
      scale  = input$pca_scaling,
      cv     = input$pca_cv,
      seed   = 123
    )
  })
  
  
  # Scaling factor for original data dimension vectors in principal component space
  
  sampleNames <- reactive({
    if (input$summariseTechnicalReplicates) {
      plotData() %>%
        ungroup() %>%
        select(sample, sample_replicate) %>%
        distinct() %>%
        mutate(
          sample           = as.character(sample),
          sample_replicate = as.character(sample_replicate)
        )
    } else {
      plotData() %>%
        ungroup() %>%
        select(sample, sample_replicate_technical) %>%
        distinct() %>%
        mutate(
          sample                     = as.character(sample),
          sample_replicate_technical = as.character(sample_replicate_technical)
        )
    }
  })
  
  scaledLoadings <- reactive({
    req(pcaObject())
    loadings <- pcaObject()@loadings %>% as_tibble(rownames = input$aesX)
    
    scores <- pcaObject()@scores %>%
      as_tibble(
        rownames = if_else(
          input$summariseTechnicalReplicates,
          "sample_replicate",
          "sample_replicate_technical"
        )
      ) %>%
      left_join(sampleNames(),
                by = if_else(
                  input$summariseTechnicalReplicates,
                  "sample_replicate",
                  "sample_replicate_technical"
                )
      )
    
    scaler <-
      min(
        max(abs(scores[, "PC1"])) / max(abs(loadings[, "PC1"])),
        max(abs(scores[, "PC2"])) / max(abs(loadings[, "PC2"]))
      )
    loadings[, c("PC1", "PC2")] <- loadings[, c("PC1", "PC2")] * scaler * 0.8
    loadings
  })
  
  
  # * pcaOutputs ------------------------------------------------------------------------------------------------------
  # Info
  output$pcaInfo <- renderPrint({
    req(pcaObject())
    pcaObject() %>% summary()
  })
  
  # ** Scores -----------------------------------------------------------------------------------------------------
  pcaScoresPlot <- reactive({
    req(pcaData(), pcaObject(), sampleNames())
    colorCount <- rownames(pcaData()) %>% length()
    scores <- pcaObject()@scores %>%
      as_tibble(
        rownames = if_else(
          input$summariseTechnicalReplicates,
          "sample_replicate",
          "sample_replicate_technical"
        )
      ) %>%
      left_join(
        sampleNames(),
        by = if_else(
          input$summariseTechnicalReplicates,
          "sample_replicate",
          "sample_replicate_technical"
        )
      )
    
    scores$sample <- factor(scores$sample)
    
    plt <- scores %>%
      ggplot(aes(PC1, PC2, fill = sample))
    
    if (input$drawPcaConvexHull) {
      plt <- plt +
        stat_chull(alpha = .15, show.legend = FALSE)
    }
    
    plt <- plt +
      geom_point(
        pch = 21,
        alpha = 1,
        size = input$pca_pointSize / 2
      ) +
      mainTheme +
      mainScale(colorCount = colorCount)
    
    if (input$pca_labels) {
      plt <- plt +
        ggrepel::geom_text_repel(aes(label = !!sym(
          ifelse(
            input$summariseTechnicalReplicates,
            "sample_replicate",
            "sample_replicate_technical"
          )
        )), show.legend = FALSE)
    }
    
    # Add scaled orginal vectors as arrows
    if (input$pca_vectors) {
      plt <- plt +
        geom_segment(
          data = scaledLoadings(),
          aes(
            x = 0,
            y = 0,
            xend = PC1,
            yend = PC2,
            group = !!sym(input$aesX)
          ),
          inherit.aes = FALSE,
          arrow = arrow(),
          alpha = .3
        ) +
        ggrepel::geom_label_repel(
          data = scaledLoadings(),
          aes(
            x = PC1,
            y = PC2,
            label = !!sym(input$aesX)
          ),
          inherit.aes = FALSE,
          alpha = .3,
          show.legend = FALSE
        )
    }
    
    plt
  })
  
  output$pca_scores <- renderPlot({
    pcaScoresPlot()
  })
  
  
  # ** Loadings -----------------------------------------------------------------------------------------------------
  
  pcaLoadingsPlot <- reactive({
    # req(pcaObject())
    loadings <-
      pcaObject()@loadings %>%
      as_tibble(rownames = input$aesX)
    
    loadings %>%
      ggplot(aes(PC1, PC2)) +
      geom_point(pch = 19, size = input$pca_pointSize / 3) +
      mainTheme +
      ggrepel::geom_text_repel(aes(label = !!sym(input$aesX)),
                               show.legend = FALSE
      )
  })
  
  
  output$pcaLoadingsPlot <- renderPlot({
    pcaLoadingsPlot()
  })
  
  # Download handlers  --------------------------------------------------------------
  # Metadata - .csv
  output$saveMeta <- downloadHandler(
    filename = function() {
      paste0("datasets_info.csv")
    },
    content = function(file) {
      readr::write_csv(x = metaData(), path = file)
    }
  )
  
  output$saveRawCSV   <- downloadHandlerFactoryCSV(metaData   = metaData(),
                                                   dataset    = rawData(),
                                                   specifier  = "-raw",
                                                   id         = input$id)
  
  output$saveMainCSV  <- downloadHandlerFactoryCSV(metaData   = metaData(),
                                                   dataset    = mainData(),
                                                   specifier  = "-filtered",
                                                   id         = input$id)
  
  output$savePlotData <- downloadHandlerFactoryCSV(metaData    = metaData(),
                                                   dataset     = plotData(),
                                                   specifier   = "-plot",
                                                   id          = input$id)
  
  output$saveMeanPlotData <- downloadHandlerFactoryCSV(metaData    = metaData(),
                                                       dataset     = meanPlotData(),
                                                       specifier   = "-means",
                                                       id          = input$id)
  
  output$saveMainPlot <- downloadHandlerFactoryPDF(metaData  = metaData(),
                                                   plot      = mainPlot(),
                                                   specifier = "-plot",
                                                   width     = input$mainWidth,
                                                   height    = input$mainHeight, 
                                                   id        = input$ID)
  
  output$saveHeatmap <- downloadHandlerFactoryPDF(metaData  = metaData(),
                                                  plot      = heatmapPlot(),
                                                  specifier = "-heatmap",
                                                  width     = input$heatWidth,
                                                  height    = input$heatHeight, 
                                                  id        = input$ID)
  
  output$savePCAScores <- downloadHandlerFactoryPDF(metaData  = metaData(),
                                                    plot      = pcaScoresPlot(),
                                                    specifier = "-PCA_scores",
                                                    width     = input$pca_Width,
                                                    height    = input$pca_Height, 
                                                    id        = input$ID)
  
  output$savePCALoadings <- downloadHandlerFactoryPDF(metaData  = metaData(),
                                                      plot      = pcaLoadingsPlot(),
                                                      specifier = "-pcaLoadingsPlot",
                                                      width     = input$pca_Width,
                                                      height    = input$pca_Height, 
                                                      id        = input$ID)
  
  # End -------------------------------------------------------------------------------------------------------------
  # End session when window is closed
  session$onSessionEnded(stopApp)
  
}
