#' @import shiny
app_server <- function(input, output, session) {
  # Setup ####
  databaseConnection <- golem::get_golem_options("db")
  
  # Data ####
  # * Metadata / list of datasets ####
  metaData <- reactive({
    collectMetaData(databaseConnection)
  })

  # * rawData ####
  rawData <- reactive({
    validate(need(input$ID, "Please select a dataset first."))
    collectRawData(id = input$ID, con = databaseConnection) %>% 
      imputeMissingIf(input$imputeMissingAs0) %>% 
      addLipidProperties(lipidClassOrder = collectLipidClassOrder(databaseConnection))
  })
  
  # * filteredData ####
  filteredData <- reactive({
    rawData() %>%
      standardizeWithinTechnicalReplicatesIf(input$standardizeWithinTechnicalReplicate) %>%
      filterRawDataFor(categoryToSelect            = input$categoryToSelect,
                       lipidClassToSelect          = input$lipidClassToSelect,
                       functionalCategoryToSelect  = input$functionalCategoryToSelect,
                       filterLengthRange           = input$filterLengthRange,
                       filterDoubleBondsRange      = input$filterDoubleBondsRange,
                       filterOhRange               = input$filterOhRange,
                       samplesToSelect             = input$samplesToSelect,
                       samplesToRemove             = input$samplesToRemove,
                       replicatesToSelect          = input$replicatesToSelect,
                       replicatesToRemove          = input$replicatesToRemove,
                       technicalReplicatesToRemove = input$technicalReplicatesToRemove)
  })
  
  # * mainData ####
  mainData <- reactive({
    validate(need(nrow(filteredData()) > 0,
                  "The data was filtered such that there is no data left."))
    filteredData() %>%
      standardizeRawDataWithin(baselineSample          = input$baselineSample,
                               standardizationFeatures = input$standardizationFeatures)
  })
  
  # * plotData ####
  # summarised based on
  # selecte plot type, standards and aes (aesthetics)
  plotData <- reactive({
    req(mainData())
    validate(
      need(
        !(input$summariseTechnicalReplicates &
            (input$aesColor    == "sample_replicate_technical" |
               input$aesX      == "sample_replicate_technical" |
               input$aesFacetCol == "sample_replicate_technical" |
               input$aesFacetRow == "sample_replicate_technical")
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
      createPlotData(summariseTechnicalReplicates = input$summariseTechnicalReplicates,
                     aesX                         = input$aesX,
                     aesColor                     = input$aesColor,
                     aesFacetCol                  = input$aesFacetCol,
                     aesFacetRow                  = input$aesFacetRow)
  })
  
  # * meanPlotData ####
  ## for bars/averages
  meanPlotData <- reactive({
    req(plotData())
    plotData() %>%
      summarisePlotData(aesX        = input$aesX,
                        aesColor    = input$aesColor,
                        aesFacetCol = input$aesFacetCol,
                        aesFacetRow = input$aesFacetRow)
  })
  
  # * PairwiseComparisons ####
  # from pairwise t-tests on log-transformed data
  pairwiseComparisons <- reactive({
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
    doAllPairwiseComparisons(data = plotData(),
                             aesX = input$aesX)
  })
  

  # Updating input choices ####
  # * Update SelectInput for datasets
  # based on sets loaded and row selected select clicked row in table
  observe({
    choices <- metaData()$id
    names(choices) <- paste(metaData()$id, "-", metaData()$title)
    selection <- input$metaDataTable_rows_selected
    if (!is.null(selection)) {
      updateSelectInput(session,
                        "ID",
                        choices  = choices,
                        selected = choices[[selection]])
    }
  })
  
  # * Updating filtering options by dataset ####
  observe({
    updateAllSelectizeInputs <- partial(updateAllSelectizeInputs,
                                        data = rawData(),
                                        session = session)
    updateAllSelectizeInputs("samplesToSelect", "sample", NULL)
    updateAllSelectizeInputs("baselineSample", "sample", "")
    updateAllSelectizeInputs("samplesToRemove", "sample", NULL)
    updateAllSelectizeInputs("replicatesToSelect", "sample_replicate", NULL)
    updateAllSelectizeInputs("replicatesToRemove", "sample_replicate", NULL)
    updateAllSelectizeInputs("technicalReplicatesToRemove", "sample_replicate_technical", NULL)
    updateAllSelectizeInputs("categoryToSelect", "category", NULL)
    updateAllSelectizeInputs("functionalCategoryToSelect", "func_cat", NULL)
    updateAllSelectizeInputs("lipidClassToSelect", "class", NULL)
    updateAllSelectizeInputs("quickSpeciesProfileClass", "class", "")
    
    updateAllRangeInputs <- partial(updateAllRangeInputs,
                                    data = rawData(),
                                    session = session)
    updateAllRangeInputs("filterLengthRange", "length")
    updateAllRangeInputs("filterDoubleBondsRange", "db")
    updateAllRangeInputs("filterOhRange", "oh")
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
  
  # * Update inputs based on selected default quickoption ####
  observeEvent(input$quickSpeciesProfileClass, {
    if (input$quickSpeciesProfileClass != "") {
      updateInputsForSpeciesProfile(session, input$quickSpeciesProfileClass)
    }
  })
  
  observeEvent(input$quickClassProfile, {
    updateSelectInput(session, "aesFacetCol", selected = "")
    updateSelectizeInput(session, "standardizationFeatures", selected = "")
    updateSelectInput(session, "aesX", selected = "class")
    updateSelectizeInput(session, "lipidClassToSelect", selected = "")
  })
  
  # Table Outputs ####
  # * Metadata table ####
  output$metaDataTable <- DT::renderDT({
    validate(
      need(req(metaData()), "No metadata loaded")
    )
    if (input$showFullMeta == TRUE) {
      metaData()
    } else {
      metaData()[c("id", "title", "date_upload", "status", "sample_from")]
    }
  },
  server = FALSE, selection = list(mode = "single", selected = 1),
  options = list(
    orderClasses   = TRUE,
    pageLength     = 10,
    order          = list(0, "desc"),
    scrollX        = TRUE,
    deferRender    = TRUE,
    scrollY        = 500,
    scrollCollapse = TRUE)
  )
  
  # * Main Dataset as a table ####
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
    scrollCollapse = TRUE)
  )
  
  # * meanPlotDataTable ####
  output$meanPlotDataTable <- DT::renderDT({
    req(meanPlotData())
    meanPlotData() %>%
      select(value,
             !!sym(input$aesX),
             everything())
  },
  filter           = "none",
  rownames         = FALSE,
  options          = list(
    orderClasses   = TRUE,
    pageLength     = 10,
    order          = list(0, "desc"),
    scrollX        = TRUE,
    deferRender    = TRUE,
    scrollCollapse = TRUE)
  )
  
  # * pairwiseComparisonsTable ####
  output$pairwiseComparisonsTable <- DT::renderDT({
    validate(need("signif" %in% input$mainPlotAdditionalOptions,
                  "Option not checked."))
    pairwiseComparisons()
  })
  
  # Plots ####
  # * Main Plot ####
  
  # ** Ranges for zooming by clicking on the plot  ####
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
  
  # ** Main Plot Object ####
  mainPlot <- reactive({
    req(plotData(), meanPlotData())
    createMainPlot(plotData                            = plotData(),
                   meanPlotData                        = meanPlotData(),
                   pairwiseComparisons                 = pairwiseComparisons(),
                   rangeX                              = ranges$x,
                   rangeY                              = ranges$y,
                   aesX                                = input$aesX,
                   aesColor                            = input$aesColor,
                   aesFacetCol                         = input$aesFacetCol,
                   aesFacetRow                         = input$aesFacetRow,
                   mainPlotAdditionalOptions           = input$mainPlotAdditionalOptions,
                   errorbarType                        = input$errorbarType,
                   summariseTechnicalReplicates        = input$summariseTechnicalReplicates,
                   standardizationFeatures             = input$standardizationFeatures,
                   standardizeWithinTechnicalReplicate = input$standardizeWithinTechnicalReplicate)
  })
  
  # ** Main Plot Render ####
  output$mainPlot <- renderPlot({
    mainPlot()
  })
  
  # * Heatmap ####
  # ** Heatmap Object ####
  heatmapPlot <- reactive({
    createHeatmap(data = meanPlotData(), input = input)
  })
  
  # ** Heatmap Render ####
  output$heatPlot <- renderPlot({
    heatmapPlot()
  })
  
  # * PCA ####
  
  # ** Updating pca-options ####
  # update number of principal components,
  # they should not exceed the dimensions of the data
  observe({
    req(pcaData())
    updateSliderInput(session,
                      "pcaNumberPrincipalComponents",
                      max = min(dim(pcaData())))
  })
  
  # ** pcaData ####
  pcaData <- reactive({
    req(plotData())
    validate(
      need(input$aesColor == "sample",
        "To perform a PCA, please set color to sample in the mappings"),
      need((input$aesX != "sample" &
              input$aesX != "sample_replicate" &
              input$aesX != "sample_replicate_technical"),
        "To perform a PCA, please select a feature other than sample as your x-axis in the mappings"
      ),
      need(input$aesFacetCol == "",
        "To perform a PCA, please remove any facetting in the mappings"),
      need(input$aesFacetRow == "",
        "To perform a PCA, please remove any facetting in the mappings"),
      need(length(unique(plotData()[[input$aesX]])) > 1,
        "Not enough datapoints to perform PCA")
    )
    createPcaData(plotData        = plotData(),
                  aesX            = input$aesX,
                  summariseTecRep = input$summariseTechnicalReplicates)
  })
  
  # ** pcaObject ####
  pcaObject <- reactive({
    createPcaResult(pcaData                      = pcaData(),
                    pcaMethod                    = input$pcaMethod,
                    pcaNumberPrincipalComponents = input$pcaNumberPrincipalComponents,
                    pcaCenter                    = input$pcaCenter,
                    pcaScalingMethod             = input$pcaScalingMethod,
                    pcaCrossValidationMethod     = input$pcaCrossValidationMethod)
  })

  pcaSampleNames <- reactive({
    getPcaSampleNames(plotData(), input$summariseTechnicalReplicates)
  })
  
  scaledLoadings <- reactive({
    req(pcaObject())
    pcaScaleLoadings(pcaObject = pcaObject(),
                     pcaSampleNames = pcaSampleNames(),
                     aesX = input$aesX,
                     summariseTechnicalReplicates = input$summariseTechnicalReplicates)
  })
  
  
  # ** pcaInfo ####
  output$pcaInfo <- renderPrint({
    req(pcaObject())
    pcaObject() %>% summary()
  })
  
  # ** Scores ####
  pcaScoresPlot <- reactive({
    req(pcaObject())
    createPcaScoresPlot(pcaData                      = pcaData(),
                        pcaObject                    = pcaObject(),
                        pcaSampleNames               = pcaSampleNames(),
                        scaledLoadings               = scaledLoadings(),
                        aesX                         = input$aesX,
                        summariseTechnicalReplicates = input$summariseTechnicalReplicates,
                        drawPcaConvexHull            = input$drawPcaConvexHull,
                        pcaPointSize                 = input$pcaPointSize,
                        pcaLabels                    = input$pcaLabels,
                        pcaVectors                   = input$pcaVectors)
  })
  
  output$pcaScoresPlot <- renderPlot({
    pcaScoresPlot()
  })
  
  
  # ** Loadings ####
  pcaLoadingsPlot <- reactive({
    createPcaLoadingsPlot(pcaObject = pcaObject(),
                          aesX = input$aesX,
                          pcaPointSize = input$pcaPointSize)
  })
  
  output$pcaLoadingsPlot <- renderPlot({
    pcaLoadingsPlot()
  })
  
  # Download handlers ####
  output$saveMeta <-
    downloadHandler(
      filename = function() {
        paste0("datasets_info.csv")
      },
      content = function(file) {
        readr::write_csv(x = metaData(), path = file)
      }
    )
  
  output$saveRawCSV <-
    downloadHandlerFactoryCSV(metaData   = metaData(),
                              dataset    = rawData(),
                              specifier  = "-raw",
                              id         = input$id)
  
  output$saveMainCSV <-
    downloadHandlerFactoryCSV(metaData   = metaData(),
                              dataset    = mainData(),
                              specifier  = "-filtered",
                              id         = input$id)
  
  output$savePlotData <-
    downloadHandlerFactoryCSV(metaData   = metaData(),
                              dataset    = plotData(),
                              specifier  = "-plot",
                              id         = input$id)
  
  output$saveMeanPlotData <-
    downloadHandlerFactoryCSV(metaData   = metaData(),
                              dataset    = meanPlotData(),
                              specifier  = "-means",
                              id         = input$id)
  
  output$saveMainPlot <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = mainPlot(),
                              specifier = "-plot",
                              width     = input$mainWidth,
                              height    = input$mainHeight, 
                              id        = input$ID)
  
  output$saveHeatmap <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = heatmapPlot(),
                              specifier = "-heatmap",
                              width     = input$heatWidth,
                              height    = input$heatHeight, 
                              id        = input$ID)
  
  output$savePCAScores <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = pcaScoresPlot(),
                              specifier = "-pcaScoresPlot",
                              width     = input$pcaWidth,
                              height    = input$pcaHeight, 
                              id        = input$ID)
  
  output$savePCALoadings <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = pcaLoadingsPlot(),
                              specifier = "-pcaLoadingsPlot",
                              width     = input$pcaWidth,
                              height    = input$pcaHeight, 
                              id        = input$ID)
  
  # End ####
  # End session when window is closed
  session$onSessionEnded(function() {
    DBI::dbDisconnect(databaseConnection); stopApp()
    })
}
