app_server <- function(input, output, session) {
  # Setup ####
  databaseConnection <- golem::get_golem_options("db")
  
  # Data ####
  # * Metadata / list of data sets ####
  metaData <- reactive({
    collectMetaData(databaseConnection)
  })
  
  # * rawData ####
  rawData <- reactive({
    validate(need(input$ID, "Please select a dataset first."))
    collectRawData(id = input$ID, con = databaseConnection) %>% 
      imputeMissingIf(input) %>% 
      addLipidProperties(lipidClassOrder = collectLipidClassOrder(databaseConnection))
  })
  
  # * filteredData ####
  filteredData <- reactive({
    rawData() %>%
      standardizeWithinTechnicalReplicatesIf(input) %>%
      filterRawDataFor(input)
  })
  
  # * mainData ####
  mainData <- reactive({
    validate(need(nrow(filteredData()) > 0,
                  "The data was filtered such that there is no data left."))
    filteredData() %>%
      standardizeRawDataWithin(input)
  })
  
  # * plotData ####
  # summarized based on
  # selected plot type, standards and aes (aesthetics)
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
        "Please select a feature to display on the x-axis"))
    
    mainData() %>%
      createPlotData(input)
  })
  
  # * meanPlotData ####
  ## for bars/averages
  meanPlotData <- reactive({
    req(plotData())
    plotData() %>%
      summarisePlotData(input)
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
        length(unique(plotData()$sample)) > 1,
        "You need at least 2 samples to compare them"
      ),
      need(
        testAllMoreThanOneReplicate(plotData(), input$aesX, input$aesColor),
        "You need more than 1 replicate per sample for everything visible in the plot"))
    
    doAllPairwiseComparisons(data = plotData(), input)
  })
  
  
  # Updating input choices ####
  # * Update selectInput for data sets
  # based on sets loaded and row selected select clicked row in table
  
  # Reset button, using shinyjs
  observeEvent(input$resetEverything, {
    names(defaultInput()) %>% 
      c("quickSpeciesProfileClass") %>%
      walk(shinyjs::reset)
  })
  
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
      shinyjs::reset("aesColor")
      shinyjs::reset("aesFacetRow")
      shinyjs::reset("aesFacetCol")
      shinyjs::reset("categoryToSelect")
      shinyjs::reset("functionalCategoryToSelect")

      updateSelectInput(session, "aesX", selected = "lipid")
      updateSelectizeInput(session, "standardizationFeatures",
                           selected = c("class", "sample_replicate"))
      updateSelectizeInput(session, "lipidClassToSelect",
                           selected = unname(input$quickSpeciesProfileClass))
    }
  })
  
  observeEvent(input$quickClassProfile, {
    shinyjs::reset("aesColor")
    shinyjs::reset("aesFacetRow")
    shinyjs::reset("aesFacetCol")
    shinyjs::reset("technicalReplicatesToRemove")
    shinyjs::reset("standardizationFeatures")
    shinyjs::reset("categoryToSelect")
    shinyjs::reset("lipidClassToSelect")
    shinyjs::reset("functionalCategoryToSelect")

    updateSelectInput(session, "aesX", selected = "class")
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
  options = dtOptions
  )
  
  # * Main Dataset as a table ####
  output$mainDataTable <- DT::renderDT({
    mainData()
  },
  filter = "none",
  rownames = FALSE,
  options  = dtOptions
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
  options          = dtOptions
  )
  
  # * pairwiseComparisonsTable ####
  output$pairwiseComparisonsTable <- DT::renderDT({
    validate(need("signif" %in% input$mainPlotAdditionalOptions,
                  "This table only shows when you tick \"Run pairwise t-tests\" in the plot options next to this. "))
    pairwiseComparisons()
  }, options = dtOptions, caption = "Calculated via t-tests on log-transformed data. \
                                  P-values corrected with the Benjamini-Hochberg procedure.")
  
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
                   input)
  })
  
  # ** Main Plot Render ####
  output$mainPlot <- renderPlot({
    mainPlot()
  })
  
  # * Heatmap ####
  # ** Heatmap Object ####
  heatmapPlot <- reactive({
    createHeatmap(data = meanPlotData(), input)
  })
  
  # ** Heatmap Render ####
  output$heatPlot <- renderPlot({
    heatmapPlot()
  })
  
  # * Dimensionality reduction ####
  
  # ** Updating pca-options ####
  # update number of principal components,
  # they should not exceed the dimensions of the data
  observe({
    req(wideData())
    updateSliderInput(session,
                      "pcaNumberPrincipalComponents",
                      max = min(dim(wideData())))
  })
  
  # ** data for dimensionality reduction ####
  wideData <- reactive({
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
    createWideData(plotData = plotData(), input)
  })
  
  # ** pcaPrep ####
  pcaPrep <- reactive({
    createPcaPrep(wideData(), input)
  })
  
  pcaTidy <- reactive(tidy(pcaPrep(), id = "pca"))
  pcaJuice <- reactive(juice(pcaPrep()))
  
  # ** pcaInfo ####
  output$pcaInfo <- renderPrint({
    req(pcaPrep())
    summary(pcaPrep())
  })
  
  # ** Scores ####
  pcaScoresPlot <- reactive({
    req(pcaJuice())
    createPcaScoresPlot(pcaJuice = pcaJuice(), pcaTidy = pcaTidy(), input)
  })
  
  output$pcaScoresPlot <- renderPlot({
    pcaScoresPlot()
  })
  
  # ** Loadings ####
  pcaLoadingsPlot <- reactive({
    createPcaLoadingsPlot(pcaTidy = pcaTidy(), input)
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
        write.csv(x = metaData(), file = file)
      }
    )
  
  output$saveRawCSV <-
    downloadHandlerFactoryCSV(metaData  = metaData(),
                              dataset   = rawData(),
                              specifier = "_raw_data",
                              id        = input$id)
  
  output$saveMainCSV <-
    downloadHandlerFactoryCSV(metaData  = metaData(),
                              dataset   = mainData(),
                              specifier = "_filtered_data",
                              id        = input$id)
  
  output$savePlotData <-
    downloadHandlerFactoryCSV(metaData  = metaData(),
                              dataset   = plotData(),
                              specifier = "_plot_data",
                              id        = input$id)
  
  output$saveMeanPlotData <-
    downloadHandlerFactoryCSV(metaData  = metaData(),
                              dataset   = meanPlotData(),
                              specifier = "_means",
                              id        = input$id)
  
  output$saveMainPlot <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = mainPlot(),
                              specifier = "_plot",
                              width     = input$mainWidth,
                              height    = input$mainHeight, 
                              id        = input$ID)
  
  output$saveMainPlotRDS <-
    downloadHandlerFactoryRDS(metaData  = metaData(),
                              plot      = mainPlot(),
                              specifier = "_plot",
                              width     = input$mainWidth,
                              height    = input$mainHeight, 
                              id        = input$ID)
  
  output$saveHeatmap <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = heatmapPlot(),
                              specifier = "_heatmap",
                              width     = input$heatWidth,
                              height    = input$heatHeight, 
                              id        = input$ID)
  
  output$saveHeatmapRDS <-
    downloadHandlerFactoryRDS(metaData  = metaData(),
                              plot      = heatmapPlot(),
                              specifier = "_heatmap",
                              width     = input$heatWidth,
                              height    = input$heatHeight, 
                              id        = input$ID)
  
  output$savePCAScores <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = pcaScoresPlot(),
                              specifier = "_pcaScoresPlot",
                              width     = input$pcaWidth,
                              height    = input$pcaHeight, 
                              id        = input$ID)
  
  output$savePCALoadings <-
    downloadHandlerFactoryPDF(metaData  = metaData(),
                              plot      = pcaLoadingsPlot(),
                              specifier = "_pcaLoadingsPlot",
                              width     = input$pcaWidth,
                              height    = input$pcaHeight, 
                              id        = input$ID)
  
  output$savePCAScoresRDS <-
    downloadHandlerFactoryRDS(metaData  = metaData(),
                              plot      = pcaScoresPlot(),
                              specifier = "_pcaScoresPlot",
                              width     = input$pcaWidth,
                              height    = input$pcaHeight, 
                              id        = input$ID)
  
  output$savePCALoadingsRDS <-
    downloadHandlerFactoryRDS(metaData  = metaData(),
                              plot      = pcaLoadingsPlot(),
                              specifier = "_pcaLoadingsPlot",
                              width     = input$pcaWidth,
                              height    = input$pcaHeight, 
                              id        = input$ID)
  # End ####
  # End session when window is closed
  session$onSessionEnded(function() {
    DBI::dbDisconnect(databaseConnection); stopApp()
  })
}
