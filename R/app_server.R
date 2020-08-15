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
  
  # ** Main Plot Object ####
  mainPlot <- reactive({
    req(plotData(), meanPlotData())
    createMainPlot(plotData                            = plotData(),
                   meanPlotData                        = meanPlotData(),
                   pairwiseComparisons                 = pairwiseComparisons(),
                   input)
  })
  
  # ** Main Plot Render ####
  output$mainPlot <- renderPlot({
    mainPlot()
  })
  
  # ** Main Plot Render with plotly ####
  output$mainPlot <- plotly::renderPlotly({
    plotly::ggplotly(mainPlot()) %>% 
      plotly::layout(legend = list(
        orientation = "h"
      ))
  })
  
  # * Heatmap ####
  # ** Heatmap Object ####
  heatmapPlot <- reactive({
    createHeatmap(plotData = meanPlotData(), input)
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
  output$pcaInfo <- renderPlot({
    req(pcaPrep())
    pcaPlotPercentVariation(pcaPrep(), input)
  })
  
  # ** Scores ####
  pcaScoresPlot <- reactive({
    req(pcaJuice())
    createPcaScoresPlot(pcaJuice = pcaJuice(), pcaTidy = pcaTidy(), input)
  })
  
  output$pcaScoresPlot <- plotly::renderPlotly({
    pcaScoresPlot() %>%
      plotly::ggplotly()
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
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_raw_data.csv")
      },
      content = function(file) {
        write.csv(x = rawData(), file = file)
      }
    )
  
  output$saveMainCSV <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_filtered_data.csv")
      },
      content = function(file) {
        write.csv(x = mainData(), file = file)
      }
    )
  
  output$savePlotData <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_plot_data.csv")
      },
      content = function(file) {
        write.csv(x = mainData(), file = file)
      }
    )
  
  output$saveMeanPlotData <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_means.csv")
      },
      content = function(file) {
        write.csv(x = meanPlotData(), file = file)
      }
    )
  
  output$saveMainPlot <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_plot.pdf")
      },
      content = function(file) {
        ggsave(file, plot = mainPlot(), width = input$mainWidth,
               height = input$mainHeight)
      }
    )
  
  output$saveMainPlotRDS <- 
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_plot.rds")
      },
      content = function(file) {
        saveRDS(mainPlot(), file)
      }
    )
  
  output$saveHeatmap <- 
    downloadHandler(
      filename = function() {
        tmp <- metaData %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_heatmap.pdf")
      },
      content = function(file) {
        ggsave(file, plot = heatmapPlot(),
               width = input$heatWidth,
               height = input$heatHeight)
      }
    )
  
  output$saveHeatmapRDS <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_heatmap.rds")
      },
      content = function(file) {
        saveRDS(heatmapPlot(), file)
      }
    )
  
  output$savePCAScores <-
    downloadHandler(
      filename = function() {
        tmp <- metaData %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_pcaScoresPlot.pdf")
      },
      content = function(file) {
        ggsave(file, plot = pcaScoresPlot(),
               width = input$pcaWidth,
               height = input$pcaHeight)
      }
    )
  
  output$savePCALoadings <-
    downloadHandler(
      filename = function() {
        tmp <- metaData %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_pcaLoadingsPlot.pdf")
      },
      content = function(file) {
        ggsave(file, plot = pcaLoadingsPlot(),
               width = input$pcaWidth,
               height = input$pcaHeight)
      }
    )
  
  output$savePCAScoresRDS <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_pcaScoresPlot.rds")
      },
      content = function(file) {
        saveRDS(pcaScoresPlot(), file)
      }
    )
  
  output$savePCALoadingsRDS <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "pcaScoresPlot.rds")
      },
      content = function(file) {
        saveRDS(pcaLoadingsPlot(), file)
      }
    )
  
  # End ####
  # End session when window is closed
  session$onSessionEnded(function() {
    DBI::dbDisconnect(databaseConnection); stopApp()
  })
}
