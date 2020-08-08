context("Plots")

input <- generateDefaultInput()
tmpInput <- input

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
rawData <- collectRawData(con = databaseConnection, id = 1)

plotData <- rawData %>%
  imputeMissingIf(input) %>% 
  addLipidProperties() %>% 
  standardizeWithinTechnicalReplicatesIf(input) %>%
  filterRawDataFor(input) %>%
  standardizeRawDataWithin(input) %>%
  createPlotData(input)

test_that("Plot with all options except swapped x and y work", {
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = input)
  
  expect_is(plt, "ggplot")
})

test_that("Main Plot works with facets", {
  tmpInput <- input
  tmpInput$aesFacetCol <- "category"
  data <- rawData %>%
    imputeMissingIf(input) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(input) %>%
    filterRawDataFor(input) %>%
    standardizeRawDataWithin(input)

  plotData <- data %>%
    standardizeRawDataWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")

  plotData <- data %>%
    standardizeRawDataWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  tmpInput$aesFacetCol = "category"
  tmpInput$aesFacetRow = "oh"

  plotData <- data %>%
    standardizeRawDataWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, input = tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")
  
})

test_that("Heatmap works with defaults", {
  plt <- createHeatmap(data = plotData)
  expect_is(plt, "ggplot")
})

test_that("Heatmap works with facets", {
  tmpInput <- input
  tmpInput$aesFacetCol = "category"
  data <- rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>%
    standardizeRawDataWithin(tmpInput) 
  
  plt <- data %>%
    standardizeRawDataWithin(tmpInput) %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  input$aesFacetRow = "category"
  
  plt <- data %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  input$aesFacetRow = "category"
  input$aesFacetCol = "func_cat"
  
  plt <- data %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
})

# TODO
# test_that("Default PCA works", {
#   
# })
