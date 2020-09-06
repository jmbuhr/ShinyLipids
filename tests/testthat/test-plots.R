context("Plots")

input <- defaultInput()

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
rawData <- collectRawData(con = databaseConnection, id = 1)

plotData <- rawData %>%
  imputeMissingIf(input) %>% 
  addLipidProperties() %>% 
  standardizeWithinTechnicalReplicatesIf(input) %>%
  filterRawDataFor(input) %>%
  standardizeWithin(input) %>%
  createPlotData(input)

allPlotOptions <- list(
  "Show points"                = "points",
  "Show bars"                  = "bars",
  "Show average"               = "mean",
  "Show value of means"        = "values",
  "Show value of points"       = "ind_values",
  "Transform y-axis log1p"     = "log",
  "Show N per sample"          = "N",
  "Label points"               = "label",
  # "Swap x- and y-axis"         = "swap",
  "Free y scale for facets"    = "free_y",
  "Run pairwise t-tests"       = "signif")

test_that("Plot with all options except swapped x and y work", {
  tmpInput <- input
  tmpInput$mainPlotAdditionalOptions <- allPlotOptions
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = input)
  expect_is(plt, "ggplot")
})

test_that("Main Plot works with facets", {
  tmpInput <- input
  tmpInput$aesFacetCol <- "category"
  tmpInput$errorbarType <- "SD"
  
  data <- rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>%
    standardizeWithin(tmpInput)

  plotData <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")

  plotData <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  tmpInput$aesFacetCol <- "category"
  tmpInput$aesFacetRow <- "oh"
  tmpInput$mainPlotAdditionalOptions <- list(
    "points", "bars", "swap"
  )

  plotData <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, input = tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")
})

test_that("Heatmap works with defaults", {
  plt <- createHeatmap(plotData = plotData, input)
  expect_is(plt, "ggplot")
})


test_that("Heatmap works with facets", {
  tmpInput <- input
  data <- rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>%
    standardizeWithin(tmpInput) 
  
  plt <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  tmpInput$aesFacetCol <- "category"
  data <- rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>%
    standardizeWithin(tmpInput) 
  
  plt <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  input$aesFacetRow <- "category"
  
  plt <- data %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  input$aesFacetRow <- "category"
  input$aesFacetCol <- "func_cat"
  
  plt <- data %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
})

# TODO
test_that("Default PCA works", {
  
  wideData <- createWideData(plotData = plotData, input)
  pcaPrep <- createPcaPrep(wideData, input)
  
  pcaTidy <- tidy(pcaPrep, id = "pca")
  pcaJuice <- juice(pcaPrep)
  
  plt1 <- createPcaScoresPlot(pcaJuice, pcaTidy, input)
  plt2 <- createPcaLoadingsPlot(pcaTidy, input)
  
  expect_is(plt1, "ggplot")
  expect_is(plt2, "ggplot")
})
