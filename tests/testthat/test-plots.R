context("Plots")

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
rawData <- collectRawData(con = databaseConnection, id = 1)

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

plotData <- rawData %>%
  imputeMissingIf() %>% 
  addLipidProperties() %>% 
  standardizeWithinTechnicalReplicatesIf() %>%
  filterRawDataFor() %>%
  standardizeRawDataWithin() %>%
  createPlotData()


test_that("Plot with all options except swapped x and y work", {
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData),
                        mainPlotAdditionalOptions = allPlotOptions,
                        errorbarType = "CI")
  
  expect_is(plt, "ggplot")
})

test_that("Main Plot works with facets", {
  data <- rawData %>%
    imputeMissingIf() %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf() %>%
    filterRawDataFor() %>%
    standardizeRawDataWithin()
  
  plotData <- data %>%
    standardizeRawDataWithin() %>%
    createPlotData(aesFacetCol = "category")
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData),
                        aesFacetCol = "category")
  expect_is(plt, "ggplot")

  plotData <- data %>%
    standardizeRawDataWithin() %>%
    createPlotData(aesFacetRow = "category")
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData),
                        aesFacetRow = "category")
  expect_is(plt, "ggplot")
  
  plotData <- data %>%
    standardizeRawDataWithin() %>%
    createPlotData(aesFacetCol = "category",
                   aesFacetRow = "oh")
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData),
                        aesFacetCol = "category",
                        aesFacetRow = "oh")
  expect_is(plt, "ggplot")
  
})

test_that("Heatmap works with defaults", {
  plt <- createHeatmap(data = plotData)
  expect_is(plt, "ggplot")
})

test_that("Heatmap works with facets", {
  data <- rawData %>%
    imputeMissingIf() %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf() %>%
    filterRawDataFor() %>%
    standardizeRawDataWithin() 
  
  plt <- data %>%
    standardizeRawDataWithin() %>%
    createPlotData(aesFacetCol = "category") %>% 
    createHeatmap(aesFacetCol = "category")
  expect_is(plt, "ggplot")
  
  plt <- data %>%
    createPlotData(aesFacetRow = "category") %>% 
    createHeatmap(aesFacetRow = "category")
  expect_is(plt, "ggplot")
  
  plt <- data %>%
    createPlotData(aesFacetRow = "category",
                   aesFacetCol = "func_cat") %>% 
    createHeatmap(aesFacetRow = "category",
                  aesFacetCol = "func_cat")
  expect_is(plt, "ggplot")
})

test_that("Default PCA works", {
  pcaData <- createPcaData(plotData)
  pcaObject <- createPcaResult(pcaData = pcaData)
  pcaSampleNames <- getPcaSampleNames(plotData)
  scoresPlt <- createPcaScoresPlot(pcaData = pcaData,
                                   pcaObject = pcaObject,
                                   pcaSampleNames = pcaSampleNames)
  expect_is(scoresPlt, "ggplot")
  
  loadingsPlt <- createPcaLoadingsPlot(pcaObject = pcaObject)
  expect_is(loadingsPlt, "ggplot")
})
