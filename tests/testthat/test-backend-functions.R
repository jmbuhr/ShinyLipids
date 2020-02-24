context("Full data processing pipeline until main plot")

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)


test_that("Example database exists", {
  DBI::dbListTables(databaseConnection) %>% 
    expect_length(2)
})

test_that("collectLipidClassOrder default return value exists", {
  collectLipidClassOrder(databaseConnection) %>% 
    expect_length(48)
})

test_that("Example meta data exists", {
    metaData <- collectMetaData(databaseConnection)
    expect_equal(metaData$sample_from[[1]], "Harry Potter")
})

test_that("Example meta data exists", {
  rawData <- collectRawData(con = databaseConnection, id = 1)
  expect_is(rawData, "tbl")
})


test_that(
  "Default data processing steps run until visualisation", {
    metaData <- collectMetaData(databaseConnection)
    rawData <- collectRawData(con = databaseConnection, id = 1)
    
    plotData <- rawData %>%
      imputeMissingIf() %>% 
      addLipidProperties() %>% 
      standardizeWithinTechnicalReplicatesIf() %>%
      filterRawDataFor() %>%
      standardizeRawDataWithin() %>%
      createPlotData()
    
    meanPlotData <- summarisePlotData(plotData)
    
    plt <- createMainPlot(plotData            = plotData,
                          meanPlotData        = meanPlotData,
                          pairwiseComparisons = NULL)
    expect_is(plt, "ggplot")
  }
)
