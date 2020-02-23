context("Data Visualization Pipeline")

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
lipidClassOrder <- collectLipidClassOrder(databaseConnection)


test_that(
  "Default data processing steps run until visualisation", {
    metaData <- collectMetaData(databaseConnection)
    rawData <- collectRawData(con = databaseConnection,
                              id = 1)
    
    plotData <- rawData %>%
      imputeMissingIf() %>% 
      addLipidProperties(lipidClassOrder = collectLipidClassOrder(databaseConnection)) %>% 
      standardizeWithinTechnicalReplicatesIf(TRUE) %>%
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
