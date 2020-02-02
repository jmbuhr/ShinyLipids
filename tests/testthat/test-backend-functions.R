context("Data Visualization Pipeline")

pkgload::load_all()

path <- system.file("inst/extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
lipidClassOrder <- collectLipidClassOrder(databaseConnection)


test_that(
  "Default data processing steps run until visualisation",
  {
    metaData <- collectMetaData(databaseConnection)
    query <- createQueryForID(metaData$id[[1]])
    rawData <- collectRawData(con             = databaseConnection,
                              query           = query,
                              lipidClassOrder = lipidClassOrder)
    
    plotData <- rawData %>%
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





