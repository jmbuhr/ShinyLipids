context("Backend Functions")

pkgload::load_all()

path <- system.file("inst/extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

test_that(
  "Data processing steps run",
  {
    input <- list(
      ID                 = 1,
      lipidClassOrder = collectLipidClassOrder(databaseConnection),
      standardizeWithinTechnicalReplicate        = TRUE,
      standardizationFeatures        = c("class", "sample_replicate"),
      summariseTechnicalReplicates     = TRUE,
      baselineSample        = "",
      lipidClassToSelect       = "PC",
      aesX              = "lipid",
      aesFacetCol         = %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% e"
    )
    
    query <- createQueryForID(input$ID)
    rawData <- collectRawData(databaseConnection,
                              query,
                              lipidClassOrder = input$lipidClassOrder)
    
    plotData <- rawData %>%
      standardizeWithinTechnicalReplicatesIf(input$standardizeWithinTechnicalReplicate) %>%
      filterRawDataFor(input) %>%
      standardizeRawDataWithin(baselineSample  = input$baselineSample,
                               standardizationFeatures = input$standardizationFeatures) %>%
      createPlotData(input)
    
    expected <- c(0.4378872, 12.9379241, 7.1688470, 0.0213048, 3.1023356)
    expect_equal(plotData$value[1:5], expected)
  }
)





