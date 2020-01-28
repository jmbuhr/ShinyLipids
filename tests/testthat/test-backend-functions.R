context("Backend Functions")

databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), "database/exampleDatabase.db")

pkgload::load_all()

test_that(
  "Data processing steps run",
  {
  input <- list(
    ID                 = 1,
    lipidClassOrder = collectLipidClassOrder(databaseConnection),
    standardizeWithinTechnicalReplicate        = TRUE,
    standardizationFeatures        = c("class", "sample_replicate"),
    tecRep_average     = TRUE,
    baselineSample        = "",
    lipidClassToSelect       = "PC",
    aes_x              = "lipid",
    aes_facet1         = "class",
    aes_facet2         = "",
    aes_color          = "sample"
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
    create_plotData(input)
  
  expected <- c(0.4378872, 12.9379241, 7.1688470, 0.0213048, 3.1023356)
  expect_equal(plotData$value[1:5], expected)
  }
)





