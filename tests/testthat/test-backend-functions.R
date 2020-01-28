context("Backend Functions")

databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), "database/exampleDatabase.db")

pkgload::load_all()

test_that(
  "Data processing steps run",
  {
  input <- list(
    ID                 = 1,
    customClassOrder = get_lipid_class_order(databaseConnection),
    std_tec_rep        = TRUE,
    std_feature        = c("class", "sample_replicate"),
    tecRep_average     = TRUE,
    base_sample        = "",
    filter_class       = "PC",
    aes_x              = "lipid",
    aes_facet1         = "class",
    aes_facet2         = "",
    aes_color          = "sample"
  )

  query <- createQueryForID(input$ID)
  rawData <- collectRawData(databaseConnection,
                              query,
                              customClassOrder = input$customClassOrder)

  plotData <- rawData %>%
    standardize_technical_replicates(input$std_tec_rep) %>%
    filter_rawData(input) %>%
    standardize_rawData(base_sample  = input$base_sample,
                        std_features = input$std_feature) %>%
    create_plotData(input)
  
  expected <- c(0.4378872, 12.9379241, 7.1688470, 0.0213048, 3.1023356)
  expect_equal(plotData$value[1:5], expected)
  }
)





