context("Data processing functions")

input <- defaultInput()

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

metaData <- collectMetaData(databaseConnection)
rawData <- collectRawData(con = databaseConnection, id = 1)
exampleSamples <- unique(rawData$sample)

test_that("Data is optional", {
  tmpInput <- input
  tmpInput$imputeMissingAs0 <- FALSE
  expect_equal(datasets::iris, imputeMissingIf(datasets::iris, tmpInput))
  tibble(
    id = rep(1, 2),
    lipid = c("hello", "world"),
    category = rep("test", 2),
    func_cat = rep("test", 2),
    sample = c("First sample", "First sample"),
    sample_replicate = c("First sample A", "First sample A"),
    sample_replicate_technical = c("First sample A a", "First sample A b"),
    sample_identifier = c("1Aa", "1Ab"),
    value = c(1, 2)
  ) %>% 
    imputeMissingIf(input) %>% 
    nrow() %>% 
    expect_equal(4)
})

test_that("Standardization within techincal replicates works and is optional", {
  tmpInput <- input
  tmpInput$standardizeWithinTechnicalReplicate <- FALSE
  expect_equal(datasets::iris, standardizeWithinTechnicalReplicatesIf(datasets::iris, tmpInput))
  tibble(id = rep(1, 3),
         sample_replicate_technical = c("1Aa", "1Aa", "1Ab"),
         value = c(2, 18, 42)) %>% 
    standardizeWithinTechnicalReplicatesIf(input) %>% 
    pull(value) %>% 
    expect_equal(c(10, 90, 100))
})

test_that("filterIfNotNull works on other data", {
  datasets::iris %>%
    filterIfNotNull("var", Sepal.Length < median(Sepal.Length)) %>% 
    nrow() %>% 
    expect_equal(73)
  
  datasets::iris %>%
    filterIfNotNull("var", Sepal.Length > median(Sepal.Length)) %>% 
    nrow() %>% 
    expect_equal(70)
})

test_that("Baseline substraction works", {
  tmpInput <- input
  tmpInput$baselineSample <- rawData$sample[[1]]
  rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>% 
    standardizeRawDataWithin(tmpInput) %>% 
    filter(sample == tmpInput$baselineSample) %>% 
    pull(value) %>% 
    mean() %>% 
    expect_equal(0)
})


test_that("Pairwise t-tests didn't change", {
  plotData <- rawData %>%
    imputeMissingIf(input) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(input) %>%
    filterRawDataFor(input) %>%
    standardizeRawDataWithin(input) %>%
    createPlotData(input)
  
  doAllPairwiseComparisons(plotData, input) %>% 
    expect_silent()
})
