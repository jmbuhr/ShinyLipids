context("Data processing functions")

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

metaData <- collectMetaData(databaseConnection)
rawData <- collectRawData(con = databaseConnection, id = 1)
exampleSamples <- unique(rawData$sample)

test_that("Data imputation works and is optional", {
  expect_equal(datasets::iris, imputeMissingIf(datasets::iris, FALSE))
  tibble(
    id = rep(1,2),
    lipid = c("hello", "world"),
    category = rep("test", 2),
    func_cat = rep("test", 2),
    sample = c("First sample", "First sample"),
    sample_replicate = c("First sample A", "First sample A"),
    sample_replicate_technical = c("First sample A a", "First sample A b"),
    sample_identifier = c("1Aa", "1Ab"),
    value = c(1,2)
  ) %>% 
    imputeMissingIf() %>% 
    nrow() %>% 
    expect_equal(4)
  
  exampleSamples <- c("1 - 0min Stim", "2 - 10min Stim", "3 - 6h Stim", "4 - 18h Stim")
  rawDataRows <- map(exampleSamples, ~{
    rawData %>% 
      filterRawDataFor(
        samplesToSelect = .x
      )
  }) %>% 
    map_int(nrow)
  
  imputedDataRows <-  
  map(exampleSamples, ~{
    rawData %>% 
      imputeMissingIf() %>% 
      filterRawDataFor(
        samplesToSelect = .x
      )
  }) %>% 
    map_int(nrow)
  expect_true(all( (imputedDataRows - rawDataRows) > 0 ))
})

test_that("Standardization within techincal replicates works and is optional", {
  expect_equal(datasets::iris, standardizeWithinTechnicalReplicatesIf(datasets::iris, FALSE))
  tibble(id = rep(1, 3),
         sample_replicate_technical = c("1Aa", "1Aa", "1Ab"),
         value = c(2, 18, 42)) %>% 
    standardizeWithinTechnicalReplicatesIf() %>% 
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

test_that("FilterRawData works on this example", {
  rawData %>% 
    filterRawDataFor() %>% 
    nrow() %>% 
    expect_equal(nrow(rawData))
  
  map(exampleSamples, ~{
    rawData %>% 
      filterRawDataFor(
        samplesToSelect = .x
      )
  }) %>% 
    map_int(nrow) %>% 
    expect_equal(c(1129, 1190, 1187, 1215))
})

test_that("Baseline substraction works", {
  rawData %>%
    imputeMissingIf() %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf() %>%
    filterRawDataFor() %>% 
    standardizeRawDataWithin(baselineSample = exampleSamples[1]) %>% 
    filter(sample == exampleSamples[1]) %>% 
    pull(value) %>% 
    mean() %>% 
    expect_equal(0)
})


test_that("Pairwise t-tests didn't change", {
  plotData <- rawData %>%
    imputeMissingIf() %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf() %>%
    filterRawDataFor() %>%
    standardizeRawDataWithin() %>%
    createPlotData()
  
  doAllPairwiseComparisons(plotData) %>% 
    pull(p.value) %>% 
    mean() %>% 
    expect_equal(0.9270269)
})