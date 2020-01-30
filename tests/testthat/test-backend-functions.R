context("Data Visualization Pipeline")

pkgload::load_all()

path <- system.file("inst/extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
lipidClassOrder <- collectLipidClassOrder(databaseConnection)

input <- list(
  ID                                         = 1,
  standardizeWithinTechnicalReplicate        = TRUE,
  standardizationFeatures                    = c("class", "sample_replicate"),
  summariseTechnicalReplicates               = TRUE,
  baselineSample                             = "",
  lipidClassToSelect                         = "PC",
  aesX                                       = "lipid",
  aesFacetCol                                = "class",
  aesFacetRow                                = "",
  aesColor                                   = "sample",
  errorbarType  = "None",
  mainPlotAdditionalOptions = list(
    "Show points"                       = "points",
    "Show bars"                         = "bars",
    "Show average"                      = "mean",
    # "Show value of means"               = "values",
    # "Show value of points"              = "ind_values",
    # "Transform y-axis log1p"            = "log",
    "Show N per sample"                 = "N"
    # "Label points"                      = "label",
    # "Swap x- and y-axis"                = "swap",
    # "Free y scale for facets"           = "free_y",
    # "Mark groups with significant hits" = "signif"
  ),
  filterDoubleBondsRange = c(0L, 10L),
  pcaPointSize = 5L,
  pcaCrossValidationMethod = "none",
  samplesToRemove = NULL,
  sidebarItemExpanded = "Visualization",
  filterOhRange = c(0L,
                10L),
  showFullMeta = FALSE,
  ID = "",
  tabs = "Quickoptions",
  quickClassProfile = structure(0L, class = c("integer", "shinyActionButtonValue")),
  pcaNumberPrincipalComponents = 2L,
  pcaWidth = 20L,
  pcaHeight = 20L,
  categoryToSelect = NULL,
  pcaScalingMethod = "none",
  pca_vectors = TRUE,
  heatWidth = 20L,
  heatLabSize = 9L,
  quickSpeciesProfileClass = "",
  heatHeight = 10L,
  replicatesToSelect = NULL,
  pca_method = "svd",
  filterLengthRange = c(1L,
                    100L),
  pca_center = TRUE,
  pca_labels = TRUE,
  replicatesToRemove = NULL,
  samplesToSelect = NULL,
  tab = "metadata",
  technicalReplicatesToRemove = NULL,
  sidebarCollapsed = FALSE,
  functionalCategoryToSelect = NULL,
  drawPcaConvexHull = FALSE,
  mainWidth = 20L,
  mainHeight = 10L,
  heatColor = "viridis"
)

test_that(
  "Default data processing steps run until visualisation",
  {
    query <- createQueryForID(input$ID)
    rawData <- collectRawData(con = databaseConnection,
                              query = query,
                              lipidClassOrder = lipidClassOrder)
    
    
    metaData <- collectMetaData(databaseConnection)
    
    plotData <- rawData %>%
      standardizeWithinTechnicalReplicatesIf(input$standardizeWithinTechnicalReplicate) %>%
      filterRawDataFor(input) %>%
      standardizeRawDataWithin(
        baselineSample  = input$baselineSample,
        standardizationFeatures = input$standardizationFeatures
      ) %>%
      createPlotData(input)
    
    meanPlotData <- summarisePlotData(plotData)
    
    plt <- createMainPlot(plotData = plotData,
                          meanPlotData = meanPlotData,
                          pairwiseComparisons = NULL,
                          rangeX = NULL,
                          rangeY = NULL,
                          input = input)
    
    expect_is(plt, "ggplot")
  }
)





