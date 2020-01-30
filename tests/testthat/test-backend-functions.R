context("Backend Functions")

pkgload::load_all()

path <- system.file("inst/extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
lipidClassOrder <- collectLipidClassOrder(databaseConnection)

# readRDS(testthat::test_path("inputList.Rds")) %>% dput()

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
  filter_db = c(0L, 10L),
  pca_pointSize = 5L,
  pca_cv = "none",
  samplesToRemove = NULL,
  sidebarItemExpanded = "Visualization",
  filter_oh = c(0L,
                10L),
  showFullMeta = FALSE,
  ID = "",
  tabs = "Quickoptions",
  class_profile = structure(0L, class = c("integer", "shinyActionButtonValue")),
  pcaNumberPrincipalComponents = 2L,
  pca_Width = 20L,
  pca_Height = 20L,
  categoryToSelect = NULL,
  pca_scaling = "none",
  pca_vectors = TRUE,
  heatWidth = 20L,
  heatLabSize = 9L,
  quickClassForProfile = "",
  heatHeight = 10L,
  replicatesToSelect = NULL,
  pca_method = "svd",
  filter_length = c(1L,
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
  "Data processing steps run",
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





