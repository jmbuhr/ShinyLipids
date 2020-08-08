dtOptions <- list(
  orderClasses   = TRUE,
  pageLength     = 10,
  order          = list(0, "desc"),
  scrollX        = TRUE,
  deferRender    = TRUE,
  scrollCollapse = TRUE
)


#' Generate list of default options
#' 
#' When runninng the shiny app, create the global variable
#' \code{defaultInput <- generateDefaultInput()}. When running
#' functions from ShinyLipids outside of the Shiny app,
#' generate the input list normally created by the web interface
#' with \code{input <- generateDefaultInput()}.
#'
#' @return list. A list of default options to be assigned to \code{input}
#' @export
generateDefaultInput <- function() {
  list(
    aesX = "class",
    aesColor = "sample",
    aesFacetCol = "",
    aesFacetRow = "",
    # Filtering
    categoryToSelect            = NULL,
    lipidClassToSelect          = NULL,
    functionalCategoryToSelect  = NULL,
    filterLengthRange           = c(1L, 100L),
    filterDoubleBondsRange      = c(0L, 100L),
    filterOhRange               = c(0L, 100L),
    samplesToSelect             = NULL,
    samplesToRemove             = NULL,
    replicatesToSelect          = NULL,
    replicatesToRemove          = NULL,
    technicalReplicatesToRemove = NULL,
    imputeMissingAs0 = TRUE,
    # Standardization
    summariseTechnicalReplicates = TRUE,
    baselineSample          = "",
    standardizationFeatures = c(""),
    standardizeWithinTechnicalReplicate = TRUE,
    # Sample selection
    samplesToSelect = NULL,
    samplesToRemove = NULL,
    replicatesToSelect = NULL,
    replicatesToRemove = NULL,
    technicalReplicatesToRemove = NULL,
    # Plotting, main plot
    errorbarType = "None",
    mainPlotAdditionalOptions = list("points", "bars"),
    mainWidth = 20,
    mainHeight = 10,
    pcaWidth = 10,
    pcaHeight = 10,
    # heatmap
    heatWidth = 20,
    heatheight = 10,
    heatColor = "viridis",
    heatLabSize = 9,
    # Dimensionality reduction
    pcaCrossValidationMethod = "none",
    pcaNumberPrincipalComponents = 2,
    pcaCenter = TRUE,
    pcaScale = TRUE,
    pcaLabels = FALSE,
    drawPcaConvexHull = TRUE,
    pcaPointSize = 5,
    pcaVectors = TRUE
  )
}


