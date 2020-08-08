createWideData <- function(plotData,
                           aesX = "class") {
  plotData %>%
    pivot_wider(names_from = aesX,
                values_from = value)
}

pcaScaleIf <- function(rec, doIt = TRUE) {
  if (doIt) {step_scale(rec, all_predictors())}
  else {rec}
}

pcaCenterIf <- function(rec, doIt = TRUE) {
  if (doIt) {step_center(rec, all_predictors())}
  else {rec}
}

createPcaPrep <- function(wideData,
                          summariseTechnicalReplicates = TRUE,
                          aesColor,
                          pcaScale,
                          pcaCenter,
                          pcaNumberPrincipalComponents) {
  selectReplicate <- ifelse(summariseTechnicalReplicates,
                            "sample_replicate",
                            "sample_replicate_technical")
  
  recipe(~., data = wideData) %>% 
    update_role(!!sym(selectReplicate),
                !!sym(aesColor),
                new_role = "id") %>% 
    pcaScaleIf(pcaScale) %>% 
    pcaCenterIf(pcaCenter) %>% 
    step_pca(all_predictors(), num_comp = pcaNumberPrincipalComponents, id = "pca") %>%
    prep()
}