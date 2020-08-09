#' Custom update selectize input
#'
#' @param inputName string, input to update
#' @param choiceColumn string, column in the data to get choices from
#' @param selectedChoice NULL or "" 
#' @param data rawData
#' @param session session
#'
#' @return IO ()
updateAllSelectizeInputs <- function(inputName, choiceColumn, selectedChoice,
                                     data, session) {
  updateSelectizeInput(session,
                       inputName,
                       choices  = levels(data[[choiceColumn]]),
                       selected = selectedChoice
  )
}

#' Custom update range input
#'
#' @param inputName string, input to update
#' @param choiceColumn string, column in the data to get choices from
#' @param data rawData
#' @param session session
#' 
#' @return IO ()
updateAllRangeInputs <- function(inputName, choiceColumn,
                                 data, session) {
  l <- range(data[[choiceColumn]], na.rm = TRUE) %>% as.integer()
  updateSliderInput(session,
                    inputName,
                    min  = l[1],
                    max  = l[2],
                    step = 1L,
                    value = l
  )
}

#' Update inputs for species profile
#' 
#' @param session session
#' @param quickSpeciesProfileClass string, lipid class to display as profile
#' 
#' @return IO ()
updateInputsForSpeciesProfile <- function(session, quickSpeciesProfileClass) {
  updateSelectInput(session, "aesFacetCol", selected = "")
  updateSelectInput(session, "aesFacetRow", selected = "")
  updateSelectizeInput(session, "standardizationFeatures", selected = c("class", "sample_replicate"))
  updateSelectInput(session, "aesX", selected = "lipid")
  updateSelectizeInput(session, "lipidClassToSelect", selected = unname(quickSpeciesProfileClass))
}

