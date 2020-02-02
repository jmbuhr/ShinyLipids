updateAllSelectizeInputs <- function(inputName, choiceColumn, selectedChoice,
                                     data, session) {
  updateSelectizeInput(session,
                       inputName,
                       choices  = levels(data[[choiceColumn]]),
                       selected = selectedChoice
  )
}

updateAllRangeInputs <- function(inputName, choiceColumn,
                                 data, session) {
  l <- range(data[[choiceColumn]], na.rm = TRUE)
  updateSliderInput(session,
                    inputName,
                    min  = l[1],
                    max  = l[2],
                    value = l
  )
}

updateInputsForSpeciesProfile <- function(session, quickSpeciesProfileClass) {
  updateSelectInput(session, "aesFacetCol", selected = "")
  updateSelectInput(session, "aesFacetRow", selected = "")
  updateSelectizeInput(session, "standardizationFeatures", selected = c("class", "sample_replicate"))
  updateSelectInput(session, "aesX", selected = "lipid")
  updateSelectizeInput(session, "lipidClassToSelect", selected = unname(quickSpeciesProfileClass))
}

