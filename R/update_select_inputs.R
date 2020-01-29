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