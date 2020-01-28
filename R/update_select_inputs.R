getLevelsFromRawDataColumn <- function(data, col) levels(data[[col]])

getRangeFromRawDataColumn <- function(data, col) range(data[[col]], na.rm = TRUE)

updateAllSelectizeInputs <- function(inputName, choiceColumn, selectedChoice,
                                     data, session) {
  updateSelectizeInput(session,
                       inputName,
                       choices  = getLevelsFromRawDataColumn(data, choiceColumn),
                       selected = selectedChoice
  )
}

updateAllRangeInputs <- function(inputName, choiceColumn,
                                 data, session) {
  l <- getRangeFromRawDataColumn(data, choiceColumn)
  updateSliderInput(session,
                    inputName,
                    min  = l[1],
                    max  = l[2],
                    value = l
  )
}