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
