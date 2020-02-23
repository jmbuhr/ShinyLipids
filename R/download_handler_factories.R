#' Create Download Handler for CSV files
#'
#' @param metaData tibble. Meta data.
#' @param dataset tibble. Data to save.
#' @param specifier string. Postfix for the file name.
#' @param id integer. Id of the data set.
#'
#' @return download handler for the shiny server function
downloadHandlerFactoryCSV <- function(metaData, dataset, specifier, id) {
  downloadHandler(
    filename = function() {
      tmp <- metaData %>% filter(id == id) %>% select(title)
      tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
      paste0(Sys.Date(), "_", tmp, specifier, ".csv")
    },
    content = function(file) {
      write.csv(x = dataset, file = file)
    }
  )
}

#' Create Download Handler for PDF files
#'
#' @param metaData tibble. Meta data.
#' @param plot ggplot. Plot to save.
#' @param specifier string. Postfix for the file name.
#' @param width numeric. Width in inches.
#' @param height numeric. Height in inches.
#' @param id integer. Id of the data set.
#'
#' @return download handler for the shiny server function
downloadHandlerFactoryPDF <- function(metaData, plot, specifier, width, height, id) {
  downloadHandler(
    filename = function() {
      tmp <- metaData %>% filter(id == id) %>% select(title)
      tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
      paste0(Sys.Date(), "_", tmp, specifier, ".pdf")
    },
    content = function(file) {
      ggsave(file, plot = plot, width = width, height = height)
    }
  )
}


#' Create Download Handler for PDF files
#'
#' @param metaData tibble. Meta data.
#' @param plot ggplot. Plot to save.
#' @param specifier string. Postfix for the file name.
#' @param width numeric. Width in inches.
#' @param height numeric. Height in inches.
#' @param id integer. Id of the data set.
#'
#' @return download handler for the shiny server function
downloadHandlerFactoryRDS <- function(metaData, plot, specifier, width, height, id) {
  downloadHandler(
    filename = function() {
      tmp <- metaData %>% filter(id == id) %>% select(title)
      tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
      paste0(Sys.Date(), "_", tmp, specifier, ".rds")
    },
    content = function(file) {
      saveRDS(plot, file)
    }
  )
}

