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
