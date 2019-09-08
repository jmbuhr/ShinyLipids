
#' Title
#'
#' @param path string,
#' Path to where you want to save the database dump file
#' 
#' @param meta_info data.frame
#' A data.frame with the information about your datasets
#' See README for the columns it needs.
#' 
#' @param dataset
#' A data.frame with the datasets
#' See README for the columns they need
#' 
#' @return
#' NULL
#' @export
createDatabase <- function(path = "databaseDump.db", meta_info, dataset) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "id_info", meta_info)
  DBI::dbWriteTable(con, "data2", datasets)
  DBI::dbDisconnect(con)
}

