#' Create your own database
#' 
#' Quickly create your own SQLite database dump from
#' dataframes without additional software.
#'
#' @param path string,
#' Path to where you want to save the database dump file
#' 
#' @param metaData tibble.
#' A data.frame with the information about your datasets
#' See README on href{https://github.com/jmbuhr/ShinyLipids}{github} for the columns it needs.
#' 
#' @param rawData
#' tibble. Metadata of the data sets
#' See README on href{https://github.com/jmbuhr/ShinyLipids}{github} for the columns they need.
#' 
#' @param overwrite boolean.
#' Overwrite existing .db file? Default is FALSE.
#' 
#' @return
#' NULL
#' @export
createDatabase <- function(path = "databaseDump.db",
                           metaData,
                           rawData,
                           overwrite = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "id_info", metaData, overwrite = overwrite)
  DBI::dbWriteTable(con, "data2", rawData, overwrite = overwrite)
  DBI::dbDisconnect(con)
}
