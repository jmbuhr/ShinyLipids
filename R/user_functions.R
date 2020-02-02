#' Create your own database
#' 
#' Quickly create your own SQLite database dump from
#' dataframes without additional software.
#'
#' @param path string,
#' Path to where you want to save the database dump file
#' 
#' @param meta_info data.frame
#' A data.frame with the information about your datasets
#' See README on href{https://github.com/jannikbuhr/ShinyLipids}{github} for the columns it needs.
#' 
#' @param dataset
#' A data.frame with the datasets
#' See README on href{https://github.com/jannikbuhr/ShinyLipids}{github} for the columns they need.
#' 
#' @param overwrite Boolean
#' Overwrite existing .db file? Default is FALSE.
#' 
#' @return
#' NULL
#' @export
createDatabase <- function(path = "databaseDump.db",
                           meta_info, dataset,
                           overwrite = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "id_info", meta_info, overwrite = overwrite)
  DBI::dbWriteTable(con, "data2", dataset, overwrite = overwrite)
  DBI::dbDisconnect(con)
}
