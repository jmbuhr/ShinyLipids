# Setup of the ShinyApp <- keep this comment for RStudio ####

# Deployment ####
# To deploy, run: rsconnect::deployApp()
# Or use the blue button in the top right corner
# of this file in RStudio

## For installation on a server running shiny-server
# remotes::install_github("jmbuhr/ShinyLipids", 
#                         force = TRUE, dependencies = TRUE, upgrade = TRUE)

# Database connections ####
## uncomment this to read from a database.
## In this example it is on the same server this App is running on
# databaseConnection <- DBI::dbConnect(RPostgres::Postgres(),
#                                      dbname = "ldb",
#                                      host = "localhost",
#                                      port = 5432,
#                                      user = "mathias")

## Local database file
# replace with
# path <- "path/to/your/data.db"
path <- system.file("extdata/Sqlite_ML.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

# Run App ####
ShinyLipids::run_app(db = databaseConnection)