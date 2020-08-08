# Setup of the ShinyApp ####
pkgload::load_all()

# Deployment ####
# To deploy, run: rsconnect::deployApp()
# Or use the blue button in the top right corner
# of this file in RStudio

## For installation on a server with shiny-server
# remotes::install_github("jannikbuhr/ShinyLipids",
# auth_token = Sys.getenv("GITHUB_PAT_SHINY"),
# force = TRUE, dependencies = TRUE, upgrade = TRUE)

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
path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

# Run App ####
ShinyLipids::run_app(db = databaseConnection)
