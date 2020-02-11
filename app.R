# File to launch the ShinyApp (Do not remove this comment)
# Setup ####
pkgload::load_all()

# Deployment ####
# To deploy, run: rsconnect::deployApp()
# Or use the blue button in the top right corner
# of this file in RStudio

# You might need this additional package for deployment
# # install.packages("BiocManager")
# options(repos = c(BiocManager::repositories()))

# Database connections ####
## uncomment this to read from a serverside database
# databaseConnection <- DBI::dbConnect(RPostgres::Postgres(),
#                          dbname = "ldb",
#                          host = "localhost",
#                          port = 8080,
#                          user= "mathias")

## uncomment this to read from a database on the same server this App is running
# databaseConnection <- DBI::dbConnect(RPostgres::Postgres(),
#                          dbname = "ldb",
#                          host = "localhost",
#                          port = 5432,
#                          user= "mathias")

## Local database file
path <- "./inst/extdata/exampleDatabase.db"
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

# Run App ####
ShinyLipids::run_app(db = databaseConnection)
