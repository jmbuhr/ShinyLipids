# File to launch the shinyApp (Do not remove this comment)
# Setup ####
pkgload::load_all() # dev and deployment

# Deployment ####
# To deploy, run: rsconnect::deployApp()
# Or use the blue button in the top right corner
# of this file in RStudio

# For deployment to shinyapps.io,
# you need to manually install BiocManager:
# (Run the next line in the terminal)
# install.packages("BiocManager")
# And uncomment the next line permanently:
# options(repos = c(BiocManager::repositories()))

## Backup for installation on a server with shiny-server
# remotes::install_github("jannikbuhr/ShinyLipids",
# auth_token = "b68cfd50c5455a7240807335fc633568634cce78",
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
