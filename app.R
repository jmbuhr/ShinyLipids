pkgload::load_all()
options( "golem.app.prod" = TRUE)

# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# # install.packages("BiocManager")
# options(repos = c(BiocManager::repositories()))

## uncomment this to read from a serverside database (and fill in credentials as necessary)
# From bash: ssh -L 8080:localhost:5432 -N -T mathias@pc195.bzh.uni-heidelberg.de -p 49200
# Have your password ready
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

## Local Databse Dump
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), "./inst/extdata/exampleDatabase.db")

ShinyLipids::run_app(db = databaseConnection)
