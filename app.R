# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# # install.packages("BiocManager")
# options(repos = c(BiocManager::repositories()))

## uncomment this to read from a serverside database (and fill in credentials as necessary)
# From bash: ssh -L 8080:localhost:5432 -N -T mathias@pc195.bzh.uni-heidelberg.de -p 49200
# Have your password ready
# db_con <- DBI::dbConnect(RPostgres::Postgres(),
#                          dbname = "ldb",
#                          host = "localhost",
#                          port = 8080,
#                          user= "mathias")

## uncomment this to read from a database on the same server this App is running
# db_con <- DBI::dbConnect(RPostgres::Postgres(),
#                          dbname = "ldb",
#                          host = "localhost",
#                          port = 5432,
#                          user= "mathias")

## Local Databse Dump
db_con <- DBI::dbConnect(RSQLite::SQLite(), "database/exampleDatabase.db")

pkgload::load_all()
options( "golem.app.prod" = TRUE)
ShinyLipids::run_app(db = db_con)