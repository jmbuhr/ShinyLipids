# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

## uncomment this to read from a serverside database (and fill in credentials as necessary)
# db <- src_postgres(dbname = "ldb",
#                    host    = "",
#                    port    = 5432,
#                    user    = "")

## uncomment this to read from a database on the same server this App is running (and fill in credentials as necessary)
# db <- src_postgres(dbname = "ldb",
#                    host    = "localhost",
#                    user    = "")

## Local Databse Dump
db <- "database/exampleDatabase.db"

ShinyLipids::run_app(db = db) # add parameters here (if any)
