# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
db_con <- DBI::dbConnect(RSQLite::SQLite(), "database/exampleDatabase.db")
ShinyLipids::run_app(db = db_con)
