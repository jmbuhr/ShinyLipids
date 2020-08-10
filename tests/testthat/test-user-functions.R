context("User functions")

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

test_that("Create database retains data", {
  tmp <- tempfile()
  tables <- DBI::dbListTables(databaseConnection) %>%
    map(~ DBI::dbReadTable(databaseConnection, .x))
  createDatabase(tmp, tables[[2]], tables[[1]])
  
  newCon <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  NewTables <- DBI::dbListTables(newCon) %>%
    map(~ DBI::dbReadTable(newCon, .x))
  
  expect_identical(tables, NewTables)
})
