context("golem tests")
library(golem)

test_that("app ui", {
  ui <- app_ui()
  expect_shinytag(ui)
})

test_that("app server", {
  server <- app_server
  expect_is(server, "function")
})


# test_that("Shiny app launches", {
#   app <- ShinyDriver$new(path = here::here())
#   # app$listWidgets()
#   tbl <- app$getAllValues()$output$metaDataTable
#   jsonlite::parse_json(tbl) %>% 
#     length() %>% 
#     expect_equal(4)
#   app$stop()
# })

# Configure this test to fit your need
# test_that(
#   "app launches", {
#     skip_on_cran()
#     skip_on_ci()
#     skip_on_travis()
#     x <- processx::process$new(
#       "R",
#       c(
#         "-e",
#         "options( 'golem.app.prod' = TRUE); path <- system.file('extdata/exampleDatabase.db', package = 'ShinyLipids'); databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path); ShinyLipids::run_app(db = databaseConnection)"
#       )
#     )
#     Sys.sleep(3)
#     expect_true(x$is_alive())
#     x$kill()
#   }
# )
