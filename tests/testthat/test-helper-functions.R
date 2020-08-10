context("misc")

test_that("Query for raw data is expected SQL", {
  query <- createQueryForID(1)
  expect_equal(query, "SELECT * FROM data2 WHERE id = 1")
})

test_that("Main color scale returns scales", {
  scales <- mainScale(8)
  expect_length(scales, 2)
  expect_type(scales, "list")
  map(scales, expect_type, "environment")
})

test_that("testAllMoreThanOneReplicate catches bad data", {
  aesX <- "x"
  aesColor <- "col"
  tbl <- tibble(x   = c("l1", "l1", "l1", "l2", "l2", "l2"),
                col = c("A", "A", "A", "A", "A", "A"))
  tbl %>%
    group_by(!!sym(aesX), !!sym(aesColor)) %>%
    count() %>%
    pull(n) %>%
    all(. > 1)
 
  expect_true(testAllMoreThanOneReplicate(tbl, aesX, aesColor))
  expect_false(testAllMoreThanOneReplicate(bind_rows(tbl, list(x = "l3", col = "A")), aesX, aesColor))
})

test_that("parseDate returns a date", {
  expect_is(parseDate("200101"), "Date")
})
