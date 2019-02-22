library(testthat)
library(shinytest)

context("Shiny Test")

test_that("trigonometric functions match identities", {
    expect_equal(sin(pi / 4), 1 / sqrt(2))
    expect_equal(cos(pi / 4), 1 / sqrt(2))
    expect_equal(tan(pi / 4), 1)
})

test_that("Application working?", {
 # Use compareImages=FALSE because the expected image screenshots were created
 # on a Mac, and they will differ from screenshots taken on the CI platform,
 # which runs on Linux.
 expect_pass(testApp(".", compareImages = FALSE, interactive = FALSE))
})
