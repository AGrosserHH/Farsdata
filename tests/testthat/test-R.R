context("Test Farsdata package")

library(dplyr)
library(maps)

setwd(system.file("extdata", package = "Farsdata"))

test_that("fars_read() works correctly", {
  expect_is(fars_read("accident_2014.csv.bz2"), "tbl_df")
})
