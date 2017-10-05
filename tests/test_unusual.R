
rm(list=ls())

# library(devtools)
# devtools::load_all()

library(testthat)

my_files <- list.files("./unusual_sgf", pattern="*.sgf", full.names=TRUE)

my_game <- './unusual_sgf/no_moves.sgf'

d <- read_sgf(my_game)

test_that("game reads correctly", {

  expect_true(length(d)==19)
  expect_true(d$GM=="1")
  expect_true(nrow(d$moves)==0)
  expect_true(all(nchar(names(d)) > 1))

})