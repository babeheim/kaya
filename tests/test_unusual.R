
rm(list=ls())


my_game <- './unusual_sgf/characters_outside_games.sgf'

sgf_lines <- readLines(my_game)

d <- read_sgf(my_game)





my_game <- './unusual_sgf/contains_three_games.sgf'

d <- read_sgf(my_game)

test_that("multigame files read right", {

  expect_true(length(d)==3)

})


my_game <- './unusual_sgf/no_moves.sgf'

d <- read_sgf(my_game)

test_that("no-move game reads correctly", {

  expect_true(length(d)==20)
  expect_true(d$GM=="1")
  expect_true(nrow(d$moves)==0)
  expect_true(all(nchar(names(d)) > 1))

})