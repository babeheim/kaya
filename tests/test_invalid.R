
rm(list=ls())

# library(devtools)
# devtools::load_all()

library(testthat)

test_that("extra brackets caught",{

  my_game <- './invalid_sgf/illegal_square_brackets.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_game(d))

  my_game <- './invalid_sgf/more_illegal_square_brackets.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_game(d))

})


test_that("missing brackets caught",{

  my_game <- './invalid_sgf/missing_right_bracket.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_game(d))

})


test_that("duplicated tags caught",{

  my_game <- './invalid_sgf/duplicate_tag.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_game(d))

})



test_that("invalid sgf coordinates caught",{

  my_game <- './invalid_sgf/invalid_moves.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_game(d))

  my_game <- './invalid_sgf/more_invalid_moves.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_game(d))

  my_game <- './invalid_sgf/yet_more_invalid_moves.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_game(d))

})



# we're gonna have corrupted games out in the wild
# rather than just break, we should have it skip and 
# identify whats wrong, so we can clean up easily

# we need an sgf validator to run on sgf_lines
# it has to pass that QC first before you run the parser

# test: games are not properly seperated by ( ) 
# test: games have ";" inside [] (actually thats unusual but not bad)