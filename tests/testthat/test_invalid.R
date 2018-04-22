

test_that("games with characters outside sgf tree fail", {

  my_game <- './invalid_sgf/characters_outside_games.sgf'

  expect_error(d <- read_sgf(my_game))

})


test_that("extra brackets caught",{

  my_game <- './invalid_sgf/illegal_square_brackets.sgf'
  expect_error(d <- read_sgf(my_game))

  my_game <- './invalid_sgf/more_illegal_square_brackets.sgf'
  expect_error(d <- read_sgf(my_game))

})


test_that("missing brackets caught",{

  my_game <- './invalid_sgf/missing_right_bracket.sgf'
  expect_error(d <- read_sgf(my_game))

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

