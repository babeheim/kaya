

test_that("kanji names okay", {
  my_game <- './unusual_sgf/name_kanji.sgf'
  sgf_lines <- readLines(my_game, warn = FALSE)
  d <- read_sgf(my_game)
  expect_true(d$PB == "ヒカル")
  expect_true(d$PW == "お顔")
  expect_true(validate_games(my_game))
})

test_that("strings with ( ) okay", {
  my_game <- './unusual_sgf/metadata_with_parentheses.sgf'
  sgf_lines <- paste0(readLines(my_game, warn = FALSE), collapse = "")
  d <- read_sgf(my_game, simplify = TRUE)
  expect_true(length(d) > 0)
  expect_true(validate_games(my_game))
})

test_that("comments with semicolons okay", {
  my_game <- './unusual_sgf/comments_have_semicolons.sgf'
  sgf_lines <- readLines(my_game, warn = FALSE)
  d <- read_sgf(my_game)
  expect_true(validate_games(my_game))
})

test_that("comments work", {
  my_game <- './unusual_sgf/comments_with_parentheses.sgf'
  sgf_lines <- readLines(my_game, warn = FALSE)
  d <- read_sgf(my_game, rotate = FALSE)
  expect_true(validate_games(my_game))
})

test_that("paired unescaped square bracket in comments ok", {
  my_game <- './unusual_sgf/metadata_with_square_brackets.sgf'
  sgf_lines <- readLines(my_game, warn = FALSE)
  d <- read_sgf(my_game, rotate = FALSE)
  expect_true(validate_games(my_game))
})


# test_that("kgs-style right escaping works", {
#   my_game <- './unusual_sgf/metadata_with_kgs_square_brackets.sgf'
#   sgf_lines <- readLines(my_game, warn = FALSE)
#   d <- read_sgf(my_game, rotate = FALSE)
#   expect_true(validate_games(my_game))
# })


test_that("no-move game reads correctly", {
  my_game <- './unusual_sgf/only_metadata.sgf'
  sgf_lines <- paste0(readLines(my_game, warn = FALSE), collapse = "")
  d <- read_sgf(my_game)
  expect_true(length(d) == 18)
  expect_true(d$GM == "1")
  expect_true(d$n_moves == 0)
  expect_true(all(nchar(names(d)) > 1))
  expect_true(validate_games(my_game))
})


