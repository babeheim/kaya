
rm(list=ls())



my_game <- './unusual_sgf/name_kanji.sgf'

sgf_lines <- readLines(my_game)

d <- read_sgf(my_game)

test_that("kanji names okay", {

  expect_true(d$PB=="ヒカル")
  expect_true(d$PW=="お顔")
  expect_true(validate_game(d))

})




my_game <- './unusual_sgf/hourglass_parentheses_problem.sgf'

sgf_lines <- readLines(my_game)

d <- read_sgf(my_game)

test_that("strings with )( okay", {

  expect_true(grep("\\)\\(", d$moves$comment) > 1)
  expect_true(length(d)>0)
  expect_true(validate_game(d))

})


my_game <- './unusual_sgf/comments_have_semicolons.sgf'

sgf_lines <- readLines(my_game)

d <- read_sgf(my_game)

test_that("comments with semicolons okay", {

  expect_true("comment;another" %in% d$moves$comment)
  expect_true(validate_game(d))

})


my_game <- './unusual_sgf/comments_with_parentheses.sgf'

sgf_lines <- readLines(my_game)

d <- read_sgf(my_game, rotate=FALSE)

test_that("comments work", {

  expect_true("aburry 13k*: very good (1)" %in% d$moves$comment)
  expect_true(validate_game(d))

})




my_game <- './unusual_sgf/characters_outside_games.sgf'

sgf_lines <- readLines(my_game)

d <- read_sgf(my_game)

test_that("comments work", {

  expect_true(length(d)==20)
  expect_true(nrow(d$moves)==58)
  expect_true(d$filename=="./unusual_sgf/characters_outside_games.sgf")
  expect_true(validate_game(d))

})




my_game <- './unusual_sgf/contains_three_games.sgf'

d <- read_sgf(my_game)

test_that("multigame files read right", {

  expect_true(length(d)==3)
  expect_true(validate_game(d))

})


my_game <- './unusual_sgf/no_moves.sgf'

d <- read_sgf(my_game)

test_that("no-move game reads correctly", {

  expect_true(length(d)==21)
  expect_true(d$GM=="1")
  expect_true(nrow(d$moves)==0)
  expect_true(all(nchar(names(d)) > 1))
  expect_true(validate_game(d))

})