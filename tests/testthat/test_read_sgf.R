

test_that("one file parses", {
  my_files <- "./redbean_sgf/redbean_no_variation.sgf"
  sgf_lines <- paste0(readLines(my_files, warn = FALSE), collapse = "")
  x <- parse_sgf(sgf_lines)
  expect_true(!is.null(names(x)))
})

test_that("one file parses and simplifies", {
  my_files <- "./redbean_sgf/redbean_no_variation.sgf"
  sgf_lines <- paste0(readLines(my_files, warn = FALSE), collapse = "")
  x <- parse_sgf(sgf_lines)
  x <- simplify_game(x)
  expect_true(!is.null(names(x)))
})

test_that("blah", {
  my_game <- "./redbean_sgf/redbean_no_variation.sgf"
  sgf_lines <- paste0(readLines(my_game, warn = FALSE), collapse = "")
  d <- parse_sgf(sgf_lines)
  d <- simplify_game(d)
  expect_true(length(d) > 0)
  expect_true(validate_games(my_game))
})

test_that("loads lines with no errors", {
  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names = TRUE)
  for(i in 1:length(my_files)){
    sgf_lines <- paste0(readLines(my_files[i], warn = FALSE), collapse = "")
    expect_silent(x <- parse_sgf(sgf_lines))
  }
})

test_that("one file reads", {
  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names = TRUE)
  expect_silent(x <- read_sgf(my_files[1], to.json = TRUE))
})

test_that("reads several files with no errors", {
  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names=TRUE)
  for(i in 1:length(my_files)){
    expect_silent(x <- read_sgf(my_files[i], to.json = FALSE))
  }
})

test_that("all games pass audit",{
  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names=TRUE)
  valid <- unlist(lapply(my_files, validate_games))
  expect_true(all(valid))
})


## branching game 



test_that("all games pass audit",{
  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names=TRUE)
  valid <- unlist(lapply(my_files, validate_games))
  expect_true(all(valid))
})



## unusual games



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

test_that("kgs-style right escaping works", {

  my_game <- './unusual_sgf/metadata_with_kgs_square_brackets.sgf'
  sgf_lines <- readLines(my_game, warn = FALSE)
  d <- read_sgf(my_game, rotate = FALSE)
  expect_true(validate_games(my_game))

  my_game <- './unusual_sgf/metadata_with_kgs_square_brackets_variant.sgf'
  sgf_lines <- readLines(my_game, warn = FALSE)
  d <- read_sgf(my_game, rotate = FALSE)
  expect_true(validate_games(my_game))
})


test_that("iron giant face loads", {

  my_game <- './unusual_sgf/metadata_with_iron_giant_face_escaped.sgf'
  sgf_lines <- readLines(my_game, warn = FALSE)
  d <- read_sgf(my_game, rotate = FALSE)
  expect_true(validate_games(my_game))

  my_game <- './unusual_sgf/metadata_with_iron_giant_face.sgf'
  expect_error(d <- read_sgf(my_game, rotate = FALSE))

})



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


test_that("games with characters outside sgf tree are okay", {
  my_game <- './invalid_sgf/characters_outside_games.sgf'
  expect_silent(d <- read_sgf(my_game))
})

# invalid games





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
  expect_false(validate_games(my_game))
})

test_that("invalid sgf coordinates caught",{
  my_game <- './invalid_sgf/invalid_moves.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_games(my_game))
  my_game <- './invalid_sgf/more_invalid_moves.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_games(my_game))
  my_game <- './invalid_sgf/yet_more_invalid_moves.sgf'
  d <- read_sgf(my_game)
  expect_false(validate_games(my_game))
})


