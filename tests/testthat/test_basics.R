

test_that("one file parses", {
  my_files <- "./real_sgf/2009-09-01-1.sgf"
  sgf_lines <- paste0(readLines(my_files, warn = FALSE), collapse = "")
  x <- parse_sgf(sgf_lines)
  expect_true(!is.null(names(x)))
})

test_that("one file parses and simplifies", {
  my_files <- "./real_sgf/2009-09-01-1.sgf"
  sgf_lines <- paste0(readLines(my_files, warn = FALSE), collapse = "")
  x <- parse_sgf(sgf_lines)
  x <- simplify_game(x)
  expect_true(!is.null(names(x)))
})

test_that("blah", {
  my_game <- './real_sgf/2009-09-01-10.sgf'
  sgf_lines <- paste0(readLines(my_game, warn = FALSE), collapse = "")
  d <- parse_sgf(sgf_lines)
  d <- simplify_game(d)
  expect_true(length(d) > 0)
  expect_true(validate_games(my_game))
})

test_that("loads lines with no errors", {
  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names = TRUE)
  for(i in 1:length(my_files)){
    sgf_lines <- paste0(readLines(my_files[i], warn = FALSE), collapse = "")
    expect_silent(x <- parse_sgf(sgf_lines))
  }
})

test_that("one file reads", {
  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names = TRUE)
  expect_silent(x <- read_sgf(my_files[1], to.json = TRUE))
})

test_that("reads several files with no errors", {
  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names=TRUE)
  for(i in 1:length(my_files)){
    expect_silent(x <- read_sgf(my_files[i], to.json = FALSE))
  }
})

test_that("all games pass audit",{
  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names=TRUE)
  valid <- unlist(lapply(my_files, validate_games))
  expect_true(all(valid))
})

