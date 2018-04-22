

test_that("one file parses", {
  my_files <- "./normal_sgf/2009-09-01-1.sgf"
  sgf_lines <- paste0(readLines(my_files), collapse = "")
  x <- parse_sgf(sgf_lines)
  expect_true(!is.null(names(x)))
})

test_that("one file parses and simplifies", {
  my_files <- "./normal_sgf/2009-09-01-1.sgf"
  sgf_lines <- paste0(readLines(my_files), collapse = "")
  x <- parse_sgf(sgf_lines)
  x <- simplify_game(x)
  expect_true(!is.null(names(x)))
})

test_that("loads lines with no errors", {
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names = TRUE)
  for(i in 1:length(my_files)){
    sgf_lines <- paste0(readLines(my_files[i]), collapse = "")
    expect_silent(x <- parse_sgf(sgf_lines))
  }
})

test_that("one file reads", {
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names = TRUE)
  expect_silent(x <- read_sgf(my_files[1], to.json = TRUE))
})

test_that("reads several files with no errors", {
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names=TRUE)
  for(i in 1:length(my_files)){
    expect_silent(x <- read_sgf(my_files[i], to.json = TRUE))
  }
})

test_that("all games pass audit",{
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names=TRUE)
  d_all <- lapply_pb( my_files, read_sgf )
  valid <- unlist(lapply(d_all, validate_game))
  expect_true(all(valid))
})

