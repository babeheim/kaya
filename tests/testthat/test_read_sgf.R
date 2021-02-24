
test_that("one file reads", {
  my_files <- list.files("./redbean_sgf", pattern = "*.sgf", full.names = TRUE)
  expect_silent(x <- read_sgf(my_files[1], to.json = TRUE))
})

test_that("reads all redbean games", {
  my_files <- list.files("./redbean_sgf", pattern = "*.sgf", full.names = TRUE)
  for(i in 1:length(my_files)) {
    expect_silent(x <- read_sgf(my_files[i], to.json = FALSE))
  }
})

test_that("reads several files", {
  my_files <- list.files("./real_sgf", pattern = "*.sgf", full.names=TRUE)
  for(i in 1:length(my_files)) {
    expect_silent(x <- read_sgf(my_files[i], to.json = FALSE))
  }
})

