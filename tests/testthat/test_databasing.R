





test_that("validate games detects good and bad games", {

  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names = TRUE)
  out <- validate_games(my_files)
  expect_true(all(out))

  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names = TRUE)
  out <- validate_games(my_files)
  expect_true(all(out))

  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names = TRUE)
  out <- validate_games(my_files)
  expect_true(all(out))

  my_files <- list.files("./unusual_sgf", pattern="*.sgf", full.names = TRUE)
  out <- validate_games(my_files)
  expect_true(all(out))

  my_files <- list.files("./invalid_sgf", pattern="*.sgf", full.names = TRUE)
  expect_error(out <- validate_games(my_files))

})



test_that("database loads from files", {

  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(32, 23)))

  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(7, 22)))

  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(3, 17)))

  my_files <- list.files("./unusual_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(17, 23)))


})


test_that("database isn't fooled by bad games", {

  my_files <- list.files("./invalid_sgf", pattern="*.sgf", full.names = TRUE)
  expect_error(out <- create_database(my_files))

})