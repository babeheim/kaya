

# validate_games
# versus
# validate sgf



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
  expect_true(all(dim(out) == c(length(my_files), 23)))

  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(length(my_files), 22)))

  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(length(my_files), 17)))

  my_files <- list.files("./unusual_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(length(my_files), 27)))


})


test_that("database helpfully tells you which is the bad game in a set", {

  my_files <- list.files("./real_sgf_one_invalid", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)

})