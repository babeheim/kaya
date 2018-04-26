

test_that("all games pass audit",{
  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names=TRUE)
  valid <- unlist(lapply(my_files, validate_games))
  expect_true(all(valid))
})

