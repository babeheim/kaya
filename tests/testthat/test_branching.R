

test_that("all games pass audit",{
  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names=TRUE)
  d_all <- lapply_pb( my_files, read_sgf )
  valid <- unlist(lapply(d_all, validate_game))
  expect_true(all(valid))
})

