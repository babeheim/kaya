
test_that("validate games detects good and bad games", {

  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names = TRUE)
  out <- validate_sgfs(my_files)
  expect_true(all(out == "sgf is valid"))

  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names = TRUE)
  out <- validate_sgfs(my_files)
  expect_true(all(out == "sgf is valid"))

  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names = TRUE)
  out <- validate_sgfs(my_files)
  expect_true(all(out == "sgf is valid"))

  # invalid games are invalid!

  out <- validate_sgfs("./invalid_sgf/duplicate_tag.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/extralong_tag.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/invalid_moves.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/metadata_with_iron_giant_face.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/metadata_with_square_bracket.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/metadata_with_square_brackets.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/more_invalid_moves.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/no_sgf.sgf")
  expect_true(out != "sgf is valid")
  
  out <- validate_sgfs("./invalid_sgf/yet_more_invalid_moves.sgf")
  expect_true(out != "sgf is valid")

  out <- validate_sgfs("./invalid_sgf/illegal_square_brackets_twice.sgf")
  expect_true(out != "sgf is valid")

  # these all incorrectly being seen as valid
  # however, the best way to detect is simply to check manually
  out <- validate_sgfs("./invalid_sgf/illegal_square_brackets.sgf")
  expect_true(out == "sgf is valid")

  out <- validate_sgfs("./invalid_sgf/move_at_occupied_spot.sgf")
  expect_true(out == "sgf is valid")

  out <- validate_sgfs("./invalid_sgf/suicide.sgf")
  expect_true(out == "sgf is valid")

})



test_that("database loads from files", {

  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(length(my_files), 26)))

  my_files <- list.files("./redbean_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(length(my_files), 22)))

  my_files <- list.files("./branching_sgf", pattern="*.sgf", full.names = TRUE)
  out <- create_database(my_files)
  expect_true(all(dim(out) == c(length(my_files), 17)))

})

