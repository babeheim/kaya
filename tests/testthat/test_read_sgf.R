

# ok, NOW THAT ITS IN JSON HOW CAN I USE IT??

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

test_that("reads several files with no errors", {
  my_files <- list.files("./real_sgf", pattern="*.sgf", full.names=TRUE)
  for(i in 1:length(my_files)){
    expect_silent(x <- read_sgf(my_files[i], to.json = FALSE))
  }
})



# each file is an unnamed list
# the first entry is a list of all the metadata
# remaining entries are the moves



# x <- scrub_whitespace(x)
# x <- escape_characters(x)
# x <- node_to_object(x)
# x <- branch_to_array(x)
# x <- check_multigame(x) # this is where the segfault error is!!
