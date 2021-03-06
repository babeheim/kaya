
# the simplify_game function inclues rotation properties
# and by default, this is turned on for read_sgf

test_that("all orientations work", {

  d <- read_sgf("./orientation_sgf/sector 1.sgf")
  first_sector <- d$moves$column[1] > 10 & d$moves$row[1] < 10 & (20 - d$moves$row[1]) < d$moves$column[1]
  expect_true(first_sector)
  
  d <- read_sgf("./orientation_sgf/sector 2.sgf")
  first_sector <- d$moves$column[1] > 10 & d$moves$row[1] < 10 & (20 - d$moves$row[1]) < d$moves$column[1]
  expect_true(first_sector)
  
  d <- read_sgf("./orientation_sgf/sector 3.sgf")
  first_sector <- d$moves$column[3] > 10 & d$moves$row[3] < 10 & (20 - d$moves$row[3]) < d$moves$column[3]
  expect_true(first_sector)
  
  d <- read_sgf("./orientation_sgf/sector 4.sgf")
  first_sector <- d$moves$column[1] > 10 & d$moves$row[1] < 10 & (20 - d$moves$row[1]) < d$moves$column[1]
  expect_true(first_sector)
  
  d <- read_sgf("./orientation_sgf/sector 5.sgf")
  first_sector <- d$moves$column[1] > 10 & d$moves$row[1] < 10 & (20 - d$moves$row[1]) < d$moves$column[1]
  expect_true(first_sector)
  
  d <- read_sgf("./orientation_sgf/sector 6.sgf")
  first_sector <- d$moves$column[1] > 10 & d$moves$row[1] < 10 & (20 - d$moves$row[1]) < d$moves$column[1]
  expect_true(first_sector)
  
  d <- read_sgf("./orientation_sgf/sector 7.sgf")
  first_sector <- d$moves$column[1] > 10 & d$moves$row[1] < 10 & (20 - d$moves$row[1]) < d$moves$column[1]
  expect_true(first_sector)

  d <- read_sgf("./orientation_sgf/sector 8.sgf")
  first_sector <- d$moves$column[1] > 10 & d$moves$row[1] < 10 & (20 - d$moves$row[1]) < d$moves$column[1]
  expect_true(first_sector)

})

test_that("games with zero-sector moves don't rotate", {
  d <- read_sgf("./orientation_sgf/no sector.sgf")
  d2 <- read_sgf("./orientation_sgf/no sector.sgf", rotate=FALSE)
  expect_equal(d2$moves, d$moves)
})

test_that("game with passes can be reoriented", {
  sgf_lines <- paste0(readLines("./real_sgf/2009-09-01-10.sgf"), collapse = "")
  expect_silent(parse_sgf(sgf_lines))
})

