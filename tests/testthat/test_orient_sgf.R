
test_that("orient sgf works with a single move", {

  expect_true(orient_sgf("aa") == "sa")
  expect_true(orient_sgf("sa") == "sa")
  expect_true(orient_sgf("as") == "sa")
  expect_true(orient_sgf("ss") == "sa")

  expect_true(orient_sgf("dd") == "pd")
  expect_true(orient_sgf("pd") == "pd")
  expect_true(orient_sgf("dp") == "pd")
  expect_true(orient_sgf("pp") == "pd")

  expect_true(orient_sgf("jj") == "jj")

})


test_that("orient sgf works with tengen", {

  expect_true(all(orient_sgf(c("jj", "aa")) == c("jj", "sa")))
  expect_true(all(orient_sgf(c("jj", "sa")) == c("jj", "sa")))
  expect_true(all(orient_sgf(c("jj", "as")) == c("jj", "sa")))
  expect_true(all(orient_sgf(c("jj", "ss")) == c("jj", "sa")))

  expect_true(all(orient_sgf(c("jj", "aa", "ac")) == c("jj", "sa", "sc")))
  expect_true(all(orient_sgf(c("jj", "aa", "ca")) == c("jj", "sa", "sc")))

})


test_that("orient sgf works with two moves", {

  expect_true(all(orient_sgf(c("aa", "ac")) == c("sa", "sc")))
  expect_true(all(orient_sgf(c("aa", "ca")) == c("sa", "sc")))

})

test_that("bad input handled correctly", {

  expect_warning(orient_sgf("ad", board_size = 3))
  expect_warning(orient_sgf("da", board_size = 3))
  expect_warning(orient_sgf("dd", board_size = 3))

  expect_warning(orient_sgf("tt", board_size = 19))
  expect_warning(orient_sgf("at", board_size = 19))
  expect_warning(orient_sgf("ta", board_size = 19))

})


