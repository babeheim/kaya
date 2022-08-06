
test_that("all rotated orientations are in sector one", {

  "(;B[aa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[as])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[sa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[ss])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[dd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[dp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[pd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[pp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)


  "(;B[dj])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[jd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[pj])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[jp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[ad])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[da])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[ds])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[ap])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[pa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[sd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[pa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[sd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[sp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[ps])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[jj];W[sa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)
  in_sector_one <- d$moves$column[2] > 10 & d$moves$row[2] <= 10 & (20 - d$moves$row[2]) <= d$moves$column[2]
  expect_true(in_sector_one)

})



test_that("non-rotated orientations are where they should be", {

  "(;B[aa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[as])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[sa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[ss])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[dd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[dp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[pd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[pp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)


  "(;B[dj])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[jd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[pj])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[jp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[ad])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[da])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[ds])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[ap])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[pa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[sd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[pa])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[sd])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)

  "(;B[sp])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

  "(;B[ps])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_false(in_sector_one)

})

test_that("games only with zero-sector moves don't rotate", {
  "(;B[jj])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = TRUE) -> d
  "(;B[jj])" |> parse_sgf(to.json = FALSE) |> simplify_game(rotate = FALSE) -> d2
  expect_equal(d2$moves$coord_sgf, d$moves$coord_sgf)
})

test_that("game with passes can be reoriented", {
  expect_silent(d <- simplify_game(parse_sgf("(;B[ps];W[];B[])", to.json = FALSE), rotate = TRUE))
  in_sector_one <- d$moves$column[1] > 10 & d$moves$row[1] <= 10 & (20 - d$moves$row[1]) <= d$moves$column[1]
  expect_true(in_sector_one)
})
