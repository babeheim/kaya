
# the kgs problem...
# the best thing i can think of is to de-escape the ] and then re-escape it on the assumption they will
# now balance...


test_that("parse_tags works", {
  
  x <- "PB[bret beheim]"
  parse_tag(x)
  expect_true(length(parse_tag(x)[[1]]) == 1)

  x <- "AB[aa][bb][cc]"
  parse_tag(x)
  expect_true(length(parse_tag(x)[[1]]) == 3)

  x <- "C[bret \\[15k\\]: hello]"
  parse_tag(x)
  expect_true(length(parse_tag(x)[[1]]) == 1)

  x <- "C[bret [15k]: hello]"
  expect_false(length(parse_tag(x)[[1]]) == 1)
  y <- check_comment_escapes(x)
  parse_tag(y)
  expect_true(length(parse_tag(y)[[1]]) == 1)

  x <- "C[bret [15k\\]: hello]"
  expect_false(length(parse_tag(x)[[1]]) == 1)
  y <- check_comment_escapes(x)
  parse_tag(y)
  expect_true(length(parse_tag(y)[[1]]) == 1)

  x <- "C[bret \\[15k\\]: hello]"
  expect_true(length(parse_tag(x)[[1]]) == 1)

  x <- "PB[b\\[ret]"
  parse_tag(x)

  x <- "PB[br\\[e\\]t]"
  parse_tag(x)

  x <- c("PB[bret]", "PW[paul]")
  kaya::parse_tag(x)

  x <- c("B[]")
  parse_tag(x)

  x <- c("gar[bage")
  expect_error(parse_tag(x))

  x <- c("ga]r[bage")
  parse_tag(x)

  x <- c("[bage]")
  expect_error(kaya::parse_tag(x))

  x <- c("garbage")
  expect_error(parse_tag(x))

  x <- c("PBA[bret]")
  expect_error(parse_tag(x))

  x <- c("PB[bret]", "PW[paul]", "PB[mel]")
  expect_error(parse_tag(x))

})

test_that("parse_node works", {

  node_string <- "DT[2009-01-01]"
  tags <- parse_node(node_string)
  expect_true(length(tags) == 1)

  node_string <- "PB[bret]PW[blah]DT[2009-01-01]"
  tags <- parse_node(node_string)
  expect_true(length(tags) == 3)

  # parse_node it should fail if given an unescaped ;, since parse_branch should have removed them!

})


test_that("parse_branch works", {

  branch_string <- ";PB[bret]PW[blah]DT[2009-01-01]"
  nodes <- parse_branch(branch_string)
  expect_true(length(nodes[[1]]) == 3)

  branch_string <- ";PB[bret]PW[blah];DT[2009-01-01]"
  nodes <- parse_branch(branch_string)
  expect_true(length(nodes[[1]]) == 2)
  expect_true(length(nodes[[2]]) == 1)

  branch_string <- c(";PB[bret]", ";PW[blah]")
  expect_error(nodes <- parse_branch(branch_string))

  # parse_ranch should fail if given an unescaped ( ), since parse_sgf should have removed them

})

test_that("parse_sgf works correctly", {

  sgf_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac])"
  d <- parse_sgf(sgf_string)
  expect_true(length(d) == 1)
  expect_true(names(d) == "nodes")
  expect_true(length(d$nodes) == 4)
  expect_true(length(d$nodes[[1]] ) == 3)


  sgf_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac](;B[aa];W[ab];B[ac])(;B[aa];W[ab];B[ac]))"
  d <- parse_sgf(sgf_string)
  expect_true(length(d) == 2)
  expect_true(names(d)[2] == "branches")
  expect_true(length(d$nodes) == 4)
  expect_true(length(d$branches) == 2)
  expect_true(names(d$branches[[1]]) == "nodes")
  expect_true(names(d$branches[[2]]) == "nodes")

  # parse_sgf should fail if....?

})



# multiple games detected

parenthesis_pattern <- "\\(((?>=\\\\\\(|\\\\\\)|[^\\(\\)])|(?R))*\\)"

sgf_string <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb];B[cc];W[dd];B[ad];W[bd])(;FF[4]GM[1]SZ[19];B[aa];W[bb];B[cc];W[dd];B[ad];W[bd])"

gregexpr(parenthesis_pattern, sgf_string, perl = TRUE)


parenthesis_pattern <- "\\(((?>=\\\\\\(|\\\\\\)|[^\\(\\)])|(?R))*\\)"

sgf_string <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb];B[cc];W[dd];B[ad];W[bd])"

gregexpr(parenthesis_pattern, sgf_string, perl = TRUE)

