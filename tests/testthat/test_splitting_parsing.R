

# split_tag

test_that("split tag works", {
  
  x <- "PB[bret beheim]"
  expect_true(length(split_tag(x)) == 2)

  x <- "AB[aa][bb][cc]"
  expect_true(length(split_tag(x)) == 4)

  x <- "C[bret \\[15k\\]: hello]"
  expect_true(length(split_tag(x)) == 2)

  x <- "C[iron giant face :-\\] here]"
  split_tag(x)

  x <- "C[iron giant face again :-\\]]"
  split_tag(x)

  x <- "C[ bret[ 15k \\]: hi]"
  split_tag(x)

  x <- "C[bret \\[15k\\]: hello]"
  expect_true(length(split_tag(x)) == 2)

  x <- "PB[b\\[ret]"
  expect_true(length(split_tag(x)) == 2)

  x <- "PB[b[ret]"
  expect_true(length(split_tag(x)) == 2)

  x <- c("B[]")
  y <- split_tag(x)
  expect_true(y[2] == "")

  x <- c("PBA[bret]")
  expect_true(length(split_tag(x)) == 2)

  # cannot split more than one tag at a time
  x <- c("PB[bret]PW[paul]")
  expect_false(length(split_tag(x)) == 4)

  # unescaped right brackets fail
  x <- "C[bret [15k]: hello]"
  split_tag(x)
  expect_true(split_tag(x)[2] == "bret [15k")

  # cannot accept more than one input
  x <- c("PB[bret]", "PW[paul]")
  expect_error(split_tag(x))

  # no tag fails
  x <- c("[bage]")
  expect_error(split_tag(x))

})

# parse_tag is a wrapper for split_tag
# which combines multiple split_tags into a named list
# inputs one or more tags

test_that("parse_tags uses downstream operations to make named lists", {
  
  x <- "PB[bret]"
  named_list <- parse_tag(x)
  expect_true(length(named_list$PB) == 1)

  x <- c("B[]")
  named_list <- parse_tag(x)
  expect_true(named_list$B == "")

  x <- c("AB[aa][bb][cc]")
  named_list <- parse_tag(x)
  expect_true(length(named_list$AB) == 3)

  # incorrectly parses two tags in one string
  x <- c("PB[bret]PW[paul]")
  named_list <- parse_tag(x)
  expect_true(length(named_list$PB) == 2)

  # cannot have same tag twice
  x <- c("PB[bret]", "PW[paul]", "PB[mel]")
  expect_error(parse_tag(x))

})


test_that("split_node produces the correct number of output strings", {

  node_string <- "PB[bret]PW[paul]"
  tags <- split_node(node_string)
  expect_true(length(tags) == 2)
  expect_true(tags[1] == "PB[bret]")

  node_string <- "PB[bret]PW[paul]AB[aa][bb][cc]"
  tags <- split_node(node_string)
  expect_true(length(tags) == 3)
  expect_true(tags[3] == "AB[aa][bb][cc]")

  node_string <- "PB[bret]asdfPW[paul]"
  tags <- split_node(node_string)
  expect_true(length(tags) == 2)
  expect_true(tags[2] == "asdfPW[paul]")

  node_string <- "PB[bret]PW[paul]DT[2018-04-22]"
  tags <- split_node(node_string)
  expect_true(length(tags) == 3)
  expect_true(tags[3] == "DT[2018-04-22]")

  node_string <- ""
  tags <- split_node(node_string)
  expect_true(length(tags) == 0)

  # you cannot pass in more than one node string"
  node_string <- c("x", "PB[bret]PW[paul]DT[2018-04-22]")
  expect_error(tags <- split_node(node_string))

})


test_that("parse_node uses downstream operations to make named lists", {

  node_string <- "DT[2009-01-01]"
  named_list <- parse_node(node_string)
  expect_true(length(named_list) == 1)

  node_string <- "PB[bret]PW[blah]DT[2009-01-01]"
  named_list <- parse_node(node_string)
  expect_true(length(named_list) == 3)

  # parse_node it should fail if given an unescaped ;, since parse_branch should have removed them!

})


# split_branch

test_that("split_branch produces the correct number of output strings", {

  branch_string <- ";PB[bret];PW[blah];DT[2009-01-01]"
  nodes <- split_branch(branch_string)
  expect_true(length(nodes) == 3)
  expect_true(nodes[1] == "PB[bret]")

  branch_string <- ";PB[bret]PW[blah];DT[2009-01-01]"
  nodes <- split_branch(branch_string)
  expect_true(length(nodes) == 2)
  expect_true(nodes[1] == "PB[bret]PW[blah]")

  # escaped semicolons properly ignored by splitting
  branch_string <- ";PB[bret\\;paul]PW[alphago]"
  nodes <- split_branch(branch_string)
  expect_true(length(nodes) == 1)
  expect_true(nchar(nodes[1]) == nchar(branch_string) - 1)

  branch_string <- ";PB[bret]PW[blah]"
  nodes <- split_branch(branch_string)
  expect_true(length(nodes) == 1)
  expect_true(nchar(nodes[1]) == nchar(branch_string) - 1)

  branch_string <- "PB[bret]PW[blah]"
  nodes <- split_branch(branch_string)
  expect_true(length(nodes) == 1)
  expect_true(nchar(nodes[1]) == nchar(branch_string))

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

  # parse_branch should fail if given an unescaped ( ), since parse_tree should have removed them

})


test_that("split_tree produces the correct number of output strings", {

  sgf_string <- ";PB[bret];PW[blah];DT[2009-01-01]"
  game <- split_tree(sgf_string)
  expect_true(length(game) == 1)
  expect_true(names(game)[1] == "nodes")
  expect_true(game$nodes[1] == ";PB[bret];PW[blah];DT[2009-01-01]")
  expect_true(length(game$branches) == 0)

  sgf_string <- ";FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc];W[dd];B[ad];W[bd])(;B[hh];W[hg])"
  game <- split_tree(sgf_string)
  expect_true(length(game) == 2)
  expect_true(names(game)[1] == "nodes")
  expect_true(game$nodes[1] == ";FF[4]GM[1]SZ[19];B[aa];W[bb]")
  expect_true(length(game$branches) == 2)

})


test_that("parse_tree works correctly", {

  tree_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac])"
  d <- parse_tree(tree_string)
  expect_true(length(d) == 1)
  expect_true(names(d) == "nodes")
  expect_true(length(d$nodes) == 4)
  expect_true(length(d$nodes[[1]] ) == 3)

  tree_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac](;B[aa];W[ab];B[ac])(;B[aa];W[ab];B[ac]))"
  d <- parse_tree(tree_string)
  expect_true(length(d) == 2)
  expect_true(names(d)[2] == "branches")
  expect_true(length(d$nodes) == 4)
  expect_true(length(d$branches) == 2)
  expect_true(names(d$branches[[1]]) == "nodes")
  expect_true(names(d$branches[[2]]) == "nodes")

  # characters outside the gametree are fine
  tree_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac])---"
  d <- parse_tree(tree_string)
  expect_true(length(d) == 1)
  expect_true(names(d) == "nodes")
  expect_true(length(d$nodes) == 4)
  expect_true(length(d$nodes[[1]] ) == 3)

})

