

test_that("nesting parentheses pattern works right", {

  nesting_brackets <- "(?<!\\\\)\\((?>[^()]|(?R))*\\)(?!\\\\)"

  x <- "(root(branch1)(branch2))"
  joints <- gregexpr(nesting_brackets, x, perl = TRUE)
  y <- regmatches(x, joints)[[1]]
  expect_true(x == y)

  x <- "root(branch1)(branch2)"
  joints <- gregexpr(nesting_brackets, x, perl = TRUE)
  y <- regmatches(x, joints)[[1]]
  expect_true(length(y) == 2)

  x <- "root(branch1(branch11)(branch12))(branch2)"
  joints <- gregexpr(nesting_brackets, x, perl = TRUE)
  y <- regmatches(x, joints)[[1]]
  expect_true(length(y) == 2)

  x <- "(branch1)(branch2)"
  joints <- gregexpr(nesting_brackets, x, perl = TRUE)
  y <- regmatches(x, joints)[[1]]
  expect_true(length(y) == 2)

})


test_that("basic bracket parsing works", {

  comment_pattern <- "\\[((?>\\\\\\[|\\\\\\]|[^\\[\\]])|(?R))*\\]"

  string <- "PB[bret]PW[paul]"
  check <- gregexpr(comment_pattern, string, perl = TRUE)
  y <- regmatches(string, check)[[1]]
  expect_true(length(y) == 2)

  string <- "PB[bret]PW[paul]DT[2018-04-22]"
  check <- gregexpr(comment_pattern, string, perl = TRUE)
  y <- regmatches(string, check)[[1]]
  expect_true(length(y) == 3)

  string <- "PB[bret]PW[paul]DT[2018-04-22];B[aa];W[ba]"
  check <- gregexpr(comment_pattern, string, perl = TRUE)
  y <- regmatches(string, check)[[1]]
  expect_true(length(y) == 5)

  string <- "PB[br[et]asdf]PW[paul]"
  check <- gregexpr(comment_pattern, string, perl = TRUE)
  y <- regmatches(string, check)[[1]]
  expect_true(length(y) == 2)
  expect_true(y[1] == "[br[et]asdf]")

  # ignore escaped
  string <- "PB[bret\\]asdf]PW[paul]"
  check <- gregexpr(comment_pattern, string, perl = TRUE)
  y <- regmatches(string, check)[[1]]
  expect_true(length(y) == 2)
  expect_true(y[1] == "[bret\\]asdf]")

  # ignore escaped
  string <- "PB[bret\\]asdf]PW[paul]"
  check <- gregexpr(comment_pattern, string, perl = TRUE)
  y <- regmatches(string, check)[[1]]
  expect_true(length(y) == 2)
  expect_true(y[1] == "[bret\\]asdf]")

  # this is the remaining problem, I dunno how you can parse this
  # correctly...
  # string <- "PB[br[et\\]asdf]PW[paul]"
  # check <- gregexpr(comment_pattern, string, perl = TRUE)
  # y <- regmatches(string, check)[[1]]
  # expect_true(y[1] == "[br[et\\]asdf]")


})



# split_tag

test_that("split tag works", {
  
  x <- "PB[bret beheim]"
  expect_true(length(split_tag(x)) == 2)

  x <- "AB[aa][bb][cc]"
  expect_true(length(split_tag(x)) == 4)

  x <- "C[bret \\[15k\\]: hello]"
  expect_true(length(split_tag(x)) == 2)

  x <- "C[bret [15k]: hello]"
  expect_false(length(split_tag(x)) == 2)
  y <- check_comment_escapes(x)
  expect_true(length(split_tag(y)) == 2)

  x <- "C[bret \\[15k\\]: hello]"
  expect_true(length(split_tag(x)) == 2)

  x <- "PB[b\\[ret]"
  expect_true(length(split_tag(x)) == 2)

  x <- "PB[br\\[e\\]t]"
  expect_true(length(split_tag(x)) == 2)

  x <- c("PB[bret]", "PW[paul]")
  expect_error(split_tag(x))

  x <- c("B[]")
  y <- split_tag(x)
  expect_true(is.na(y[2]))

  x <- c("gar[bage")
  expect_true(length(split_tag(x)) == 2)

  x <- c("ga]r[bage")
  expect_true(length(split_tag(x)) == 3)

  x <- c("[bage]")
  expect_error(split_tag(x))

  x <- c("garbage")
  expect_true(length(split_tag(x)) == 1)

  x <- c("PBA[bret]")
  expect_true(length(split_tag(x)) == 2)

  x <- c("PB[bret]", "PW[paul]", "PB[mel]")
  expect_error(split_tag(x))

})


test_that("split_node produces the correct number of output strings", {

  node_string <- "PB[bret]PW[paul]"
  tags <- split_node(node_string)
  expect_true(length(tags) == 2)
  expect_true(tags[1] == "PB[bret]")

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

})

test_that("you cannot pass in more than one node string",{
  node_string <- c("x", "PB[bret]PW[paul]DT[2018-04-22]")
  expect_error(tags <- split_node(node_string))
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

test_that("split_sgf produces the correct number of output strings", {

  sgf_string <- ";PB[bret];PW[blah];DT[2009-01-01]"
  game <- split_sgf(sgf_string)
  expect_true(length(game) == 1)
  expect_true(names(game)[1] == "nodes")
  expect_true(game$nodes[1] == ";PB[bret];PW[blah];DT[2009-01-01]")
  expect_true(length(game$branches) == 0)

  sgf_string <- ";FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc];W[dd];B[ad];W[bd])(;B[hh];W[hg])"
  game <- split_sgf(sgf_string)
  expect_true(length(game) == 2)
  expect_true(names(game)[1] == "nodes")
  expect_true(game$nodes[1] == ";FF[4]GM[1]SZ[19];B[aa];W[bb]")
  expect_true(length(game$branches) == 2)

})

