

# split_tags

node_string <- "PB[bret]PW[paul]"
tags <- split_tags(node_string)

node_string <- "PB[bret]asdfPW[paul]"
tags <- split_tags(node_string)

node_string <- "PB[bret]PW[paul]DT[2018-04-22]"
tags <- split_tags(node_string)

test_that("you cannot pass in more than one node string",{
  node_string <- c("x", "PB[bret]PW[paul]DT[2018-04-22]")
  expect_error(tags <- split_tags(node_string))
})

node_string <- ""
tags <- split_tags(node_string)
# returns character(0) not ""


# split_nodes

node_string <- ";PB[bret];PW[blah];DT[2009-01-01]"
nodes <- split_nodes(node_string)

node_string <- ";PB[bret]PW[blah];DT[2009-01-01]"
nodes <- split_nodes(node_string)
nodes

# escaped semicolons properly ignored by splitting
node_string <- ";PB[bret\\;paul]PW[alphago]"
nodes <- split_nodes(node_string)
nodes

# non-escaped semicolons produce invalid tags
node_string <- ";PB[bret;paul]PW[alphago]"
nodes <- split_nodes(node_string)
expect_error(parse_tag(nodes[1]))

node_string <- ";PB[bret]PW[blah]"
nodes <- split_nodes(node_string)
nodes

node_string <- "PB[bret]PW[blah]"
nodes <- split_nodes(node_string)
nodes

# split_branches??


# parse tags

# output is named list of length 1, where names are object keys

x <- "PB[bret]"
parse_tag(x)

x <- c("PB[bret]", "PW[paul]")
kaya::parse_tag(x)

x <- c("B[]")
parse_tag(x)

x <- c("gar[bage")
expect_error(kaya::parse_tag(x))

x <- c("ga]r[bage")
expect_error(kaya::parse_tag(x))

x <- c("[bage]")
expect_error(kaya::parse_tag(x))

x <- c("garbage")
expect_error(parse_tag(x))

x <- c("PBA[bret]")
expect_error(parse_tag(x))

x <- c("PB[bret]", "PW[paul]", "PB[mel]")
expect_error(parse_tag(x))

# parse_node

# takes a node and returns a named list

# parse branch
# combines split_nodes, split_tags and parse_tag

# output should be an unnamed list
# each element of list is a node on the branch, a named list

node_string <- ";PB[bret]PW[blah]DT[2009-01-01]"
nodes <- parse_branch(node_string)

node_string <- ";PB[bret]PW[blah];DT[2009-01-01]"
nodes <- parse_branch(node_string)

node_string <- c(";PB[bret]", ";PW[blah]")
expect_error(nodes <- parse_branch(node_string))

# check that it's a valid node: must start with semicolon! 


# parse sgf

sgf_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac])"
d <- parse_sgf(sgf_string)

sgf_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac](;B[aa];W[ab];B[ac])(;B[aa];W[ab];B[ac]))"
d <- parse_sgf(sgf_string)

sgf_string <- "(;PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac](;B[aa];W[ab];B[ac])(;B[aa];W[ab];B[ac]))"
d <- parse_sgf(sgf_string)

sgf_string <- ";PB[bret]PW[blah]DT[2009-01-01];B[aa];W[ab];B[ac]"
expect_error(d <- parse_sgf(sgf_string))

sgf_string <- "(;GM[1]FF[4]SZ[19]PW[okao]WR[7d]PB[ss501]BR[2d]DT[2009-09-01]PC[The KGS Go Server at http://www.gokgs.com/]KM[0.50]RE[B+2.50]RU[Japanese]OT[3x10 byo-yomi]CA[UTF-8]ST[2]AP[CGoban:3]TM[0]HA[5]AB[dd][pd][jj][dp][pp];W[nq];B[oq];W[np];B[pn])"
parse_sgf(sgf_string)

# parse sgf is awkward in how it passes around the sgf_string
# and it needs to be able to handle MULTIPLE GAMES
# seperated at the top level by () ()

# also needs to be able to discard all stuff OUTSIDE
# the ( ) ( )

game_list <- read_sgf("./normal_sgf/2009-09-01-1.sgf", simplify = FALSE) 

game_list <- read_sgf("./branching_sgf/single_bifurcation.sgf", simplify = FALSE) 

game_list <- read_sgf("./redbean_sgf/redbean_one_variation.sgf", simplify = FALSE) 

game_list <- read_sgf("./unusual_sgf/one_move_node.sgf", simplify = FALSE) 


# bracket_matcher

x <- "(root(branch1)(branch2))"
bracket_matcher(x)

x <- "root(branch1)(branch2)"
bracket_matcher(x)

x <- "root(branch1(branch11)(branch12))(branch2)"
bracket_matcher(x)

x <- "(branch1)(branch2)"
bracket_matcher(x)


# strip comments

test1 <- list()
test1[[1]] <- list(B = "aa")
test1[[2]] <- list(W = "aa")
test1[[3]] <- list(B = "aa")
test1[[4]] <- list(W = "aa")
test1[[5]] <- list(B = "aa")
test1[[6]] <- list(W = "aa")

out <- strip_comments(test1)
all(unlist(lapply(out, length)) == 1)

test2 <- list()
test2[[1]] <- list(B = "aa", comments = "blah")
test2[[2]] <- list(W = "aa", Black = "dc")
test2[[3]] <- list(B = "aa", White = "cdcd")
test2[[4]] <- list(W = "aa", C = "cds")
test2[[5]] <- list(B = "aa")
test2[[6]] <- list(W = "aa")

out <- strip_comments(test2)
all(unlist(lapply(out, length)) == 1)



# check_comment_escapes

test_that("check_comment_escapes works", {

  string <- "(;PB[bret];PW[paul];C[test])"
  x <- check_comment_escapes(string)
  expect_true(identical(x, string))

  string <- c(";PB[bret];PW[paul];C[test;test2]")
  x <- check_comment_escapes(string)
  expect_true(grep("test\\\\;test2", x)==1)
  expect_true(nchar(x) == nchar(string) + 1)

  string <- "(;PB[bret];PW[paul];C[te;t2](;PB[bret];PW[paul];C[te;t2])(;PB[bret];PW[paul];C[te;t2]))"
  x <- check_comment_escapes(string)
  expect_true(grep("te\\\\;t2", x) == 1)
  expect_true(nchar(x) == nchar(string) + 3)

  string <- ";PB[bret];PW[paul];C[te(stte)st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("te\\\\\\{stte\\\\\\}st2", x)==1)
  expect_true(nchar(x) == nchar(string) + 2)

  string <- ";PB[bret];PW[paul];C[te[stte]st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("te\\\\\\[stte\\\\\\]st2", x) == 1)
  expect_true(nchar(x) == nchar(string) + 2)

  string <- ";PB[bret];PW[paul];C[)]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\}", x) == 1)
  expect_true(nchar(x) == nchar(string) + 1)

  string <- ";PB[bret];PW[paul];C[(]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\{", x) == 1)
  expect_true(nchar(x) == nchar(string) + 1)

  string <- ";PB[bret];PW[paul];C[;]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\;", x) == 1)
  expect_true(nchar(x) == nchar(string) + 1)


  string <- "(;PB[bret];PW[paul];C[te[sttest2])"
  expect_error(check_comment_escapes(string))

})




# sgf validator ???

