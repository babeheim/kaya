


# check_comment_escapes relies on this:

test_that("basic bracket parsing works", {

#  comment_pattern <- "\\[((?>[^\\[\\]]+)|(?R))*\\]"
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

# split_node

# in other words, splits do the operation of one to many strings
# parse does the operation of creating the new logical structure on individual string

# the split operation removes whatever the seperator was, if any
# split_node removes nothing, 
# split_branch removes ;
# split_sgf removes the ( )

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

# returns character(0) not ""


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


split_sgf <- function(sgf_string) {
  output <- list(nodes = character(), branches = list())
  node_pattern <- "(^.*?(?<!\\\\))(\\(|$)" # everything from start of line to first unescaped (, or end of line
  m <- gregexpr(node_pattern, sgf_string, perl = TRUE)
  output$nodes <- regmatches(sgf_string, m)[[1]]
  parenthesis_pattern <- "\\(((?>=\\\\\\(|\\\\\\)|[^\\(\\)])|(?R))*\\)"
  m <- gregexpr(parenthesis_pattern, sgf_string, perl = TRUE)
  if(m[[1]][1] != (-1)) output$branches <- as.list(regmatches(sgf_string, m)[[1]])
  return(output)
}


# i suspect I can use regmatches here to clean up this code...
parse_sgf_redo <- function(sgf_string, to.json = FALSE) {

  # string cleaning should really be here since it always needs to happen first

  if (length(sgf_string) > 1) stop("parse_sgf accepts only single strings")

  output <- split_sgf(sgf_string)

  # check for more than one game
#  if (length(x) > 1) stop("string contains more than one game! Kaya is not designed for this, so please separate these first.")

  # check that it's a valid sgf surrounded by ( )
  if (!(x[1] == 1 & attr(x, "match.length")[1] == nchar(sgf_string))) stop("sgf_string is not surrounded by parentheses; this isn't a valid SGF")
  
  # if the string is surrounded, take away outer parentheses and re-split
  # if (length(x) == 1 & x[1] == 12) {
  #   sgf_string <- substr(sgf_string, 2, nchar(sgf_string) - 1)
  #   x <- bracket_matcher_redo(sgf_string)
  # }

  # if the string is actually just one branch, parse it
  output$nodes <- parse_branch(output$nodes)
  # if the string has sub-branches, identify their locations and execute parse_sgf on each one recursively
  if (x[1] != (-1)) {
    for(i in 1:(length(output$branches))){
      output$branches[[i]] <- parse_sgf(output$branches[[i]]) # wow!
    }
  }

  if (to.json) output <- jsonlite::toJSON(output, pretty = TRUE)
  return(output)
}



test_that("split_sgf produces the correct number of output strings", {

  sgf_string <- ";PB[bret];PW[blah];DT[2009-01-01]"
  game <- split_sgf(sgf_string)
  expect_true(length(game) == 2)
  expect_true(names(game)[1] == "nodes")
  expect_true(game$nodes[1] == ";FF[4]GM[1]SZ[19];B[aa];W[bb](")
  expect_true(length(game$branches) == 2)

  # first, detect everything before the first unescaped (
  # to extract branches...
  sgf_string <- ";FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc];W[dd];B[ad];W[bd])(;B[hh];W[hg])"
  game <- split_sgf(sgf_string)
  expect_true(length(game) == 2)
  expect_true(names(game)[1] == "nodes")
  expect_true(game$nodes[1] == ";FF[4]GM[1]SZ[19];B[aa];W[bb](")
  expect_true(length(game$branches) == 2)

})




# parse tags

# parse tags expects individua tag strings,
# e.g. "PB[bret]" and returns a named list
# it will also work for setup stone tags, e.g.
# "AB[aa][bb]"

# so, it's looking for square brackets to break up

# it will ignore escaped square brackets, but 
# without escaping it will
# will improperly break up nested square brackets

# it is smart enough to ignore escaped 

# output is named list of length 1, where names are object keys


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
  split_tag(x)

  x <- c("B[]")
  split_tag(x)

  x <- c("gar[bage")
  expect_error(split_tag(x))

  x <- c("ga]r[bage")
  split_tag(x)

  x <- c("[bage]")
  expect_error(split_tag(x))

  x <- c("garbage")
  expect_error(split_tag(x))

  x <- c("PBA[bret]")
  expect_error(split_tag(x))

  x <- c("PB[bret]", "PW[paul]", "PB[mel]")
  expect_error(split_tag(x))

})


test_that("parse tags works", {
  
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

# parse_node

# takes a node and returns a named list

# parse branch
# combines split_branch, split_node and parse_tag

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

game_list <- read_sgf("./real_sgf/2009-09-01-1.sgf", simplify = FALSE) 

game_list <- read_sgf("./branching_sgf/single_bifurcation.sgf", simplify = FALSE) 

game_list <- read_sgf("./redbean_sgf/redbean_one_variation.sgf", simplify = FALSE) 

game_list <- read_sgf("./unusual_sgf/one_move_node.sgf", simplify = FALSE) 


# bracket_matcher


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



test_that("bracket matching is good", {

  x <- "(root(branch1)(branch2))"
  joints <- bracket_matcher(x)
  y <- regmatches(x, joints)
  expect_true(x == y)

  x <- "root(branch1)(branch2)"
  joints <- bracket_matcher(x)
  y <- regmatches(x, joints)[[1]]

  x <- "root(branch1(branch11)(branch12))(branch2)"
  joints <- bracket_matcher(x)
  y <- regmatches(x, joints)

  x <- "(branch1)(branch2)"
  joints <- bracket_matcher(x)
  y <- regmatches(x, joints)

})


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

# without solving the iron giant problem
# we cannot solve the kgs problem...

# check_comment_escapes

test_that("check_comment_escapes works", {

  # normal sgf
  string <- "(;PB[bret];PW[paul];C[test])"
  x <- check_comment_escapes(string)
  expect_true(identical(x, string))

  # semicolon in comment
  string <- c(";PB[bret];PW[paul];C[test;test2]")
  x <- check_comment_escapes(string)
  expect_true(grep("test\\\\;test2", x)==1)
  expect_true(nchar(x) == nchar(string) + 1)

  # semicolon in comment variant
  string <- "(;PB[bret];PW[paul];C[te;t2](;PB[bret];PW[paul];C[te;t2])(;PB[bret];PW[paul];C[te;t2]))"
  x <- check_comment_escapes(string)
  expect_true(grep("te\\\\;t2", x) == 1)
  expect_true(nchar(x) == nchar(string) + 3)

  # pair of parentheses in comment
  string <- ";PB[bret];PW[paul];C[te(stte)st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("te\\\\\\{stte\\\\\\}st2", x)==1)
  expect_true(nchar(x) == nchar(string) + 2)

  # pair of unescaped square brackets in comment
  string <- ";PB[bret];PW[paul];C[te[stte]st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("te\\\\\\[stte\\\\\\]st2", x) == 1)
  expect_true(nchar(x) == nchar(string) + 2)

  # # pair of kgs-style square brackets in comment
  # string <- ";PB[bret];PW[paul];C[te[stte\\]st2]"
  # x <- check_comment_escapes(string)
  # expect_true(grep("te\\\\\\[stte\\\\\\]st2", x) == 1)
  # expect_true(nchar(x) == nchar(string) + 1)

  # one escaped left square bracket in comment
  string <- ";PB[bret];PW[paul];C[testte\\]st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("testte\\\\\\]st2", x) == 1)
  expect_true(nchar(x) == nchar(string))

  # one escaped right square bracket in comment
  string <- ";PB[bret];PW[paul];C[testte\\[st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("testte\\\\\\[st2", x) == 1)
  expect_true(nchar(x) == nchar(string))

  # escaped square brackets nonpair in comment
  string <- ";PB[bret];PW[paul];C[tes\\]tte\\[st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\\\\\[", x) == 1)
  expect_true(grep("\\\\\\]", x) == 1)
  expect_true(nchar(x) == nchar(string))

  # escaped square bracket pair in comment
  string <- ";PB[bret];PW[paul];C[tes\\[tte\\]st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\\\\\[", x) == 1)
  expect_true(grep("\\\\\\]", x) == 1)
  expect_true(nchar(x) == nchar(string))

  # extreme cases
  string <- ";PB[bret];PW[paul];C[)]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\}", x) == 1)
  expect_true(nchar(x) == nchar(string) + 1)

  # extreme cases
  string <- ";PB[bret];PW[paul];C[(]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\{", x) == 1)
  expect_true(nchar(x) == nchar(string) + 1)

  # extreme cases
  string <- ";PB[bret];PW[paul];C[;]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\;", x) == 1)
  expect_true(nchar(x) == nchar(string) + 1)

  # extreme cases
  string <- ";PB[bret];PW[paul];C[\\]]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\]", x) == 1)
  expect_true(nchar(x) == nchar(string))

  # extreme cases
  string <- ";PB[bret];PW[paul];C[\\[]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\[", x) == 1)
  expect_true(nchar(x) == nchar(string))

  string <- "(;FF[4]GM[1]SZ[19]PB[Bret];B[aa]C[iron giant face :-]]"
  expect_error(check_comment_escapes(string))
  # iron giant face is a persistent problem
  # the best I think I can do with regex is 
  # identify error and manually fix

  string <- "(;FF[4]GM[1]SZ[19]PB[Bret];B[aa]C[sad iron giant face :-[]"
  expect_error(check_comment_escapes(string))


})

