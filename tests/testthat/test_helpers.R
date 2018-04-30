

# group_parentheses

test_that("group_parentheses works", {

  # the current version cannot distinguish

  x <- "(root)"
  y <- group_parentheses(x)
  expect_true(length(y) == 1)

  x <- "(root)---"
  y <- group_parentheses(x)
  expect_true(length(y) == 1)

  x <- "(root(branch1)(branch2))"
  y <- group_parentheses(x)
  expect_true(length(y) == 1)

  x <- "root(branch1)(branch2)"
  y <- group_parentheses(x) 
  expect_true(length(y) == 2)

  x <- "root(branch1(branch11)(branch12))(branch2)"
  y <- group_parentheses(x)
  expect_true(length(y) == 2)

  x <- "test(this)"
  y <- group_parentheses(x)
  expect_true(length(y) == 1)

  x <- "test(this (and) this)"
  y <- group_parentheses(x)
  expect_true(nchar(y) == 17)

  # all parentheses MUST be escaped or paired
  x <- "test(this \\(and this)"
  y <- group_parentheses(x)
  expect_true(nchar(y) == 17)

  # this is also failing...
  x <- "test(this \\(and\\) this)"
  y <- group_parentheses(x)
  expect_true(nchar(y) == 19)

  x <- "root(branch1(branch11)(branch12))(branch2)---"
  y <- group_parentheses(x)
  expect_true(length(y) == 2)

  x <- "(branch1)(branch2)"
  y <- group_parentheses(x)
  expect_true(length(y) == 2)

})


# purge comments!

test_that("purge_comments removes comments only", {

  test <- "(;PB[bret]PW[paul];B[dd];W[qq]C[I resign])"
  x <- purge_comments(test)
  expect_true(x == "(;PB[bret]PW[paul];B[dd];W[qq])")

  test <- "(;PB[bret]PW[paul];B[dd];W[qq]C[the comment tag is C\\[.])"
  x <- purge_comments(test)
  expect_true(x == "(;PB[bret]PW[paul];B[dd];W[qq])")

  test <- "(;PB[bret]PW[paul];B[dd];W[qq]PC[the comment tag is C\\[.])"
  x <- purge_comments(test)
  expect_true(x == test)

})

# strip comments

test_that("strip_comments removes comments", {

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

})

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
  expect_true(grep("te\\\\\\(stte\\\\\\)st2", x)==1)
  expect_true(nchar(x) == nchar(string) + 2)

  # unescaped right brackets in comment is illegal!
  string <- ";PB[bret];PW[paul];C[te[stte]st2]"
  x <- check_comment_escapes(string)
  # this should be illegal!! but not a this stage...

  # pair of kgs-style square brackets in comment
  string <- ";PB[bret];PW[paul];C[te[stte\\]st2]"
  x <- check_comment_escapes(string)
  expect_true(grep("te\\[stte\\\\\\]st2", x) == 1)
  expect_true(nchar(x) == nchar(string))

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
  expect_true(grep("\\)", x) == 1)
  expect_true(nchar(x) == nchar(string) + 1)

  # extreme cases
  string <- ";PB[bret];PW[paul];C[(]"
  x <- check_comment_escapes(string)
  expect_true(grep("\\(", x) == 1)
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

  string <- "(;FF[4]GM[1]SZ[19]PB[Bret];B[aa]C[iron giant face :-\\]]"
  check_comment_escapes(string)
  
  string <- "(;FF[4]GM[1]SZ[19]PB[Bret];B[aa]C[sad iron giant face :-[]"
  check_comment_escapes(string)


})

