
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

  string <- "(;FF[4]GM[1]SZ[19]PB[Bret];B[aa]C[iron giant face :-]]"
  expect_error(check_comment_escapes(string))
  # iron giant face is a persistent problem
  # the best I think I can do with regex is 
  # identify error and manually fix

  string <- "(;FF[4]GM[1]SZ[19]PB[Bret];B[aa]C[sad iron giant face :-[]"
  expect_error(check_comment_escapes(string))


})

