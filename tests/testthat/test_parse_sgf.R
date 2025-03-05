
test_that("parse_sgf works on most strings", {

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]C[here's ( johnny )];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x, to.json = FALSE))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]C[here's ( johnny )];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb];B[cc];W[dd];B[ad];W[bd])"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]N[Var A];W[dd];B[ad];W[bd])(;B[hh]N[Var B];W[hg])(;B[gg]N[Var C];W[gh];B[hh];W[hg];B[kk]))"
  expect_silent(kaya:::parse_sgf(x))

  # officially only "]", "\" and ":" have to be escaped in SGFs specs
  # which means other structural markup, ")" and ";" can mess everything up

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]C[here's\\:johnny];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]C[here's\\]johnny];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]C[here's\\johnny];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]C[here's;johnny];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]C[here's)johnny];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]N[Var A];W[dd];B[ad];W[bd])(;B[hh]N[Var B];W[hg])(;B[gg]N[Var C];W[gh];B[hh]  (;W[hg]N[Var A];B[kk])  (;W[kl]N[Var B])))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]N[Var A];W[dd];B[ad];W[bd])(;B[hh]N[Var B];W[hg])(;B[gg]N[Var C];W[gh];B[hh](;W[hg]N[Var A];B[kk])(;W[kl]N[Var B])))"
  expect_silent(kaya:::parse_sgf(x))

  x <- "(;FF[4]GM[1]SZ[19];B[aa];W[bb];B[cc];W[dd];B[ad];W[bd])(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc];W[dd];B[ad];W[bd])(;B[hh];W[hg]))"
  expect_silent(kaya:::parse_sgf(x))

})
