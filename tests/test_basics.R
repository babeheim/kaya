
rm(list=ls())

# library(devtools)
# devtools::load_all()

library(testthat)

my_files <- "./normal_sgf/2009-09-01-1.sgf"

sgf_lines <- readLines(my_files[1])
x <- parse_sgf(sgf_lines)

test_that("one file parses", {
  expect_true(length(x)==22)
  expect_true(!is.null(names(x)))
})

## 

my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names=TRUE)

test_that("loads lines with no errors", {
  for(i in 1:length(my_files)){
    sgf_lines <- readLines(my_files[i])
    expect_silent(x <- parse_sgf(sgf_lines))
  }
})

x <- read_sgf(my_files[1])

test_that("one file reads", {
  expect_true(length(x)==23)
  expect_true(!is.null(names(x)))
})



my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names=TRUE)

test_that("reads several files with no errors", {
  for(i in 1:length(my_files)){
    expect_silent(x <- read_sgf(my_files[i]))
  }
})


d_all <- lapply_pb( my_files, read_sgf )

test_that("all games pass audit",{

  valid <- unlist(lapply(d_all, validate_game))
  expect_true(all(valid))

})


library(yamltools)

d_chunk <- d_all

names(d_chunk) <- NULL

dj <- toJSON(d_chunk)
chunk_json <- "test.json"
writeLines(dj, chunk_json)

d_full <- read_json(chunk_json, simplifyVector=TRUE)

file.remove("test.json")

d <- d_full

# d <- vectorize(d) # vectorize fails!

d <- d_full[,!colnames(d_full) %in% c("moves", "AB", "AW")]

d <- vectorize(d)

test_that("all games being saved correctly", {
  expect_true( nrow(d) == length(d_all) )
})

moves <- d_full$moves
for(i in 1:length(moves)) moves[[i]]$hash_id <- d_full$hash_id[i] 
moves <- rbind_list(moves)

test_that("moves are being saved correctly", {
  expect_true( nrow(moves) == sum(d$n_moves) )
})


