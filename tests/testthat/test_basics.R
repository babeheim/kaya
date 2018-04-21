

test_that("one file parses", {
  my_files <- "./normal_sgf/2009-09-01-1.sgf"
  sgf_lines <- paste0(readLines(my_files), collapse = "")
  x <- parse_sgf(sgf_lines)
  expect_true(!is.null(names(x)))
})


test_that("loads lines with no errors", {
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names = TRUE)
  for(i in 1:length(my_files)){
    sgf_lines <- paste0(readLines(my_files[i]), collapse = "")
    expect_silent(x <- parse_sgf(sgf_lines))
  }
})

test_that("one file reads", {
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names = TRUE)
  x <- read_sgf(my_files[1], rotate = FALSE)
  expect_true(length(x)==22)
  expect_true(!is.null(names(x)))
})

test_that("reads several files with no errors", {
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names=TRUE)
  for(i in 1:length(my_files)){
    expect_silent(x <- read_sgf(my_files[i]))
  }
})


test_that("all games pass audit",{
  my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names=TRUE)
  d_all <- lapply_pb( my_files, read_sgf )
  valid <- unlist(lapply(d_all, validate_game))
  expect_true(all(valid))
})


# im not sure these are supposed to be here...

# library(yamltools)
# my_files <- list.files("./normal_sgf", pattern="*.sgf", full.names=TRUE)
# d_all <- lapply_pb( my_files, read_sgf )
# d_chunk <- d_all
# names(d_chunk) <- NULL
# dj <- toJSON(d_chunk)
# chunk_json <- "test.json"
# writeLines(dj, chunk_json)
# d_full <- read_json(chunk_json, simplifyVector=TRUE)
# file.remove("test.json")
# d <- d_full
# # d <- vectorize(d) # vectorize fails!
# d <- d_full[,!colnames(d_full) %in% c("moves", "AB", "AW")]
# d <- vectorize(d)

# test_that("all games being saved correctly", {
#   expect_true( nrow(d) == length(d_all) )
# })

# moves <- d_full$moves
# for(i in 1:length(moves)) moves[[i]]$hash_id <- d_full$hash_id[i] 
# moves <- rbind_list(moves)

# test_that("moves are being saved correctly", {
#   expect_true( nrow(moves) == sum(d$n_moves) )
# })


