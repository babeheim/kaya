
rm(list=ls())

library(kaya)

library(testthat)

test_that("all orientations work", {


  ######################

  d <- read_sgf('./orientation_sgf/sector 1.sgf')

  dat <- d$moves$coord_sgf
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################




  ######################

  d <- read_sgf('./orientation_sgf/sector 2.sgf')

  dat <- d$moves$coord_sgf
  dat
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################



  ######################

  d <- read_sgf('./orientation_sgf/sector 3.sgf')

  dat <- d$moves$coord_sgf
  dat
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################




  ######################

  d <- read_sgf('./orientation_sgf/sector 4.sgf')

  dat <- d$moves$coord_sgf
  dat
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################




  ######################

  d <- read_sgf('./orientation_sgf/sector 5.sgf')

  dat <- d$moves$coord_sgf
  dat
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################



  ######################

  d <- read_sgf('./orientation_sgf/sector 6.sgf')

  dat <- d$moves$coord_sgf
  dat
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################



  ######################

  d <- read_sgf('./orientation_sgf/sector 7.sgf')

  dat <- d$moves$coord_sgf
  dat
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################



  ######################

  d <- read_sgf('./orientation_sgf/sector 8.sgf')

  dat <- d$moves$coord_sgf
  dat
  dat2 <- orient_sgf(dat)

  coord_sgf <- dat2

  # seperate this into its own function
  rows <- rep(1:19, each=19)
  cols <- rep(1:19, times=19)
  sector <- rep(NA, length=361)
  tar <- which(cols > 10 & rows < 10 & (20 - rows) < cols)
  sector[tar] <- 1
  tar <- which(cols > 10 & rows < 10 & (20 - rows) > cols)
  sector[tar] <- 2
  tar <- which(cols < 10 & rows < 10 & rows < cols)
  sector[tar] <- 3
  tar <- which(cols < 10 & rows < 10 & rows > cols)
  sector[tar] <- 4
  tar <- which(cols < 10 & rows > 10 & rows < (20 - cols))
  sector[tar] <- 5
  tar <- which(cols < 10 & rows > 10 & rows > (20 - cols))
  sector[tar] <- 6
  tar <- which(cols > 10 & rows > 10 & rows > cols)
  sector[tar] <- 7
  tar <- which(cols > 10 & rows > 10 & rows < cols)
  sector[tar] <- 8
  liberty <- 1:361
  lookup <- data.frame(rows, cols, liberty, sector)

  coord_row_letter <- substr(coord_sgf, 2, 2)
  coord_col_letter <- substr(coord_sgf, 1, 1)

  coord_rows <- match(coord_row_letter, letters)
  coord_cols <- match(coord_col_letter, letters)

  coord_liberty <- NA
  for(i in 1:length(coord_rows)) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  first_sector <- coord_sector[min(which(!is.na(coord_sector)))]

  expect_true(first_sector==1)

  ######################

})



test_that("games with no sector moves don't crash", {
  d <- read_sgf('./orientation_sgf/no sector.sgf')
  dat <- as.character(d$moves$coord_sgf)
  expect_silent(dat2 <- orient_sgf(dat))
  dat2 <- orient_sgf(dat)
  expect_equal(dat2, dat)
})







# 1,1 to 19,19 all in quadrant 1


