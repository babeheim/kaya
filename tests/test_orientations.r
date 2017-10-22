
rm(list=ls())

library(kaya)

library(testthat)

test_that("all orientations work", {


  ######################

  d <- read_sgf('./orientation_sgf/sector 1.sgf')

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
  dat
  expect_silent(dat2 <- orient_sgf(dat))
  expect_equal(dat2, dat)
})








### ghetto move plotter...what if there's already stones?
# it can't just be handi stones either! 

moves <- dat2[1:5]

goban.side <- par("pin")[1]
stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the plot_board as the fixed size of the board

x.letter <- substr(moves,1,1)
x.coord <- sapply(x.letter, function(z) which(letters==z))
y.letter <- substr(moves,2,2)
y.coord <- sapply(y.letter, function(z) 20-which(letters==z))

colors <- ifelse( d$move$color=="B" , "black", "white" )[1:5]
rev_colors <- ifelse( colors=="black", "white", "black" )

nums <- d$moves$move[1:5]

plot_board()

if(!is.null(moves)){
    if(is.na(colors)[1]) colors <- rep("gray", length(y.coord))
    points(x.coord, y.coord, cex=stone.size,pch=21,bg=colors)
    text(x.coord, y.coord, labels=nums, col=rev_colors)
}




# 1,1 to 19,19 all in quadrant 1


