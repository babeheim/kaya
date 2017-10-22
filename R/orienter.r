

orient_sgf <- function(sgf_moves){

  coord_sgf <- as.character(sgf_moves)

  # how to handle passes? convert to NA

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

  coord_rows <- match(coord_row_letter, letters[1:19])
  coord_cols <- match(coord_col_letter, letters[1:19])

  coord_rows[coord_rows>19] <- NA
  coord_cols[coord_cols>19] <- NA

  coord_liberty <- rep(NA, length(coord_rows))
  for(i in 1:length(coord_rows)){
    if(!is.na(coord_rows[i])) coord_liberty[i] <- lookup$liberty[lookup$row==coord_rows[i] & lookup$col==coord_cols[i]]
  }

  coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]

  if(!all(is.na(coord_sector))){
    first_sector <- coord_sector[min(which(!is.na(coord_sector)))]
  } else {
    first_sector <- 1
  }

  liberty <- matrix(1:361, ncol=19, byrow=TRUE)
  # sector <- matrix(lookup$sector, ncol=19, byrow=TRUE)

  # apply transformation based on "first sector" value

  if(first_sector==1) standard_liberty <- liberty
  if(first_sector==2) standard_liberty <- t(liberty[,19:1])[,19:1]
  if(first_sector==3) standard_liberty <- t(liberty[19:1,])
  if(first_sector==4) standard_liberty <- liberty[,19:1]
  if(first_sector==5) standard_liberty <- t(t(liberty[,19:1])[,19:1])
  if(first_sector==6) standard_liberty <- t(liberty)
  if(first_sector==7) standard_liberty <- t(liberty)[19:1,] 
  if(first_sector==8) standard_liberty <- liberty[19:1,]

  x <- as.vector(t(liberty))
  x_trans <- as.vector(t(standard_liberty))

  coord_standard_liberty <- x[match(coord_liberty, x_trans)]  #bingo! 

  new_rows <- lookup$rows[match(coord_standard_liberty, lookup$liberty)]
  new_cols <- lookup$cols[match(coord_standard_liberty, lookup$liberty)]

  new_coord_sgf <- paste(letters[new_cols], letters[new_rows], sep="")
  # transformed game! 

  # leave old moves if the new ones failed to match
  keep <- which(new_coord_sgf=="NANA")
  if(length(keep)>0) new_coord_sgf[keep] <- coord_sgf[keep]

  return(new_coord_sgf)

}