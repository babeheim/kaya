reflect_45 <- function(x) t(x)

reflect_y <- function(x) x[,ncol(x):1]

reflect_x <- function(x) x[nrow(x):1,]

reflect_315 <- function(x) {
  x |> reflect_y() |> reflect_x() |> reflect_45()
}

rotate_n90 <- function(x) {
  x |> reflect_315() |> reflect_y()
}

rotate_90 <- function(x) {
  x |> reflect_x() |> reflect_45()
}

orient_sgf <- function (sgf_moves) {
    if (!all(nchar(sgf_moves) %in% c(0, 2))) {
        stop("input is not a vector of sgf coordinates")
    }
    coord_sgf <- as.character(sgf_moves)
    
    
    # define a sector lookup table
    rows <- rep(1:19, each = 19)
    cols <- rep(1:19, times = 19)
    sector <- rep(NA, length = 361)

    tar <- which(cols == 10 & rows == 10)
    sector[tar] <- NA

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

    # diagonals
    tar <- which(cols > 10 & rows > 10 & rows == cols)
    sector[tar] <- 7
    tar <- which(cols < 10 & rows > 10 & (20 - rows) == cols)
    sector[tar] <- 5
    tar <- which(cols < 10 & rows < 10 & rows == cols)
    sector[tar] <- 3
    tar <- which(cols > 10 & rows < 10 & (20 - rows) == cols)
    sector[tar] <- 1

    # central rows and columns
    tar <- which(cols == 10 & rows != 10 & rows > cols)
    sector[tar] <- 7
    tar <- which(cols != 10 & rows == 10 & rows > cols)
    sector[tar] <- 5
    tar <- which(cols == 10 & rows != 10 & rows < cols)
    sector[tar] <- 3
    tar <- which(cols != 10 & rows == 10 & rows < cols)
    sector[tar] <- 1

    liberty <- 1:361
    lookup <- data.frame(row = rows, col = cols, liberty = liberty, 
        sector = sector)

    coord_col_letter <- substr(coord_sgf, 1, 1)
    coord_row_letter <- substr(coord_sgf, 2, 2)
    coord_cols <- match(coord_col_letter, letters[1:19])
    coord_rows <- match(coord_row_letter, letters[1:19])
    coord_rows[coord_rows > 19] <- NA
    coord_cols[coord_cols > 19] <- NA
    coord_liberty <- rep(NA, length(coord_rows))
    for (i in 1:length(coord_rows)) {
        if (!is.na(coord_rows[i])) {
            coord_liberty[i] <- lookup$liberty[lookup$row == 
                coord_rows[i] & lookup$col == coord_cols[i]]
        }
    }
    coord_sector <- lookup$sector[match(coord_liberty, lookup$liberty)]
    if (!all(is.na(coord_sector))) {
        first_sector <- coord_sector[min(which(!is.na(coord_sector)))]
    }
    else {
        first_sector <- 1
    }
    liberty <- matrix(1:361, ncol = 19, byrow = TRUE)

    if (first_sector == 1) 
        standard_liberty <- liberty
    if (first_sector == 2) 
        standard_liberty <- liberty |> reflect_315()
    if (first_sector == 3) 
        standard_liberty <- liberty |> rotate_90()
    if (first_sector == 4) 
        standard_liberty <- liberty |> reflect_y()
    if (first_sector == 5) 
        standard_liberty <- liberty |> rotate_n90() |> rotate_n90()
    if (first_sector == 6) 
        standard_liberty <- liberty |> reflect_45()
    if (first_sector == 7) 
        standard_liberty <- liberty |> rotate_n90()
    if (first_sector == 8) 
        standard_liberty <- liberty |> reflect_x()

    x <- as.vector(t(liberty))
    x_trans <- as.vector(t(standard_liberty))
    coord_standard_liberty <- x[match(coord_liberty, x_trans)]
    new_rows <- lookup$row[match(coord_standard_liberty, lookup$liberty)]
    new_cols <- lookup$col[match(coord_standard_liberty, lookup$liberty)]
    new_coord_sgf <- paste(letters[new_cols], letters[new_rows], 
        sep = "")
    keep <- which(new_coord_sgf == "NANA")
    if (length(keep) > 0) 
        new_coord_sgf[keep] <- coord_sgf[keep]
    return(new_coord_sgf)
}
