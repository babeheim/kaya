



update_status <- function(game_moves){
  n_moves <- max(game_moves$number)
  game_moves$n_liberties <- NA
  for(i in 1:n_moves){
    current_color <- game_moves$color[game_moves$number == i]
    other_color <- ifelse(current_color == "black", "white", "black")

    # 1. update group identity of stones of same color for all moves up to this move
    update_rows <- which(game_moves$color == current_color & game_moves$number <= i & game_moves$group_id != "removed")
    if(length(update_rows) > 0) game_moves$group_id[update_rows] <- id_groups(game_moves[update_rows, c("column", "row", "group_id")])

    bad <- any(game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color=="black")] %in%  game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color=="white")])
    if(bad) stop("mixup groups!")

    # 2. recount liberties for all stones
    active_rows <- which(game_moves$number <= i & game_moves$group_id != "removed")
    game_moves$n_liberties[active_rows] <- count_liberties(game_moves[active_rows,])

    # 3. remove enemy groups with 0 liberties! 
    update_rows <- which(game_moves$color == other_color & game_moves$number < i & game_moves$group_id != "removed")
    group_liberties <- tapply(game_moves$n_liberties[update_rows], game_moves$group_id[update_rows], sum)
    removable_groups <- names(which(group_liberties == 0))
    if(length(removable_groups) > 0){
      game_moves$group_id[update_rows][which(game_moves$group_id[update_rows] %in% removable_groups)] <- "removed"
    }

    # 4. recount liberties again
    active_rows <- which(game_moves$number <= i & game_moves$group_id != "removed")
    game_moves$n_liberties[active_rows] <- count_liberties(game_moves[active_rows,])

    # 5. suicide check
    update_rows <- which(game_moves$color == current_color & game_moves$number <= i & game_moves$group_id != "removed")
    group_liberties <- tapply(game_moves$n_liberties[update_rows], game_moves$group_id[update_rows], sum)
    removable_groups <- names(which(group_liberties == 0))
    if(length(removable_groups) > 0){
      stop(paste("suicide detected at move ", i))
    }

  }
  return(game_moves$group_id)
}


count_liberties <- function(moves, board_size = 19){
  # assumes all are active
  n_liberties <- rep(NA, nrow(moves))
  for (i in 1:nrow(moves)){
    i_x <- moves$column[i]
    i_y <- moves$row[i]
    south_free <- !any(moves$row == (i_y - 1) & moves$column == i_x) & i_y != 1
    north_free <- !any(moves$row == (i_y + 1) & moves$column == i_x) & i_y != board_size
    west_free <- !any(moves$row == (i_y) & moves$column == (i_x - 1)) & i_x != 1
    east_free <- !any(moves$row == (i_y) & moves$column == (i_x + 1)) & i_x != board_size
    n_liberties[i] <- sum(south_free, north_free, west_free, east_free)
  }
  return(n_liberties)
}



id_maker <- function(n, reserved='', seed=NA, nchars=NA){
    my_let <- letters 
    my_num <- 0:9 
    if(is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
    if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
    output <- replicate(n, paste(sample(c(my_let, my_num), nchars, replace=TRUE), 
        collapse=''))
    rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
    while(any(rejected)){
        output <- output[-which(rejected)]
        remaining <- n-length(output)
        output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num), nchars, 
            replace=TRUE), collapse='')))
        rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
    }
    output
}

id_groups <- function(moves){

  direct_mat <- id_direct_connections(moves)

  group_IDs <- moves$group_id

  for(i in 1:length(group_IDs)){
    tie_cols <- which(direct_mat[i,])
    leftmost <- group_IDs[tie_cols][1]
    ingroup <- tie_cols
    for(j in 1:length(tie_cols)){
      tie_rows <- which(direct_mat[,tie_cols[j]])
      ingroup <- c(ingroup, tie_rows)
      for(k in 1:length(tie_rows)){
        tie_cols_2 <- which(direct_mat[tie_rows[k],])
        ingroup <- c(ingroup, tie_cols_2)
        for(l in 1:length(tie_cols_2)){
          tie_rows_2 <- which(direct_mat[,tie_cols_2[l]])
          ingroup <- c(ingroup, tie_rows_2)
          for(m in 1:length(tie_rows_2)){
            tie_cols_3 <- which(direct_mat[tie_rows_2[m],])
            ingroup <- c(ingroup, tie_cols_3)
          }
        }
      }
    }
    ingroup <- sort(unique(ingroup))
    group_IDs[ingroup] <- leftmost
  }

  return(group_IDs)

}





id_direct_connections <- function(moves){
  direct_mat <- matrix(FALSE, nrow = nrow(moves), ncol = nrow(moves))
  diag(direct_mat) <- TRUE
  for(i in 1:nrow(moves)){
    i_y <- moves$row[i]
    i_x <- moves$column[i]
    south <- which(moves$row == (i_y - 1) & moves$column == i_x)
    north <- which(moves$row == (i_y + 1) & moves$column == i_x)
    west <- which(moves$row == (i_y) & moves$column == (i_x - 1))
    east <- which(moves$row == (i_y) & moves$column == (i_x + 1))
    if(length(south) > 0) direct_mat[i, south] <- TRUE
    if(length(north) > 0) direct_mat[i, north] <- TRUE
    if(length(west) > 0) direct_mat[i, west] <- TRUE
    if(length(east) > 0) direct_mat[i, east] <- TRUE
  }
  return(direct_mat)
}



write_gif <- function(game_object, file, number=FALSE, delay=50, n_loops=0, start = NA, stop = NA){

  if(is.na(start)) start <- 1
  if(is.na(stop)) stop <- game_object$n_moves
  for(i in start:stop){
    pane_filename <- paste("animated_pane_", sprintf("%04d", i), ".png", sep="")
    png(pane_filename, height=5.25, width=5, units="in", res=300)
    plot_game(game_object, stop = i)
    dev.off()
  }
  my_filename <- file
  convert_call <- paste0("convert -loop ", n_loops, " -delay ", delay, " animated_pane* " , my_filename)
  print("compiling gif")
  system(convert_call)
  pane_temp <- list.files(".", pattern="animated_pane*")
  file.remove(pane_temp)
}



plot_game <- function(game_object, number = FALSE, stop = NA, ...){
  if(is.na(stop)) stop <- game_object$n_moves
  moves <- game_object$moves[game_object$moves$number <= stop,]
  # evaluate
  moves$group_id <- id_maker(nrow(moves), nchar=3)
  moves$group_id <- update_status(moves)
  moves$rev_color <- ifelse(moves$color=="black", "white", "black" )
  tar <- which(moves$number <= stop & moves$group_id != "removed")
  plot_board(...)
  goban.side <- par("pin")[1]
  stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the plot_board as the fixed size of the board
  points(moves$column[tar], moves$row[tar], cex=stone.size, pch=21, bg=moves$color[tar])
  if(number==TRUE) text(moves$column[tar], moves$row[tar], labels=moves$number[tar], col = moves$rev_color[tar])
}

validate_game <- function(game_data){
  coords <- as.character(game_data$moves$coord_sgf)
  coords_invalid <- !all(unlist(strsplit(coords, "")) %in% letters[1:20])
  coords_wronglength <- !all(nchar(coords) %in% c(0, 2))
  duplicate_key <- any(duplicated(names(game_data)))
  long_keys <- unlist(lapply(game_data, length)>1)
  long_keys <- names(long_keys[long_keys])
  long_key_error <- !all(long_keys %in% c("moves", "AB", "AW"))
  output <- !(long_key_error | duplicate_key | coords_wronglength | coords_invalid)
  return(output)
}



write_sgf <- function(game_list, path){
  tags <- names(game_list)
  tags <- tags[-which(tags %in% c("kaya_notes", "hash_id", "n_moves", "moves", "filename"))]

  if("AB" %in% tags) game_list$AB <- paste0("[", game_list$AB, "]", collapse="" )
  if("AW" %in% tags) game_list$AW <- paste0("[", game_list$AW, "]", collapse="" )

  meta <- paste0( tags, "[", game_list[tags], "]", collapse="" ) 
  meta <- gsub("\\[\\[", "[", meta)
  meta <- gsub("\\]\\]", "]", meta)

  coord_sgf <- paste0(letters[game_list$moves$column], letters[game_list$moves$row])

  print("this doesn't do handicap stones yet")

  colors <- ifelse(game_list$moves$color=="white", "B", "W")

  movestring <- paste0(colors, "[", coord_sgf, "];", collapse="")
  output <- paste0( "(;", meta, ";", movestring, ")")
  writeLines(output, path)
}



read_sgf <- function(sgf_file, ...){
  if(length(sgf_file)!=1) stop("only one path allowed")
  raw <- readLines(sgf_file)
  output <- parse_sgf(raw, ...)
  if(length(output)>0){
    if(is.null(names(output))){  # condition true if output is an unnamed list
      for(i in 1:length(output)) output[[i]]$filename <- sgf_file
    } else {
      output$filename <- sgf_file
    }
  }
  return(output)
}

sapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)

  wrapper <- function(...){
  curVal <- get("counter", envir = env)
  assign("counter", curVal +1 ,envir=env)
  setTxtProgressBar(get("pb", envir=env), curVal +1)
  FUN(...)
  }
  res <- sapply(X, wrapper, ...)
  close(pb)
  res
}

lapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)

  wrapper <- function(...){
  curVal <- get("counter", envir = env)
  assign("counter", curVal +1 ,envir=env)
  setTxtProgressBar(get("pb", envir=env), curVal +1)
  FUN(...)
  }
  res <- lapply(X, wrapper, ...)
  close(pb)
  res
}


extract_sgf_tag <- function(sgf_tag){
  if(length(sgf_tag) != 1) stop("sgf tag improper")
  sgf_tag <- strsplit(sgf_tag, "\\[|\\]\\[|\\]")[[1]]
  sgf_tag <- stringi::stri_trans_general(sgf_tag, "latin-ascii") # convert non-ASCII to closest ascii
  sgf_tag <- gsub("[\x01-\x1F]", "", sgf_tag) # takes care of non-printing ASCII
  # sgf_tag <- iconv(sgf_tag, "latin1", "ASCII", sub="") # strip out non-ASCII entirely
  sgf_tag <- gsub(" *$|^ *", "", sgf_tag)
  output <- list()
  output[[1]] <- sgf_tag[2:length(sgf_tag)]
  names(output) <- sgf_tag[1] # might have trouble here
  return(output)
}
