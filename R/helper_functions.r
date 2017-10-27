

write_kifu <- function(game_object, file, max){

  goban.side <- par("pin")[1]
  stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the plot_board as the fixed size of the board
  if(is.na(max)) max <- nrow(game_object$moves)
  x.coord <- game_object$moves$column[1:max]
  y.coord <- game_object$moves$row[1:max]
  colors <- game_object$moves$color[1:max]
  rev_colors <- ifelse( colors=="black", "white", "black" )

  pdf(file)
  plot_board(goban.color=gray(0.8))
  points(x.coord, y.coord, cex=stone.size, pch=21, bg=colors)
  text(x.coord, y.coord, labels=1:max, col=rev_colors)
  dev.off()

} 



write_gif <- function(game_object, file, max=NA, number=FALSE, delay=50, n_loops=0){

  goban.side <- par("pin")[1]
  stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the plot_board as the fixed size of the board
  if(is.na(max)) max <- nrow(game_object$moves)
  x.coord <- game_object$moves$column[1:max]
  y.coord <- game_object$moves$row[1:max]
  colors <- game_object$moves$color[1:max]
  rev_colors <- ifelse( colors=="black", "white", "black" )

  print("producing animation panes")
  for(i in 1:length(x.coord)){
    pane_filename <- paste("animated_pane_", sprintf("%04d", i), ".png", sep="")
    png(pane_filename)
    plot_board()
    points(x.coord[1:i], y.coord[1:i], cex=stone.size, pch=21, bg=colors[1:i])
    if(number==TRUE) text(x.coord[1:i], y.coord[1:i], labels=1:i, col=rev_colors[1:i])
    dev.off()
  }

  my_filename <- file

  convert_call <- paste0("convert -loop ", n_loops, " -delay ", delay, " animated_pane* " , my_filename)

  print("compiling gif")
  system(convert_call)

  pane_temp <- list.files(".", pattern="animated_pane*")

  file.remove(pane_temp)

}




plot_game <- function(game_object, speed=NA, number=FALSE, max=NA){
  goban.side <- par("pin")[1]
  stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the plot_board as the fixed size of the board
  if(is.na(max)) max <- nrow(game_object$moves)
  x.coord <- game_object$moves$column[1:max]
  y.coord <- game_object$moves$row[1:max]
  colors <- game_object$moves$color[1:max]
  rev_colors <- ifelse( colors=="black", "white", "black" )
  plot_board()
  for(i in 1:length(x.coord)){
    if(!is.na(speed)) Sys.sleep(speed)
    points(x.coord[i], y.coord[i], cex=stone.size, pch=21, bg=colors[i])
    if(number==TRUE) text(x.coord[i], y.coord[i], labels=i, col=rev_colors[i])
  }
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
  sgf_tag <- iconv(sgf_tag, "latin1", "ASCII", sub="") # strip out non-ASCII entirely
  sgf_tag <- gsub(" *$|^ *", "", sgf_tag)
  output <- list()
  output[[1]] <- sgf_tag[2:length(sgf_tag)]
  names(output) <- sgf_tag[1] # might have trouble here
  return(output)
}
