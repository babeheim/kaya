
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