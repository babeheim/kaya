
update_status <- function(game_moves){
  n_moves <- max(game_moves$number)
  game_moves$n_liberties <- NA
  for(i in 1:n_moves){
    current_color <- game_moves$color[game_moves$number == i]
    other_color <- ifelse(current_color == "black", "white", "black")

    # 0. check that the current move's location isn't occupied
    past_moves <- which(game_moves$number < i & game_moves$group_id != "removed")
    if(length(past_moves) > 0){
      collision <- any(game_moves$column[past_moves] == game_moves$column[game_moves$number == i] & 
        game_moves$row[past_moves] == game_moves$row[game_moves$number == i])
      if(collision) stop(paste0("illegal collision detected, move ", i, " is to an occupied location"))
    }

    # 1. update group identity of stones of same color for all moves up to this move
    update_rows <- which(game_moves$color == current_color & game_moves$number <= i & game_moves$group_id != "removed")
    if(length(update_rows) > 0) game_moves$group_id[update_rows] <- id_groups(game_moves[update_rows, c("column", "row", "group_id")])

    bad <- any(game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color=="black")] %in%  game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color=="white")])
    if(bad) stop("groups are mixing up colors!")

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
      stop(paste0("suicide detected at move ", i))
    }

  }
  return(game_moves$group_id)
}
