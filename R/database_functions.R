
create_database <- function(sgf_paths) {
  data_list <- list()
  counter <- 1
  for (i in seq_along(sgf_paths)) {
    game_data <- read_sgf(sgf_paths[i])
    if (class(game_data) != "try-error") {
      game_data$m1 <- game_data$moves$coord_sgf[1]
      game_data$m2 <- game_data$moves$coord_sgf[2]
      game_data$filename <- sgf_paths[i]
      data_list[[counter]] <- game_data[-which(names(game_data) %in% c("AB", "AW", "moves"))]
      counter <- counter + 1
    }
    if (i %% 100 == 0) print(i)
  }
  output <- as.data.frame(bind_rows(data_list))
  return(output)
}
