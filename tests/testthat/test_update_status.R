

test_that("id_direct_connections only connects stones to at most 4 others in random games", {
  for(i in 1:100){
    x <- sample(1:19, 150, replace = TRUE)
    y <- sample(1:19, 150, replace = TRUE)
    moves <- data.frame(column = x, row = y)
    drop <- which(duplicated(moves))
    if(length(drop)>0) moves <- moves[-drop,]
    dat <- id_direct_connections(moves)
    dat <- dat | t(dat)
    expect_false( any(colSums(dat) < 1) )
    expect_false( any(colSums(dat) > 5) )
  }
})

test_that("id_groups doesn't have any weird inconsistencies on random games", {
  for(i in 1:40){
    x <- sample(1:19, 100, replace = TRUE)
    y <- sample(1:19, 100, replace = TRUE)
    moves <- data.frame(column = x, row = y)
    drop <- which(duplicated(moves))
    if(length(drop)>0) moves <- moves[-drop,]
    moves$group_id <- id_maker(n=nrow(moves), nchar=3)
    dat <- id_direct_connections(moves)
    dat <- dat | t(dat)
    moves$group_id <- id_groups(moves)
    singletons <- moves$group_id[which(colSums(dat) == 1)]
    groupers <- moves$group_id[which(colSums(dat) > 1)]
    multistone_ids <- sort(unique(moves$group_id[duplicated(moves$group_id)]))
    expect_true(!any(singletons %in% multistone_ids))
    expect_true(all(groupers %in% multistone_ids))
  }
})

test_that("test igraph refactor of id_groups against my original version", {
  for(i in 1:50){
    x <- sample(1:19, 100, replace = TRUE)
    y <- sample(1:19, 100, replace = TRUE)
    moves <- data.frame(column = x, row = y)
    drop <- which(duplicated(moves))
    if(length(drop)>0) moves <- moves[-drop,]
    moves$group_id <- id_maker(n=nrow(moves), nchar=3)
    dat <- id_direct_connections(moves)
    dat <- dat | t(dat)
    moves$group_id <- id_groups_old(moves)
    moves$group_id2 <- id_groups(moves)
    expect_true(length(unique(moves$group_id2)) == sum(table(moves$group_id2, moves$group_id)!=0))
  }
})

test_that("id_groups doesn't have any weird inconsistencies on random games", {
  for(i in 1:40){
    x <- sample(1:19, 100, replace = TRUE)
    y <- sample(1:19, 100, replace = TRUE)
    moves <- data.frame(column = x, row = y)
    drop <- which(duplicated(moves))
    if(length(drop)>0) moves <- moves[-drop,]
    moves$group_id <- id_maker(n=nrow(moves), nchar=3)
    dat <- id_direct_connections(moves)
    dat <- dat | t(dat)
    moves$group_id <- id_groups(moves)
    singletons <- moves$group_id[which(colSums(dat) == 1)]
    groupers <- moves$group_id[which(colSums(dat) > 1)]
    multistone_ids <- sort(unique(moves$group_id[duplicated(moves$group_id)]))
    expect_true(!any(singletons %in% multistone_ids))
    expect_true(all(groupers %in% multistone_ids))
  }
})

test_that("update_status is okay with games with passes", {
  d <- read_sgf('./real_sgf/has_pass.sgf')
  d$moves$group_id <- id_maker(n = nrow(d$moves), nchar = 3)
  expect_silent(update_status(d$moves, viz = FALSE))
  d <- read_sgf('./real_sgf/has_passes.sgf')
  d$moves$group_id <- id_maker(n = nrow(d$moves), nchar = 3)
  expect_silent(update_status(d$moves, viz = FALSE))
})


test_that("update_status works fine on a few valid games",{
  my_games <- list.files("real_sgf", full.names = TRUE)[1:10]
  for(i in 1:length(my_games)){
    d <- read_sgf(my_games[i])
    d$moves$group_id <- id_maker(n = nrow(d$moves), nchar = 3)
    expect_silent(update_status(d$moves, viz = FALSE))
  }
})

test_that("update_status detects illegal move at occupied spot",{
  d <- read_sgf('./invalid_sgf/move_at_occupied_spot.sgf')
  d$moves$group_id <- id_maker(n = nrow(d$moves), nchar = 3)
  expect_error(update_status(d$moves), regexpr = "illegal collision detected")
})

test_that("update_status detects suicide move",{
  d <- read_sgf('./invalid_sgf/suicide.sgf')
  d$moves$group_id <- id_maker(n = nrow(d$moves), nchar = 3)
  expect_warning(update_status(d$moves), regexpr = "suicide detected at move")
})

