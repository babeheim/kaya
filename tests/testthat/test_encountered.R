

test_that("encountered bugs are resolved", {
  sgf_file <- "./unusual_sgf/whitespace_before_game.sgf"
  expect_true(validate_sgf(sgf_file))
}



my_files <- list.files(".")

# segfault error solved
# the problem was THERE WERE TOO MANY COMMENTS
# when you completely remove comments from the properly-escaped string, loads just fine!

my_files <- list.files(".", pattern = "\\.sgf$", recursive = TRUE, full.names = TRUE)

# I guess kaya odesn't like empty nodes, but those are legal in SGF context ;;;; is fine!

[1] "./1668/7/HoninboDosaku-YasuiSantetsu17.sgf"    
[2] "./1957/12/SakataEio-ShimamuraToshihiro5352.sgf"
[3] "./1996/10/LeeChangho-KobayashiSatoru20595.sgf" 
[4] "./2011/2/HaSungbong-KamimuraHaruo66132.sgf"    
[5] "./2011/2/MiyazakiRyutaro-HaSungbong66130.sgf"  
[6] "./2011/2/ShimojiGensho-HaSungbong66133.sgf"    
[7] "./2011/3/HaSungbong-OhashiHirofumi66140.sgf"   
[8] "./2011/5/HaSungbong-FuruyaYutaka66143.sgf"     
[9] "./2013/1/LeeYounggu-LeeDonghoon46204.sgf"      


valid <- validate_sgf(my_files)

my_files[!valid]


valid <- rep(NA, length(my_files))
for(i in 1:length(my_files)){
  valid[i] <- validate_sgf(my_files[i])
}



# put game file here

sgf_file <- "./encountered_bugs/2010-08-22_atticus_mikan_W+43.50.sgf"

read_sgf(sgf_file)

# i cannot explain whats going on here, other than it's extremely nested....
# need minimal working example!

# check that it fails
# validate_sgf(sgf_file)



# now go step by step

# read_sgf
raw <- paste0(readLines(sgf_file, warn = FALSE), collapse = "")

# check_comment_escapes
string <- raw
string <- gsub("\\\\\\[", "\\[", string)
string <- gsub("\\\\\\]", "\\]", string)
# whats going on here?
n_left_brackets <- length(gregexpr("(?<!\\\\)\\]", string, perl = TRUE)[[1]])
n_right_brackets <- length(gregexpr("(?<!\\\\)\\[", string, perl = TRUE)[[1]])

if(n_left_brackets != n_right_brackets) stop("sgf seems invalid; square brackets don't balance, must fix first")
#  comment_pattern <- "\\[(?>[^\\[\\]]|(?R))*\\]"
#  comment_pattern <- "\\[((?>[^\\[\\]]+)|(?R))*\\]"
bracket_pattern <- "\\[((?>\\\\\\[|\\\\\\]|[^\\[\\]])|(?R))*\\]"
check <- gregexpr(bracket_pattern, string, perl = TRUE)
if (check[[1]][1]!="-1") {
  corrected <- regmatches(string, check)
  corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\(", "\\\\(", z, perl = TRUE))
  corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\)", "\\\\)", z, perl = TRUE))
  corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\](?!$)", "\\\\]", z, perl = TRUE))
  corrected <- lapply(corrected, function(z) gsub("(?<!\\\\|^)\\[", "\\\\[", z, perl = TRUE))
  corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\;", "\\\\;", z, perl = TRUE))
  regmatches(string, check) <- corrected
}

# segfault if try to do this in functions, but manually ti works??

# parse_sgf
sgf_string <- strip_comments(string)
# sgf_string <- check_comment_escapes(sgf_string)
if (length(sgf_string) > 1) stop("parse_sgf accepts only single strings")
sgf_string <- gsub(" *$|^ *", "", sgf_string)
sgf_string <- gsub("^\\(|\\)$", "", sgf_string)

# split_sgf
output <- list()
node_pattern <- "(^.*?(?<!\\\\))(\\(|$)" 
# group from start of line to first unescaped (, or end of line
m <- gregexpr(node_pattern, sgf_string, perl = TRUE)
node_data <- regmatches(sgf_string, m)[[1]]
output$nodes <- gsub("\\($", "", node_data)

# parse_branch
branch_string <- output$nodes
node_vec <- split_branch(branch_string)
output <- list()
for(i in 1:length(node_vec)) output$nodes[[i]] <- parse_node(node_vec[i])

# ????

# simplify_game

game_list <- output

  meta <- list()
  moves <- data.frame(number = integer(), color = character(),
      coord_sgf = character())

  has_meta <- !all(names(game_list$nodes[[1]]) %in% c("B", "W"))
  if (has_meta) {
    meta <- game_list$nodes[[1]]
    if ("AB" %in% names(meta)){
      ab_coord_sgf <- meta$AB
      ab_number <- rep(0, length(ab_coord_sgf))
      ab_color <- rep("black", length(ab_coord_sgf))
      ab_setup_moves <- data.frame(number = ab_number, color = ab_color,
        coord_sgf = ab_coord_sgf, stringsAsFactors = FALSE)
      moves <- rbind(ab_setup_moves, moves)
    }
    if ("AW" %in% names(meta)){
      aw_coord_sgf <- meta$AW
      aw_number <- rep(0, length(aw_coord_sgf))
      aw_color <- rep("white", length(aw_coord_sgf))
      aw_comment <- rep("", length(aw_coord_sgf))
      aw_setup_moves <- data.frame(number = aw_number, color = aw_color,
        coord_sgf = aw_coord_sgf, stringsAsFactors = FALSE)
      moves <- rbind(aw_setup_moves, moves)
    }
  }

  has_moves <- any(unlist(lapply(game_list$nodes, function(z) any(names(z) %in% c("B", "W")))))
  if (has_moves) {
    game_moves <- simplify_move_nodes(game_list)
    game_moves$number <- 1:nrow(game_moves)
    moves <- rbind(moves, game_moves)
    hash_id <- NA
    n_moves <- 0
    if (nrow(moves) > 0) {
      moves$coord_sgf[is.na(moves$coord_sgf)] <- "tt"
      trans_coord_sgf <- moves$coord_sgf
      if (rotate == TRUE) trans_coord_sgf <- orient_sgf(moves$coord_sgf)
      # do i need to subtract from 20?
      moves$column <- match(substr(trans_coord_sgf, 1, 1), letters[1:19])
      moves$row <- match(substr(trans_coord_sgf, 2, 2), letters[1:19])
      moves <- moves[, c("color", "coord_sgf", "number", "column", "row")]
      meta$hash_id <- substr(digest::sha1(moves[, c("column", "row")]), 1, 19)
      meta$n_moves <- nrow(moves)
    }
  }

  if(!has_meta & !has_moves) stop("not a valid game!")

  output <- meta
  if(nrow(moves) != 0) output$moves <- moves
  if(nrow(moves) == 0) output$n_moves <- 0

output

# i think ther'es some kind of inifite recursion going on.....in this game