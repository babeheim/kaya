


# ah ha! the problem is hat there's a leading space...doh!

my_files <- list.files(".")
my_files <- setdiff(my_files, "2009-12-27_kx8k_atticus_W+R.sgf")
my_files <- setdiff(my_files, "2010-08-22_atticus_mikan_W+43.50.sgf") # another segfault error

valid <- rep(NA, length(my_files))
for(i in 1:length(my_files)){
  valid[i] <- validate_sgf(my_files[i])
}




test_that("encountered bugs are resolved", {
  sgf_file <- "./unusual_sgf/whitespace_before_game.sgf"
  expect_true(validate_sgf(sgf_file))
}


# put game file here

sgf_file <- "./encountered_bugs/2017-11-07_Jawor_atticus_W+10.50.sgf"

# i cannot explain whats going on here, other than it's extremely nested....
# need minimal working example!

# check that it fails
validate_sgf(sgf_file)

# segfault error!

# now go step by step

# read_sgf
raw <- paste0(readLines(sgf_file, warn = FALSE), collapse = "")


# scrubbed <- check_comment_escapes(raw)
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
  return(string)


  n_left_brackets <- length(gregexpr("(?<!\\\\)\\)", string, perl = TRUE)[[1]])
  n_right_brackets <- length(gregexpr("(?<!\\\\)\\(", string, perl = TRUE)[[1]])


# parse_sgf
sgf_string <- scrubbed
sgf_string <- check_comment_escapes(sgf_string)
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

parse_branch(output$nodes)

# i think ther'es some kind of inifite recursion going on.....in this game