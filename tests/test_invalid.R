
rm(list=ls())

# library(devtools)
# devtools::load_all()

library(testthat)

my_files <- list.files("./invalid_sgf", pattern="*.sgf", full.names=TRUE)

my_game <- './invalid_sgf/no_moves.sgf'

# we're gonna have corrupted games out in the wild
# rather than just break, we should have it skip and 
# identify whats wrong, so we can clean up easily

# we need an sgf validator to run on sgf_lines
# it has to pass that QC first before you run the parser

# test: games are not properly seperated by ( ) 
# test: games have ";" inside [] (actually thats unusual but not bad)
# test: games have [] inside [] (this is illegal)
# test: games have missing ] in metadata / in moves
# test: games have missing [ in metadata / in moves