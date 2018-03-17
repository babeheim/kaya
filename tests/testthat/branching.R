
rm(list=ls())

my_game <- './branching_sgf/several_bifurcations.sgf'

sgf_lines <- readLines(my_game)

sgf_lines <- paste(sgf_lines, collapse="\n")
sgf_lines <- gsub("\n", "", sgf_lines)

x <- sgf_lines
x <- gsub("\\(;", "<sgf>", x)
x <- gsub("\\)", "</sgf>", x)

library(xml2)
y <- read_xml(x)
y <- as_list(y)

y[1][[1]]

# if we're going to do that, then...
# my solution iss alreayd good: treat them as seperate games, 
# just discard the other entries...

m <- regexpr("FF\\[.{0,2}\\]", sgf_lines)
regmatches(sgf_lines, m)

m <- regexpr("\(x\)", sgf_lines)
regmatches(sgf_lines, m)

read_xml("<foo><bar /></foo>")
x <- read_html("<html><title>Hi<title></html>")
class(x) # xml_document, xml_node

xml_structure(y)

as_list(y)

# this is the complete structure! it will work....

# nodes

xml_text


read_html("<html><title>Hi")


