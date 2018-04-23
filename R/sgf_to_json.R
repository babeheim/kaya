

split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}

sgf_to_json <- function(path, outdir = "."){
  if(!(length(grep("\\.sgf$", path)) == 1 | length(grep("\\.SGF$", path)) == 1)) stop("file does not end with sgf or SGF")
  d <- read_sgf(path, to.json = TRUE)
  outfile <- split_path(path)[1]
  outfile <- gsub(".sgf$", ".json", outfile)
  outfile <- gsub(".SGF$", ".json", outfile)
  out <- file.path(outdir, outfile)
  writeLines(d, out)
}
