


spellcheck_demos <- function(dir = getwd()) {
  textfile2rds <- function(file) {
    x <- readLines(file)
    saveRDS(x[nzchar(x)], file <- tempfile(fileext = ".rds"))
    file
  }  
  demo_filter <- function(ifile, encoding = "unknown") {
    x <- readLines(ifile, encoding = encoding, warn = FALSE)
    x[!grepl("^\\s*#", x, FALSE, TRUE)] <- ""
    x <- gsub("`[^`]+`", "CODE", x, FALSE, TRUE)
    x <- gsub("\\*{2}(?!\\s)[^*]+\\*{2}", "STRONG", x, FALSE, TRUE)
    x <- gsub("\\*(?!\\s)[^*]+\\*", "EMPHASIS", x, FALSE, TRUE)
    x
  }
  whitelist <- textfile2rds(file.path(dir, "misc", "whitelist-manual.txt"))
  ff <- list.files(file.path(dir, "opm_in", "demo"), full.names = TRUE)
  for (f in grep("\\.R$", ff, TRUE, TRUE, TRUE))
    print(aspell(f, dictionaries = whitelist, control = "-d en_GB",
      filter = demo_filter))
}


spellcheck_demos()


