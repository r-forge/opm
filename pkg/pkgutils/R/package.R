pack_desc <- function(pkg, ...) UseMethod("pack_desc")

pack_desc.character <- function(pkg,
    action = c("read", "update", "source", "spell"),
    version = TRUE, demo = FALSE, date.format = "%Y-%m-%d",
    envir = globalenv(), ...) {
  LL(version, demo, date.format)
  action <- match.arg(action)
  x <- normalizePath(file.path(pkg, "DESCRIPTION"))
  if (action == "spell")
    return(aspell(files = x, filter = "dcf", ...))
  x <- lapply(x, function(file) {
      stopifnot(nrow(y <- read.dcf(file)) == 1L)
      structure(as.list(y[1L, ]), file = file,
        class = c("pack_desc", "packageDescription"))
    })
  x <- structure(x, names = pkg, class = "pack_descs")
  case(action,
    read = x,
    update = {
      x <- update(object = x, version = version, date.format = date.format)
      if (!demo)
        puts(x = x, file = vapply(x, attr, "", which = "file"), ...)
      wanted <- "Date"
      if (version)
        wanted <- c(wanted, "Version")
      x <- lapply(wanted, function(i) vapply(x, `[[`, "", i = i))
      x <- do.call(cbind, x)
      colnames(x) <- wanted
      x
    },
    source = source_files(x = x, demo = demo, envir = envir, ...)
  )
}

pkg_files <- function(x, ...) UseMethod("pkg_files")

pkg_files.character <- function(x, what, installed = TRUE, ignore = NULL,
    ...) {
  filter <- function(x) {
    if (length(ignore))
      x <- x[!tolower(basename(x)) %in% tolower(ignore)]
    x
  }
  if (L(installed)) {
    result <- find.package(basename(x), quiet = TRUE, verbose = FALSE)
    if (!length(result))
      return(character())
    result <- list.files(path = file.path(result, what), full.names = TRUE, ...)
    normalizePath(filter(result), winslash = "/")
  } else if (any(is.pack.dir <- is_pkg_dir(x))) {
    result <- as.list(x)
    result[is.pack.dir] <- lapply(x[is.pack.dir], function(name) {
      list.files(path = file.path(path = name, what), full.names = TRUE, ...)
    })
    filter(unlist(result))
  } else
    filter(x)
}

is_pkg_dir <- function(x) UseMethod("is_pkg_dir")

is_pkg_dir.character <- function(x) {
  result <- file_test("-d", x)
  result[result] <- file_test("-f", file.path(x[result], "DESCRIPTION"))
  result
}

run_R_CMD <- function(x, ...) UseMethod("run_R_CMD")

run_R_CMD.character <- function(x, what = "check", ...,
    sudo = identical(what, "INSTALL"), system.args = list()) {
  r.exe <- Sys.which("R")
  r.exe <- r.exe[nzchar(r.exe)]
  LL(what, sudo, r.exe)
  pat <- "%s --vanilla CMD %s %s %s"
  if (sudo)
    pat <- paste("sudo", pat)
  args <- paste0(prepare_options(unlist(c(...))), collapse = " ")
  cmd <- sprintf(pat, r.exe, what, args, paste0(x, collapse = " "))
  do.call(system, c(list(command = cmd), system.args))
}

copy_pkg_files <- function(x, ...) UseMethod("copy_pkg_files")

copy_pkg_files.character <- function(x, what = "scripts",
    to = file.path(Sys.getenv("HOME"), "bin"), installed = TRUE,
    ignore = NULL, overwrite = TRUE, ...) {
  files <- pkg_files(x, what, installed = installed, ignore = ignore)
  file.copy(from = files, to = to, overwrite = overwrite, ...)
}

delete_o_files <- function(x, ...) UseMethod("delete_o_files")

delete_o_files.character <- function(x, ext = "o", ignore = NULL, ...) {
  ext <- tolower(c(ext, .Platform$dynlib.ext))
  ext <- unique.default(tolower(sub("^\\.", "", ext, perl = TRUE)))
  ext <- sprintf("\\.(%s)$", paste0(ext, collapse = "|"))
  x <- pkg_files(x, what = "src", installed = FALSE, ignore = ignore, ...)
  if (length(x <- x[grepl(ext, x, perl = TRUE, ignore.case = TRUE)]))
    file.remove(x)
  else
    logical()
}

swap_code <- function(x, ...) UseMethod("swap_code")

swap_code.character <- function(x, ..., ignore = NULL) {
  run_pkgutils_ruby(x = x, script = "swap_code.rb", ignore = ignore, ...)
}

check_R_code <- function(x, ...) UseMethod("check_R_code")

check_R_code.character <- function(x, lwd = 80L, indention = 2L,
    roxygen.space = 1L, comma = TRUE, ops = TRUE, parens = TRUE,
    assign = TRUE, modify = FALSE, ignore = NULL, accept.tabs = FALSE,
    three.dots = TRUE, what = "R", encoding = "", ...) {
  spaces <- function(n) paste0(rep.int(" ", n), collapse = "")
  LL(lwd, indention, roxygen.space, modify, comma, ops, parens, assign,
    accept.tabs, three.dots)
  roxygen.space <- sprintf("^#'%s", spaces(roxygen.space))
  check_fun <- function(x) {
    infile <- attr(x, ".filename")
    complain <- function(text, is.bad) {
      if (any(is.bad))
        problem(text, infile = infile, line = which(is.bad))
    }
    bad_ind <- function(text, n) {
      attr(regexpr("^ *", text, perl = TRUE), "match.length") %% n != 0L
    }
    unquote <- function(x) {
      x <- gsub(QUOTED, "QUOTED", x, perl = TRUE)
      x <- sub(QUOTED_END, "QUOTED", x, perl = TRUE)
      x <- sub(QUOTED_BEGIN, "QUOTED", x, perl = TRUE)
      gsub("%[^%]+%", "%%", x, perl = TRUE)
    }
    code_check <- function(x) {
      x <- sub("^\\s+", "", x, perl = TRUE)
      complain("semicolon contained", grepl(";", x, fixed = TRUE))
      complain("space followed by space", grepl("  ", x, fixed = TRUE))
      if (three.dots)
        complain("':::' operator used", grepl(":::", x, fixed = TRUE))
      if (comma) {
        complain("comma not followed by space",
          grepl(",[^\\s]", x, perl = TRUE))
        complain("comma preceded by space", grepl("[^,]\\s+,", x, perl = TRUE))
      }
      if (ops) {
        complain("operator not preceded by space",
          grepl(OPS_LEFT, x, perl = TRUE))
        complain("operator not followed by space",
          grepl(OPS_RIGHT, x, perl = TRUE))
      }
      if (assign)
        complain("line ends in single equals sign",
          grepl("(^|[^=<>!])=\\s*$", x, perl = TRUE))
      if (parens) {
        complain("'if', 'for' or 'while' directly followed by parenthesis",
          grepl("\\b(if|for|while)\\(", x, perl = TRUE))
        complain("opening parenthesis or bracket followed by space",
          grepl("[([{]\\s", x, perl = TRUE))
        complain("closing parenthesis or bracket preceded by space",
          grepl("[^,]\\s+[)}\\]]", x, perl = TRUE))
        complain("closing parenthesis or bracket followed by wrong character",
          grepl("[)\\]}][^\\s()[\\]}$@:,;]", x, perl = TRUE))
      }
    }
    if (modify) {
      x <- sub("\\s+$", "", x, perl = TRUE)
      x <- gsub("\t", spaces(indention), x, fixed = TRUE)
    } else if (!accept.tabs)
      complain("tab contained", grepl("\t", x, fixed = TRUE))
    complain(sprintf("line longer than %i", lwd), nchar(x) > lwd)
    complain(sprintf("indention not multiple of %i", indention),
      bad_ind(sub(roxygen.space, "", x, perl = TRUE), indention))
    complain("roxygen comment not placed at beginning of line",
      grepl("^\\s+#'", x, perl = TRUE))
    code_check(sub("\\s*#.*", "", unquote(x), perl = TRUE))
    if (modify)
      x
    else
      NULL
  }
  invisible(map_files(pkg_files(x = x, what = what, installed = FALSE,
    ignore = ignore, ...), check_fun, .encoding = encoding))
}

logfile <- function(x) UseMethod("logfile")

logfile.NULL <- function(x) {
  PKGUTILS_OPTIONS$logfile
}

logfile.character <- function(x) {
  old <- PKGUTILS_OPTIONS$logfile
  PKGUTILS_OPTIONS$logfile <- L(x[!is.na(x)])
  if (nzchar(x))
    tryCatch(cat(sprintf("\nLOGFILE RESET AT %s\n", date()), file = x,
      append = TRUE), error = function(e) {
        PKGUTILS_OPTIONS$logfile <- old
        stop(e)
      })
  invisible(x)
}

