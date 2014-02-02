#!/usr/local/bin/Rscript --vanilla


library(methods)


################################################################################


split_opt <- function(x, sep = ",") {
  if (is.character(x))
    unlist(strsplit(x, sep, TRUE), FALSE, FALSE)
  else
    x
}


# list2html <- function(x, ordered = FALSE, deep = FALSE, collapse = " ") {
#   tag <- function(name, data) hwriter::hmakeTag(name, data, newline = TRUE)
#   if (is.list(x)) {
#     y <- vapply(x, list2html, "", ordered, deep, collapse)
#     if (is.null(names(x)))
#       tag(if (ordered)
#           "ol"
#         else
#           "ul", paste0(tag("li", y), collapse = "\n"))
#     else
#       paste0(tag("dl", paste(tag("dt", names(x)), tag("dd", y), sep = collapse,
#         collapse = "\n")), collapse = "\n")
#   } else if (deep && length(x) > 1L) {
#     list2html(as.list(x), ordered, deep, collapse)
#   } else {
#     paste0(x, collapse = collapse)
#   }
# }


################################################################################


# Given a vector _x_ containing all lines of an HTML file, insert links to
# the corresponding HTML page from the manual located under _parent_, where
# necessary mapping to another help topic for the package _pkg_. If there is
# no such HTML manual file, set no link but print an according message.
#
insert_href <- function(x, parent, pkg) {

  insert_into_body <- function(x, parent, pkg) {

    insert <- function(x, m, parent, pkg) {
      get_aliased <- function(x, pkg) {
        y <- lapply(x, function(name) do.call(help, list(name, pkg)))
        ok <- vapply(y, length, 0L) == 1L
        y[!ok] <- x[!ok]
        basename(unlist(y, FALSE, FALSE))
      }
      create_link <- function(x, parent, pkg) {
        y <- get_aliased(x, pkg)
        files <- file.path(parent, sprintf("%s.html", y))
        links <- sprintf("<a href=\"%s\">%s</a>", files, x)
        if (any(orphan <- !file.exists(files)))
          message("missing files(s) for: ", paste0(x[orphan], collapse = " "))
        ifelse(orphan, x, links)
      }
      if (!any(m > 0L))
        return(x)
      before <- c(1L, m[-length(m)] + attr(m, "match.length")[-length(m)])
      before <- substring(x, before, m - 1L)
      last <- m[length(m)] + attr(m, "match.length")[length(m)]
      last <- substr(x, last, nchar(x))
      links <- substring(x, m, m + attr(m, "match.length") - 1L)
      links <- create_link(links, parent, pkg)
      paste0(paste0(before, links, collapse = ""), last)
    }

    m <- gregexpr("\\b[\\w.]+(?=\\()", x, FALSE, TRUE)
    mapply(insert, x, m, MoreArgs = list(parent = parent, pkg = pkg),
      USE.NAMES = FALSE)
  }

  x <- pkgutils::sections(x, "^\\s*<body>\\s*$", ignore.case = TRUE)
  if (length(x) != 2L)
    stop("expected two sections: one before, one including HTML body")
  c(x[[1L]], insert_into_body(x[[2L]], parent, pkg))
}


################################################################################


# Use knitr and some other packages for creating HTML documentation for the
# _installed_ package _pkg_. Place stuff into directory _outdir_. Use _mdir_
# within _outdir_ for storing the HTML manual pages. Use _url_ for linking to
# general R logos. Set leftwards links to package _alt_ (for which a similar
# HTML documentation is expected).
#
package2htmldoc <- function(pkg, outdir = "%s_doc", mdir = "manual",
    installed = TRUE, url = "http://stat.ethz.ch/R-manual/R-devel",
    alt = pkg) {

  store_description <- function(pkg, outdir, installed) {
    files <- pkgutils::pkg_files(pkg, ".", installed, I("DESCRIPTION"))
    # TODO: we might render this in HTML, too
    file.copy(files, outdir, TRUE)
  }

  store_news <- function(pkg, outdir, installed) {
    # this approach does not allow for a link back to the manual start page
    news2html <- function(infile, outfile) {
      rdfile <- tempfile()
      on.exit(unlink(rdfile))
      tools:::news2Rd(infile, rdfile)
      tools::Rd2HTML(rdfile, outfile)
    }
    news <- pkgutils::pkg_files(pkg, ".", installed, I("NEWS"))
    if (!length(news))
      return(FALSE)
    dir.create(outdir <- file.path(outdir, "NEWS"), FALSE, TRUE)
    news2html(news, file.path(outdir, "index.html"))
    TRUE
  }

  store_demos <- function(pkg, outdir, mdir, installed, url) {
    demo2html <- function(x, mdir, url) {
      result2table <- function(x) {
        x <- as.data.frame(x[, c("Item", "Title"), drop = FALSE])
        for (fmt in c("html", "md", "Rmd", "R")) {
          txt <- paste(x[, "Item"], fmt, sep = ".")
          x[, fmt] <- hwriter::hmakeTag("a", txt, href = txt)
        }
        x[, "Item"] <- NULL
        colnames(x) <- hwriter::hmakeTag("b", colnames(x))
        x
      }
      c(
        opm:::HTML_DOCTYPE,
        '<html>',
        opm:::html_head(title = structure(sprintf("%s: %s", pkg, x$title),
          opm = FALSE), css = sprintf("../%s/R.css", mdir), meta = NULL,
          embed = FALSE),
        '<body>',
        hwriter::hmakeTag("h1", paste(x$title, opm:::single_tag("img",
          alt = "[R logo]", src = paste0(url, "/doc/html/logo.jpg"),
          class = "toplogo"))),
        '<hr>',
        hwriter::hmakeTag("div", align = "center", hwriter::hmakeTag("a",
          href = sprintf("../%s/00Index.html", mdir), opm:::single_tag("img",
            src = paste0(url, "/doc/html/up.jpg"), alt = "[Top]",
            width = 30, height = 30, border = 0))),
        hwriter::hmakeTag("h2", sprintf("%s: %s", pkg, x$title)),
        hwriter::hwrite(result2table(x$results), table.summary = x$title),
        '</body>',
        '</html>'
      )
    }
    demo.obj <- demo(package = pkg)
    if (!nrow(demo.obj$results))
      return(FALSE)
    # 'demo' is hardcoded because this is the name used in the knitr HTML
    dir.create(outdir <- file.path(outdir, "demo"), FALSE, TRUE)
    file.copy(pkgutils::pkg_files(pkg, "demo", installed), outdir, TRUE)
    current.dir <- getwd()
    on.exit(setwd(current.dir))
    setwd(outdir)
    for (file in list.files(pattern = "\\.R$", ignore.case = TRUE))
      knitr::spin(file)
    write(demo2html(demo.obj, mdir, url), "index.html")
    TRUE
  }

  insert_links_into_demos <- function(outdir, mdir, pkg) {
    current.dir <- getwd()
    on.exit(setwd(current.dir))
    setwd(file.path(outdir, "demo"))
    files <- list.files(pattern = "\\.html$", ignore.case = TRUE)
    pkgutils::map_files(files, insert_href, parent = file.path("..", mdir),
      pkg = pkg)
  }

  store_vignettes <- function(pkg, outdir, mdir, installed, url) {
    fix_hrefs <- function(x, mdir) {
      mdir <- sprintf("../%s/", mdir)
      x <- sub("(?<=\\bsrc=\")(?=/doc/html/)", url, x, FALSE, TRUE)
      x <- sub("(?<=\\bhref=\")/doc/html/", mdir, x, FALSE, TRUE)
      x <- sub(paste0("(?<=\\bhref=\"", mdir, ")index.html(?=\")"),
        "00Index.html", x, FALSE, TRUE)
      sub("(?<=\\bhref=\")[^\"]+/doc/([^\"/]+\")", "\\1", x, FALSE, TRUE)
    }
    # 'doc' is hardcoded because this is the name used in the knitr HTML
    files <- pkgutils::pkg_files(pkg, "doc", installed)
    if (!length(files))
      return(FALSE)
    dir.create(outdir <- file.path(outdir, "doc"), FALSE, TRUE)
    file.copy(files, outdir, TRUE)
    pkgutils::map_files(file.path(outdir, "index.html"), fix_hrefs, mdir)
    TRUE
  }

  store_manual <- function(pkg, outdir, mdir, url, alt) {
    fix_hrefs <- function(x, url, mdir, alt) {
      pat <- sprintf("\\bhref=\"%s/doc/html/index.html\"", url)
      x <- sub(pat, "target=_parent href=\"../../\"", x, FALSE, TRUE)
      pat <- sprintf("\\bhref=\"%s/doc/html/packages.html\"", url)
      alt <- sprintf("../../%s/%s/index.html", alt, mdir)
      sub(pat, sprintf("target=_parent href=\"%s\"", alt), x, FALSE, TRUE)
    }
    dir.create(outdir <- file.path(outdir, mdir), FALSE, TRUE)
    current.dir <- getwd()
    on.exit(setwd(current.dir))
    setwd(outdir)
    # for some reason the evil knitr modifies the 'outdir' variable!
    knitr::knit_rd(pkg)
    alt <- if (length(alt) && nzchar(alt))
        sub(pkg, alt, dirname(outdir), FALSE, FALSE, TRUE)
      else
        dirname(outdir)
    pkgutils::map_files("00Index.html", fix_hrefs, url, mdir, alt)
  }

  alt <- basename(alt)
  backup <- outdir <- sprintf(outdir, pkg <- basename(pkg))
  dir.create(outdir, FALSE, TRUE)
  store_description(pkg, outdir, installed)
  if (!store_news(pkg, outdir, installed))
    message("no news found")
  if (!store_vignettes(pkg, outdir, mdir, installed, url))
    message("no vignettes found")
  have.demos <- store_demos(pkg, outdir, mdir, installed, url)
  if (!have.demos)
    message("no demos found")
  store_manual(pkg, outdir, mdir, url, alt)
  # the knitr call in store_manual() modifies 'outdir', hence 'backup' is used
  if (have.demos)
    insert_links_into_demos(backup, mdir, pkg)
}


################################################################################
#
# Option processing
#


option.parser <- optparse::OptionParser(option_list = list(

  optparse::make_option(c("-a", "--alternative"), type = "character",
    help = "Alternative R package to link [default: '%default']",
    metavar = "PACKAGE", default = ""),

  optparse::make_option(c("-o", "--outdir"), type = "character",
    help = "Output directory (pattern) to use [default: '%default']",
    metavar = "PATTERN", default = "%s_doc"),

  optparse::make_option(c("-m", "--mdir"), type = "character",
    help = "Manual subdirectory to create [default: '%default']",
    metavar = "NAME", default = "manual")

), usage = "%prog [options] pkgname_1 pkgname_2 ...", prog = "htmldoc.R")


opt <- optparse::parse_args(option.parser, positional_arguments = TRUE)
pkgs <- opt$args
opt <- lapply(opt$options, split_opt)


################################################################################
#
# Help message if not enough package names are provided
#


if (opt$help || !length(pkgs)) {
  optparse::print_help(option.parser)
  quit(status = 1L)
}


################################################################################


# this introduces a cyclic linking
if (!length(opt$alternative))
  opt$alternative <- c(pkgs[-1L], pkgs[1L])


invisible(mapply(package2htmldoc, pkg = pkgs, alt = opt$alternative,
  mdir = opt$mdir, outdir = opt$outdir))

