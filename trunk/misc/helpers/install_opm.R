

################################################################################
#
# (C) 2015 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


# Helper function for installing opm and friends. 'pkg' is character vector of
# package names, 'www' a template for the URL in which the package name is
# inserted, and '...' are arguments passed to devtools::install_url(). Users
# will be prompted in interactive sessions unless they set 'confirm' to 'FALSE'.
#
install_opm <- function(pkg = c("pkgutils", "opm", "opmdata"),
    www = "http://www.goeker.org/opm/%s_latest.tar.gz",
    confirm = interactive(), ...) {

  install.all <- !confirm
  choices <- "Please enter 'all', 'yes' or 'no': "

  ask <- function(question) {
    if (install.all)
      return(TRUE)
    while (TRUE) {
      answer <- tolower(substr(readline(question), 1L, 1L))
      if (answer %in% c("y", "n"))
        return(answer == "y")
      if (answer == "a") {
        install.all <<- TRUE
        return(TRUE)
      }
    }
  }

  if (!length(pkg))
    return(invisible(NULL))

  if (!"devtools" %in% rownames(installed.packages())) {
    get.it <- ask(paste("Must install 'devtools' package to proceed. OK?",
      choices))
    if (!get.it) {
      warning("will not install ", paste0("'", pkg, "'", collapse = "/"),
        ": 'devtools' package needed, but not chosen for installation")
      return(invisible(NULL))
    }
    install.packages("devtools")
  }

  pkg <- structure(sprintf(www, pkg), names = pkg)

  for (i in seq_along(pkg)) {
    get.it <- ask(sprintf("OK to install package '%s' and its dependencies? %s",
      names(pkg)[[i]], choices))
    if (!get.it)
      return(invisible(pkg[seq_len(i - 1L)]))
    devtools::install_url(url = pkg[[i]], ...)
  }

  invisible(pkg)
}


################################################################################


install_opm()



