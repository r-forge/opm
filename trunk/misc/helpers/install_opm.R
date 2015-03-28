

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
# will be prompted in interactive sessions.
#
install_opm <- function(pkg = c("pkgutils", "opm", "opmdata"),
    www = "http://www.goeker.org/opm/%s_latest.tar.gz", ...) {

  ask <- function(question) {
    if (!interactive())
      return(TRUE)
    while (TRUE) {
      answer <- tolower(substr(readline(question), 1L, 1L))
      if (answer %in% c("y", "n"))
        return(answer == "y")
      cat("Please enter 'yes' or 'no'.\n")
    }
  }

  if (!length(pkg))
    return(invisible(NULL))

  if (!"devtools" %in% rownames(installed.packages())) {
    get.it <- ask("Must install 'devtools' package to proceed. OK? ")
    if (!get.it) {
      warning("will not install ", paste0(pkg, collapse = "/"),
        ": 'devtools' package needed, but not chosen for installation")
      return(invisible(NULL))
    }
    install.packages("devtools")
  }

  pkg <- structure(sprintf(www, pkg), names = pkg)

  for (i in seq_along(pkg)) {
    get.it <- ask(sprintf("OK to install package '%s' and its dependencies? ",
      names(pkg)[[i]]))
    if (!get.it)
      return(invisible(pkg[seq_len(i - 1L)]))
    devtools::install_url(url = pkg[[i]], ...)
  }

  invisible(pkg)
}


################################################################################


install_opm()



