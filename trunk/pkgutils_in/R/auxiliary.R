


################################################################################


#' Run a Ruby script
#'
#' Run Ruby with an externally provided Ruby script or with code provided at
#' the command line with \sQuote{-e}. See \code{\link{repair_S4_docu}} and
#' \code{\link{swap_code}} as examples of functions based on this one.
#'
#' @param x Character vector containing the name of a script and optionally,
#'   after that name, the script's arguments. If a numeric vector, a required
#'   minimum Ruby version. A command is then constructed that only results if
#'   this version requirement is met. If \code{NULL}, the path to the Ruby
#'   executable is returned, or an empty string if this is not found.
#' @param args Character vector with arguments passed to Ruby before the
#'   content of \code{x}. \sQuote{--} is appended automatically. Note that any
#'   \sQuote{-e} argument would cause a character vector \code{x} to be
#'   ignored, and that otherwise an empty \code{x} character vector would cause
#'   the Ruby process to hang (wait for input that will not arrive).
#' @param ruby Character scalar containing the name of the Ruby executable. It
#'   is an error if this file is not found using \code{Sys.which}.
#' @param ... Optional arguments (except \sQuote{command}) passed to
#'   \code{system} from the \pkg{base} package.
#' @return Unless \code{x} is \code{NULL}, the result of a call to
#'   \code{system}. This is an integer scalar unless \code{\dots} dictates
#'   otherwise.
#' @keywords interface
#' @export
#' @seealso base::system base::Sys.which
#' @family auxiliary-functions
#' @examples
#' if (nzchar(run_ruby(NULL))) {
#'   # run a dummy Ruby command that does nothing
#'   (x <- run_ruby(x = character(), args = "-e'nil'"))
#'   stopifnot(identical(x, 0L))
#' } else {
#'   warning("cannot find 'ruby'")
#' }
#'
run_ruby <- function(x, ...) UseMethod("run_ruby")

#' @rdname run_ruby
#' @method run_ruby NULL
#' @export
#'
run_ruby.NULL <- function(x, ruby = "ruby", ...) {
  unname(Sys.which(L(ruby)))
}

#' @rdname run_ruby
#' @method run_ruby numeric
#' @export
#'
run_ruby.numeric <- function(x, args = "-w", ruby = "ruby", ...) {
  y <- "-e'raise \"need Ruby %.1f.0 or higher\" if RUBY_VERSION.to_f < %.1f'"
  args <- c(sprintf(y, x, x), args)
  run_ruby(x = character(), args = args, ruby = ruby, ...)
}

#' @rdname run_ruby
#' @method run_ruby character
#' @export
#'
run_ruby.character <- function(x, args = "-w", ruby = "ruby", ...) {
  if (!nzchar(ruby <- run_ruby(x = NULL, ruby = ruby)))
    stop(sprintf("cannot find executable '%s'", ruby))
  args <- c(setdiff(as.character(args), "--"), "--")
  command <- paste0(c(ruby, args, x), collapse = " ")
  do.call(system, list(command = command, ...))
}



