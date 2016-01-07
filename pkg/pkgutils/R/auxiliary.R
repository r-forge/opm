run_ruby <- function(x, ...) UseMethod("run_ruby")

run_ruby.NULL <- function(x, ruby = "ruby", ...) {
  unname(Sys.which(L(ruby)))
}

run_ruby.numeric <- function(x, args = "-w", ruby = "ruby", ...) {
  y <- "-e'raise \"need Ruby %.1f.0 or higher\" if RUBY_VERSION.to_f < %.1f'"
  args <- c(sprintf(y, x, x), args)
  run_ruby(x = character(), args = args, ruby = ruby, ...)
}

run_ruby.character <- function(x, args = "-w", ruby = "ruby", ...) {
  if (!nzchar(ruby <- run_ruby(x = NULL, ruby = ruby)))
    stop(sprintf("cannot find executable '%s'", ruby))
  args <- c(setdiff(as.character(args), "--"), "--")
  command <- paste0(c(ruby, args, x), collapse = " ")
  do.call(system, list(command = command, ...))
}

