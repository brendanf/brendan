#' Shortcut to cat with/without spaces and with/without newline
#'
#' @param ... parameters to be passed to \code{\link[base]{cat}}
#' @param sep as \code{\link[base]{cat}}
#'
#' @return None (invisible \code{NULL})
#' @export
#'
#' @examples
#' cat("This", "function", "makes", "spaces, ")
#' cat0("but", "this", "function", "does", "not.")
#' catn("This", "function", "ends", "with", "a", "newline.")
#' cat0n("So", "does", "this", "one.")
cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

#' @rdname cat0
#' @export
cat0n <- function(..., sep = "") {
  cat(..., "\n", sep = "")
}

#' @rdname cat0
#' @export
catn <- function(...) {
  cat(..., "\n")
}


#' Time a command
#'
#' Prints a status string, evaluates an expression, and then prints the elapsed time
#' required.
#'
#' @param text A character string to be printed before \code{body} is executed.
#' @param body An expression.
#' @param to A connection to print the \code{text} and timing information to.
#'
#' No space is printed between the message text and the time.
#'
#' @return The result of \code{expression}, invisibly.
#' @export
#'
#' @examples
#' time_process("Sleeping for 3 seconds...", Sys.sleep(3))
time_process <- function(text, body, to = stdout()) {
  cat0(text, file = to)
  elapsed <- system.time(out <- body)
  cat0n(" ", format(as.difftime(elapsed["elapsed"], units = "secs")),
       file = to)
  return(invisible(out))
}
