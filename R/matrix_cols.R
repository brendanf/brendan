#' Convert between a matrix column and several vector columns
#'
#' @param .data a \code{data.frame}.
#' @param name a \code{character} of length 1 giving the name of the matrix
#'   column.  The corresponding vector columns' names are formatted as
#'   "name.col", where "col" is the column name within the matrix.
#'
#' @return A modified version of the input \code{data.frame} with matching
#'   vector columns combined into a matrix column (for \code{cols_to_matrix}) or
#'   the matrix column separated into vector columns (for
#'   \code{matrix_to_cols}).
#'
#' @details If \code{.data} is a \code{\link[tibble]{tibble}}, then
#'   \code{cols_to_matrix} will return a \code{\link[tibble]{tibble}} which does
#'   not conform to the standards, i.e., it will not pass
#'   \code{\link[tibble]{as_tibble}(validate = TRUE)}.  However, most
#'   \code{dplyr} verbs seem to work as intended. One set of exceptions is that
#'   it does not work to execute a join on matrix column(s). Joining on other
#'   columns is fine.
#' @export
#'
#' @examples
#' d <- data.frame(a = sin(1:10),
#'                 x.a = 1:10,
#'                 x.b = 10:1,
#'                 y = 2*(1:10)-3)
#' d <- cols_to_matrix(d, "x")
#' str(d)
#' d <- matrix_to_cols(d, "x")
#' str(d)
cols_to_matrix <- function(.data, name) {

  stopifnot(is.data.frame(.data))

  # This doesn't work on tibbles.
  was_tibble <- FALSE
  if (requireNamespace("tibble", quietly = TRUE)) {
    was_tibble <- tibble::is_tibble(.data)
    if (was_tibble) {
      .data <- data.frame(.data, check.names = FALSE)
    }
  }

  namepat <- paste0("^", name, "[.]")
  colsi <- grep(namepat, names(.data))

  if (length(colsi) == 0) {
    warning("No columns found matching name '", name, "'.")
    return(.data)
  }

  i <- min(colsi)
  cols <- names(.data)[colsi]
  m <- as.matrix(.data[,colsi, FALSE])
  colnames(m) <- sub(namepat, "", colnames(m))
  .data <- .data[,-colsi, FALSE]
  .data <- cbind(.data[,seq_along(.data) < i, drop = FALSE],
                 data.frame(I(m)),
                 .data[,seq_along(.data) >= i, drop = FALSE])
  names(.data)[i] <- name

  # It can be turned back in to a tibble using validate = FALSE.
  # As far as I can tell, most stuff works here?
  if (was_tibble) {
    .data <- tibble::as_tibble(.data, validate = FALSE)
  }
  .data
}


#' @export
#' @rdname cols_to_matrix
matrix_to_cols <- function(.data, name) {
  stopifnot(is.data.frame(.data),
            name %in% names(.data),
            is.matrix(.data[[name]]))
  i <- which(names(.data) == name)
  m <- .data[[name]]
  class(m) <- setdiff(class(m), "AsIs")
  m <- as.data.frame(m)
  names(m) <- paste(name, names(m), sep = ".")
  cbind(.data[, seq_along(.data) < i, drop = FALSE],
        m,
        .data[, seq_along(.data) > i, drop = FALSE])
}
