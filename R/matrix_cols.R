#' Convert between a matrix column and several vector columns
#'
#' @param .data a \code{data.frame}.
#' @param name a \code{character} of length 1 giving the name of the matrix column.  The vector columns will be formatted as "name.col", where "col" is the column name within the matrix.
#'
#' @return the input \code{data.frame} with matching vector columns combined into a matrix column (for \code{cols_to_matrix}) or the matrix column separated into vector columns (for \code{matrix_to_cols}).
#' @export
#'
#' @examples
cols_to_matrix <- function(.data, name) {

  stopifnot(is.data.frame(.data))

  # this doesn't work on tibbles.
  if (tibble::is_tibble(.data)) {
    .data <- data.frame(.data)
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
  .data
}


#' @export
#' @describeIn cols_to_matrix
matrix_to_cols <- function(.data, name) {
  stopifnot(is.data.frame(.data),
            name %in% names(.data),
            is.matrix(.data[[name]]))
  i <- which(names(.data) == name)
  cbind(.data[, seq_along(.data) < i, drop = FALSE],
        as.data.frame(.data[[name]]),
        .data[, seq_along(.data) > i, drop = FALSE])
}
