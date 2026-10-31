#' Input checks
#'
#' Internal helpers that stop with an informative message when an argument
#' is not of the expected form.
#'
#' @param data Object to check as an age-by-period matrix.
#' @param x Value to check.
#' @param arg Name of the argument, used in the error message.
#' @param positive Logical; whether `x` must be strictly positive.
#' @return `check_apc_matrix()` returns `data` as a numeric matrix; the other
#'   helpers return `x` invisibly.
#' @noRd
check_apc_matrix <- function(data) {
  if (is.data.frame(data)) {
    if (!all(vapply(data, is.numeric, logical(1)))) {
      stop("`data` must be a numeric matrix with ages as rows and periods ",
           "as columns; the data frame supplied has non-numeric columns.",
           call. = FALSE)
    }
    data <- as.matrix(data)
  }
  if (!is.matrix(data) || !is.numeric(data)) {
    stop("`data` must be a numeric matrix with ages as rows and periods ",
         "as columns.", call. = FALSE)
  }
  if (nrow(data) < 1 || ncol(data) < 1) {
    stop("`data` must have at least one row and one column.", call. = FALSE)
  }
  data
}

#' @rdname check_apc_matrix
#' @noRd
check_number <- function(x, arg, positive = FALSE) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    stop("`", arg, "` must be a single finite number.", call. = FALSE)
  }
  if (positive && x <= 0) {
    stop("`", arg, "` must be positive.", call. = FALSE)
  }
  invisible(x)
}

#' @rdname check_apc_matrix
#' @noRd
check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}
