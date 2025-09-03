#' Sum with NA Handling
#'
#' Computes the sum of a numeric vector but returns \code{NA}
#' if all elements are missing.
#'
#' @param x Numeric vector.
#'
#' @return Sum of \code{x} with \code{NA} handling.
#' @export
#'
#' @examples
#' plus(c(1, 2, 3, NA))
#' plus(c(NA, NA, NA))

plus <- function(x) {
  if (all(is.na(x))) return(NA)
  sum(x, na.rm = TRUE)
}
