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
#' sum_na(c(1, 2, 3, NA))
#' sum_na(c(NA, NA, NA))

sum_na <- function(x) {
  if (all(is.na(x))) return(NA)
  sum(x, na.rm = TRUE)
}
