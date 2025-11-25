#' Weighted Median
#'
#' Calculates weighted quantiles/medians using linear or constant interpolation.
#'
#' @param x Numeric vector of values.
#' @param w Numeric vector of weights.
#' @param probs Numeric vector of probabilities (default: quartiles).
#' @param na.rm Logical; should missing values be removed?
#' @param type Interpolation type: 1 = lower, 2 = midpoint, 4 = linear.
#'
#' @return A named numeric vector with quantiles.
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 8)
#' w <- c(1, 1, 2, 2, 4)
#' w.median(x, w, probs = 0.5)
w.median <- function(x, w, probs = seq(0, 1, 0.25), na.rm = TRUE, type = 4) {
  x <- as.numeric(x)
  w <- as.numeric(w)

  if (na.rm) {
    ok <- !is.na(x) & !is.na(w)
    x <- x[ok]
    w <- w[ok]
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]


  Fx <- (cumsum(w) ) / (sum(w) )
  keep <- !duplicated(x, fromLast = TRUE)
  x <- x[keep]
  Fx <- Fx[keep]

  method <- switch(as.character(type),
                   `1` = "constant",
                   `2` = "constant",
                   `4` = "linear")

  f_val <- switch(as.character(type),
                  `1` = 1,
                  `2` = 0.5,
                  `4` = NA)

  out <- approx(Fx, x, xout = probs, method = method, f = f_val,
                ties = "ordered", rule = 2)
  result <- out$y
  names(result) <- paste0(format(100 * probs, trim = TRUE), "%")
  return(result)
}
