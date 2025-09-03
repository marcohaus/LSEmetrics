#' BFS Macro Variance Estimation
#'
#' Calculates macro-level variance estimates, confidence intervals,
#' and coefficient of variation (CV) based on weighted medians.
#'
#' @param quant Numeric scalar. Quantile to estimate (default = 0.5 = median).
#' @param data A \code{data.frame} containing at least:
#'   \itemize{
#'     \item \code{mbls}   - outcome variable
#'     \item \code{gewibgrs} - weights
#'     \item \code{entid_n} - entity identifier
#'     \item \code{thi}, \code{anzlohn}, \code{stra_n} - stratification vars
#'   }
#'
#' @return A \code{data.frame} with aggregated statistics, including:
#'   \itemize{
#'     \item \code{median} - weighted median
#'     \item \code{CV_sync95} - coefficient of variation
#'     \item \code{b_i95}, \code{b_s95} - lower/upper confidence bounds
#'     \item \code{vari} - variance estimate
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' set.seed(123)
#' df <- data.frame(
#'   mbls = rnorm(100, mean = 50, sd = 10),
#'   gewibgrs = runif(100, 1, 5),
#'   entid_n = rep(1:20, each = 5),
#'   thi = sample(0:1, 100, replace = TRUE),
#'   anzlohn = sample(2:10, 100, replace = TRUE),
#'   stra_n = rep(letters[1:5], each = 20)
#' )
#' BFS_makro(quant = 0.5, data = df)
#' }
LSE_makro <- function(quant = 0.5, data) {
  y <- w.median(data$mbls, data$gewibgrs, probs = quant, type = 1)
  theta <- quant
  Q975 <- qnorm(0.975)

  ent_agg <- data %>%
    group_by(entid_n) %>%
    summarise(
      svhi = sum(gewibgrs, na.rm = TRUE),
      NDhi = n(),
      e_hi = sum(gewibgrs * (ifelse(mbls > y, 0, 1) - 0.5)),
      ej_var = var(gewibgrs * (ifelse(mbls > y, 0, 1) - 0.5)),
      thi = first(thi),
      anzlohn = first(anzlohn),
      stra_n = first(stra_n),
      .groups = "drop"
    ) %>%
    mutate(
      NDhi1 = NDhi - 1,
      Bhi = NDhi1 * ej_var + NDhi * (1 - NDhi / anzlohn) * (e_hi / NDhi)^2,
      Bhi = ifelse(anzlohn > 1, Bhi / (anzlohn - 1), 0),
      Bhi = Bhi * (1 - thi) * anzlohn,
      Bhi = ifelse(thi == 1, 0, Bhi)
    )

  stra_agg <- ent_agg %>%
    group_by(stra_n) %>%
    summarise(
      Bh = plus(Bhi),
      svh = sum(svhi, na.rm = TRUE),
      dlh = sum(NDhi1, na.rm = TRUE),
      toth = sum(NDhi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      SV2st = Bh / (svh^2),
      demi95 = sqrt(SV2st) * Q975,
      demi = sqrt(SV2st),
      c1 = pmax((theta - demi95) * 100, 0),
      c2 = pmin((theta + demi95) * 100, 100)
    )

  # Vectorisierte weighted quantiles
  probs_vec <- c(stra_agg$c1 / 100, stra_agg$c2 / 100)
  w_medians <- sapply(probs_vec, function(p) w.median(data$mbls, data$gewibgrs, probs = p, type = 1))

  n <- nrow(stra_agg)
  stra_agg$b_i95 <- w_medians[1:n]
  stra_agg$b_s95 <- w_medians[(n + 1):(2 * n)]

  stra_agg <- stra_agg %>%
    mutate(
      median = y,
      CV_sync95 = 100 * pmax(median - b_i95, b_s95 - median) / (Q975 * median),
      n_e = toth - dlh,
      n_s = toth,
      vari = SV2st
    )

  return(stra_agg)
}
