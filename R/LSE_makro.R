#' LSE Macro Variance Estimation
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
#' LSE_makro(quant = 0.5, data = df)
#' }
LSE_makro = function(
    data,
    value_col = "mbls",             # Standardisierter Bruttomonatslohn ohne Überstunden
    weight_col="gewibgrs",          #  Standardisiertes Stichprobengewicht
    company_col="entid_n",          # Unternehmensidentifikator
    company_size_col="anzlohn",     # Anzahl Lohnangaben pro Unternehmen
    thi_col="thi",                  # Korrigierte Antwortrate intra-Unternehmen
    th_col="th",                    # Korrigierte Antwortrate inter-Unternehmen
    nrep_col="nrep",                # Anzahl der antwortenden Unternehmen pro Schicht
    street_col="stra_n",            # Schichtungsvariable
    group1_col="gr",                # Grossregion
    group2_col= "nog_2_08_pub",     # Wirtschaftsbranche, NOGA 2008 (2-stellig), Stichprobengruppierungen
    quant = 0.5
) {

  library(dplyr)

  # Weighted median
  y = w.median(data[[value_col]], data[[weight_col]], probs = quant, type = 1)

  # -------------------------------
  # Unternehmensniveau
  # -------------------------------

  company_level = data %>%
    group_by(.data[[company_col]]) %>%
    mutate(svhi = sum(.data[[weight_col]])) %>%
    mutate(zhij = ifelse(.data[[value_col]] > y, 0, 1)) %>%
    mutate(ej = .data[[weight_col]] * (zhij - 0.5)) %>%
    mutate(
      e_hi = sum(ej),
      Bhi = var(ej),
      NDhi = n()
    )

  # -------------------------------
  # Straßenniveau
  # -------------------------------

  street_level = company_level %>%
    mutate(NDhi1 = NDhi - 1) %>%
    mutate(Bhi = NDhi1 * Bhi + NDhi * (1 - NDhi / .data[[company_size_col]]) * (e_hi / NDhi)^2) %>%
    mutate(Bhi = ifelse(.data[[company_size_col]] > 1, Bhi / (.data[[company_size_col]] - 1), 0)) %>%
    mutate(Bhi = Bhi * (1 - .data[[thi_col]]) * .data[[company_size_col]]) %>%
    mutate(Bhi = ifelse(.data[[thi_col]] == 1, 0, Bhi)) %>%
    distinct(.data[[company_col]], .keep_all = TRUE) %>%
    group_by(.data[[street_col]]) %>%
    mutate(
      Bh = plus(Bhi),
      svh = sum(svhi, na.rm = TRUE),
      dlh = sum(NDhi1, na.rm = TRUE),
      toth = sum(NDhi, na.rm = TRUE)
    ) %>%
    mutate(Ah = var(e_hi), e_h = sum(e_hi)) %>%
    mutate(n_e = toth - dlh) %>%
    mutate(Ah = ifelse(
      n_e >= 1 & .data[[nrep_col]] > 1,
      ((n_e - 1) * Ah + n_e * (1 - n_e / .data[[nrep_col]]) * (e_h / n_e)^2) / (.data[[nrep_col]] - 1),
      Ah
    )) %>%
    mutate(Ahrel = Ah / (svh * svh)) %>%
    group_by(.data[[group1_col]], .data[[group2_col]]) %>%
    mutate(m_Ahrel = mean(unique(Ahrel), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Ah = ifelse(
      n_e == 1 | (n_e > 1 & .data[[nrep_col]] == 1),
      svh * svh * m_Ahrel,
      Ah
    )) %>%
    mutate(Ah = ifelse(.data[[th_col]] == 1, 0, Ah)) %>%
    mutate(V2sth = .data[[nrep_col]] * (1 - .data[[th_col]]) * Ah + .data[[th_col]] * Bh)

  theta = quant

  # -------------------------------
  # Zusammenfassung / Aggregation
  # -------------------------------

  summary_stats = street_level %>%
    group_by(.data[[street_col]]) %>%
    mutate(k = rank(.data[[company_col]])) %>%
    filter(k == 1) %>%
    ungroup() %>%
    summarize(
      SV2st = sum(V2sth, na.rm = TRUE),
      denom = sum(svh),
      dl = sum(dlh),
      totd = sum(toth)
    ) %>%
    mutate(
      SV2st = SV2st / (denom * denom),
      demi95 = sqrt(SV2st) * 1.96,
      demi = sqrt(SV2st),
      c1 = (theta - demi95) * 100,
      c2 = (theta + demi95) * 100,
      c3 = (theta - demi) * 100,
      c4 = (theta + demi) * 100,
      c3 = pmax(c3, 0),
      c1 = pmax(c1, 0),
      c4 = pmin(c4, 100),
      c2 = pmin(c2, 100)
    )

  summary_stats$b_i95 = w.median(data[[value_col]], data[[weight_col]], probs = summary_stats$c1 / 100, type = 1)
  summary_stats$b_s95 = w.median(data[[value_col]], data[[weight_col]], probs = summary_stats$c2 / 100, type = 1)

  quant_label <- if (quant == 0.5) "median" else paste0("quant_", quant)

  output = summary_stats %>%
    mutate(
      !!quant_label := y,  # dynamischer Spaltenname für Median oder Quantil
      CV_sync95 = max(y - b_i95, b_s95 - y),
      CV_sync95 = 100 * CV_sync95 / (1.96 * y),
      n_e = totd - dl,
      n_s = totd,
      vari = SV2st
    )%>%
    select(b_i95, b_s95, !!sym(quant_label), CV_sync95, n_e, n_s, vari)

  return(output)
}

