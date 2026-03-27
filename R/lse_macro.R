#' Calculation of Wage Quantiles and Confidence Intervals Using BFS Macro Method
#'
#' \code{LSE_macro()} calculates weighted wage quantiles (e.g., the median) as well as
#' their variance estimates and confidence intervals according to the BFS method.
#' The calculations take into account company-level factors, stratification, as well as
#' weighting and adjustment factors.
#'
#' @param data A \code{data.frame} or \code{tibble} containing the necessary variables.
#' @param quant Numeric, the desired quantile (default = \code{0.5}, median).
#' @param value_col Character, name of the column with wage values
#'   (default: \code{"mbls"} = standardized gross monthly wage without overtime).
#' @param weight_col Character, name of the column with survey weights
#'   (default: \code{"gewibgrs"}).
#' @param company_col Character, company identifier (default: \code{"entid_n"}).
#' @param company_size_col Character, number of wages per company
#'   (default: \code{"anzlohn"}).
#' @param thi_col Character, column with corrected intra-company response rate
#'   (default: \code{"thi"}).
#' @param th_col Character, column with corrected inter-company response rate
#'   (default: \code{"th"}).
#' @param nrep_col Character, column with the number of responding companies
#'   per stratum (default: \code{"nrep"}).
#' @param stra_col Character, stratification variable (default: \code{"stra_n"}).
#' @param group1_col Character, first grouping variable (default: \code{"gr"}).
#' @param group2_col Character, second grouping variable, typically industry classification
#'   (default: \code{"nog_2_08_pub"}).
#' @param type Method used for median calculation.
#'   (default: \code{"1"}).
#'
#' @details
#' The function implements the macro method used by BFS for variance estimation of wage quantiles.
#' It works in several steps:
#' \enumerate{
#'   \item Calculation of the weighted quantile using \code{w.median}.
#'   \item Aggregation at the company level.
#'   \item Extrapolation and variance estimation for stratified groups including adjustment factors.
#'   \item Aggregation and calculation of confidence intervals.
#' }
#'
#' @return A \code{data.frame} with the following columns:
#' \itemize{
#'   \item \code{quant}: Quantile (0.5 = median)
#'   \item \code{value}: Estimated value
#'   \item \code{b_i95}: Lower bound of the 95% confidence interval
#'   \item \code{b_s95}: Upper bound of the 95% confidence interval
#'   \item \code{CV_sync95}: Synthetic coefficient of variation based on the variance index (95%)
#'   \item \code{vari}: Estimated variance
#'   \item \code{n_e}: Number of companies included in the calculation
#'   \item \code{n_s}: Number of wages included in the calculation
#' }
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(purrr)
#'
#' # Simple call: Median wage for TG, private sector
#' DATA %>%
#'   filter(arbkto == "TG", privoef == 1) %>%
#'   lse_macro(quant = 0.5)
#'
#' # Example with grouping: 90% quantile by gender and occupation
#' DATA %>%
#'   filter(arbkto == "TG", privoef == 1) %>%
#'   group_split(geschle, berufst) %>%
#'   map_dfr(~ lse_macro(.x, quant = 0.5) %>%
#'             mutate(
#'               geschle = unique(.x$geschle),
#'               berufst = unique(.x$berufst)
#'             ))
#'
#' # Multiple quantiles (25%, 50%, 75%) by gender and occupation
#' DATA %>%
#'   filter(arbkto == "TG", privoef == 1) %>%
#'   group_split(geschle, berufst) %>%
#'   map_dfr(~ {
#'     map_dfr(c(0.25, 0.5, 0.75), function(q) {
#'       lse_macro(.x, quant = q) %>%
#'         mutate(
#'           geschle = unique(.x$geschle),
#'           berufst = unique(.x$berufst)
#'         )
#'     })
#'   })
#' }
#' @export
lse_macro = function(
    data,
    quant = 0.5,
    value_col = "mbls",             # Standardisierter Bruttomonatslohn ohne Überstunden
    weight_col="gewibgrs",          # Standardisiertes Stichprobengewicht
    company_col="entid_n",          # Unternehmensidentifikator
    company_size_col="anzlohn",     # Anzahl Lohnangaben pro Unternehmen
    thi_col="thi",                  # Korrigierte Antwortrate intra-Unternehmen
    th_col="th",                    # Korrigierte Antwortrate inter-Unternehmen
    nrep_col="nrep",                # Anzahl der antwortenden Unternehmen pro Schicht
    stra_col="stra_n",              # Schichtungsvariable
    group1_col="gr",                # Grossregion
    group2_col= "nog_2_08_pub",      # Wirtschaftsbranche, NOGA 2008 (2-stellig), Stichprobengruppierungen
    type = 1
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
  # Stratum
  # -------------------------------

  street_level = company_level %>%
    mutate(NDhi1 = NDhi - 1) %>%
    mutate(Bhi = NDhi1 * Bhi + NDhi * (1 - NDhi / .data[[company_size_col]]) * (e_hi / NDhi)^2) %>%
    mutate(Bhi = ifelse(.data[[company_size_col]] > 1, Bhi / (.data[[company_size_col]] - 1), 0)) %>%
    mutate(Bhi = Bhi * (1 - .data[[thi_col]]) * .data[[company_size_col]]) %>%
    mutate(Bhi = ifelse(.data[[thi_col]] == 1, 0, Bhi)) %>%
    distinct(.data[[company_col]], .keep_all = TRUE) %>%
    group_by(.data[[stra_col]]) %>%
    mutate(
      Bh = sum_na(Bhi),
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
    group_by(.data[[stra_col]]) %>%
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



  output = summary_stats %>%
    mutate(
      quant=quant,
      value= y,
      CV_sync95 = max(y - b_i95, b_s95 - y),
      CV_sync95 = 100 * CV_sync95 / (1.96 * y),
      n_e = totd - dl,
      n_s = totd,
      vari = SV2st
    )%>%
    select(quant,value,b_i95, b_s95,  CV_sync95,vari, n_e, n_s)

  return(output)
}
