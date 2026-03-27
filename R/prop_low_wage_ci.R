#' Calculate Weighted Proportion of Low-Wage Jobs with Confidence Interval
#'
#' This function calculates the weighted proportion of low-wage jobs within specified groups,
#' including confidence intervals and flags for small sample sizes or high coefficients of variation.
#'
#' @param data A data frame containing the variables.
#' @param weight_var Character. Name of the weight variable. Default is "gewicht".
#' @param group_var Character vector. Names of grouping variables. Default is c("").
#' @param CV_min Numeric. Minimum acceptable coefficient of variation. Default is 5.
#' @param Konfidenz Numeric. Z-score for confidence interval. Default is qnorm(0.975).
#'
#' @examples
#' \dontrun{
#' # Example data
#' DATA <- data.frame(
#'   age = c(25, 32, 45, 55, 67),
#'   gender = c("Male", "Female", "Female", "Male", "Female"),
#'   gewicht = c(1.2, 0.8, 1.5, 1.0, 0.9),
#'   mbls = c(1200, 900, 1500, 2000, 1000)
#' )
#'
#' DATA %>%
#'   dplyr::mutate(age_group = dplyr::case_when(
#'     age < 20 ~ "Under 20",
#'     age < 30 ~ "20-29",
#'     age < 40 ~ "30-39",
#'     age < 50 ~ "40-49",
#'     age < 65 & gender == "Female" ~ "50-64/65",
#'     age < 66 & gender == "Male" ~ "50-64/65",
#'     age >= 62 ~ "65+"
#'   )) %>%
#'   LSEmetrics::prop_low_wage_ci(
#'     weight_var = "gewicht",
#'     group_var = c("age_group"),
#'     CV_min = 10
#'   )
#' }
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
prop_low_wage_ci <- function(data, weight_var = "gewicht", group_var = c(""), CV_min = 5, Konfidenz = qnorm(0.975)) {

  # Prüfen, ob Tieflohn existiert
  if (!exists("Tieflohn")) stop("Variable 'Tieflohn' muss im globalen Environment definiert sein.")

  ind_var <- "Tieflohn_ind"

  # Variablen als Symbols für dplyr
  ind_sym <- rlang::sym(ind_var)
  weight_sym <- rlang::sym(weight_var)
  group_syms <- rlang::syms(group_var)

  # 1. Tieflohn-Indikator erstellen
  data <- data %>%
    dplyr::mutate(!!ind_sym := ifelse(mbls < Tieflohn, 1, 0))

  # 2. Betriebsaggregation pro entid_n + Gruppen
  ent_agg <- data %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::mutate(Anteil_raw = sum(!!ind_sym * !!weight_sym) / sum(!!weight_sym)) %>%
    dplyr::group_by(entid_n, !!!group_syms, Anteil_raw) %>%
    dplyr::summarise(
      svhi = sum(!!weight_sym, na.rm = TRUE),
      NDhi = dplyr::n(),
      e_hi = sum(!!weight_sym * (!!ind_sym - 0.5)),
      ej_var = var(!!weight_sym * (!!ind_sym - 0.5)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      NDhi1 = NDhi - 1,
      Bhi = NDhi1 * ej_var + NDhi * (e_hi / NDhi)^2,
      Bhi = ifelse(NDhi > 1, Bhi / NDhi1, 0)
    )

  # 3. Gruppenaggregation auf allen Gruppierungsebenen
  stra_agg <- ent_agg %>%
    dplyr::group_by(!!!group_syms, Anteil_raw) %>%
    dplyr::summarise(
      Bh = sum(Bhi, na.rm = TRUE),
      svh = sum(svhi, na.rm = TRUE),
      dlh = sum(NDhi1, na.rm = TRUE),
      toth = sum(NDhi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      SV2st = Bh / svh^2,
      demi95 = sqrt(SV2st) * Konfidenz
    )

  # 4. Berechnung von CV und Anteil
  result <- stra_agg %>%
    dplyr::mutate(
      n_e = toth - dlh,
      n_s = toth,
      b_i95 = pmax(0, Anteil_raw - demi95),
      b_s95 = Anteil_raw + demi95,
      CV_sync95 = 100 * pmax(Anteil_raw - b_i95, b_s95 - Anteil_raw) / (qnorm(0.975) * Anteil_raw),
      Anteil = dplyr::case_when(
        n_s * Anteil_raw < 60 ~ "*",
        n_e * Anteil_raw < 5 ~ "*",
        CV_sync95 > CV_min * 2 ~ "*",
        CV_sync95 > CV_min ~ paste0("[", formatC(Anteil_raw * 100, format = "f", digits = 1), "]"),
        TRUE ~ formatC(Anteil_raw * 100, format = "f", digits = 1)
      )
    ) %>%
    dplyr::select(dplyr::all_of(group_var), Anteil, Anteil_raw, CV_sync95, n_e, n_s, b_i95, b_s95)

  return(result)
}
