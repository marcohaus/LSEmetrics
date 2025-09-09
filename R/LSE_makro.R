#' Berechnung von Lohnquantilen und Konfidenzintervallen nach BFS-Makromethode
#'
#' \code{LSE_makro()} berechnet gewichtete Lohnquantile (z. B. Median) sowie
#' deren Varianzschätzung und Konfidenzintervalle gemäss der BFS-Makromethode.
#' Die Berechnungen berücksichtigen Unternehmens- und Strassenniveau sowie
#' Gewichtungs- und Korrekturfaktoren.
#'
#' @param data Ein \code{data.frame} oder \code{tibble}, das die notwendigen Variablen enthält.
#' @param quant Numerisch, das gewünschte Quantil (Standard = \code{0.5}, Median).
#' @param value_col Zeichenkette, Name der Spalte mit den Lohnangaben
#'   (Standard: \code{"mbls"} = standardisierter Bruttomonatslohn ohne Überstunden).
#' @param weight_col Zeichenkette, Name der Spalte mit den Stichprobengewichten
#'   (Standard: \code{"gewibgrs"}).
#' @param company_col Zeichenkette, Unternehmensidentifikator (Standard: \code{"entid_n"}).
#' @param company_size_col Zeichenkette, Anzahl der Lohnangaben pro Unternehmen
#'   (Standard: \code{"anzlohn"}).
#' @param thi_col Zeichenkette, Spalte mit korrigierter Antwortrate intra-Unternehmen
#'   (Standard: \code{"thi"}).
#' @param th_col Zeichenkette, Spalte mit korrigierter Antwortrate inter-Unternehmen
#'   (Standard: \code{"th"}).
#' @param nrep_col Zeichenkette, Spalte mit der Anzahl der antwortenden Unternehmen
#'   pro Schicht (Standard: \code{"nrep"}).
#' @param stra_col Zeichenkette, Schichtungsvariable (Standard: \code{"stra_n"}).
#' @param group1_col Zeichenkette, erste Gruppierungsvariable (Standard: \code{"gr"}).
#' @param group2_col Zeichenkette, zweite Gruppierungsvariable, typischerweise
#'   Branchenklassifikation (Standard: \code{"nog_2_08_pub"}).
#'
#' @details
#' Die Funktion implementiert die vom BFS verwendete Makromethode zur
#' Varianzschätzung von Lohnquantilen. Sie arbeitet in mehreren Stufen:
#' \enumerate{
#'   \item Berechnung des gewichteten Quantils mit \code{w.median}.
#'   \item Aggregation auf Unternehmensebene.
#'   \item Hochrechnung und Varianzschätzung auf Strassenniveau
#'         unter Berücksichtigung von Korrekturfaktoren.
#'   \item Aggregation und Berechnung von Konfidenzintervallen.
#' }
#'
#' @return Ein \code{data.frame} mit folgenden Spalten:
#' \itemize{
#'   \item \code{b_i95}: Untere Grenze des 95\%-Konfidenzintervalls.
#'   \item \code{b_s95}: Obere Grenze des 95\%-Konfidenzintervalls.
#'   \item \code{median} oder \code{quant_xx}: Das geschätzte Quantil.
#'   \item \code{CV_sync95}: Variationskoeffizient.
#'   \item \code{n_e}: Effektive Stichprobengrösse.
#'   \item \code{n_s}: Gesamtstichprobengrösse.
#'   \item \code{vari}: Geschätzte Varianz.
#' }
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(purrr)
#'
#' # Einfacher Aufruf: Medianlohn für TG, privater Sektor
#' DATEN %>%
#'   filter(arbkto == "TG", privoef == 1) %>%
#'   LSE_makro(quant = 0.5)
#'
#' # Beispiel mit Gruppierung: 90%-Quantil nach Geschlecht und Beruf
#' DATEN %>%
#'   filter(arbkto == "TG", privoef == 1) %>%
#'   group_split(geschle, berufst) %>%
#'   map_dfr(~ LSE_makro(.x, quant = 0.9) %>%
#'             mutate(
#'               geschle = unique(.x$geschle),
#'               berufst = unique(.x$berufst)
#'             ))
#'
#' # Mehrere Quantile (25%, 50%, 75%) nach Geschlecht und Beruf
#' DATEN %>%
#'   filter(arbkto == "TG", privoef == 1) %>%
#'   group_split(geschle, berufst) %>%
#'   map_dfr(~ {
#'     map_dfr(c(0.25, 0.5, 0.75), function(q) {
#'       LSE_makro(.x, quant = q) %>%
#'         mutate(
#'           geschle = unique(.x$geschle),
#'           berufst = unique(.x$berufst),
#'           quantile = q
#'         )
#'     })
#'   })
#' }
#' @export
LSE_makro = function(
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
    group2_col= "nog_2_08_pub"      # Wirtschaftsbranche, NOGA 2008 (2-stellig), Stichprobengruppierungen
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
    group_by(.data[[stra_col]]) %>%
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

