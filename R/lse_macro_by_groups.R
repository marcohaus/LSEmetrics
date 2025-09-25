#' Generate LSE Macro Estimates for Group Combinations
#'
#' This function computes macro-level LSE estimates for a dataset across all
#' combinations of specified grouping variables. It iteratively applies the
#' `lse_macro` function to each subgroup and specified quantile(s),
#' appending group identifiers or a total label for groups not included in
#' a particular combination.
#'
#' @param df A data frame containing the dataset to analyze.
#' @param group_vars A character vector of column names to use for grouping.
#' @param quants Numeric vector of quantiles to compute. Default is 0.5 (median).
#' @param total_labels Character string to use for total/aggregated groups.
#'   Default is `"TOTAL"`.
#' @param value_col Character string specifying the column with the main values
#'   for LSE calculation. Default is `"mbls"`.
#' @param weight_col Character string specifying the column with weights. Default is `"gewibgrs"`.
#' @param company_col Character string specifying the column identifying companies. Default is `"entid_n"`.
#' @param company_size_col Character string specifying the column for company size. Default is `"anzlohn"`.
#' @param thi_col Character string specifying the THI column. Default is `"thi"`.
#' @param th_col Character string specifying the TH column. Default is `"th"`.
#' @param nrep_col Character string specifying the column with replication counts. Default is `"nrep"`.
#' @param stra_col Character string specifying the strategy column. Default is `"stra_n"`.
#' @param group1_col Character string for an additional grouping variable. Default is `"gr"`.
#' @param group2_col Character string for another grouping variable. Default is `"nog_2_08_pub"`.
#'
#' @return Ein \code{data.frame} mit folgenden Spalten:
#' \itemize{
#'   \item \code{quant}: Quantile (0.5 = median)
#'   \item \code{value}: Estimated value
#'   \item \code{b_i95}: Lower bound of the 95% confidence interval
#'   \item \code{b_s95}: Upper bound of the 95% confidence interval
#'   \item \code{CV_sync95}: Synthetic coefficient of variation based on the variance index (95%)
#'   \item \code{vari}: Estimated variance
#'   \item \code{n_e}: Number of companies included in the calculation
#'   \item \code{n_s}: Number of wages included in the calculation
#'   \item \code{..}: Grouping variables
#' }
#'
#' @details The function first generates all combinations of the provided
#'   grouping variables, from the full set down to no grouping (total).
#'   For each combination, the dataset is split into subgroups, and
#'   `lse_macro` is applied to each subgroup for each requested quantile.
#'   Any errors encountered during computation are silently skipped (returning
#'   `NULL` for that subgroup). The resulting data frames are combined
#'   into a single output.
#'
#' @examples
#' \dontrun{
#'df<-DATA %>%
#'  filter(arbkto == "TG", privoef == 1, nog_2_08 %in% c(86,69)) %>%
#'  lse_macro_by_groups(
#'    group_vars = c( "berufst","geschle","nog_2_08"),
#'    quants = 0.5
#'  )
#'result<-df %>%
#'  mutate(median_char=
#'           case_when(
#'             vari >0.05 ~ paste0("[",value,"]"),
#'             n_s<60  ~ "...",
#'             n_e<5  ~ "...",
#'             .default = as.character(value)
#'           )
#'  ) %>% select(-c(b_i95 ,b_s95 ,value ,CV_sync95  , n_e   ,n_s,vari )) %>%
#'  pivot_wider(
#'    names_from = berufst ,
#'    values_from = median_char,
#'    values_fill = "-"
#'  )
#' }
#' @importFrom purrr map_dbl
#' @export
lse_macro_by_groups <- function(
                                    df,
                                    group_vars,
                                    quants = 0.5,
                                    total_labels = "TOTAL",
                                    value_col = "mbls",
                                    weight_col="gewibgrs",
                                    company_col="entid_n",
                                    company_size_col="anzlohn",
                                    thi_col="thi",
                                    th_col="th",
                                    nrep_col="nrep",
                                    stra_col="stra_n",
                                    group1_col="gr",
                                    group2_col= "nog_2_08_pub"
                                    ) {


  combos <- map(0:length(group_vars), ~combn(group_vars, m = length(group_vars) - .x, simplify = FALSE)) %>%
    flatten()
  map_dfr(combos, function(grps) {
    grouped <- df %>% group_by(across(all_of(grps)))
    keys <- grouped %>% group_keys()
    splits <- grouped %>% group_split()
    map2_dfr(splits, seq_len(nrow(keys)), function(data, i) {
      if (nrow(data) == 0) return(NULL)
      key_row <- keys[i, , drop = FALSE]
      map_dfr(quants, function(q) {
        result <- tryCatch(
          lse_macro(data,
                    quant = q,
                    value_col = value_col,
                    weight_col=weight_col,
                    company_col=company_col,
                    company_size_col=company_size_col,
                    thi_col=thi_col,
                    th_col=th_col,
                    nrep_col=nrep_col,
                    stra_col=stra_col,
                    group1_col=group1_col,
                    group2_col= group2_col
                    ),
          error = function(e) return(NULL)
        )
        if (is.null(result)) return(NULL)
        for (var in group_vars) {
          result[[var]] <- if (var %in% grps) key_row[[var]] else total_labels
        }
        result %>%
          mutate(across(all_of(group_vars), as.character))
      })
    })
  })
}
