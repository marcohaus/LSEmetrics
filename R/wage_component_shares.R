#' Calculate Weighted Wage Component Shares, Optional by Groups
#'
#' This function calculates the weighted shares of different wage components
#' (e.g., overtime, allowances, bonuses) relative to total gross wages.
#' The calculation can optionally be performed by one or more grouping variables.
#'
#' @param data A \code{data.frame} or \code{tibble} containing the required variables.
#' @param weight_var Character. Name of the weighting variable (default: \code{"gewicht"}).
#' @param group_var Character vector. Optional grouping variables.
#'   Default: \code{NULL} (no grouping).
#'
#' @details
#' The function first computes weighted aggregate sums of the individual wage components.
#' It then derives their percentage shares relative to total gross wages.
#'
#' Included components:
#' \itemize{
#'   \item Overtime pay (\code{verduz})
#'   \item Allowances (\code{zulagen})
#'   \item 13th month salary (converted to a monthly basis)
#'   \item Bonuses / special payments (converted to a monthly basis)
#' }
#'
#' The base wage is calculated as a residual:
#' \deqn{Base wage = 100 - sum(other components)}
#'
#' If \code{group_var} is provided, the calculation is performed separately
#' for each group.
#'
#' @return A \code{data.frame} with the following columns:
#' \itemize{
#'   \item Grouping variables (if specified)
#'   \item \code{overtime}: Share of overtime pay in %
#'   \item \code{allowances}: Share of allowances in %
#'   \item \code{thirteenth_month}: Share of 13th month salary in %
#'   \item \code{bonuses}: Share of bonuses in %
#'   \item \code{base_wage}: Residual share in %
#' }
#'
#' All shares are rounded to one decimal place.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Without grouping
#' wage_component_shares(DATA)
#'
#' # By gender
#' wage_component_shares(DATA, group_var = "gender")
#'
#' # By multiple variables
#' wage_component_shares(DATA, group_var = c("gender", "industry"))
#' }
#'
#' @export
wage_component_shares <- function(data, weight_var = "gewicht", group_var = NULL) {

  # Optional grouping
  if (!is.null(group_var)) {
    data <- data %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_var)))
  }

  data %>%
    dplyr::summarise(
      gross_agg        = sum(blimok * .data[[weight_var]], na.rm = TRUE),
      overtime_agg     = sum(verduz * .data[[weight_var]], na.rm = TRUE),
      allowances_agg   = sum(zulagen * .data[[weight_var]], na.rm = TRUE),
      thirteenth_agg   = sum((xiiimloh / 12) * .data[[weight_var]], na.rm = TRUE),
      bonuses_agg      = sum((sonderza / 12) * .data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      overtime       = overtime_agg / gross_agg * 100,
      allowances     = allowances_agg / gross_agg * 100,
      thirteenth_month = thirteenth_agg / gross_agg * 100,
      bonuses        = bonuses_agg / gross_agg * 100
    ) %>%
    dplyr::select(dplyr::any_of(group_var), overtime, allowances, thirteenth_month, bonuses) %>%
    dplyr::mutate(
      dplyr::across(dplyr::c(overtime, allowances, thirteenth_month, bonuses), ~ round(.x, 1)),
      base_wage = 100 - rowSums(dplyr::across(dplyr::c(overtime, allowances, thirteenth_month, bonuses)))
    )
}
