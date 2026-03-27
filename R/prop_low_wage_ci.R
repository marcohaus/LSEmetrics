#' Calculate Weighted Proportion of Low-Wage Jobs with Confidence Interval
#'
#' This function computes the weighted proportion of low-wage jobs in a dataset,
#' optionally grouped by one or more variables, and provides a confidence interval
#' for the estimate. It also allows for filtering groups with a small sample size
#' based on a minimum coefficient of variation (CV).
#'
#' @param data A data frame containing the survey or employment data.
#' @param weight_var Character. Name of the variable containing survey weights. Default is "gewicht".
#' @param group_var Character vector. Names of variables to group by. Default is an empty vector (no grouping).
#' @param CV_min Numeric. Minimum acceptable coefficient of variation for reporting the estimate. Default is 5.
#' @param Konfidenz Numeric. Z-value for constructing the confidence interval (e.g., 0.975 for 95% CI). Default is qnorm(0.975).
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{group}{Grouping variable(s) if specified.}
#'   \item{prop_low_wage}{Weighted proportion of low-wage jobs.}
#'   \item{ci_lower}{Lower bound of the confidence interval.}
#'   \item{ci_upper}{Upper bound of the confidence interval.}
#'   \item{cv}{Coefficient of variation.}
#' }
#'
#' @details
#' This function is designed for survey or administrative data where weighted estimates
#' are needed. Groups with a CV below `CV_min` can be flagged or omitted to ensure
#' reliability. The confidence interval is calculated using the normal approximation
#' method based on the provided `Konfidenz` level.
#'
#' @examples
#' \dontrun{
#' # -----------------------------
#' # Example 1: Without grouping
#' # -----------------------------
#' result_all <- wage_component_shares(DATA)
#' print(result_all)
#'
#' # -----------------------------
#' # Example 2: By gender
#' # -----------------------------
#' result_gender <- wage_component_shares(DATA, group_var = "gender")
#' print(result_gender)
#'
#' # -----------------------------
#' # Example 3: By multiple variables
#' # -----------------------------
#' result_multi <- wage_component_shares(DATA, group_var = c("gender", "industry"))
#' print(result_multi)
#'
#' # -----------------------------
#' # Optional: Create age groups before calling function
#' # -----------------------------
#' DATA$age_group <- dplyr::case_when(
#'   DATA$age < 20 ~ "Under 20",
#'   DATA$age < 30 ~ "20-29",
#'   DATA$age < 40 ~ "30-39",
#'   DATA$age < 50 ~ "40-49",
#'   DATA$age < 65 & DATA$gender == "Female" ~ "50-64/65",
#'   DATA$age < 66 & DATA$gender == "Male" ~ "50-64/65",
#'   DATA$age >= 62 ~ "65+"
#' )
#'
#' result_age <- wage_component_shares(DATA, group_var = "age_group")
#' print(result_age)
#' }
#' @export


wage_component_shares <- function(data, weight_var = "gewicht", group_var = NULL) {

  # Optional grouping
  if (!is.null(group_var)) {
    data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(group_var)))
  }

  out <- dplyr::summarise(
    data,
    gross_agg        = sum(.data$blimok * .data[[weight_var]], na.rm = TRUE),
    overtime_agg     = sum(.data$verduz * .data[[weight_var]], na.rm = TRUE),
    allowances_agg   = sum(.data$zulagen * .data[[weight_var]], na.rm = TRUE),
    thirteenth_agg   = sum((.data$xiiimloh / 12) * .data[[weight_var]], na.rm = TRUE),
    bonuses_agg      = sum((.data$sonderza / 12) * .data[[weight_var]], na.rm = TRUE),
    .groups = "drop"
  )

  out$overtime        <- out$overtime_agg / out$gross_agg * 100
  out$allowances      <- out$allowances_agg / out$gross_agg * 100
  out$thirteenth_month <- out$thirteenth_agg / out$gross_agg * 100
  out$bonuses         <- out$bonuses_agg / out$gross_agg * 100

  # Select relevant columns
  out <- dplyr::select(
    out,
    dplyr::any_of(group_var),
    "overtime", "allowances", "thirteenth_month", "bonuses"
  )

  # Round and calculate base wage
  out <- dplyr::mutate(
    out,
    dplyr::across(dplyr::c("overtime", "allowances", "thirteenth_month", "bonuses"),
                  ~ round(.x, 1)),
    base_wage = 100 - rowSums(
      dplyr::across(dplyr::c("overtime", "allowances", "thirteenth_month", "bonuses"))
    )
  )

  return(out)
}
