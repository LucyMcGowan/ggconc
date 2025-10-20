#' Construct dataset to build a concentration curve
#' This function creates an ordered, aggregate (where applicable) dataset from which to build a concentration curve
#'
#' @param data dataset containing at least \code{outcome_var} and \code{rank_var}
#' @param outcome_var variable name of the outcome, to be passed through to \code{dplyr} (so no quotation marks)
#' @param rank_var variable name of the disadvantage or socioeconomic exposure used to group, to be passed through to \code{dplyr} (so no quotation marks)
#' @param rank_ascend logical, if \code{TRUE} (the default) then rows are ranked from smallest to largest values of \code{rank_var}
#' @param round_rank (optional) numeric, use to round \code{rank_var} before ordering and grouping
#' @return A \code{data.frame} is returned:
#' \item{delta}{Estimated average treatment effect on the primary outcome.}
#' @export
make_conc_data = function(data, outcome_var, rank_var, rank_ascend = TRUE, round_rank = 2) {
  # Round rank_var based on round_rank
  data = data |>
    dplyr::mutate(round_rank_var = round(x = {{rank_var}}, digits = round_rank))
  # Group by round_rank_var and calculate counts/sum of health per value
  data = data |>
    dplyr::group_by(round_rank_var) |>
    dplyr::summarize(num = n(),
                     sum_health = sum({{outcome_var}}))
  # If ordering descendingly, negate round_rank_var
  if (!rank_ascend) {
    data = data |>
      dplyr::mutate(round_rank_var = - round_rank_var)
  }
  # Then order by round_rank_var
  data = data |>
    dplyr::arrange(round_rank_var)
  # Calculate cumulative sums and proportions in the ordered data
  data = data |>
    dplyr::mutate(cumsum_num = cumsum(num),
                  cumprop_num = cumsum_num / sum(num),
                  cumsum_health = cumsum(sum_health),
                  cumprop_health = cumsum_health / sum(sum_health))
  # Create a row of zeros to complete the curve
  row_of_zeros = data.frame(num = 0,
                            sum_health = 0,
                            cumsum_num = 0,
                            cumprop_num = 0,
                            cumsum_health = 0,
                            cumprop_health = 0)
  # And add it to the end of the ordered data before calculating the CI oordinate
  data = data |>
    dplyr::bind_rows(row_of_zeros) |>
    dplyr::mutate(CI = cumprop_num * dplyr::lead(x = cumprop_health, n = 1, default = 0) -
                    cumprop_health * dplyr::lead(x = cumprop_num, n = 1, default = 0))
  # Return the data
  return(data)
}
