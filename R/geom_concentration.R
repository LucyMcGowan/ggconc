#' Add a concentration curve layer to a ggplot
#'
#' This geom creates a concentration curve directly from raw data, handling the
#' ranking and aggregation internally.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. Must include
#'   `x` (rank variable) and `y` (outcome variable).
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data (default: "concentration").
#' @param position Position adjustment (default: "identity").
#' @param na.rm If `FALSE` (default), missing values are removed with a warning.
#'   If `TRUE`, missing values are removed silently.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param rank_ascend Logical. If `TRUE` (default), ranks from smallest to
#'   largest values of x.
#' @param round_rank Numeric. Number of decimal places to round x before ordering
#'   and grouping. If `NULL` (default), no rounding occurs.
#' @param ... Other arguments passed on to `geom_line()`.
#' @return A ggplot2 layer.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(food_dat, aes(x = INCOME, y = X_full)) +
#'   geom_concentration(rank_ascend = TRUE, round_rank = 0) +
#'   geom_abline(slope = 1, intercept = 0, linetype = 2) +
#'   coord_equal() +
#'   theme_minimal()
geom_concentration <- function(mapping = NULL, data = NULL,
                               stat = "concentration",
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               rank_ascend = TRUE,
                               round_rank = NULL,
                               ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rank_ascend = rank_ascend,
      round_rank = round_rank,
      ...
    )
  )
}

#' Statistical transformation for concentration curves
#'
#' @rdname geom_concentration
#' @format NULL
#' @usage NULL
#' @export
StatConcentration <-
  ggplot2::ggproto("StatConcentration", ggplot2::Stat,
                   required_aes = c("x", "y"),

                   compute_group = function(data, scales, rank_ascend = TRUE,
                                            round_rank = NULL, na.rm = FALSE) {
                     # Handle missing values
                     if (!na.rm && any(!complete.cases(data[, c("x", "y")]))) {
                       warning("Removed ", sum(!complete.cases(data[, c("x", "y")])),
                               " rows containing missing values (geom_concentration).",
                               call. = FALSE)
                     }

                     data <- data[complete.cases(data[, c("x", "y")]), ]

                     if (nrow(data) == 0) {
                       return(data.frame(x = numeric(0), y = numeric(0)))
                     }

                     # Round rank variable if specified
                     if (!is.null(round_rank)) {
                       data$round_rank_var <- round(data$x, digits = round_rank)
                     } else {
                       data$round_rank_var <- data$x
                     }

                     # Group by rounded rank variable and aggregate
                     agg_data <- data |>
                       dplyr::group_by(round_rank_var) |>
                       dplyr::summarize(
                         num = dplyr::n(),
                         sum_health = sum(y),
                         .groups = "drop"
                       )

                     # If ordering descendingly, negate round_rank_var
                     if (!rank_ascend) {
                       agg_data$round_rank_var <- -agg_data$round_rank_var
                     }

                     # Order by round_rank_var
                     agg_data <- agg_data |>
                       dplyr::arrange(round_rank_var)

                     # Calculate cumulative proportions
                     agg_data <- agg_data |>
                       dplyr::mutate(
                         cumsum_num = cumsum(num),
                         cumprop_num = cumsum_num / sum(num),
                         cumsum_health = cumsum(sum_health),
                         cumprop_health = cumsum_health / sum(sum_health)
                       )

                     # Add starting point (0, 0)
                     result <- data.frame(
                       x = c(0, agg_data$cumprop_num),
                       y = c(0, agg_data$cumprop_health)
                     )

                     return(result)
                   }
  )

#' @rdname geom_concentration
#' @format NULL
#' @usage NULL
#' @export
stat_concentration <- function(mapping = NULL, data = NULL,
                               geom = "line",
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               rank_ascend = TRUE,
                               round_rank = NULL,
                               ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatConcentration,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rank_ascend = rank_ascend,
      round_rank = round_rank,
      ...
    )
  )
}
