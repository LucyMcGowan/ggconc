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


StatConcentration <-
  ggplot2::ggproto("StatConcentration", ggplot2::Stat,
                   required_aes = c("x", "y"),

                   compute_group = function(data, scales, rank_ascend = TRUE,
                                            round_rank = NULL, na.rm = FALSE) {
                     if (na.rm) {
                       data <- data[complete.cases(data[, c("x", "y")]), , drop = FALSE]
                     } else {
                       if (any(!complete.cases(data[, c("x", "y")]))) {
                         warning("Removed rows with missing x or y (geom_concentration).", call. = FALSE)
                       }
                       data <- data[complete.cases(data[, c("x", "y")]), , drop = FALSE]
                     }

                     if (nrow(data) == 0) return(data.frame(x = numeric(0), y = numeric(0)))

                     if (!is.null(round_rank)) {
                       rr <- round(data$x, digits = round_rank)
                     } else {
                       rr <- data$x
                     }

                     keys <- as.character(rr)

                     counts <- tapply(data$y, keys, length)
                     sums   <- tapply(data$y, keys, sum)

                     agg <- data.frame(
                       round_rank_var = names(counts),
                       num = as.integer(unname(counts)),
                       sum_health = as.numeric(unname(sums)),
                       stringsAsFactors = FALSE
                     )

                     rr_num <- suppressWarnings(as.numeric(agg$round_rank_var))
                     if (!any(is.na(rr_num))) agg$round_rank_var <- rr_num

                     if (is.numeric(agg$round_rank_var)) {
                       if (!rank_ascend) agg$round_rank_var <- -agg$round_rank_var
                       agg <- agg[order(agg$round_rank_var), , drop = FALSE]
                     } else {
                       agg <- agg[order(agg$round_rank_var), , drop = FALSE]
                       if (!rank_ascend) agg <- agg[rev(seq_len(nrow(agg))), , drop = FALSE]
                     }

                     total_num <- sum(agg$num)
                     total_health <- sum(agg$sum_health)
                     if (total_num == 0 || total_health == 0) return(data.frame(x = 0, y = 0))

                     cumnum <- cumsum(agg$num) / total_num
                     cumsum_health <- cumsum(agg$sum_health) / total_health

                     data.frame(
                       x = c(0, as.numeric(cumnum)),
                       y = c(0, as.numeric(cumsum_health))
                     )
                   }
  )


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
