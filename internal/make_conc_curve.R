#' Plot a concentration curve
#' This function plots a concentration curve using \code{ggplot2}
#'
#' @param conc_data pre-processed dataset of ordered, aggregate data returned by \code{make_conc_data} function
#' @return A \code{ggplot} object of the concentration curve.
#' @export
make_conc_curve = function(conc_data) {
  ## Create plot
  if("Group" %in% colnames(conc_data)) {
    conc_data |>
      ggplot(aes(x = cumprop_num, y = cumprop_health, color = Group)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, color = "black") +
      geom_line(linewidth = 1.2, alpha = 1) +
      coord_equal() +
      theme_minimal(base_size = 14) +
      labs(x = "Cumulative Proportion of Observations\n(Ranked by X)",
           y = "Cumulative Proportion of Y") +
      theme(axis.title = element_text(color = "black", face = "bold"),
            legend.title = element_text(color = "black", face = "bold"),
            legend.position = "top") +
      scale_x_continuous(labels = scales::percent_format()) +
      scale_y_continuous(labels = scales::percent_format())
  } else {
    conc_data |>
      ggplot(aes(x = cumprop_num, y = cumprop_health)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, color = "black") +
      geom_line(linewidth = 1.2, alpha = 1) +
      coord_equal() +
      theme_minimal(base_size = 14) +
      labs(x = "Cumulative Proportion of Observations\n(Ranked by X)",
           y = "Cumulative Proportion of Y") +
      theme(axis.title = element_text(color = "black", face = "bold"),
            legend.title = element_text(color = "black", face = "bold"),
            legend.position = "top") +
      scale_x_continuous(labels = scales::percent_format()) +
      scale_y_continuous(labels = scales::percent_format())
  }
}
