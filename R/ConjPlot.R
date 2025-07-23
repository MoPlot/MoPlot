#' Create combined plots for interaction contrasts
#'
#' This function is for internal usage
#'
#' @param results A list of contrast data frame
#' @param modellino A fitted model object
#' @param sig.level Significance level threshold. Default is 0.05
#' @param colonne Number of columns in the patchwork layout. Default is 2
#' @param focus Logical: if `TRUE`, prints each plot separately; if `FALSE`, returns a combined plot.
#'
#' @keywords internal
#' @importFrom patchwork wrap_plots
ConjPlot <- function(results, modellino, sig.level = 0.05, colonne = 2, focus = FALSE) {
  plot_list <- list()
  model_summary <- summary(modellino)

  coeff_table <- model_summary$coefficients
  interaction_p_values <- coeff_table[grep(":", rownames(coeff_table)), "Pr(>|t|)"]
  interaction_p_values <- as.vector(interaction_p_values)

  for (i in seq_along(results)) {
    contrast_data <- results[[i]]
    p <- PlotContrast(contrast_data, pvalue = interaction_p_values[i], sig = sig.level)
    plot_list[[i]] <- p
  }

  if (focus) {
    for (p in plot_list) {
      print(p)
    }
    return(invisible(NULL))
  } else {
    combined_plot <- wrap_plots(plot_list, ncol = colonne)
    return(combined_plot)
  }
}
