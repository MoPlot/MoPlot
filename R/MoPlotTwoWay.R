#' Plot Interactions Linear Models
#'
#' This function generates plots of estimated means and interaction contrasts
#' from a two-way model with interaction terms. It can visualize contrasts with or without
#' coefficients, and optionally combine all plots.
#'
#' @param LinearModel2Way A fitted `lm` model object.
#' @param sig.level Numeric. Significance threshold for highlighting contrasts. Default is `0.05`.
#' @param coef Logical. Whether to include coefficient plots. Default is `TRUE`.
#' @param focus Logical. If `TRUE`, plots are printed separately (one per contrast); if `FALSE`, combined using `patchwork`.
#'
#' @return Plots of estimated means and interaction contrasts
#'
#' @details
#' Works only for models with two categorical predictors and their interaction.
#' Colors and annotations in plots change based on the significance level (`sig.level`).
#'
#' @examples
#' \dontrun{
#' model <- lm(y ~ A * B, data = mydata)
#' MoPlotTwoWay(model, sig.level = 0.05, coef = TRUE, focus = FALSE)
#' }
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom utils capture.output
#' @export
MoPlotTwoWay <- function(LinearModel2Way, sig.level = 0.05, coef = TRUE,
                         focus = FALSE) {

  Table_contr <- suppressWarnings(ModelContrastTable(LinearModel2Way, type = "x"))
  Table_y <- suppressWarnings(ModelContrastTable(LinearModel2Way, type = "y"))
  Table_Gen <- suppressWarnings(ModelContrastTable(LinearModel2Way, type = "summary"))
  add_param <- ExtractParams(LinearModel2Way)

  # Check model structure
  if (Table_Gen[1, 2] == "3" & Table_Gen[3, 2] == "2" &
      (Table_Gen[6, 2] == "numeric" | Table_Gen[6, 2] == "integer") &
      Table_Gen[9, 2] == "factor, factor") {

    if (!grepl("\\*", deparse(stats::formula(LinearModel2Way)))) {
      return("Model does not contain an interaction term.")
    }

    int <- paste(Table_contr[1, 1], Table_contr[2, 1], sep = ":")
    r <- suppressWarnings(IntMeanCont(int, LinearModel2Way, stats::model.frame(LinearModel2Way),
                                      Table_y[1, 1], sig.level = sig.level))

    if (length(r) == 0 || all(sapply(r, is.null))) {
      return("No valid results for interaction contrasts.")
    }

    coeff_table <- summary(LinearModel2Way)$coefficients
    interaction_p_values <- coeff_table[grep(":", rownames(coeff_table)), "Pr(>|t|)"]
    interaction_names <- rownames(coeff_table)[grep(":", rownames(coeff_table))]

    if (!focus) {
      # Plot all contrasts together
      p <- ConjPlot(r, LinearModel2Way, sig.level = sig.level, focus = FALSE)

      if (coef) {
        interaction_coeffs <- coef(LinearModel2Way)[interaction_names]
        conf_intervals <- confint(LinearModel2Way)[interaction_names, ]

        coeffs_df <- suppressWarnings(data.frame(
          coefficient = interaction_coeffs,
          lower_ci = conf_intervals[, 1],
          upper_ci = conf_intervals[, 2],
          index = 1:length(interaction_coeffs),
          de = add_param[interaction_names, "d"]
        ))

        coeffs_df$label <- mapply(function(i, c, de) {
          paste0("atop(beta == ", round(c, 2),
                 ", italic(d) == ", round(de, 2), ")")
        }, i = coeffs_df$index, c = coeffs_df$coefficient, de = coeffs_df$de)

        plot2 <- ggplot(coeffs_df, aes(x = index, y = coefficient)) +
          geom_point(shape = 9, color = "black", size = 3) +
          geom_label(aes(x = index, y = coefficient, label = label), parse = TRUE, hjust = 1.2, size = 2, fill = "white") +
          geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.01, color = "darkblue") +
          scale_x_continuous(breaks = coeffs_df$index) +
          labs(x = "Coefficient #", y = "Coefficient Value") +
          theme_light()

        n_plots_in_p <- length(p$patches)  # number of plots inside p
        graph <- patchwork::wrap_plots(p, plot2, ncol = 1, heights = c(2, 1))

        invisible(capture.output(suppressMessages(print(graph))))
      } else {
        invisible(capture.output(suppressMessages(print(p))))
      }

    } else {
      # Focus mode: plot each contrast and coeff separately
      for (i in seq_along(r)) {
        contrast_data <- r[[i]]
        pvalue <- interaction_p_values[i]
        coef_name <- interaction_names[i]
        contrast_plot <- PlotContrast(contrast_data, pvalue = pvalue, sig = sig.level)

        if (coef) {
          interaction_coeff <- coef(LinearModel2Way)[coef_name]
          ci <- confint(LinearModel2Way)[coef_name, ]
          # Note: interaction_rows should probably be dynamic; here kept as in original
          interaction_rows <- interaction_names

          coeffs_df <- suppressWarnings(data.frame(
            coefficient = interaction_coeff,
            lower_ci = ci[1],
            upper_ci = ci[2],
            index = i,
            de = add_param[interaction_rows, "d"]
          ))

          coeffs_df$label <- mapply(function(i, c, de) {
            paste0("atop(beta == ", round(c, 2),
                   ", italic(d) == ", round(de, 2), ")")
          }, i = coeffs_df$index, c = coeffs_df$coefficient, de = coeffs_df$de)

          plot2 <- ggplot(coeffs_df, aes(x = as.factor(index), y = coefficient)) +
            geom_point(shape = 9, color = "black", size = 3) +
            coord_cartesian(ylim = c(-1, 1)) +
            geom_label(aes(label = label), parse = TRUE, hjust = 1.2, size = 2, fill = "white") +
            geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.01, color = "darkblue") +
            labs(x = "Coefficient #", y = "Coefficient Value") +
            theme_light()

          graph <- contrast_plot + plot2 + plot_layout(ncol = 1, heights = c(2, 1))

          invisible(capture.output(suppressMessages(print(graph))))
        } else {
          invisible(capture.output(suppressMessages(print(contrast_plot))))
        }
      }
    }

    return(cat(
      "This graphical representation illustrates the linear model, where\n",
      sprintf("%s are the categorical predictors (represented on the x-axis and with color),\n", Table_Gen[8, 2]),
      sprintf("and %s is the numerical dependent variable (plotted on the y-axis).\n", Table_Gen[5, 2]),
      sprintf("The contrast type applied is %s. \n", Table_Gen[11, 2]),
      "Blue dots indicate the expected values (means), while the error bars\n",
      "represent the uncertainty associated with these estimates.\n",
      sprintf("Green color highlights significant interactions, with a significance threshold of %s for the first type of error.\n", sig.level)
    ))

  } else {
    return("Not a two-way model or model structure is incorrect.")
  }
}
