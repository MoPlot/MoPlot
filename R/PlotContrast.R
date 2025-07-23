#' Internal plotting function for interaction contrasts
#'
#' This function is for internal usage
#'
#' @param contrast_data A data frame
#' @param pvalue A numeric value indicating the p-value of the interaction effect.
#' @param sig Significance level threshold (e.g., 0.05).
#'
#' @return A ggplot object representing the interaction contrast plot.
#'
#' @import ggplot2
PlotContrast <- function(contrast_data, pvalue, sig) {
  mypalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#E69F00",
                 "#0072B2", "#D55E00", "#CC79A7")

  contrast_data$Group <- factor(contrast_data[, 6], levels = unique(contrast_data[, 6]))
  contrast_data$Subgroup <- factor(contrast_data[, 7], levels = unique(contrast_data[, 7]))
  contrast_data$Contrasto <- factor(contrast_data$Contrasto, levels = unique(contrast_data$Contrasto))

  contrast_data$Group_Subgroup <- paste(contrast_data$Subgroup, contrast_data$Group, sep = " - ")

  facet_color <- ifelse(pvalue >= sig, "#D55E00", "#009E73")
  y_mean <- mean(contrast_data$Media[1:2])
  y_mean2 <- mean(contrast_data$Media[3:4])
  y_midpoint <- mean(c(y_mean, y_mean2))

  mid_label <- contrast_data$Group_Subgroup[2]
  mid_point_df <- data.frame(Group_Subgroup = mid_label, y = y_midpoint)

  ggplot(contrast_data, aes(x = Group_Subgroup, y = Media, color = Subgroup, group = Subgroup)) +
    geom_point(shape = 16, size = 2) +
    scale_color_manual(values = c("black", "black")) +
    geom_line(color = "black", lty = "solid") +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.05) +
    geom_segment(aes(x = 1.5, xend = 3.5, y = y_mean, yend = y_mean2),
                 color = facet_color, size = 0.6, lty = "dotted") +
    geom_point(data = mid_point_df, aes(x = 2.5, y = y),
               inherit.aes = FALSE, shape = 9, size = 3, fill = "black") +
    labs(
      title = "",
      x = "Groups",
      y = "Predicted Mean",
      color = "Variable levels"
    ) +
    facet_wrap(~ Contrasto, scales = "free_y") +
    theme_light() +
    theme(
      strip.background = element_rect(fill = "grey77"),
      strip.text = element_text(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}
