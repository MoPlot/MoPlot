#' Plot Custom Contrasts and Coefficients from a Linear Model with One Categorical Predictor
#'
#' This function is intended for internal usage.
#'
#' @param predictor A string. The name of the categorical predictor variable.
#' @param vd A string. The name of the dependent numeric variable.
#' @param data A data frame containing the variables.
#' @param model A fitted linear model object (from `lm()`).
#' @param sig.level Significance level for CI and p-value coloring (default = 0.05).
#' @param show.data Logical. Whether to show individual data points (default = TRUE).
#' @param show.coef Logical. Whether to show the coefficient panel (default = TRUE).
#' @param facet Logical. Whether to facet the plot by contrast (default = TRUE).
#'
#' @return A `ggplot2::ggplot` object (single or combined using `patchwork::/` layout).
#'
#' @keywords internal
CustomPlot <- function(predictor, vd, data, model, sig.level = 0.05,
                       show.data= T, show.coef=T,facet = TRUE) {

  c <- ContrastsOnePred(predictor, vd, data, sig.level)
  c$Livelli <- factor(c$Livelli, levels = unique(c$Livelli))

  pvals <- summary(model)$coefficients[-1, 4]
  d_values <- ExtractParams(model)[, 5]
  contrast_names <- unique(c$Contrasto)

  if (length(contrast_names) != length(pvals)) {
    contrast_names <- paste0("Contrast n", seq_along(pvals))
  }

  pval_df <- data.frame(
    Contrasto = contrast_names,
    p_value = pvals,
    d = d_values,
    color = ifelse(pvals < sig.level, "#009E73", "#D55E00"),
    stringsAsFactors = FALSE
  )

  split_contrasts <- split(c, c$Contrasto)
  segments <- list()

  for (contrast_name in names(split_contrasts)) {
    contrast_data <- split_contrasts[[contrast_name]]
    if (nrow(contrast_data) == 2) {
      p_row <- pval_df[pval_df$Contrasto == contrast_name, ]
      seg_color <- if (nrow(p_row) == 1) p_row$color else "#D55E00"

      segments[[contrast_name]] <- data.frame(
        Contrasto = contrast_name,
        x = contrast_data$Livelli[1],
        xend = contrast_data$Livelli[2],
        y = contrast_data$Media[1],
        yend = contrast_data$Media[2],
        color = seg_color,
        stringsAsFactors = FALSE
      )
    }
  }

  segment_data <- if (length(segments) > 0) do.call(rbind, segments) else data.frame()

  contrast_matrix <- contrasts(data[[predictor]])
  level_names <- levels(data[[predictor]])
  jitter_data <- data.frame()

  for (i in seq_len(ncol(contrast_matrix))) {
    contrast_name <- contrast_names[i]
    contrast_vec <- contrast_matrix[, i]
    pos <- level_names[contrast_vec > 0]
    neg <- level_names[contrast_vec < 0]

    pos_label <- paste(pos, collapse = ", ")
    neg_label <- paste(neg, collapse = ", ")

    data_pos <- data[data[[predictor]] %in% pos, ]
    data_pos$Group <- pos_label
    data_pos$Contrasto <- contrast_name

    data_neg <- data[data[[predictor]] %in% neg, ]
    data_neg$Group <- neg_label
    data_neg$Contrasto <- contrast_name

    jitter_data <- rbind(jitter_data, data_pos, data_neg)
  }
  jitter_data$Group <- factor(jitter_data$Group, levels = unique(c$Livelli))

  alpha <- 0
  ifelse(show.data==T, alpha<-.3,alpha<-0)
  p <- ggplot2::ggplot(c, aes(x = Livelli, y = Media)) +
    ggplot2::geom_point(color = "#0072B2") +
    ggplot2::geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.05, color = "#0072B2") +
    ggplot2::geom_segment(data = segment_data,
                 aes(x = x, xend = xend, y = y, yend = yend, color = color),
                 inherit.aes = FALSE,
                 linewidth = 0.9) +
    ggplot2::geom_jitter(data = jitter_data,
                aes(x = Group, y = .data[[vd]]),
                width = 0.1, alpha = alpha, color = "#F0E442",
                inherit.aes = FALSE) +
    ggplot2::scale_color_identity() +
    ggplot2::labs(
      title = "Customized Contrasts",
      y = "Means", x = "Groups Compared"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (facet) {
    p <- p + ggplot2::facet_wrap(~ Contrasto, scales = "free_x")
  }

  coeffs <- stats::coef(model)[-1]
  conf_intervals <- stats::confint(model)[-1, ]

  coeffs_df <- data.frame(
    coefficient = coeffs,
    lower_ci = conf_intervals[, 1],
    upper_ci = conf_intervals[, 2],
    d = d_values,
    index = seq_along(coeffs)
  )

  coeffs_df$label <- mapply(function(i, c, d) {
    paste0("atop(beta[", i, "] == ", round(c, 2),
           ", italic(d)[", i, "] == ", round(d, 2), ")")
  }, i = coeffs_df$index, c = coeffs_df$coefficient, d = coeffs_df$d)

  plot2 <- ggplot2::ggplot(coeffs_df, aes(x = index, y = coefficient)) +
    ggplot2::geom_point(color = "black", size = 2) +
    ggplot2::geom_label(aes(label = label), parse = TRUE, hjust = 1.05, size = 3, fill = "white") +
    ggplot2::geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.05, color = "darkblue") +
    ggplot2::scale_x_continuous(breaks = seq_along(coeffs)) +
    ggplot2::labs(x = "Contrast #", y = "Coefficient Value") +
    ggplot2::theme_light()

  ifelse(show.coef==T, suppressWarnings(return(p / plot2 + patchwork::plot_layout(ncol = 1, heights = c(2, 1)))),
         suppressWarnings(return(p)))
}
