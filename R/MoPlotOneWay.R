#' Plot One-Way Linear Model Highlighting Contrasts
#'
#' Visualizes a one-way linear model with a single categorical predictor and numeric dependent variable.
#' Supports multiple contrast types including treatment, sum, successive differences (sdif), Helmert,
#' reverse Helmert, polynomial and customized contrasts.
#'
#' The plot shows means, confidence intervals, significance of contrasts, and optionally overlays raw data points.
#' It also display standardized coefficients and effect sizes.
#'
#' @param LinearMod A fitted linear model object (from \code{lm()}) with one numeric response and one categorical predictor.
#' @param data Logical. If \code{TRUE}, overlays individual data points on the plot (default: \code{FALSE}).
#' @param siglevel Numeric. Significance level for testing contrasts and confidence intervals (default: \code{0.05}).
#' @param coef Logical. If \code{TRUE}, adds a second panel with coefficients and standardized effect sizes (default: \code{FALSE}).
#' @param facet.pers Logical. If \code{TRUE} personalized contrasts are seen one by one (default: \code{FALSE}).
#'
#' @return A \code{ggplot2} plot object or a combined plot (with \pkg{patchwork}) if \code{coef = TRUE}.
#'
#' @details
#' Supported contrast types and their interpretations:
#' \itemize{
#'   \item \strong{Treatment:} Each group is compared to a baseline (first level).
#'   \item \strong{Sum:} Contrasts compare each group to the grand mean.
#'   \item \strong{Successive differences (sdif):} Each group is compared to the previous one.
#'   \item \strong{Helmert:} Each group is compared to the mean of all previous groups.
#'   \item \strong{Reverse Helmert:} Each group is compared to the mean of all subsequent groups.
#'   \item \strong{Polynomial:} Contrasts test polynomial trends across ordered factor levels.
#'   \item \strong{Customized:} User-defined contrasts.
#' }
#'
#' Error bars represent 95% confidence intervals. Significant contrasts are highlighted according to the chosen significance level.
#' The coefficient panel (if enabled) shows coefficient estimates with confidence intervals and standardized effect sizes.
#'
#' @import ggplot2
#' @importFrom stats coef confint qt
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#' \dontrun{
#'   data(iris)
#'   iris$Species <- factor(iris$Species)
#'   model <- lm(Sepal.Length ~ Species, data = iris)
#'   MoPlotOneWay(model, data = TRUE, coef = TRUE)
#' }
MoPlotOneWay <- function(LinearMod, data=FALSE, siglevel=0.05, coef=FALSE,
                         facet.pers = TRUE){
  Table_contr <- ModelContrastTable(LinearMod, type="x")
  Table_Gen <- ModelContrastTable(LinearMod, type="summary")
  if (Table_Gen[1,2]=="2" & Table_Gen[3,2]=="1" &
      (Table_Gen[6,2]=="numeric"|Table_Gen[6,2]=="integer") & Table_Gen[9,2]=="factor"){
    mypalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#E69F00",
                   "#0072B2", "#D55E00", "#CC79A7")
    df_model <- stats::model.frame(LinearMod)
    predictor <- names(df_model)[2]
    vd <- names(df_model)[1]
    level_order <- levels(df_model[[predictor]])
    df2_full <- ContrastsOnePred(
      predictor = predictor,
      vd = vd,
      data = df_model,
      sig.level = siglevel
    )
    df2_full <- df2_full[!duplicated(df2_full$Livelli), ]
    df2_full$Livelli <- factor(df2_full$Livelli, levels = level_order)
    df2 <- data.frame(
      x = df2_full$Livelli,
      y = df2_full$Media,
      std.error = df2_full$Se,
      lower = df2_full$CI_low,
      upper = df2_full$CI_high
    )
    desired_order <- levels(df_model[[predictor]])
    df2$x <- factor(df2$x, levels = desired_order)
    df2 <- df2[order(df2$x), ]
    df3 <- ExtractParams(LinearMod)
    if(unname(Table_contr[4]=="treatment")){
      signif_contrast <- summary(LinearMod)$coefficients[-1, 4] < siglevel
      colors_vec <- ifelse(signif_contrast, "#009E73", "#D55E00")
      baseline_color <- mypalette[8]
      color_column <- c(baseline_color, colors_vec)
      df2$segment_color <- color_column
      plot <- ggplot(df2, aes(x = x , y = y)) +
        geom_segment(aes(x = x, xend = x[1],
                         y = y, yend = y[1],
                         color = segment_color),
                     linetype = "solid", linewidth = 0.6, alpha = 0.7)+
        geom_hline(aes(yintercept = y[1], color = mypalette[8]),
                   linetype = "dashed", linewidth = 0.5, alpha=1) +
        scale_color_identity()+
        geom_point(color = mypalette[6]) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width=.05,color = mypalette[6])+
        labs(title = "Predicted values for treatment kind of contrast",
             x = "Levels", y = "Predicted Values") +
        theme_light() +
        theme(legend.position = "none")+
        ylim(min(LinearMod$model[,1]), max(LinearMod$model[,1]))
      if (data==TRUE){
        plot <- plot +
          geom_jitter(data = LinearMod$model, mapping = aes(y = LinearMod$model[,1],
                                                            x = LinearMod$model[,2]),
                      color = mypalette[4], alpha = 0.2, width = 0.05)
      }

      cat(
        "This graphical representation illustrates the linear model, where\n",
        sprintf("%s is the categorical predictor (represented on the x-axis with its levels),\n", Table_Gen[8, 2]),
        sprintf("and %s is the numerical dependent variable (plotted on the y-axis).\n", Table_Gen[5, 2]),
        sprintf("The contrast type applied is %s, which compares each group mean against\n", Table_Gen[11, 2]),
        sprintf("the baseline mean of %s (the first level of the predictor).\n", LinearMod$xlevels[[1]][1]),
        "Blue dots indicate the expected values (means) for each group, while the error bars\n",
        "represent the uncertainty associated with these estimates.\n",
        "The dashed purple line marks the baseline expected value.\n",
        sprintf("Green lines highlight significant contrasts, with a significance threshold of %s for the first type of error.\n", siglevel)
      )
      if (coef == TRUE) {
        coeffs <- coef(LinearMod)[-1]
        conf_intervals <- confint(LinearMod)[-1, ]

        coeffs_df <- data.frame(
          coefficient = coeffs,
          lower_ci = conf_intervals[, 1],
          upper_ci = conf_intervals[, 2],
          index = 1:length(coeffs)
        )

        coeffs_df$d <- df3$d

        coeffs_df$label <- mapply(function(i, c, d) {
          paste0("atop(beta[", i, "] == ", round(c, 2),
                 ", italic(d)[", i, "] == ", round(d, 2), ")")
        }, i = coeffs_df$index, c = coeffs_df$coefficient, d = coeffs_df$d)

        plot2 <- ggplot(coeffs_df, aes(x = index, y = coefficient)) +
          geom_point(color = "black", size = 1) +
          geom_label(aes(label = label), parse = TRUE, hjust = 1.2, size = 2, fill = "white") +
          geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.05, color = "darkblue") +
          scale_x_continuous(breaks = 1:length(coeffs)) +
          labs(x = "Coefficient #", y = "Coefficient Value") +
          theme_light()
        suppressWarnings(return(plot/plot2+ plot_layout(ncol = 1, heights = c(2, 1))))
      }
      suppressWarnings(return(plot))
    }
    if (as.character(Table_contr[4]) == "helmert") {

      df_model <- stats::model.frame(LinearMod)
      dep_var <- df_model[[1]]
      indep_var <- as.factor(df_model[[2]])

      group_means <- tapply(dep_var, indep_var, mean)
      group_sds   <- tapply(dep_var, indep_var, sd)
      group_ns    <- tapply(dep_var, indep_var, length)
      group_se    <- group_sds / sqrt(group_ns)

      alpha <- siglevel
      df_resid <- LinearMod$df.residual
      t_crit <- qt(1 - alpha / 2, df_resid)

      group_labels <- levels(indep_var)

      df_H <- data.frame(Gruppo = character(0), Mean = numeric(0),
                         Std.Error = numeric(0), lower = numeric(0), upper = numeric(0),
                         stringsAsFactors = FALSE)

      for (i in 1:length(group_labels)) {
        label_single <- group_labels[i]
        mean_single <- group_means[label_single]
        se_single <- group_se[label_single]

        lower_single <- mean_single - t_crit * se_single
        upper_single <- mean_single + t_crit * se_single

        df_H <- rbind(df_H, data.frame(
          Gruppo = label_single,
          Mean = mean_single,
          Std.Error = se_single,
          lower = lower_single,
          upper = upper_single
        ))

        if (i > 1 & i < length(group_labels)) {
          group_set <- group_labels[1:i]
          means_set <- group_means[group_set]
          ses_set <- group_se[group_set]

          mean_cum <- mean(means_set)
          se_cum <- sqrt(sum(ses_set^2) / (length(group_set)^2))
          lower_cum <- mean_cum - t_crit * se_cum
          upper_cum <- mean_cum + t_crit * se_cum

          label_cum <- paste0("(", paste(group_set, collapse = "+"), ")/", length(group_set))

          df_H <- rbind(df_H, data.frame(
            Gruppo = label_cum,
            Mean = mean_cum,
            Std.Error = se_cum,
            lower = lower_cum,
            upper = upper_cum
          ))
        }
      }

      p_vals <- summary(LinearMod)$coefficients[2:(nrow(summary(LinearMod)$coefficients)), 4]
      df_H$Gruppo <- factor(df_H$Gruppo, levels = df_H$Gruppo)

      plot <- ggplot(df_H, aes(x = Gruppo, y = Mean)) +
        geom_point(aes(color = grepl("\\(", Gruppo)), size = 2) +
        geom_errorbar(aes(ymin = lower, ymax = upper, color = grepl("\\(", Gruppo)),
                      linetype = "solid", width = 0.05, alpha = 0.8) +
        scale_color_manual(values = c("#E69F00", "#0072B2", "#CC79A7"),
                           labels = c("Intercept", "Group Means", "Combined Means"), name = "Type") +
        geom_segment(data = {
          lines_df <- data.frame(
            x = seq(1, nrow(df_H) - 1, by = 2),
            xend = seq(2, nrow(df_H), by = 2),
            y = df_H$Mean[seq(1, nrow(df_H) - 1, by = 2)],
            yend = df_H$Mean[seq(2, nrow(df_H), by = 2)]
          )
          lines_df
        }, aes(x = x, xend = xend, y = y, yend = yend,
               color = I(ifelse(p_vals < siglevel, "#009E73", "#D55E00"))),
        show.legend = FALSE) +
        geom_hline(mapping = aes(yintercept = LinearMod$coefficients[1], color = "#CC79A7"),
                   linetype = "dashed", linewidth = 0.6, alpha = 0.8) +
        labs(title = "Predicted values for Helmert contrast",
             x = "Levels", y = "Predicted Values") +
        theme_light() +
        ylim(min(LinearMod$model[, 1]), max(LinearMod$model[, 1])) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (data == TRUE) {
        plot <- plot +
          geom_jitter(data = df_model,
                      aes(x = as.factor(df_model[[2]]), y = df_model[[1]]),
                      color = "#F0E442", alpha = 0.1, width = 0.05)
      }

      if (coef == TRUE) {
        df3 <- ExtractParams(LinearMod)
        coeffs <- coef(LinearMod)[-1]
        conf_intervals <- confint(LinearMod)[-1, ]

        coeffs_df <- data.frame(
          coefficient = coeffs,
          lower_ci = conf_intervals[, 1],
          upper_ci = conf_intervals[, 2],
          index = 1:length(coeffs),
          d = df3$d
        )

        coeffs_df$label <- mapply(function(i, c, d) {
          paste0("atop(beta[", i, "] == ", round(c, 2),
                 ", italic(d)[", i, "] == ", round(d, 2), ")")
        }, i = coeffs_df$index, c = coeffs_df$coefficient, d = coeffs_df$d)

        plot2 <- ggplot(coeffs_df, aes(x = index, y = coefficient)) +
          geom_point(color = "black", size = 1) +
          geom_label(aes(label = label), parse = TRUE, hjust = 1.5, size = 2, fill = "white") +
          geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.05, color = "darkblue") +
          scale_x_continuous(breaks = 1:length(coeffs)) +
          labs(x = "Coefficient #", y = "Coefficient Value") +
          theme_light()

        suppressWarnings(return(plot / plot2 + patchwork::plot_layout(ncol = 1, heights = c(2, 1))))
      }

      cat(
        "This graphical representation depicts the linear model, with\n",
        sprintf("%s as the categorical predictor (levels are represented on the x-axis),\n", Table_Gen[8, 2]),
        sprintf("and %s as the numerical dependent variable (values are shown on the y-axis).\n", Table_Gen[5, 2]),
        sprintf("The applied contrast type is %s. In this type of contrast, each group mean is compared to\n", Table_Gen[11, 2]),
        "the cumulative mean of all previous groups.\n",
        "Blue dots represent the expected values (means) for the cumulative means,\n",
        "while orange dots represent the expected values (means) for each individual group.\n",
        "Error bars indicate the uncertainty associated with each expected value.\n",
        "The dashed purple line marks the baseline expected value.\n",
        sprintf("Green lines highlight significant contrasts, with %s serving as the threshold for the first type of error.\n", siglevel)
      )

      suppressWarnings(return(plot))
    }
    if (unname(Table_contr[4] == "sdif")) {
      n_groups <- length(LinearMod$xlevels[[1]])
      base_cols <- viridis::viridis(n_groups)
      df2$x <- seq_len(nrow(df2))

      subset_df2 <- data.frame(
        x1        = df2$x[-nrow(df2)],
        xend      = df2$x[-1],
        y1        = df2$y[-nrow(df2)],
        yend      = df2$y[-1],
        p         = summary(LinearMod)$coefficients[-1 , 4],
        delta_raw = round(df2$y[-nrow(df2)] - df2$y[-1], 2)
      )
      df_coef <- as.data.frame(summary(LinearMod)$coefficients)[-1, ]
      df_coef$Contrast <- paste(seq_len(nrow(df_coef)), "vs",
                                seq_len(nrow(df_coef)) + 1)
      fill_colors            <- base_cols[seq_len(nrow(df_coef))]
      df_coef$fill_color     <- fill_colors
      df_coef$d              <- df3$d
      rombi_df <- data.frame(
        x          = (subset_df2$x1 + subset_df2$xend)/2,
        y          = (subset_df2$y1 + subset_df2$yend)/2,
        fill_color = fill_colors
      )
      plot_top <- ggplot(df2, aes(x = x, y = y)) +
        annotate("rect",
                 xmin = seq(1, nrow(df2) - 1, 2),
                 xmax = seq(2, nrow(df2),     2),
                 ymin = -Inf, ymax =  Inf,
                 fill = "grey90", alpha = .3) +
        geom_segment(data = subset_df2,
                     aes(x = x1, xend = xend,
                         y = y1, yend = yend,
                         colour = I(ifelse(p < siglevel,
                                           "#009E73", "#D55E00"))),
                     linewidth = .6) +
        geom_point(data = rombi_df,
                   aes(x = x, y = y, fill = fill_color),
                   shape = 23, size = 2, colour = "black") +
        geom_point(colour = mypalette[6]) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      width = .05, colour = mypalette[6]) +
        geom_hline(yintercept = coef(LinearMod)[1],
                   linetype = "dashed", colour = mypalette[8]) +
        labs(title = "Predicted values for sliding-difference contrast",
             x = "Levels", y = "Predicted values") +
        theme_light() +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(min(LinearMod$model[, 1]), max(df2$y) + 3))
      if (isTRUE(data)) {
        plot_top <- plot_top +
          geom_jitter(data = LinearMod$model,
                      aes(y = LinearMod$model[, 1],
                          x = as.numeric(LinearMod$model[, 2])),
                      colour = mypalette[4], alpha = .2, width = .05)
      }
      if (isTRUE(coef)) {
        plot_bot <- ggplot(df_coef,
                           aes(x = Contrast, y = Estimate)) +
          geom_point(aes(fill = fill_color), shape = 23, size = 4,
                     colour = "black", show.legend = FALSE) +
          geom_text(aes(label = sprintf("Beta = %.2f\n d = %.2f",
                                        Estimate, d)),
                    nudge_x = .1, hjust = 0, size = 3) +
          geom_errorbar(aes(ymin = Estimate - `Std. Error`,
                            ymax = Estimate + `Std. Error`),
                        width = .05) +
          geom_hline(yintercept = 0, linetype = "dashed",
                     colour = "grey50") +
          labs(title = "Coefficient #",
               y = "Coefficient Value", x = NULL) +
          theme_bw()

        plot <- plot_top / plot_bot +
          patchwork::plot_layout(heights = c(2, 1))
      } else {
        plot <- plot_top
      }

      cat(
        "This graphical representation illustrates the linear model, where\n",
        sprintf("%s is the categorical predictor (levels represented on the x-axis),\n",
                Table_Gen[8, 2]),
        sprintf("and %s is the numerical dependent variable (values shown on the y-axis).\n",
                Table_Gen[5, 2]),
        sprintf("The contrast type applied is %s, where each group mean is compared to\n",
                Table_Gen[11, 2]),
        "the mean of the subsequent group.\n",
        "Blue squares represent the estimated differences (Beta) between adjacent groups;\n",
        "green segments denote significant contrasts (p < ", siglevel, ").\n",
        "Error bars show uncertainity around each group mean; the dashed line is the intercept.\n",
        sep = ""
      )

      suppressWarnings(return(plot))
    }

    if (unname(Table_contr[4] == "sum")) {
      df2 <- df2[-nrow(df2), ]

      coefs_summary <- summary(LinearMod)$coefficients
      pvals <- coefs_summary[-1, 4]

      colors_vec <- ifelse(pvals < siglevel, "#009E73", "#D55E00")

      plot <- ggplot(df2, aes(x = x, y = y)) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      color = colors_vec, width = 0.05) +
        geom_point(color = mypalette[6]) +
        geom_hline(yintercept = coefs_summary[1, 1], color = mypalette[8],
                   linetype = "dashed", linewidth = 0.5, alpha = 1) +
        labs(title = "Predicted values for sum kind of contrast",
             x = "Levels", y = "Predicted Values") +
        theme_light() +
        theme(legend.position = "none") +
        coord_flip() +
        ylim(min(LinearMod$model[, 1]), max(LinearMod$model[, 1]))

      if (data == TRUE) {
        sub_model <- subset(LinearMod$model, LinearMod$model[, 2] != LinearMod$model[nrow(LinearMod$model), 2])

        plot <- plot +
          geom_jitter(data = sub_model, aes(x = sub_model[, 2], y = sub_model[, 1]),
                      color = mypalette[4], alpha = 0.2, width = 0.05)

        if (coef == TRUE) {
          coeffs <- coef(LinearMod)[-1]
          conf_intervals <- confint(LinearMod)[-1, ]

          coeffs_df <- data.frame(
            coefficient = coeffs,
            lower_ci = conf_intervals[, 1],
            upper_ci = conf_intervals[, 2],
            index = 1:length(coeffs)
          )

          coeffs_df$d <- df3$d

          coeffs_df$label <- mapply(function(i, c, d) {
            paste0("atop(b[", i, "] == ", round(c, 2),
                   ", italic(d)[", i, "] == ", round(d, 2), ")")
          }, i = coeffs_df$index, c = coeffs_df$coefficient, d = coeffs_df$d)

          plot2 <- ggplot(coeffs_df, aes(x = index, y = coefficient)) +
            geom_point(color = "black", size = 1) +
            geom_label(aes(label = label), parse = TRUE, hjust = 1.2, size = 2, fill = "white") +
            geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.05, color = "darkblue") +
            scale_x_continuous(breaks = 1:length(coeffs)) +
            labs(x = "Coefficient #", y = "Coefficient Value") +
            theme_light()

          suppressWarnings(return(plot / plot2 + plot_layout(ncol = 1, heights = c(2, 1))))
        }
      }

      cat(
        "This graphical representation depicts the linear model, with\n",
        sprintf("%s as the categorical predictor (levels shown on the x-axis),\n", Table_Gen[8, 2]),
        sprintf("and %s as the numerical dependent variable (values displayed on the y-axis).\n", Table_Gen[5, 2]),
        sprintf("The contrast type is %s, where each group mean is compared to\n", Table_Gen[11, 2]),
        "the intercept, which serves as the baseline.\n",
        "Blue dots represent the expected values (means) for each group, and error bars capture\n",
        "the uncertainty associated with these estimates.\n",
        "The dashed purple line marks the intercept expected value.\n",
        sprintf("The green error bars highlight significant contrasts, with %s as the threshold for the first type of error.\n", siglevel)
      )


      suppressWarnings(return(plot))
    }

    if (unname(Table_contr[4] == "poly")) {
      dep_var <- df_model[, 1]
      indep_var <- as.factor(df_model[, 2])
      group_means <- tapply(dep_var, indep_var, mean)
      group_sds <- tapply(dep_var, indep_var, sd)
      group_ns <- tapply(dep_var, indep_var, length)
      group_se <- group_sds / sqrt(group_ns)
      df2 <- data.frame(
        x = factor(names(group_means), levels = levels(indep_var)),
        y = as.numeric(group_means),
        std.error = as.numeric(group_se)
      )
      df3 = ExtractParams(LinearMod)

      alpha <- siglevel
      df_resid <- LinearMod$df.residual
      t_crit <- qt(1 - alpha/2, df_resid)

      df2$lower <- df2$y - t_crit * df2$std.error
      df2$upper <- df2$y + t_crit * df2$std.error

      poly_degree <- length(LinearMod$coefficients) - 1

      df4 <- df2
      df4$x <- as.numeric(df4$x)

      plot <- ggplot(df2, aes(x = x, y = y)) +
        geom_hline(mapping=aes(yintercept = LinearMod$coefficients[1], color = mypalette[8]),
                   linetype = "dashed", linewidth = 0.5, alpha = 1) +
        scale_color_identity() +
        geom_point(color = mypalette[6]) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = .05, color = mypalette[6]) +
        labs(title = paste("Predicted values for polynomial contrast (Degree 1 to", poly_degree, ")"),
             x = "Levels", y = "Predicted Values") +
        theme_light() +
        theme(legend.position = "none") +
        ylim(min(LinearMod$model[, 1]), max(LinearMod$model[, 1]))

      if (data == TRUE) {
        plot <- plot +
          geom_jitter(data = LinearMod$model, mapping = aes(y = LinearMod$model[, 1],
                                                            x = as.factor(LinearMod$model[, 2])),
                      color = mypalette[4], alpha = 0.2, width = 0.05)
      }

      for (i in 1:poly_degree) {
        sig_col <- ifelse(summary(LinearMod)$coefficients[1 + i, 4] < siglevel, "#009E73", "#D55E00")
        plot <- plot +
          geom_smooth(
            data = df2,
            aes(x = as.numeric(x), y = y),
            method = "lm",
            formula = y ~ poly(x, i),
            se = FALSE,
            color = sig_col,
            linetype = "solid",
            linewidth = 0.7
          )
        plot <- plot + labs(title = paste("Predicted values for polynomial contrast (degree ", i, ")", sep = ""))
        if (coef == TRUE) {
          coeffs <- coef(LinearMod)[-1]
          conf_intervals <- confint(LinearMod)[-1, ]

          coeffs_df <- data.frame(
            coefficient = coeffs,
            lower_ci = conf_intervals[, 1],
            upper_ci = conf_intervals[, 2],
            index = 1:length(coeffs)
          )

          coeffs_df$d <- df3$d

          coeffs_df$label <- mapply(function(i, c, d) {
            paste0("atop(beta[", i, "] == ", round(c, 2),
                   ", italic(d)[", i, "] == ", round(d, 2), ")")
          }, i = coeffs_df$index, c = coeffs_df$coefficient, d = coeffs_df$d)

          plot2 <- ggplot(coeffs_df, aes(x = index, y = coefficient)) +
            geom_point(color = "black", size = 1) +
            geom_label(aes(label = label), parse = TRUE, hjust = 1.2, size = 2, fill = "white") +
            geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.05, color = "darkblue") +
            scale_x_continuous(breaks = 1:length(coeffs)) +
            labs(x = "Coefficient #", y = "Coefficient Value") +
            theme_light()
          print(plot/plot2+ plot_layout(ncol = 1, heights = c(2, 1)))
        }
        if (coef == FALSE) {
          print(plot)
        }
      }

      cat(
        "This graphical representation illustrates the linear model, where\n",
        sprintf("%s is the categorical predictor (shown on the x-axis with its levels),\n", Table_Gen[8, 2]),
        sprintf("and %s is the numerical dependent variable (plotted on the y-axis).\n", Table_Gen[5, 2]),
        sprintf("The contrast type applied is %s, which decomposes the overall trend across levels into polynomial components\n", Table_Gen[11, 2]),
        "such as linear, quadratic, and cubic trends.\n",
        "Each plot shows how well a trend of a given degree captures the pattern in the data.\n",
        "The lines interpolate the expected values based on polynomial regression,\n",
        "and are shown in green when the corresponding component is statistically significant (p < ", siglevel, ").\n"
      )

      return("Use the ARROWS to see the different plots")
    }

    if(unname(Table_contr[4]=="personalizzato")){

      print(
        CustomPlot(
          predictor = as.character(names(LinearMod$xlevels)),
          vd = as.character(ModelContrastTable(LinearMod, type = "y")[1]),
          data = stats::model.frame(LinearMod),
          model = LinearMod,
          sig.level = siglevel,
          show.data = data,
          show.coef = coef,
          facet = facet.pers
        )
      )

      suppressWarnings(return(
        CustomDescription(
          output_contrasti = ContrastsOnePred(
            predictor = as.character(names(LinearMod$xlevels)),
            vd = as.character(ModelContrastTable(LinearMod, type = "y")[1]),
            data = stats::model.frame(LinearMod),
            sig.level = siglevel
          ),
          sig.level = siglevel
        )
      ))

    }

    if(unname(Table_contr[4] ==  "ReverseHelmert")) {
      df_model <- stats::model.frame(LinearMod)
      dep_var <- df_model[[1]]
      indep_var <- as.factor(df_model[[2]])

      group_means <- tapply(dep_var, indep_var, mean)
      group_sds <- tapply(dep_var, indep_var, sd)
      group_ns <- tapply(dep_var, indep_var, length)
      group_se <- group_sds / sqrt(group_ns)

      alpha <- siglevel
      df_resid <- LinearMod$df.residual
      t_crit <- qt(1 - alpha / 2, df_resid)

      group_labels <- levels(indep_var)
      n_groups <- length(group_labels)

      df_H <- data.frame(Gruppo = character(0), Mean = numeric(0),
                         Std.Error = numeric(0), lower = numeric(0), upper = numeric(0),
                         stringsAsFactors = FALSE)

      for (i in 1:n_groups) {
        label_single <- group_labels[i]
        mean_single <- group_means[label_single]
        se_single <- group_se[label_single]
        lower_single <- mean_single - t_crit * se_single
        upper_single <- mean_single + t_crit * se_single

        df_H <- rbind(df_H, data.frame(
          Gruppo = label_single,
          Mean = mean_single,
          Std.Error = se_single,
          lower = lower_single,
          upper = upper_single
        ))

        if (i < n_groups) {
          group_set <- group_labels[(i + 1):n_groups]
          means_set <- group_means[group_set]
          ses_set <- group_se[group_set]

          mean_cum <- mean(means_set)
          se_cum <- sqrt(sum(ses_set^2) / length(group_set)^2)
          lower_cum <- mean_cum - t_crit * se_cum
          upper_cum <- mean_cum + t_crit * se_cum


          if (length(group_set) == 1) {
            label_cum <- group_set
            if (label_cum %in% df_H$Gruppo) next
          } else {
            label_cum <- paste0("(", paste(group_set, collapse = "+"), ")/", length(group_set))
          }

          df_H <- rbind(df_H, data.frame(
            Gruppo = label_cum,
            Mean = mean_cum,
            Std.Error = se_cum,
            lower = lower_cum,
            upper = upper_cum
          ))
        }
      }

      df_H$Gruppo <- factor(df_H$Gruppo, levels = unique(df_H$Gruppo))

      p_vals <- summary(LinearMod)$coefficients[2:(nrow(summary(LinearMod)$coefficients)), 4]

      plot <- ggplot(df_H[-nrow(df_H),], aes(x = Gruppo, y = Mean)) +
        geom_point(aes(color = grepl("\\(", Gruppo)), size = 2) +
        geom_errorbar(aes(ymin = lower, ymax = upper, color = grepl("\\(", Gruppo)),
                      linetype = "solid", width = 0.05, alpha = 0.8) +
        scale_color_manual(values = c("#CC79A7", "#0072B2", "#E69F00"),
                           labels = c("Intercept", "Group Mean", "Combined Means"), name = "Type") +
        geom_segment(data = {
          lines_df <- data.frame(
            x = seq(1, nrow(df_H) - 1, by = 2),
            xend = seq(2, nrow(df_H), by = 2),
            y = df_H$Mean[seq(1, nrow(df_H) - 1, by = 2)],
            yend = df_H$Mean[seq(2, nrow(df_H), by = 2)]
          )
          lines_df
        }, aes(x = x, xend = xend, y = y, yend = yend,
               color = I(ifelse(p_vals < siglevel, "#009E73", "#D55E00"))),
        show.legend = FALSE) +
        geom_hline(mapping = aes(yintercept = LinearMod$coefficients[1], color = "#CC79A7"),
                   linetype = "dashed", linewidth = 0.6, alpha = 0.8) +
        labs(title = "Predicted values for Reverse Helmert contrast",
             x = "Levels", y = "Predicted Values") +
        theme_light() +
        ylim(min(LinearMod$model[, 1]), max(LinearMod$model[, 1])) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (data == TRUE) {
        plot <- plot +
          geom_jitter(
            data = df_model,
            aes(x = as.factor(df_model[[2]]), y = df_model[[1]]),
            color = "#F0E442", alpha = 0.1, width = 0.05
          )
      }

      if (coef == TRUE) {
        df3 <- ExtractParams(LinearMod)
        coeffs <- coef(LinearMod)[-1]
        conf_intervals <- confint(LinearMod)[-1, ]

        coeffs_df <- data.frame(
          coefficient = coeffs,
          lower_ci = conf_intervals[, 1],
          upper_ci = conf_intervals[, 2],
          index = 1:length(coeffs),
          d = df3$d
        )

        coeffs_df$label <- mapply(function(i, c, d) {
          paste0("atop(beta[", i, "] == ", round(c, 2),
                 ", italic(d)[", i, "] == ", round(d, 2), ")")
        }, i = coeffs_df$index, c = coeffs_df$coefficient, d = coeffs_df$d)

        plot2 <- ggplot(coeffs_df, aes(x = index, y = coefficient)) +
          geom_point(color = "black", size = 1) +
          geom_label(aes(label = label), parse = TRUE, hjust = 1.5, size = 2, fill = "white") +
          geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.05, color = "darkblue") +
          scale_x_continuous(breaks = 1:length(coeffs)) +
          labs(x = "Coefficient #", y = "Coefficient Value") +
          theme_light()

        suppressWarnings(return(plot / plot2 + patchwork::plot_layout(ncol = 1, heights = c(2, 1))))
      }

      cat(
        "This graphical representation depicts the linear model, with\n",
        sprintf("%s as the categorical predictor (levels are represented on the x-axis),\n", Table_Gen[8, 2]),
        sprintf("and %s as the numerical dependent variable (values are shown on the y-axis).\n", Table_Gen[5, 2]),
        sprintf("The applied contrast type is %s. In this type of contrast, each group mean is compared to\n", Table_Gen[11, 2]),
        "the cumulative mean of all previous groups.\n",
        "Blue dots represent the expected values (means) for the cumulative means,\n",
        "while orange dots represent the expected values (means) for each individual group.\n",
        "Error bars indicate the uncertainty associated with each expected value.\n",
        "The dashed purple line marks the baseline expected value.\n",
        sprintf("Green lines highlight significant contrasts, with %s serving as the threshold for the first type of error.\n", siglevel)
      )
      if (coef == TRUE) {
        coeffs <- coef(LinearMod)[-1]
        conf_intervals <- confint(LinearMod)[-1, ]

        coeffs_df <- data.frame(
          coefficient = coeffs,
          lower_ci = conf_intervals[, 1],
          upper_ci = conf_intervals[, 2],
          index = 1:length(coeffs)
        )

        coeffs_df$d <- df3$d

        coeffs_df$label <- mapply(function(i, c, d) {
          paste0("atop(beta[", i, "] == ", round(c, 2),
                 ", italic(d)[", i, "] == ", round(d, 2), ")")
        }, i = coeffs_df$index, c = coeffs_df$coefficient, d = coeffs_df$d)


        plot2 <- ggplot(coeffs_df, aes(x = index, y = coefficient)) +
          geom_point(color = "black", size = 1) +
          geom_label(aes(label = label), parse = TRUE, hjust = 1.5, size = 2, fill = "white") +
          geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.05, color = "darkblue") +
          scale_x_continuous(breaks = 1:length(coeffs)) +
          labs(x = "Coefficient #", y = "Coefficient Value") +
          theme_light()
        suppressWarnings(return(plot/plot2+ plot_layout(ncol = 1, heights = c(2, 1))))
      }

      suppressWarnings(return(plot))
    }

  }

  if (Table_Gen[1,2]=="2" & Table_Gen[3,2]=="1" &
      (Table_Gen[6,2]=="numeric"|Table_Gen[6,2]=="integer") & Table_Gen[9,2]=="numeric"|Table_Gen[6,2]=="integer"){
    plot <- ggplot(data = stats::model.frame(LinearMod), mapping = aes(x = stats::model.frame(LinearMod)[,2], y = stats::model.frame(LinearMod)[,1])) +
      geom_smooth(method = "lm", se = T, fill = "#D55E00",
                  color="#D55E00", alpha= .5) + theme_bw() +
      labs(title = "Linear regression with quantitative predictor",
           y=as.character(ModelContrastTable(LinearMod,"x&y")[1,1]),
           x=as.character(ModelContrastTable(LinearMod,"x&y")[2,1]))
    if (data==TRUE){
      plot <- plot+
        geom_point(color="#D55E00", alpha=.3)
    }

    cat(
      "This graphical representation illustrates the linear model, with\n",
      sprintf("%s as the numerical predictor,\n", Table_Gen[8, 2]),
      sprintf("and %s as the numerical dependent variable.\n", Table_Gen[5, 2])
    )

    return(plot)

  }
  else {
    stop("model not idoneo")
  }
}
