#' Print Descriptive Summaries for Custom Contrasts (Internal Use)
#'
#' Intended for internal usage
#' @param output_contrasti A data frame output
#' @param sig.level Significance level
#' @param digits Number of decimal places
#' @return No return value.
#' @keywords internal

CustomDescription <- function(output_contrasti, sig.level = 0.05, digits = 2) {
  contrasti_unici <- unique(output_contrasti$Contrasto)

  for (contrasto in contrasti_unici) {
    subset <- output_contrasti[output_contrasti$Contrasto == contrasto, ]

    if (nrow(subset) != 2) {
      cat(paste0("W: ", contrasto, " not interpretable (at least two groups).\n\n"))
      next
    }

    gruppo_1 <- subset$Livelli[1]
    gruppo_2 <- subset$Livelli[2]

    media_1 <- round(subset$Media[1], digits)
    media_2 <- round(subset$Media[2], digits)

    se_1 <- round(subset$Se[1], digits)
    se_2 <- round(subset$Se[2], digits)

    ci_1 <- paste0("[", round(subset$CI_low[1], digits), ", ", round(subset$CI_high[1], digits), "]")
    ci_2 <- paste0("[", round(subset$CI_low[2], digits), ", ", round(subset$CI_high[2], digits), "]")

    cat(sprintf(
      "In %s, the contrast compares:\n- Group 1: %s (Mean = %.2f, SE = %.2f, %.0f%% CI = %s)\n- Group 2: %s (Mean = %.2f, SE = %.2f, %.0f%% CI = %s)\n\n",
      contrasto, gruppo_1, media_1, se_1, 100 * (1 - sig.level), ci_1,
      gruppo_2, media_2, se_2, 100 * (1 - sig.level), ci_2
    ))

    cat(sprintf("This contrast evaluates whether the mean of %s differs from that of %s.\n\n",
                gruppo_1, gruppo_2))
  }
}
