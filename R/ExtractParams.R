#' Extract Effect Sizes and Standardized Coefficients from a Regression Model
#'
#' Calculates Cohen's d and standardized coefficients.
#'
#' @param model A fitted regression model object.
#'
#' @return A data frame.
#'
#' @importFrom magrittr %>%
#' @keywords internal
ExtractParams <- function(model) {
  summ <- summary(model)
  coefs <- as.data.frame(summ$coefficients)
  coefs$Term <- rownames(coefs)

  results <- dplyr::filter(coefs, Term != "(Intercept)") %>%
    dplyr::mutate(
      d = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_
    )

  for (i in seq_len(nrow(results))) {
    term <- results$Term[i]
    stat <- if ("t value" %in% colnames(coefs)) "t" else if ("z value" %in% colnames(coefs)) "z" else NULL
    stat_value <- coefs[term, paste0(stat, " value")]
    df_val <- tryCatch(stats::df.residual(model), error = function(e) NA)

    d_res <- tryCatch({
      if (stat == "t") {
        effectsize::t_to_d(t = stat_value, df = df_val, ci = 0.95)
      } else if (stat == "z") {
        effectsize::z_to_d(z = stat_value, ci = 0.95)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(d_res) && !is.null(d_res$d)) {
      results$d[i] <- d_res$d
      results$CI_low[i] <- d_res$CI_low
      results$CI_high[i] <- d_res$CI_high
    }
  }

  # Remove Term column before adding standardized params
  results$Term <- NULL
  std <- parameters::standardize_parameters(model)
  std <- std[-1, ]
  results$st_param <- std$Std_Coefficient
  results$st_param_lci <- std$CI_low
  results$st_param_hci <- std$CI_high

  suppressWarnings(return(results))
}

