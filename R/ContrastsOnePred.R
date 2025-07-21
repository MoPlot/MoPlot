#' Summarize Means and Confidence Intervals for Contrasts in a Single-Predictor Linear Model
#'
#' Computes means, standard errors, and confidence intervals for the positive and negative
#' components of contrast vectors derived from a categorical predictor.
#'
#' @param predictor A string specifying the name of the categorical predictor variable in \code{data}.
#' @param vd A string specifying the name of the numeric dependent variable.
#' @param data A data frame containing the variables.
#' @param sig.level Significance level for the confidence intervals (e.g., 0.05 for 95% CI).
#' @return A data frame with means, standard errors, and confidence intervals for each contrast group.
#' @examples
#' df <- data.frame(
#'   group = factor(rep(c("A", "B", "C"), each = 10)),
#'   score = c(rnorm(10, 5), rnorm(10, 6), rnorm(10, 7))
#' )
#' contrasts(df$group) <- contr.treatment(3)
#' ContrastsOnePred("group", "score", df, 0.05)
#' @importFrom stats contrasts contr.treatment qnorm sd
#' @importFrom MASS contr.sdif
#' @importFrom hypr hypr cmat hmat
#' @export
ContrastsOnePred <- function(predictor, vd, data, sig.level) {
  if (!is.factor(data[[predictor]])) stop("Predictor should be one categorical variable.")
  if (!is.numeric(data[[vd]])) stop("The dependent variable should be numeric.")

  contr_matrix <- stats::contrasts(data[[predictor]])
  dimnames(contr_matrix) <- NULL

  if (identical(contr_matrix,
                matrix(stats::contr.treatment(nrow(contr_matrix)),
                       nrow(contr_matrix),
                       ncol(contr_matrix)))) {
    h <- hypr::hypr()
    hypr::cmat(h, add_intercept = TRUE) <- stats::contr.treatment(nrow(contr_matrix))
    contr_matrix <- t(hypr::hmat(h))[, -1]
  } else if (identical(contr_matrix,
                       matrix(MASS::contr.sdif(nrow(contr_matrix)),
                              nrow(contr_matrix),
                              ncol(contr_matrix)))) {
    h <- hypr::hypr()
    hypr::cmat(h, add_intercept = TRUE) <- contr_matrix
    contr_matrix <- t(hypr::hmat(h))[, -1]
  }

  descrizioni <- data.frame(
    Contrasto = character(),
    Livelli = character(),
    Media = numeric(),
    Se = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    stringsAsFactors = FALSE
  )

  fre <- as.data.frame(table(data[[predictor]]))
  colnames(fre) <- c("Level", "Freq")
  level_names <- levels(data[[predictor]])

  for (i in seq_len(ncol(contr_matrix))) {
    contrasto <- contr_matrix[, i]
    pos <- which(contrasto > 0)
    neg <- which(contrasto < 0)

    if (length(pos) == 0 | length(neg) == 0) {
      message(paste("Contrast_", i, ": Nessun confronto interpretabile"))
      next
    }

    gruppo_pos <- paste(level_names[pos], collapse = ", ")
    gruppo_neg <- paste(level_names[neg], collapse = ", ")

    dati_pos <- data[data[[predictor]] %in% level_names[pos], ]
    dati_neg <- data[data[[predictor]] %in% level_names[neg], ]

    media_c1 <- mean(dati_pos[[vd]], na.rm = TRUE)
    media_c2 <- mean(dati_neg[[vd]], na.rm = TRUE)

    sd_c1 <- stats::sd(dati_pos[[vd]], na.rm = TRUE)
    sd_c2 <- stats::sd(dati_neg[[vd]], na.rm = TRUE)

    n_c1 <- sum(fre$Freq[fre$Level %in% level_names[pos]])
    n_c2 <- sum(fre$Freq[fre$Level %in% level_names[neg]])

    if (n_c1 > 0) {
      se_c1 <- sd_c1 / sqrt(n_c1)
      z_c1 <- stats::qnorm(1 - sig.level / 2)
      ci_low_c1 <- media_c1 - z_c1 * se_c1
      ci_high_c1 <- media_c1 + z_c1 * se_c1
    } else {
      se_c1 <- ci_low_c1 <- ci_high_c1 <- NA
    }

    if (n_c2 > 0) {
      se_c2 <- sd_c2 / sqrt(n_c2)
      z_c2 <- stats::qnorm(1 - sig.level / 2)
      ci_low_c2 <- media_c2 - z_c2 * se_c2
      ci_high_c2 <- media_c2 + z_c2 * se_c2
    } else {
      se_c2 <- ci_low_c2 <- ci_high_c2 <- NA
    }

    temp <- data.frame(
      Contrasto = paste0("Contrast_", i),
      Livelli = c(gruppo_pos, gruppo_neg),
      Media = c(media_c1, media_c2),
      Se = c(se_c1, se_c2),
      CI_low = c(ci_low_c1, ci_low_c2),
      CI_high = c(ci_high_c1, ci_high_c2),
      stringsAsFactors = FALSE
    )

    descrizioni <- rbind(descrizioni, temp)
  }

  suppressWarnings(return(descrizioni))
}

