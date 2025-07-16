#' Generate a Dataset with One Categorical Independent Variable and One Continuous Dependent Variable
#'
#' This function simulates a dataset with multiple groups (levels of one IV),
#' each with a specified mean and a shared standard deviation. The dependent
#' variable is continuous, and the standard deviation is constant across groups,
#' emulating homoscedasticity. The number of observations per group is equal,
#' creating a balanced design.
#'
#' @param n_sample Number of observations per group.
#' @param means A numeric vector of group means.
#' @param sds A single numeric value representing the shared standard deviation.
#'
#' @return A data.frame with columns \code{iv} (group identifier) and \code{dv} (dependent variable).
#'
#' @examples
#' set.seed(2025)
#' GenerateDataset1IV(n_sample = 30, means = c(120, 135, 115), sds = 20)
#'
#' @export
GenerateDataset1IV <- function(n_sample, means, sds) {
  n_groups <- length(means)

  dfContrasts <- matrix(NA, nrow = n_sample, ncol = n_groups)

  for (i in seq_along(means)) {
    dfContrasts[, i] <- stats::rnorm(n_sample, mean = means[i], sd = sds)
  }

  colnames(dfContrasts) <- paste("G", seq_len(n_groups), sep = ".")

  dfLong <- data.frame(
    iv = rep(colnames(dfContrasts), each = n_sample),
    dv = as.vector(dfContrasts)
  )

  return(dfLong)
}
ilmiofioRe97
