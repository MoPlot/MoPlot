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
#' @examples
#' set.seed(2025)
#' DatasetOne(n_sample = 30, means = c(120, 135, 115), sds = 20)
#'
#' @export
DatasetOne <- function(n_sample,
                                  means,
                                  sds) {

  dfContrasts <- data.frame(x = rep(NA, n_sample))

  for (i in 1:length(means)) {
    dfContrasts[, i] <- stats::rnorm(n_sample, means[i], sds)
  }

  colnames(dfContrasts) <- paste("Gr.", 1:length(means), sep = "_")

  dfLong <- data.frame(Group = rep(colnames(dfContrasts), each = n_sample),
                       Value = as.vector(as.matrix(dfContrasts)))

  names(dfLong) <- c("Ind", "Dep")
  return(dfLong)
}
