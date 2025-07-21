#' Generate Reverse Helmert Contrast Matrix
#'
#' Creates a reverse Helmert contrast matrix of size \code{n x (n - 1)}.
#' For internal usage
#'
#' @param n Integer. The number of levels of a factor (must be >= 2).
#' @return A numeric matrix
#' @keywords internal
ReverseHelmert <- function(n) {
  mat <- matrix(0, n, n - 1)
  for (i in 2:n) {
    mat[i, i - 1] <- 1
    mat[1:(i - 1), i - 1] <- -1 / (i - 1)
  }
  suppressWarnings(return(mat))
}
