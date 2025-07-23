#' Identify Factor Variables with Orthogonal Contrasts
#'
#' For internal usage
#'
#' @param model A fitted model object
#'
#' @return A character vector of variable names whose contrast matrices are orthogonal.
#' @keywords internal
FindOrthogonal <- function(model) {
  data <- stats::model.frame(model)
  categorical_vars <- names(Filter(is.factor, data))

  ortho_vars <- c()

  for (var in categorical_vars) {
    contrast_matrix <- contrasts(data[[var]])
    if (is.null(contrast_matrix)) next
    if (ncol(contrast_matrix) < 2) next
    corr_matrix <- stats::cor(contrast_matrix)
    off_diag_values <- corr_matrix[lower.tri(corr_matrix) | upper.tri(corr_matrix)]
    if (all(abs(off_diag_values) < 1e-10)) {
      ortho_vars <- c(ortho_vars, var)
    }
  }
  if (length(ortho_vars) == 0) {
    message("Nessuna variabile categoriale ha contrasti ortogonali.")
    return(NULL)
  } else {
    message("Variabili con contrasti ortogonali: ", paste(ortho_vars, collapse = ", "))
    return(ortho_vars)
  }
}
