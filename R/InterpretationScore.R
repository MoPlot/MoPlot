#' Compute Interpretation Score for Categorical Contrasts
#'
#' For internal usage
#'
#' @param variabile A factor or categorical variable
#' @param tol A numeric tolerance (default `1e-8`)
#'
#' @return An integer interpretation score
#'
#' @keywords internal
#' @import MASS
InterpretationScore <- function(variabile, tol = 1e-8) {
  contr <- contrasts(variabile)
  contr <- as.matrix(contr)
  cor_matrix <- stats::cor(contr)
  is_orthogonal <- all(abs(cor_matrix[lower.tri(cor_matrix)]) < tol)
  dimnames(contr) <- NULL
  n <- nrow(contr)

  poly_ref <- as.matrix(stats::contr.poly(n));     dimnames(poly_ref) <- NULL
  helm_ref <- as.matrix(stats::contr.helmert(n));  dimnames(helm_ref) <- NULL
  tmp0.1=apply(helm_ref,2,rev)
  tmp0.2=tmp0.1[,ncol(tmp0.1):1]
  helm_inv <- tmp0.2

  approx_equal <- function(a, b, tol = 1e-8) {
    all(dim(a) == dim(b)) && all(abs(a - b) < tol)
  }

  if (is.null(contr) || nrow(contr) == 2){
    return(0)
  }else if (approx_equal(contr, poly_ref, tol)){
    return(1)
  } else if (approx_equal(contr, helm_ref, tol) || approx_equal(contr, helm_inv, tol)){
    return(2)
  } else if (is_orthogonal &&
             !approx_equal(contr, poly_ref, tol) &&
             !(approx_equal(contr, helm_ref, tol) || approx_equal(contr, helm_inv, tol))){
    return(3)
  } else {
    return(0)
  }

}
