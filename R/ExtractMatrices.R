#' Extract Contrast and Hypothesis Matrices from a Model
#'
#' For internal usage.
#'
#' @param model A fitted model object
#' @param data A data frame used to fit the model
#'
#' @return A named list
#' @keywords internal
#'
#' @importFrom insight find_predictors
#' @importFrom dplyr group_by summarise across %>% all_of
#' @importFrom MASS ginv
#' @importFrom hypr ginv2
ExtractMatrices <- function(model, data) {

  predictors <- insight::find_predictors(model, flatten = TRUE)
  predictors <- predictors[predictors %in% names(data)]
  mm <- stats::model.matrix(model)
  df_mm <- cbind(data[, predictors, drop = FALSE], as.data.frame(mm))
  if (length(predictors) > 0) {
    contrasts_cut <- df_mm %>%
      group_by(across(all_of(predictors))) %>%
      summarise(across(dplyr::where(is.numeric), mean), .groups = "drop")
    rownames(contrasts_cut) <- do.call(paste, c(contrasts_cut[predictors], sep = ":"))
  } else {
    contrasts_cut <- summarise(df_mm, across(dplyr::where(is.numeric), mean))
    rownames(contrasts_cut) <- "(Intercept)"
  }

  overall_matrix <- as.data.frame(contrasts_cut)
  categorical_vars <- predictors[sapply(data[predictors], is.factor)]
  rownames(overall_matrix) <- do.call(paste, c(contrasts_cut[predictors], sep = ":"))
  col_index <- which(colnames(overall_matrix) == "(Intercept)")
  cont_mat <- overall_matrix[, -(1:col_index), drop = FALSE]
  hyp_mat <- as.data.frame(t(ginv2(as.matrix(cont_mat))))

  return(list(
    contrast_matrix = cont_mat,
    hypothesis_matrix = hyp_mat
  ))
}
