#' Visualizing the Structure of a Linear Model
#'
#' @description
#' This function takes a linear model object created with the `lm()` function and returns tabular summaries describing the model structure. It reports the number and types of variables, levels of factors, and the type of contrast matrices applied to categorical predictors.
#'
#' @param LinearModel A linear model object created using `lm()`.
#' @param type A character string specifying the type of table to return. Options include:
#' \itemize{
#'   \item `"all"` (default): Returns both model-level and variable-level summaries.
#'   \item `"summary"`: Returns the model-level summary.
#'   \item `"x&y"`: Returns the variable-level summary (including dependent and independent variables).
#'   \item `"x"`: Returns a summary of independent variables only.
#'   \item `"y"`: Returns a summary of the dependent variable only.
#' }
#'
#' @return A table or a list of tables summarizing the structure of the model, depending on the `type` argument:
#' \itemize{
#'   \item For `"all"`: A list containing both model-level and variable-level summary tables.
#'   \item For `"summary"`: A data frame summarizing overall model structure.
#'   \item For `"x&y"`: A data frame with variable names, types, levels, and contrast types.
#'   \item For `"x"` or `"y"`: A data frame focused on the specified subset of variables.
#' }
#'
#' @examples
#' # Creating a linear model with the generative function
#' DT <- DatasetOne(n_sample = 30, means = c(120, 135, 115), sds = 20)
#' DT$Ind <- as.factor(DT$Ind)
#' model <- lm(Dep ~ Ind, data = DT)
#'
#' # Without specifing the additional parameters (All tables by default)
#' ModelContrastTable(model)
#'
#' # Function with only indipendent variables informations
#' ModelContrastTable(model, type = "x")
#'
#' @export
ModelContrastTable <- function(LinearModel, type = "all") {
  if (inherits(LinearModel, "lm")) {

    formula_terms <- all.vars(stats::formula(LinearModel))
    number_variables <- length(formula_terms)
    number_dependent <- length(all.vars(stats::formula(LinearModel))[1])
    number_independe <- length(all.vars(stats::formula(LinearModel))[-1])
    number_interacti <- length(grep(" * ", as.character(stats::formula(LinearModel))))

    dependent_variable_name <- formula_terms[1]
    independent_variable_names <- formula_terms[-1]


    type_of_response <- rep(NA, number_dependent)
    number_of_levelsD <- rep(NA, number_dependent)
    for (i in 1:number_dependent) {
      type_of_response[i] <- as.character(sapply(LinearModel$model, class)[i])
      if (type_of_response[i] == "factor") {
        number_of_levelsD[i] <- length(levels(LinearModel$model[, i]))
      }
    }

    type_of_predictor <- rep(NA, number_independe)
    number_of_levelsI <- rep(NA, number_independe)
    matrix_type <- rep(NA, number_independe)

    for (j in 1:number_independe) {
      type_of_predictor[j] <- as.character(sapply(LinearModel$model, class)[number_dependent + j])
      if (type_of_predictor[j] == "factor") {
        number_of_levelsI[j] <- length(levels(LinearModel$model[, number_dependent + j]))
      }
    }

    if (sum(type_of_predictor == "factor") > 0) {
      matrixes <- list()
      counter <- 1

      for (k in 1:number_independe) {
        if (is.factor(LinearModel$model[, number_dependent + k])) {
          matrixes[[counter]] <- as.matrix(contrasts(LinearModel$model[, number_dependent + k]))
          counter <- counter + 1
        }
      }

      levels_up <- number_of_levelsI[!is.na(number_of_levelsI)]
      factor_counter <- 1
      for (l in 1:number_independe) {
        if (type_of_predictor[l] == "factor") {
          contrast_matrix <- as.matrix(matrixes[[factor_counter]])

          if (isTRUE(all.equal(unname(contrast_matrix),
                               unname(stats::contr.sum(levels_up[factor_counter]))))) {
            matrix_type[l] <- "sum"
          } else if (isTRUE(all.equal(unname(contrast_matrix),
                                      unname(contr.treatment(levels_up[factor_counter]))))) {
            matrix_type[l] <- "treatment"
          } else if (isTRUE(all.equal(unname(contrast_matrix),
                                      unname(stats::contr.poly(levels_up[factor_counter]))))) {
            matrix_type[l] <- "poly"
          } else if (isTRUE(all.equal(unname(contrast_matrix),
                                      unname(stats::contr.helmert(levels_up[factor_counter]))))) {
            matrix_type[l] <- "helmert"
          } else if (isTRUE(all.equal(unname(contrast_matrix),
                                      unname(MASS::contr.sdif(levels_up[factor_counter]))))) {
            matrix_type[l] <- "sdif"
          } else if (isTRUE(all.equal(unname(contrast_matrix),
                                      unname(ReverseHelmert(levels_up[factor_counter]))))) {
            matrix_type[l] <- "ReverseHelmert"
          } else {
            matrix_type[l] <- "personalizzato"
          }

          factor_counter <- factor_counter + 1
        }
      }
    }


    number_of_levelsD[is.na(number_of_levelsD)] <- "Num"
    number_of_levelsI[is.na(number_of_levelsI)] <- "Num"

    matrix_type_unique <- unique(matrix_type[!is.na(matrix_type)])
    if (length(matrix_type_unique) == 1) {
      contrast_matrix_type_summary <- paste(matrix_type_unique, collapse = ", ")
    } else {
      contrast_matrix_type_summary <- paste(matrix_type_unique, collapse = ", ")
    }

    summary_table_1 <- data.frame(
      Metric = c("Total Variables", "Dependent Variables", "Predictors", "Interactions",
                 "Dependent Variable Name", "Dependent Variable Types", "Dependent Levels",
                 "Predictor Names", "Predictor Types", "Predictor Levels", "Contrast Matrix Types"),
      Value = c(number_variables, number_dependent, number_independe, number_interacti,
                dependent_variable_name,
                paste(type_of_response, collapse = ", "),
                paste(number_of_levelsD, collapse = ", "),
                paste(independent_variable_names, collapse = ", "),
                paste(type_of_predictor, collapse = ", "),
                paste(number_of_levelsI, collapse = ", "),
                contrast_matrix_type_summary)
    )

    variable_names <- c(dependent_variable_name, independent_variable_names)
    variable_types <- c(type_of_response, type_of_predictor)
    variable_levels <- c(number_of_levelsD, number_of_levelsI)
    variable_matrix_type <- c(NA, matrix_type)

    summary_table_2 <- data.frame(
      Variable = variable_names,
      Type = variable_types,
      Levels = variable_levels,
      Contrast_Matrix_Type = variable_matrix_type
    )

    if (type == "all") {
      return(list(
        "Model-Level Summary" = (summary_table_1),
        "Variable-Level Summary" = (summary_table_2)
      ))
    } else if (type == "summary") {
      return((summary_table_1))
    } else if (type == "x&y") {
      return((summary_table_2))
    } else if (type == "y") {
      summary_table_y <- summary_table_2[summary_table_2$Variable == dependent_variable_name, ]
      return((summary_table_y[,-4]))
    } else if (type == "x") {
      summary_table_x <- summary_table_2[summary_table_2$Variable != dependent_variable_name, ]
      return((summary_table_x))
    } else {
      stop("Invalid type parameter. Choose from 'all', 'summary', 'x&y', 'x', or 'y'.")
    }

  } else {
    stop("The input model must be of class 'lm' (linear model).")
  }
}
