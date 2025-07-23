#' Group Contrast Columns by Interaction Structure
#'
#' For internal usage
#'
#' @param contrast_matrix A design or contrast matrix
#' @param data The original data frame used to fit the model
#' @param model A fitted model object
#'
#' @return A named list
#' @keywords internal
GroupContrastCol <- function(contrast_matrix, data, model) {
  original_vars <- names(Filter(is.factor, data))

  infer_base_var <- function(name) {
    matched <- original_vars[startsWith(name, original_vars)]
    if (length(matched) > 0) return(matched[1])
    return(NA)
  }

  extract_interaction <- function(colname) {
    if (!grepl(":", colname)) return(NA)
    parts <- unlist(strsplit(colname, ":"))
    base_parts <- vapply(parts, infer_base_var, character(1))
    base_parts <- base_parts[!is.na(base_parts)]
    paste((unique(base_parts)), collapse = ":")
  }

  col_names <- colnames(contrast_matrix)
  interaction_labels <- sapply(col_names, extract_interaction)
  interaction_indices <- which(!is.na(interaction_labels))
  grouped_indices <- split(interaction_indices, interaction_labels[interaction_indices])

  grouped_indices <- grouped_indices[
    order(
      sapply(strsplit(names(grouped_indices), ":"), length),
      names(grouped_indices)
    )
  ]

  result <- list()
  for (interaction in names(grouped_indices)) {
    vars_in_interaction <- unlist(strsplit(interaction, ":"))
    orthogonal_vars <- FindOrthogonal(model)

    result[[interaction]] <- list(
      columns = grouped_indices[[interaction]],
      orthogonal = intersect(orthogonal_vars,vars_in_interaction)
    )
  }

  return(result)
}
