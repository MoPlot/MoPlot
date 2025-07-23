#' Compute Grouped Means and Confidence Intervals for an Interaction Contrast
#'
#' For internal usage
#'
#' @param interaction_name A character string naming the interaction term (e.g., `"A:B"`).
#' @param model A fitted model object (e.g., from `lm()`).
#' @param data A data frame used to fit the model.
#' @param vd A string specifying the name of the dependent (response) variable.
#' @param sig.level Significance level for computing confidence intervals (default is `0.05`).
#'
#' @return A named list of data frames
#' @importFrom dplyr filter group_by summarise across %>% bind_rows
#' @importFrom stats qnorm sd
#' @importFrom insight find_predictors
#' @importFrom MASS contr.sdif
#'
#' @keywords internal
IntMeanCont <- function(interaction_name, model, data, vd, sig.level=0.05) {
  matrices <- ExtractMatrices(model, data)
  contrast_matrix <- matrices$contrast_matrix
  hypothesis_matrix <- matrices$hypothesis_matrix
    predictors <- insight::find_predictors(model, flatten = TRUE)
  factor_vars <- predictors[predictors %in% names(data)][sapply(data[predictors], is.factor)]

  usa_hypothesis <- any(sapply(factor_vars, function(var) {
    contr <- contrasts(data[[var]])
    rownames(contr) <- 1:nrow(contr)
    isTRUE(all.equal(contr, contr.treatment(nrow(contr)))) ||
      isTRUE(all.equal(contr, MASS::contr.sdif(nrow(contr))))
  }))

  matrix_to_use <- if (usa_hypothesis) hypothesis_matrix else contrast_matrix

  row_names <- rownames(matrix_to_use)
  row_levels_df <- as.data.frame(do.call(rbind, strsplit(row_names, ":")), stringsAsFactors = FALSE)
  colnames(row_levels_df) <- predictors

  gruppi <- GroupContrastCol(matrix_to_use, data, model)
  colnames_interazione <- gruppi[[interaction_name]]$columns
  contrasti <- colnames(matrix_to_use)[colnames_interazione]

  vars <- unlist(strsplit(interaction_name, ":"))

  complessita <- sapply(vars, function(v) InterpretationScore(data[[v]]))
  target_var <- vars[which.max(complessita)]
  split_vars <- setdiff(vars, target_var)

  risultati <- list()

  for (col in contrasti) {
    pesi <- matrix_to_use[[col]]
    attive <- which(pesi != 0)
    pesi_attivi <- pesi[attive]
    livelli_attivi <- row_levels_df[attive, , drop = FALSE]
    livelli_attivi$peso <- NA
    livelli_attivi$peso <- pesi_attivi
    livelli_attivi <- livelli_attivi[, c(vars, "peso"), drop = FALSE]

    split_combos <- unique(livelli_attivi[split_vars])

    temp <- data.frame()

    for (i in seq_len(nrow(split_combos))) {
      filtro_split <- Reduce(`&`, lapply(split_vars, function(v) {
        livelli_attivi[[v]] == split_combos[[v]][i]
      }))

      subset_livelli <- livelli_attivi[filtro_split, ]

      gruppi <- list(
        Positivo = subset_livelli %>% filter(peso > 0),
        Negativo = subset_livelli %>% filter(peso < 0)
      )

      for (gname in names(gruppi)) {
        gruppo <- gruppi[[gname]]
        lvls_target <- unique(gruppo[[target_var]])

        filtro_dati <- Reduce(`&`, lapply(split_vars, function(v) {
          data[[v]] == split_combos[[v]][i]
        })) & data[[target_var]] %in% lvls_target

        dati_subset <- data[filtro_dati, ]
        if (nrow(dati_subset) == 0) next

        media <- mean(dati_subset[[vd]], na.rm = TRUE)
        se <- sd(dati_subset[[vd]]) / sqrt(length(dati_subset[[vd]]))
        z <- qnorm(1 - sig.level / 2)
        ci_low <- media - z * se
        ci_high <- media + z * se


        row_out <- data.frame(
          Contrasto = paste0("Contrast ", which(contrasti == col)),
          Media = media,
          Se=se,
          CI_low=ci_low,
          CI_high=ci_high,
          stringsAsFactors = FALSE
        )
        row_out[[target_var]] <- paste(lvls_target, collapse = ", ")

        # Aggiungi una colonna per ciascuna variabile di split
        for (v in split_vars) {
          row_out[[v]] <- split_combos[[v]][i]
        }

        temp <- rbind(temp, row_out)
      }
    }

    risultati[[paste0("Contrast ", col)]] <- temp
  }

  return(risultati)
}
