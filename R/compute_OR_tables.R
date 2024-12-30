#' get OR univariate model
#'
#' provide statistics table of OR, IC and p-values for glm binomial model applied on each explanatory_vars
#' modele : dependent_var ~ explanatory_vars_i
#'
#' @param dataframe A data.frame, tibble or data.table.
#' @param dependent_var A character. Name of the stratification variable, making groups to compare.
#' @param explanatory_vars A vector of characters. Names of dataframe's columns to describe.
#' @param check_n_levels A logical, Default FALSE. Maybe wanted on qualitative explanatory_vars, so those with 0 levels are removed.
#'  keep numeric variables too.
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#'
#' @return A final table with Variable names, Modalites asso to the given OR (IC, p).
#' @importFrom finalfit summary_factorlist
#' @importFrom finalfit glmuni
#' @importFrom finalfit fit2df
#' @export
#' @examples
#' \dontrun{
#' dependent_var <- "binary_test"
#' explanatory_vars <- c(
#'   "Population", "Income", "Illiteracy", "Life Exp", "Murder",
#'   "HS Grad", "Frost", "state.division",
#'   "var_conti_trap",
#'   "state.region",
#'   "var_quali_trap",
#'   "yes_no_french_question", "all_count_zero",
#'   "var_ab_NAN"
#' )
#' get_OR_univar(
#'   dataframe = modified_state,
#'   dependent_var = dependent_var,
#'   explanatory_vars = explanatory_vars
#' )
#' }
get_OR_univar <- function(
  dataframe,
  dependent_var = "Y",
  explanatory_vars = c("X1", "X2"),
  check_n_levels = FALSE,
  signif_digits = 4
) {
  message("[get_OR_univar] ", length(explanatory_vars), " variables wanted in OR table")

  stopifnot(dependent_var %in% names(dataframe))
  stopifnot(is.factor(dataframe[[dependent_var]]))
  stopifnot(length(explanatory_vars) > 0)
  stopifnot(all(explanatory_vars %in% names(dataframe)))

  dataframe <- data.table::setDT(dataframe)[, .SD, .SDcols = c(dependent_var, explanatory_vars)]

  ## Remove variables with 0 levels (and keep numeric vars) ##
  if (check_n_levels) {
    have_levels <- unlist(lapply(X = explanatory_vars, FUN = function(vari) {
      nlevels(dataframe[[vari]]) > 0
    }))
    are_numeric <- explanatory_vars %in% get_numerics(dataframe = dataframe, vars = explanatory_vars)
    explanatory_vars <- explanatory_vars[have_levels | are_numeric]
    dataframe <- data.table::setDT(dataframe)[, .SD, .SDcols = c(dependent_var, explanatory_vars)]
    message("[get_OR_univar] ", length(explanatory_vars), " variables remaines (numeric or with levels>0)")
  }

  # glmuni function need cleaned names (no spaces for instance)
  dataframe_renamed <- dataframe
  vars_renamed <- gsub(" ", "_", base::iconv(names(dataframe), to = "ASCII//TRANSLIT"))
  names(dataframe_renamed) <- vars_renamed
  # so fix also wanted selection
  dependent_var_renamed <- gsub(" ", "_", base::iconv(dependent_var, to = "ASCII//TRANSLIT"))
  explanatory_vars_renamed <- gsub(" ", "_", base::iconv(explanatory_vars, to = "ASCII//TRANSLIT"))

  ## drop levels never observed :
  dataframe_renamed <- base::droplevels(dataframe_renamed)

  ## need summary to know perfect segmentation or unique levels
  res_summary <- finalfit::summary_factorlist(
    .data = dataframe_renamed,
    dependent = dependent_var_renamed,
    explanatory = explanatory_vars_renamed,
    fit_id = TRUE
  )

  var_index_keep <- unlist(lapply(X = explanatory_vars_renamed, FUN = function(vari) {
    if (grepl("Mean", res_summary$levels[1])) {
      # quanti case
      tmp <- res_summary[grepl(pattern = vari, x = res_summary$fit_id), , drop = FALSE]
    } else {
      # quali cases
      detect_fid_id <- paste0(vari, levels(dataframe_renamed[[vari]]))
      tmp <- res_summary[res_summary$fit_id %in% detect_fid_id, , drop = FALSE]
    }

    if (grepl("Mean", tmp$levels[1])) {
      # detect pefect segmentation
      pefect_segment <- any(apply(X = tmp[1, ], MARGIN = 2, FUN = grepl, "NaN (NA)", fixed = TRUE))

      # keep var if more than 3 values
      # have_enought <- sum(!is.na(dataframe[[vari]])) >= 3

      # keep_vari <- have_enought && !pefect_segment
      keep_vari <- !pefect_segment
    } else {
      # detect pefect segmentation
      pefect_segment <- any(
        apply(
          X = tmp, MARGIN = 2, FUN = function(coli) all(grepl("0 (NaN)", coli, fixed = TRUE))
        )
      )

      # unique levels
      not_only_one_level <- nrow(tmp) > 1 | nlevels(dataframe_renamed[[vari]]) > 1

      # keep var if more than 3 values
      # have_enought <- sum(!is.na(dataframe[[vari]])) >= 3

      # keep_vari <- not_only_one_level && !pefect_segment && have_enought
      keep_vari <- not_only_one_level && !pefect_segment
    }
    return(keep_vari)
  }))

  message("[get_OR_univar] ", "Keep ", sum(var_index_keep), " variables to compute OR")
  explanatory_vars_keep <- explanatory_vars[var_index_keep]
  explanatory_vars_keep_renamed <- explanatory_vars_renamed[var_index_keep]
  dico <- explanatory_vars_keep
  names(dico) <- explanatory_vars_keep_renamed

  ## remove (from final table) OR for modalities with zero in its TC
  var_mod_OR_hide <- unlist(lapply(X = explanatory_vars_renamed, FUN = function(vari) {
    if (grepl("Mean", res_summary$levels[1])) {
      # quanti case
      # tmp <- res_summary[grepl(pattern = vari, x = res_summary$fit_id), , drop = FALSE]
      return(NULL)
    } else {
      # quali cases
      detect_fid_id <- paste0(vari, levels(dataframe_renamed[[vari]]))
      tmp <- res_summary[res_summary$fit_id %in% detect_fid_id, , drop = FALSE]
      OR_var_mod_to_hide <- lapply(X = tmp$fit_id, FUN = function(modalit_i) {
        line_i <- tmp[tmp$fit_id %in% modalit_i, ]
        eff1 <- as.character(line_i[3]) # eff ctrl
        eff2 <- as.character(line_i[4]) # eff cases
        # if zero in TC : hide this OR
        if (grepl("^0 \\(", eff1) || grepl("^0 \\(", eff2)) {
          return(modalit_i)
        } # we will hide OR for this var_mod
        ## if sample size at this mod is < 3 : hide this OR ## no more wanted
        # Ntot <- as.numeric(unlist(strsplit(x = eff1, split = " "))[[1]]) +
        #   as.numeric(unlist(strsplit(x = eff2, split = " "))[[1]])
        # if (Ntot < 3) return(modalit_i)
        return(NULL)
      })
      return(OR_var_mod_to_hide)
    }
  }))

  if (length(explanatory_vars_keep) > 0) {
    #### go for glm univariate ####
    res_glm_uni <- data.table::rbindlist(l = lapply(
      X = explanatory_vars_keep_renamed,
      function(vari) {
        res <- finalfit::fit2df(
          finalfit::glmuni(
            .data = dataframe_renamed,
            dependent = dependent_var_renamed,
            explanatory = vari
          ),
          digits = c(2, 2, 10) # possible to turn as parameter 1 and 2 ? Attention, need to tel pvalue (3) set at 10.
          # Number of digits to round to (1) estimate, (2) confidence interval limits, (3) p-value.
        )
        res$var_renamed <- vari ##  get back names from those levels
        return(data.table::setDT(res))
      }
    ))
    #### get back names and levels ####
    res_glm_uni$Variable <- unlist(lapply(X = res_glm_uni$var_renamed, FUN = function(vari_renamed) {
      dico[vari_renamed]
    }))
    res_glm_uni$Modalites <- unlist(lapply(X = seq_len(nrow(res_glm_uni)), FUN = function(linei) {
      return(gsub(res_glm_uni$var_renamed[linei], "", res_glm_uni$explanatory[linei]))
    }))
    res_glm_uni <- data.table::setDT(res_glm_uni)

    ## hide OR not relevant regarding var_mod_OR_hide
    res_glm_uni <- res_glm_uni[!explanatory %in% var_mod_OR_hide, ]
    ## also hide if NA or Inf is present in IC
    res_glm_uni <- res_glm_uni[!grepl(pattern = "NA|Inf", x = res_glm_uni$OR), ]

    # extract OR_P_valeur
    res_glm_uni$OR_P_valeur <- gsub("p=|p", "", gsub("(.*) \\((.*)-(.*) (.*)\\)", "\\4", res_glm_uni$OR))
    res_glm_uni$OR_P_valeur <- ifelse(
      grepl("<", res_glm_uni$OR_P_valeur), # if detection of p "<" ...
      0.0000000001, # so "<0.0001" will be shown in excel
      signif(as.numeric(res_glm_uni$OR_P_valeur), signif_digits)
    )
    res_glm_uni$OR <- gsub(",", "", gsub("(.*) \\((.*)-(.*) (.*)\\)", "\\1 (\\2-\\3)", res_glm_uni$OR))
    # reorder
    res_glm_uni <- res_glm_uni[, .SD, .SDcols = c("Variable", "Modalites", "OR", "OR_P_valeur")]
  } else {
    # no more variable to analyse...
    message("[get_OR_univar] After exclusion, no more OR computable")
    res_glm_uni <- NULL
  }

  return(res_glm_uni)
}
