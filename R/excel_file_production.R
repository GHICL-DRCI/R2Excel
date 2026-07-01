
#' save_excel_results
#'
#' Create Excel file with description of qualitative and quantitative variables
#' 
#' Create Excel file of descriptive analysis
#' Provide descriptive Excel file for factorial (qualitative) 
#'   and continuous (quantitative) data
#' Add statistical test according a variable of stratification
#'
#' @param dataframe A data.frame. Columns must be well formatted with factor or 
#'   numeric class (important). will be converted as data.table inside functions.
#' @param file A character. Name of the Excel file or path + name of the Excel file
#'   (file name must end by ".xlsx").
#'   Default "Excel_report_description.xlsx".
#' @param vars A vector of characters. Names of dataframe's columns to describe.
#'   Only consider numerics, factors and dates. 
#' @param varstrat A character. Default NULL. Name of the stratification variable,
#'   making groups to compare.
#'   Only one varstrat is expected in general.
#'   About 2 crossed varstrats : you can provide "var1*var2" to get a description 
#'   of the 2nd varstrat for each level of the 1st varstrat.
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#'   Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of 
#'   decimal places (signif) for pvalues.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two 
#'   lines should be displayed for binary variables.
#'   TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'   FALSE = both levels of the variables.
#' @param prop_table_margin A vector giving the margins to split by. 
#'   Default 2. 1 indicates rows, 2 indicates columns,
#'   c(1, 2) indicates rows and columns. When x has named dimnames, 
#'   it can be a character vector selecting dimension names.
#' @param force_generate_1_when_0 A logical, Default TRUE.
#'   If TRUE, will test if the unique modality is 0 or "non" and
#'   add the level 1 or "oui" so it can be display in counts.
#'   Can be combined with simplify to only show the modality (1).
#' @param force_non_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality. 
#'  (so will use and show medians instead of means)
#' @param force_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  parametric test, whatever shapiro test said about normality. 
#'  (so will use and show means instead of medians)
#'  This may be useful when considering the central limit theorem or small deviations. 
#' @param show_metric A character, Default "auto". What is the metric to show in
#'  cell content ?
#'  "auto" = mean or median, automatic choice according shapiro test,
#'  "mean" = mean +/- sd forced, whatever shapiro said,
#'  "median" = media [Q1;Q3] forced, whatever shapiro said.
#'  Caution, if you force_non_parametric_test as TRUE, show_metric is forced 
#'  as 'median' and if you force_parametric_test as TRUE, show_metric is forced 
#'  as 'mean', to be consistent. 
#' @param global_summary A logical. Default TRUE. Do you want to get global summary.
#' @param show_OR A logical. Default FALSE. Do you want to get OR, IC and p
#'  (when varstrat is a binary fact).
#' @param show_SMD A logical, Default FALSE. If turn TRUE, add SMD column
#'  (standardized mean difference) between 2 groups.
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail one column by group levels's order.
#' @param light_contents A logical, Default TRUE. If FALSE, the information like 
#'  Nb_mesures, Valeurs manquantes et p will be repeated.
#' @param crossedvarstrat_long_wide A character, "wide" by default, or "long" if,
#'  in case of crossed varstrat, you want each level of the first variable in a column. 
#'  "long" option can be usefull if many levels (more readable).
#' @param do_test A logical, Default TRUE will apply bivariate statistical tests.
#'  do_test = FALSE => show_SMD and show_OR trun FALSE
#' @param show_exact_p A logical, Default FALSE. So if p<0.0001, 
#'  the character "<0.0001" is returned.
#'  If turn TRUE, p-values will be returned will all numbers as numeric.
#' @param show_p_adj A logical, Default FALSE. If turn TRUE, add P_adj_holm column 
#'  based on p.adjust.
#' @param drop_levels A logical, Default FALSE. If turn TRUE, apply droplevels(dataframe)
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#' @param dico_labels A data.frame, Default NULL. If data.frame provided, the first
#'  column must be the vars' name and the 2nd column must be the labels.
#'  Other information will be ignored.
#'
#' @return The name or path + name of the saved Excel file.
#' 
#' Content : 
#' 
#' "Variable"	: a given variable of interest, provided in the dataset, to describe.
#' "Nb_mesures" : N (sample size) shown is the total number of observations 
#' for the given variable, not related to the varstrat (group). 
#' The '_N' shown for each levels give the sample size for each levels 
#' (with maybe missing data, so N is impacted).
#' "Valeurs_manquantes" : Number of missing values for the given variable.
#' Population_totale	: Statistics for the whole dataset.
#' varstrat=level_i : Statistics for the level i of the varstrat (group)
#' P_valeur : P-value.
#' Test : Test related to the P-value
#' Message : Message captured for the statistician's attention
#' 
#' @export
#' @import data.table
#' @importFrom zoo na.locf
#' @importFrom writexl write_xlsx
#' @examples
#' \dontrun{
#' save_excel_results(
#'   dataframe = modified_state,
#'   file = "tmp/Descriptive_bivariate_analysis_0.xlsx",
#'   vars = c("Population", "Income", "state.region", "binary_test")
#' )
#' save_excel_results(
#'   dataframe = modified_state,
#'   file = "tmp/Descriptive_bivariate_analysis_1.xlsx",
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp", "Murder",
#'     "HS Grad", "Frost", "Area",
#'     "state.division", "state.region", "binary_test"
#'   ),
#'   varstrat = "election",
#'   precision = 2
#' )
#' save_excel_results(
#'   dataframe = modified_state,
#'   file = "tmp/Descriptive_bivariate_analysis_2.xlsx",
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp",
#'     "state.division", "state.region", "election"
#'   ),
#'   varstrat = "Area",
#'   precision = 2
#' )
#' save_excel_results(
#'   dataframe = modified_state,
#'   file = "tmp/Descriptive_bivariate_analysis_3.xlsx",
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp", "Murder",
#'     "HS Grad", "Frost", "Area",
#'     "state.division", "state.region",
#'     "special_condition", "special_measures" # trap
#'   ),
#'   varstrat = "election*binary_test",
#'   precision = 2
#' )
#' }
save_excel_results <- function(
    dataframe,
    file = "tmp/description.xlsx",
    vars,
    varstrat = NULL,
    precision = 2, 
    signif_digits = 4,
    simplify = FALSE,
    prop_table_margin = 2,
    force_generate_1_when_0 = TRUE,
    force_non_parametric_test = FALSE,
    force_parametric_test = FALSE,
    show_metric = "auto",
    global_summary = TRUE,
    show_OR = FALSE,
    show_SMD = FALSE,
    detail_NB_measures = FALSE,
    light_contents = TRUE,
    crossedvarstrat_long_wide = "wide", 
    do_test = TRUE,  # v0.1.27
    show_exact_p = FALSE,
    show_p_adj = FALSE,
    drop_levels = FALSE,
    verbose = TRUE,
    dico_labels = NULL
) {
  
  if (verbose) message("[save_excel_results] Starts")

  P_valeur <- NULL
  
  stopifnot(is.data.frame(dataframe))
  stopifnot(vars %in% names(dataframe))
  # stopifnot(digits >= 0)
  stopifnot(precision == "auto" || is.numeric(precision))
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(is.logical(force_non_parametric_test))
  stopifnot(is.logical(force_parametric_test))
  stopifnot(sum(c(force_parametric_test, force_non_parametric_test)) <= 1) # v0.1.27
   # obviously, you can't force both 
  stopifnot(show_metric %in% c("mean", "median", "auto"))
  stopifnot(crossedvarstrat_long_wide %in% c("wide", "long"))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(show_OR))
  stopifnot(is.logical(light_contents))
  stopifnot(is.logical(detail_NB_measures))
  stopifnot(is.logical(do_test))# dev 1.27
  stopifnot(is.logical(show_exact_p)) # dev 1.27
  stopifnot(is.logical(show_p_adj))
  stopifnot(is.logical(show_SMD)) # dev 1.23
  stopifnot(is.logical(drop_levels))
  
  if (!is.null(dico_labels)) {
    stopifnot(is.data.frame(dico_labels))
    if (verbose) {
      message(
        "[save_excel_results] ",
        "We only use the first two columns (re-)named 'Variable' and 'Label'."
      )
    }
    names(dico_labels)[1:2] <- c("Variable", "Label")
    # only select the first 2 columns? should remove others ? 
  }
  
  #### crossed_varstrat: Detect crossed varstrat ####
  if (is.null(varstrat[1]) || varstrat[1] %in% "") {
    # no varstrat
    varstrat <- ""
    crossed_varstrat <- FALSE
  } else {
    # yes there is a varstrat
    varstrat_splited <- strsplit(x = varstrat, split = "*", fixed = TRUE)
    if (length(unlist(varstrat_splited)) == 2) {
      # if there is a 2nd var strat
      varstrat <- unlist(varstrat_splited)
      crossed_varstrat <- TRUE
      stopifnot(varstrat[2] %in% names(dataframe))

      if (drop_levels) dataframe[[varstrat[2]]] <- droplevels(dataframe[[varstrat[2]]]) # v0.1.22
      if (nlevels(dataframe[[varstrat[2]]]) == 1) {
        varstrat <- varstrat[1] # no more 2nd var strat
        crossed_varstrat <- FALSE
        if (verbose) {
          message(
            "[save_excel_results] Warning!!! only 1 level in 2nd varstrat, ",
            "so turn varstrat = varstrat[1]."
          )
        }
      }
    } else {
      crossed_varstrat <- FALSE

      if (drop_levels & is.factor(dataframe[[varstrat]])) { # v0.1.24
        dataframe[[varstrat]] <- droplevels(dataframe[[varstrat]]) # v0.1.22
      }
      # droplevels, stop to test
      if (nlevels(dataframe[[varstrat]]) == 1) {
        varstrat <- NULL # no more var strat
        if (verbose) {
          message(
            "[save_excel_results] Warning!!! ",
            " only 1 level in varstrat, so turn varstrat = NULL."
          )
        }
      }
    }
    stopifnot(varstrat[1] %in% names(dataframe))
  }

  if (drop_levels) { # v0.1.22
    if (verbose) message("[save_excel_results] droplevels(dataframe)")
    dataframe <- droplevels(dataframe)
  }

  ## Update params
  if((force_parametric_test | force_non_parametric_test) & !do_test) {
    if (verbose) message("Warning : you force test so do_test is turned TRUE")
    do_test <- TRUE
  }
  if(show_p_adj & !do_test) {
    if (verbose) message("Warning : you want p_adj so do_test is turned TRUE")
    do_test <- TRUE
  }
  if(show_exact_p & !do_test) {
    if (verbose) message("Warning : you want exact_p so do_test is turned TRUE")
    do_test <- TRUE
  } 
  if (crossed_varstrat) show_p_adj <- FALSE # no adj p-values in cross varstrat situation
  
  # SMD and OR only if do_test = v0.1.27
  if (!do_test) {
    show_SMD <- FALSE
    show_OR <- FALSE
    if (verbose) message("[save_excel_results] do_test = FALSE so show_SMD and show_OR truned FALSE.")
  }
  if (force_non_parametric_test) {
    if (verbose) message("[save_excel_results] Warning : you want force_non_parametric_test so show_metric is turned median")
    show_metric <- "median"
  } 
  #  reverse ? if show_metric == "median", force_non_parametric_test ?? --here if wanted
  if (force_parametric_test) {
    if (verbose) message("[save_excel_results] Warning : you want force_parametric_test so show_metric is turned mean")
    show_metric <- "mean"
  }
  #  reverse ? if show_metric == "mean", force_parametric_test ?? --here if wanted
  

  vars <- setdiff(vars, varstrat) # do not desc the varstrat...
  dt <- data.table::setDT(data.table::copy(dataframe))
  if (is.null(varstrat[1]) || varstrat[1] %in% "") {
    dt <- dt[, .SD, .SDcols = c(vars)]
  } else {
    dt <- dt[, .SD, .SDcols = c(vars, varstrat)]
  }
  
  #### tab_na_sheet_list: Clear useless columns ####
  
  ## remove vars (columns) with all NA # v0.1.22
  dt <- dt[, .SD, .SDcols = colSums(is.na(dt)) < nrow(dt)]
  newcols <- names(dt)
  var_setdiff <- setdiff(vars, newcols)
  if (length(var_setdiff) > 0) {
    if (verbose) {
      message(
        "[save_excel_results] Warning!!! Column(s) ",
        paste0(var_setdiff, collapse = ", "), 
        " removed from vars list, because all NA. => cf sheet 'Variables_all_na'."
      )
    }
    vars <- setdiff(vars, var_setdiff)
    tab_na_sheet_list <- list(data.frame("Variables_all_na" = var_setdiff))
    names(tab_na_sheet_list) <- "Variables_all_na"
  } else {
    tab_na_sheet_list <- NULL
  }

  #### Check vars' class ####
  
  # Separation of variables quali / quanti / dates
  vars_quanti <- get_numerics(dt, vars = vars)
  if (verbose && any(c("Q1", "Q3", "mean", "sd", "median", "min", "max") %in% vars_quanti)) {
    ## trouble... # reported by Klervi in v0.2.0
    message(
      "[save_excel_results] Warning : ", 
      "your dataset have some columns named like statistics ", 
      "(mean, sd, median, min, max, Q1 or Q3), ", 
      "so we suggest to rename them in an other way...", 
      " to avoid troubles !"
    )
  }
  vars_quali <- get_factors(dt, vars = vars)
  vars_dates <- get_dates(dt, vars = vars)
  
  if (verbose && any(!vars %in% c(vars_quanti, vars_quali, vars_dates))) {
    message( # message asked by Laurene
      "[save_excel_results] Warning : Following variables ", 
      paste0(vars[(!vars %in% c(vars_quanti, vars_quali, vars_dates))], collapse = ", "),
      " (from your dataset) are ignored because not factor, numeric or date type...", 
      " Remember to format variables before calling R2Excel functions."
    )
  }
  
  #### Variables quantitatives ####
  
  if (length(vars_quanti) == 0) {
    ##### Skip variables quantitatives #####
    if (verbose) message("[save_excel_results] There is no quantitative variables")
    tab_quanti_sheet_list <- NULL
    
  } else {
    
    ##### Call quanti_sheet #####
    
    if (crossed_varstrat) {
      
      sheet_leveli <- lapply(X = levels(dt[[varstrat[[1]]]]), FUN = function(leveli){
        dti <- data.table::copy(dt)
        dti <- dti[dti[[varstrat[[1]]]] %in% leveli, ]
        if (verbose) message("[save_excel_results] sheet : ", varstrat[[1]], " == ", leveli)
        
        tab_quanti_sheet_list <- quanti_sheet(
          dataframe = dti,
          vars_quanti = vars_quanti,
          varstrat = varstrat[[2]],
          precision = precision,
          signif_digits = signif_digits,
          force_non_parametric_test = force_non_parametric_test,
          force_parametric_test = force_parametric_test,
          show_metric = show_metric,
          global_summary = global_summary,
          show_OR = show_OR,
          show_SMD = show_SMD,
          detail_NB_measures = detail_NB_measures,
          light_contents = light_contents,
          do_test = do_test, 
          verbose = verbose
        )
        dt_sheet <- tab_quanti_sheet_list[[1]]
        
        if (crossedvarstrat_long_wide %in% "wide") {
          names(dt_sheet) <- paste0(
            varstrat[[1]], "==", leveli, "__", names(dt_sheet)
          )
          names(dt_sheet)[1] <- "Variable"
        } else {
          # crossedvarstrat_long_wide %in% "long"
          dt_sheet$varstrat1 <- paste0(varstrat[[1]], " == ", leveli)
          # --done first col
          data.table::setcolorder(
            x = dt_sheet, 
            neworder = unique(c("varstrat1", names(dt_sheet)))
          )
        }
    
        return(dt_sheet)
      })
      
      if (crossedvarstrat_long_wide %in% "long") {
        # easier ... just to rbind...
        tab_quanti_sheet <- data.table::rbindlist(l = sheet_leveli, use.names = TRUE, fill = TRUE)
      } else {
        # original formated wanted by the team : 
        # crossedvarstrat_long_wide = wide
        tab_quanti_sheet <- Reduce(
          f = function(...) merge(..., by = "Variable", sort = FALSE, all = TRUE),
          x = sheet_leveli
        )
      }
      tab_quanti_sheet_list <- list(tab_quanti_sheet)
      names(tab_quanti_sheet_list) <- paste0("quantitative - ", varstrat[[1]], " - ", varstrat[[2]])
      
    } else {
      
      tab_quanti_sheet_list <- quanti_sheet(
        dataframe = dt,
        vars_quanti = vars_quanti,
        varstrat = varstrat,
        precision = precision,
        signif_digits = signif_digits,
        force_non_parametric_test = force_non_parametric_test,
        force_parametric_test = force_parametric_test,
        show_metric = show_metric,
        global_summary = global_summary,
        show_OR = show_OR,
        show_SMD = show_SMD,
        detail_NB_measures = detail_NB_measures,
        light_contents = light_contents,
        do_test = do_test, 
        verbose = verbose
      )
  
    } 

  }

  #### Variables qualitatives ####

  if (length(vars_quali) == 0) {
    
    ##### Skip variables qualitative #####
    # print("Il n'y a pas de variable qualitative")
    if (verbose) message("[save_excel_results] There is no qualitative variables")
    tab_quali_sheet_list <- NULL
    
  } else {
    
    ##### call quali_sheet #####
    
    if (crossed_varstrat) {
      
      qualsheet_leveli <- lapply(X = levels(dt[[varstrat[[1]]]]), FUN = function(leveli){
        dti <- data.table::copy(dt)
        dti <- dti[dti[[varstrat[[1]]]] %in% leveli, ]
        if (verbose) message("[save_excel_results] sheet : ", varstrat[[1]], " == ", leveli)
        
        tab_quali_sheet_list <- quali_sheet(
          dataframe = dti,
          vars_quali = vars_quali,
          varstrat = varstrat[[2]],
          precision = precision,
          signif_digits = signif_digits,
          force_non_parametric_test = force_non_parametric_test,
          force_parametric_test = force_parametric_test,
          show_metric = show_metric,
          global_summary = global_summary,
          show_OR = show_OR,
          show_SMD = show_SMD,
          detail_NB_measures = detail_NB_measures,
          light_contents = light_contents,
          simplify = simplify,
          force_generate_1_when_0 = force_generate_1_when_0,
          prop_table_margin = prop_table_margin,
          do_test = do_test, 
          verbose = verbose
        )
          
        dt_sheet <- tab_quali_sheet_list[[1]]
      
        if (crossedvarstrat_long_wide %in% "wide") {
          names(dt_sheet) <- paste0(
            varstrat[[1]], "==", leveli, "__", names(dt_sheet)
          )
          names(dt_sheet)[1] <- "Variable"
          names(dt_sheet)[2] <- "Modalites"
          ## fill to merge after
          dt_sheet$Variable <- zoo::na.locf(dt_sheet$Variable)
          dt_sheet$Modalites <- zoo::na.locf(dt_sheet$Modalites)
        } else {
          # crossedvarstrat_long_wide %in% "long"
          dt_sheet$varstrat1 <- paste0(varstrat[[1]], " == ", leveli)
          # --done first col
          data.table::setcolorder(
            x = dt_sheet, 
            neworder = unique(c("varstrat1", names(dt_sheet)))
          )
        }
        
        return(dt_sheet)
      })

      if (crossedvarstrat_long_wide %in% "long") {
        # easier ... just to rbind...
        tab_quali_sheet <- data.table::rbindlist(
          l = qualsheet_leveli, use.names = TRUE, fill = TRUE
        )
      } else {
        # original formated wanted by the team : 
        # crossedvarstrat_long_wide = wide
        tab_quali_sheet <- Reduce(
          f = function(...) merge(..., by = c("Variable", "Modalites"), sort = FALSE, all = TRUE),
          x = qualsheet_leveli
        )
        # clean Variable and Modalities
        # under with "light_contents"  arg
        
        ## now light_contents
        if (light_contents) {
          tab_quali_sheet <- data.table::rbindlist(
            l = lapply(
              X = unique(tab_quali_sheet$Variable), FUN = function(vari) {
                tmp <- tab_quali_sheet[tab_quali_sheet$Variable %in% vari, ]
                
                # + add simplify ? --here
                # if (nrow(tmp) == 2 & modalites 0/1 ou oui/non )
                
                # light content in excel cells
                tmp$Variable <- c(as.character(tmp$Variable[1]), rep(NA, nrow(tmp) - 1))
                idx_val <- which(!is.na(tmp[, 3]))[1]
                idx_na <- setdiff(seq_len(nrow(tmp)), idx_val)
                tmp[idx_na, grep("Nb_mesures$", names(tmp))] <- NA
                tmp[idx_na, grep("Valeurs_manquantes$", names(tmp))] <- NA

                return(tmp)
              }
            )
          )
        }
        
      }
      
      tab_quali_sheet_list <- list(tab_quali_sheet)
      names(tab_quali_sheet_list) <- paste0("qualitative - ", varstrat[[1]], " - ", varstrat[[2]])
      
    } else {
      
      tab_quali_sheet_list <- quali_sheet(
        dataframe = dt,
        vars_quali = vars_quali,
        varstrat = varstrat,
        precision = precision,
        signif_digits = signif_digits,
        force_non_parametric_test = force_non_parametric_test,
        force_parametric_test = force_parametric_test,
        show_metric = show_metric,
        global_summary = global_summary,
        show_OR = show_OR,
        show_SMD = show_SMD,
        detail_NB_measures = detail_NB_measures,
        light_contents = light_contents,
        simplify = simplify,
        force_generate_1_when_0 = force_generate_1_when_0,
        prop_table_margin = prop_table_margin,
        do_test = do_test, 
        verbose = verbose
      )
    }
  
  }
    

  #### Show p adjusted ####
  
  if (show_p_adj && !is.null(varstrat[[1]]) && !varstrat[[1]] %in% "") {
   
    if (verbose) message("[save_excel_results] show_p_adj")

    ## adj this other way
    name_quanti <- names(tab_quanti_sheet_list)
    name_quali <- names(tab_quali_sheet_list)
    tab_quanti_sheet_tab <- tab_quanti_sheet_list[[1]]
    tab_quali_sheet_tab <- tab_quali_sheet_list[[1]]
    tab_pval <- data.table::rbindlist(l = list(
      tab_quanti_sheet_tab[, .SD, .SDcols = c("Variable", "P_valeur")],
      tab_quali_sheet_tab[, .SD, .SDcols = c("Variable", "P_valeur")]
    ), use.names = TRUE, fill = TRUE)[
      !is.na(P_valeur),
    ][
      order(P_valeur, decreasing = FALSE), # sorted from the lowest to highest.
    ]
    tab_pval$P_adj_holm <- signif(stats::p.adjust(
      p = tab_pval$P_valeur, method = "holm"
    ), digits = signif_digits)
    tab_pval$P_valeur <- NULL

    if (length(vars_quanti) > 0) {
      tab_quanti_sheet_tab <- merge(
        x = tab_quanti_sheet_tab, y = tab_pval, 
        by = "Variable", all.x = TRUE, sort = FALSE
      )
      # order cols
      pcol <- grep("^P_valeur$", names(tab_quanti_sheet_tab))
      data.table::setcolorder(x = tab_quanti_sheet_tab, neworder = c(
        unique(c(
          names(tab_quanti_sheet_tab)[1:pcol],
          "P_adj_holm", names(tab_quanti_sheet_tab)[(pcol + 1):ncol(tab_quanti_sheet_tab)]
        ))
      ))
      # remake the lists
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- name_quanti
    } else {
      tab_quanti_sheet_list <- NULL
    }

    if (length(vars_quali) > 0) {
      tab_quali_sheet_tab <- merge(
        x = tab_quali_sheet_tab, y = tab_pval, by = "Variable", 
        all.x = TRUE, sort = FALSE
      )
      # order cols
      pcol <- grep("^P_valeur$", names(tab_quali_sheet_tab))
      data.table::setcolorder(x = tab_quali_sheet_tab, neworder = c(
        unique(c(
          names(tab_quali_sheet_tab)[1:pcol], "P_adj_holm",
          names(tab_quali_sheet_tab)[(pcol + 1):ncol(tab_quali_sheet_tab)]
        ))
      ))
      # remake the lists
      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- name_quali
    } else {
      tab_quali_sheet_list <- NULL
    }
    
    
  }

  #### reFormat tab sheet ####
  ## add label from dico_labels for instance... 
  
  if (crossed_varstrat) {
    ##### crossed_varstrat #####
    ## finish to formated crossed vars tabs

    ## --done v0.1.18
    name_quanti <- names(tab_quanti_sheet_list)
    name_quali <- names(tab_quali_sheet_list)
    tab_quanti_sheet_tab <- tab_quanti_sheet_list[[1]]
    tab_quali_sheet_tab <- tab_quali_sheet_list[[1]]

    ## finish to format sheet list with dico_labels ## 

    if (!is.null(tab_quanti_sheet_tab)) { ## --done v0.1.18

      if (!is.null(dico_labels)) {
        # add label #v0.1.22
        tab_quanti_sheet_tab <- merge(
          x = tab_quanti_sheet_tab,
          y = dico_labels,
          by = "Variable", all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quanti_sheet_tab, 
          neworder = unique(c("Label", names(tab_quanti_sheet_tab)))
        )
      }

      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- paste0("quanti-", varstrat[1], "-", varstrat[2])
    } else {
      tab_quanti_sheet_list <- NULL ## --done v0.1.18
    }

    if (!is.null(tab_quali_sheet_tab)) { ## --done v0.1.18

      if (!is.null(dico_labels)) {
        
        # add label #--done v0.1.22
        tab_quali_sheet_tab <- merge(
          x = tab_quali_sheet_tab,
          y = dico_labels,
          by = "Variable", 
          all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quali_sheet_tab,
          neworder = unique(c("Label", names(tab_quali_sheet_tab)))
        )
      
      }

      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- paste0("quali-", varstrat[1], "-", varstrat[2])
    } else {
      tab_quali_sheet_list <- NULL ## --done v0.1.18
    }
    
  } else {
    
    ##### Classique ( no cross varstrat ) #####

    # just format p values, ever light content done if wanted

    name_quanti <- names(tab_quanti_sheet_list)
    name_quali <- names(tab_quali_sheet_list)
    tab_quanti_sheet_tab <- tab_quanti_sheet_list[[1]]
    tab_quali_sheet_tab <- tab_quali_sheet_list[[1]]

    ##### format p val in tab_quanti_sheet_tab #####
    pval_cols_quanti <- c(
      grep("P_valeur$", names(tab_quanti_sheet_tab), value = TRUE),
      grep("P_adj_holm$", names(tab_quanti_sheet_tab), value = TRUE)
    )
    if (length(pval_cols_quanti) > 0) {
      tab_quanti_sheet_tab <- tab_quanti_sheet_tab[,
        (pval_cols_quanti) := lapply(.SD, function(icol) {
          icol_num <- as.numeric(icol)
          if (show_exact_p) {
            return(icol_num)
          } else {
            val <- ifelse(
              test = icol_num < 0.0001,
              yes = "<0.0001",
              no = icol
            )
            return(val)
          }
        }),
        .SDcols = pval_cols_quanti
      ]
    }

    ##### format p val in tab_quali_sheet_tab #####
    pval_cols_quali <- c(
      grep("P_valeur$", names(tab_quali_sheet_tab), value = TRUE),
      grep("P_adj_holm$", names(tab_quali_sheet_tab), value = TRUE)
    )
    if (length(pval_cols_quali) > 0) {
      tab_quali_sheet_tab <- tab_quali_sheet_tab[,
        (pval_cols_quali) := lapply(.SD, function(icol) {
          icol_num <- as.numeric(icol)
          if (show_exact_p) {
            return(icol_num)
          } else {
            val <- ifelse(
              test = icol_num < 0.0001,
              yes = "<0.0001",
              no = icol
            )
            return(val)
          }
        }),
        .SDcols = pval_cols_quali
      ]
    }

    # remake the lists
    if (is.null(name_quanti)) {
      tab_quanti_sheet_list <- NULL
    } else {
      if (!is.null(dico_labels)) {
        # add label #v0.1.22
        tab_quanti_sheet_tab <- merge(
          x = tab_quanti_sheet_tab,
          y = dico_labels,
          by = "Variable", all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quanti_sheet_tab, 
          neworder = unique(c("Label", names(tab_quanti_sheet_tab)))
        )
      }

      # remake the list
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- name_quanti
    }
    if (is.null(name_quali)) {
      tab_quali_sheet_list <- NULL
    } else {
      
      if (!is.null(dico_labels)) {
        # add label
        tab_quali_sheet_tab <- merge(
          x = tab_quali_sheet_tab,
          y = dico_labels,
          by = "Variable", all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quali_sheet_tab, 
          neworder = unique(c("Label", names(tab_quali_sheet_tab)))
        )
        
        # v0.1.24 fix label when quanti varstrat
        if (is.numeric(dt[[varstrat[1]]])) {
          tab_quali_sheet_tab <- merge(
            x = tab_quali_sheet_tab,
            y = dico_labels,
            by.x = "varstrat", by.y = "Variable",
            all.x = TRUE,
            sort = FALSE
          )
          data.table::setcolorder(
            x = tab_quali_sheet_tab, 
            neworder = unique(c(
              "Label.x", "Variable", "Label.y", "varstrat", "Modalites",
              names(tab_quali_sheet_tab)
            ))
          )
        }
      }

      # remake the list
      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- name_quali
    }
  }

  #### Variables Dates #### 
  ## Only describe 
  
  if (length(vars_dates) == 0) {
    ##### Skip variables date #####
    if (verbose) message("[save_excel_results] There is no date variables")
    tab_date_sheet_list <- NULL
    
  } else {
    
    if (crossed_varstrat) {
 
      tab_date_sheet_list <- list(
        data.table::rbindlist(
          lapply(X = levels(dt[[varstrat[[1]]]]), FUN = function(leveli) {
           
            dti <- data.table::copy(dt)
            dti <- dti[dti[[varstrat[[1]]]] %in% leveli, ]
            
            tab1i <- compute_date_table(
              dataframe = dti,
              vars = vars_dates,
              varstrat = varstrat[[2]],
              verbose = verbose
            )
            
            tab1i$varstrat1 <- paste0(varstrat[[1]], " == ", leveli)
            
            data.table::setcolorder(x = tab1i, neworder = unique(c(
              "varstrat1", names(tab1i)
            )))
            return(tab1i)
          }),
          fill = TRUE, use.names = TRUE
        )
      )
      names(tab_date_sheet_list) <- paste0("date - ", varstrat[1], "__", varstrat[2])
      
    } else {
      tab_date_sheet_list <- date_sheet(
        dataframe = dt, 
        varstrat = varstrat, 
        vars_dates = vars_dates,
        verbose = verbose
      )
    }
  }
  
  #### End: Write Excel ####
  if (verbose) message("[save_excel_results] Ends: write_xlsx")
  writexl::write_xlsx(
    x = c(
      tab_quanti_sheet_list,
      tab_quali_sheet_list,
      tab_date_sheet_list,
      tab_na_sheet_list
    ),
    path = file, col_names = TRUE
  )

  return(file)
}


#### SHEETS ####
#### SHEETS ####
#### SHEETS ####

#' quanti_sheet
#' 
#' Compute quantitative variables sheet for Excel output
#'
#' Internal function that processes quantitative variables and prepares
#' the formatted table for Excel export, with optional statistical tests.
#'
#' @param dataframe A data.frame. Columns must be well formatted with factor or 
#'   numeric class (important). will be converted as data.table inside functions.
#' @param vars_quanti A vector of characters. Names of dataframe's columns to describe.
#'   Only consider numerics (subset of vars). 
#' @param varstrat A character. Default NULL. Name of the stratification variable,
#'   making groups to compare.
#'   Only one varstrat is expected in general.
#'   About 2 crossed varstrats : you can provide "var1*var2" to get a description 
#'   of the 2nd varstrat for each level of the 1st varstrat.
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#'   Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Integer indicating the number of 
#'   decimal places (signif) for pvalues.
#' @param force_non_parametric_test A logical. 
#'  You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality. 
#'  (so will use and show medians instead of means)
#' @param force_parametric_test A logical.
#'  You can turn it TRUE if you want to force the use of
#'  parametric test, whatever shapiro test said about normality. 
#'  (so will use and show means instead of medians)
#' @param show_metric A character,
#'  What is the metric to show in cell content ?
#'  "auto" = mean or median, automatic choice according shapiro test,
#'  "mean" = mean +/- sd forced, whatever shapiro said,
#'  "median" = media [Q1;Q3] forced, whatever shapiro said.
#'  Caution, if you force_non_parametric_test as TRUE, show_metric is forced 
#'  as 'median' and if you force_parametric_test as TRUE, show_metric is forced 
#'  as 'mean', to be consistent. 
#' @param global_summary A logical. Do you want to get global summary.
#' @param show_OR A logical. Default FALSE. Do you want to get OR, IC and p
#'  (when varstrat is a binary fact).
#' @param show_SMD A logical, Default FALSE. If turn TRUE, add SMD column
#'  (standardized mean difference) between 2 groups.
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail one column by group levels's order.
#' @param light_contents A logical, Default TRUE. If FALSE, the information like 
#'  Nb_mesures, Valeurs manquantes et p will be repeated.
#' @param do_test A logical, Default TRUE will apply bivariate statistical tests.
#'  do_test = FALSE => show_SMD and show_OR trun FALSE
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#'  
#' @return list with formatted quantitative variables description
#'
#' @export
#' 
quanti_sheet <- function(
    dataframe,
    vars_quanti,
    varstrat,
    precision,
    signif_digits,
    force_non_parametric_test,
    force_parametric_test,
    show_metric,
    global_summary,
    show_OR,
    show_SMD,
    detail_NB_measures,
    light_contents,
    do_test, 
    verbose
) {
  
  # message("[quanti_sheet]")
  
  Variable <- Modalites <- Q1 <- Q3 <- NULL
  
  if (is.null(varstrat)) varstrat <- ""
  
  tab_quanti_sheet_list <- lapply(varstrat, function(varstrat_i) {
    ## for each variable of the group varstrat ##
    
    ###### Describe ######
    mm <- ifelse(
      is.null(varstrat_i) || varstrat_i %in% "",
      paste0("[quanti_sheet] without varstat"),
      paste0("[quanti_sheet] with varstat : ", varstrat_i)
    )
    if (verbose) message(mm)
    
    if (!is.null(varstrat_i) && !varstrat_i %in% "" & is.numeric(dataframe[[varstrat_i]])) {
      
      ###### Varstrat continuous ######
      analyse_desc_quanti <- compute_correlation_table(
        dataframe = dataframe,
        vars = vars_quanti,
        varstrat = varstrat_i,
        method_corr = "detect_auto",
        precision = precision,
        signif_digits = signif_digits, 
        verbose = verbose
      )
      
    } else {
      
      ###### Varstrat factorial or No varstrat ######
      analyse_desc_quanti <- compute_continuous_table(
        dataframe = dataframe,
        vars = vars_quanti,
        varstrat = varstrat_i,
        stats_choice = c(
          "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", 
          "Valeurs_manquantes", "Nb_mesures", "is_Normal"
        ),
        # all stats are excepted in excel workbook function
        precision = precision, 
        verbose = verbose
      )
    }
    
    ###### Format tab sheet ######
    # and
    ##### Statistical tests #####
    
    if (is.null(varstrat_i) || varstrat_i %in% "" || is.numeric(dataframe[[varstrat_i]])) {
      
      ## CAS : Pas de varstrat factorial → Pas de test possible =
      pvaleur_quanti <- NULL
      tab_quanti_sheet <- data.table::as.data.table(
        analyse_desc_quanti, keep.rownames = "Variable"
      )
      
      tab_quanti_sheet <- tab_quanti_sheet[, `:=`(
        `Moy +/- Sd` = paste0(as.character(mean), " +/- ", as.character(sd)),
        `Med [Q1;Q3]` = paste0(median, " [", Q1, ";", Q3, "]"),
        `Min - Max` = paste0(min, " - ", max)
      )]
      
      if (is.null(varstrat_i) || varstrat_i %in% "") {
        ##### no varstrat = no test #####
        tab_quanti_sheet <- tab_quanti_sheet[, .SD, .SDcols = c(
          "Variable", "Nb_mesures", "Valeurs_manquantes",
          "Moy +/- Sd", "Med [Q1;Q3]", "Min - Max", "is_Normal"
        )]
      } else { 
        
        ## case: Varstrat continuous → Tests possibles = 
        
        # is.numeric(dataframe[[varstrat_i]])
        
        ##### stat test varstrat continuous #####
        
        tab_quanti_sheet <- tab_quanti_sheet[, .SD, .SDcols = c(
          # "varstrat",  # do not show
          "Variable", "Nb_mesures", "Valeurs_manquantes",
          # "Moy +/- Sd", "Med [Q1;Q3]", "Min - Max",  # do not show
          "is_Normal", # variable normal ? (not said if varstrat is normal)
          "correlation", "IC95", "P_valeur", "correlation_method", "message"
        )]
        # add varstrat name before correlation
        names(tab_quanti_sheet)[
          grep("^correlation$", names(tab_quanti_sheet))
        ] <- paste0(
          varstrat_i, " correlation"
        )
        
        if (!do_test) {
          # remove pvalues ...
          tab_quanti_sheet$P_valeur <- NULL
        }
        
      }
      
    } else {
      
      ## Case : Varstrat factorial → Tests possibles =
      
      ##### stat test varstrat factorial #####
      
      # = MODIFICATION : Conditionner les tests à do_test = V0.1.27
      if (do_test) {
        
        ##### AVEC TESTS #####
        pvaleur_quanti <- test_means(
          dataframe = dataframe,
          vars = vars_quanti,
          varstrat = varstrat_i,
          force_non_parametric_test = force_non_parametric_test,
          force_parametric_test = force_parametric_test,
          signif_digits = signif_digits, 
          verbose = verbose
        )
        tab_test <- data.table::as.data.table(
          pvaleur_quanti$result, keep.rownames = "Variable"
        )
      } 
      #  else { # SANS TESTS # ne rien faire...   }
      
      tab_quanti_sheet <- data.table::rbindlist(l = lapply(
        X = seq_len(length(analyse_desc_quanti)), FUN = function(i) {
          var_i <- names(analyse_desc_quanti)[i]
          if (verbose) message("[quanti_sheet] ", var_i)
          tmp_raw <- analyse_desc_quanti[i]
          tmp <- data.table::as.data.table(tmp_raw, keep.rownames = "Modalites")
          # names(tmp) <- gsub(paste0(gsub(" ", "\\.", var_i), "\\."), "", names(tmp))
          names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
          tmp$Variable <- var_i
          tmp$Modalites[1] <- "Population_totale"
          
          tmp$Mean_sd <- ifelse(
              is.na(tmp$mean),
              "/", paste0(tmp$mean, " +/- ", tmp$sd)
          )
          tmp$Med_q1_q3 <- ifelse(
              is.na(tmp$median),
              "/", paste0(tmp$median, " [", tmp$Q1, ";", tmp$Q3, "]")
          )
          
          # Metric selection based on test (if do_test)  = v 0.1.27
          if (do_test) {
            # Existing test-based logic
            cell_auto <- ifelse(
              test = tab_test[Variable %in% var_i, "Test"] %in%
                c("Wilcoxon rank sum exact test (Mann-Whitney)", 
                  "Kruskal-Wallis rank sum test"),
              yes = "Med_q1_q3",
              no = "Mean_sd" # default show means if no test
            )
          } else {
            # No test : use is_Normal for description
            # get is_Normal from tmp
            is_normal_var <- tmp$is_Normal[1]
            cell_auto <- ifelse(
              test = is.na(is_normal_var) || !is_normal_var,
              yes = "Med_q1_q3",
              no = "Mean_sd"
            )
          }
          
          cell_content <- ifelse(
            test = show_metric %in% "auto",
            yes = cell_auto, # selection auto according test
            no = ifelse(
              test = show_metric %in% "mean",
              yes = "Mean_sd",
              no = "Med_q1_q3"
            )
          )
          
          tmp$Nb_mesures <- as.character(tmp$Nb_mesures)
          
          # --done 0.1.27 keep N as col
          tmp2_N <- data.table::dcast(
            data = tmp[!Modalites %in% "Population_totale", ],
            formula = stats::as.formula(
              "Variable ~ Modalites"
            ),
            value.var = "Nb_mesures" # ou "N" --here ? 
          )
          names(tmp2_N) <- c("Variable", paste0("", names(tmp2_N)[-1], "_N"))
          
          tmp$Nb_mesures <- ifelse(
            tmp$Nb_mesures[tmp$Modalites %in% "Population_totale"] == "0",
            "0",
            paste0(
              tmp$Nb_mesures[tmp$Modalites %in% "Population_totale"],
              ifelse(
                detail_NB_measures,
                paste0(" ", "(", paste0(
                  tmp$Nb_mesures[!tmp$Modalites %in% "Population_totale"],
                  collapse = "+"
                ), ")"),
                ""
              )
            )
          )
          tmp$Valeurs_manquantes <- as.character(tmp$Valeurs_manquantes)
          tmp$Valeurs_manquantes <- ifelse(
            tmp$Valeurs_manquantes[tmp$Modalites %in% "Population_totale"] == "0",
            "0",
            paste0(
              tmp$Valeurs_manquantes[tmp$Modalites %in% "Population_totale"],
              ifelse(
                detail_NB_measures,
                paste0(" ", "(", paste0(
                  tmp$Valeurs_manquantes[!tmp$Modalites %in% "Population_totale"],
                  collapse = "+"
                ), ")"),
                ""
              )
            )
          )
          
          tmp2 <- data.table::dcast(
            data = tmp,
            formula = stats::as.formula(
              "Variable + Nb_mesures + Valeurs_manquantes ~ Modalites"
            ),
            value.var = cell_content
          )
          
          tmp2 <- merge(tmp2, tmp2_N, by = "Variable")
          # names(tmp2) <- gsub(varstrat_i, paste0(varstrat_i, "="), names(tmp2))
          
          # problem gsub remplace all occurences... 
          # v0.1.27 = sub will replace only the first occurence (if SOIN=SOIN 1, for instance)
          names(tmp2) <- sub(paste0("^", varstrat_i), paste0(varstrat_i, "="), names(tmp2))
          # order "Population_totale"
          
          # alpha_order <- unique(c(
          #   "Variable", "Nb_mesures", "Valeurs_manquantes", "Population_totale",
          #   sort(names(tmp2)) # alpha order yeap
          # ))
          # tmp2 <- tmp2[, .SD, .SDcols = c(alpha_order)]
          # order following levels' order # v1.26 : 13/10/2025
          
          # Reorder the columns
          cols_order <- unlist(lapply(
            X = paste0(varstrat_i, "=", levels(dataframe[[varstrat_i]])),
            FUN = function(x) paste0(x, c("", "_N")))
          )
          data.table::setcolorder(x = tmp2, neworder = c(
            setdiff(names(tmp2), cols_order), cols_order
          ))
          
          return(tmp2)
        }
      ))
      
      if (do_test) {
        if (is.null(tab_test)) {
          stop("[quanti_sheet] ERREUR INTERNE: tab_test est NULL alors que do_test = TRUE")
        }
        # Merger avec tab_test
        tab_quanti_sheet <- merge(
          tab_quanti_sheet, tab_test, by = "Variable", sort = FALSE
        )
        
        ##### Show SMD #####
        if (show_SMD && is.factor(dataframe[[varstrat_i]]) && nlevels(dataframe[[varstrat_i]]) == 2) {
          if (verbose) message("[quanti_sheet] show_SMD")
          SMD_tab <- compute_SMD_table(
            dataframe = dataframe,
            vars = vars_quanti,
            varstrat = varstrat_i,
            precision =  2 # no more passing digits param v0.1.25
          )
          if (!show_SMD || is.null(SMD_tab)) {
            # tab_quanti_sheet <- tab_quanti_sheet # stay the same...
          } else {
            tab_quanti_sheet <- merge(
              x = tab_quanti_sheet,
              y = SMD_tab,
              by = "Variable",
              all = TRUE,
              sort = FALSE
            )
          }
        }
        
        ##### Show OR #####
        if (show_OR && is.factor(dataframe[[varstrat_i]]) && length(levels(dataframe[[varstrat_i]])) == 2) {
          if (verbose) message("[quanti_sheet] show_OR")
          OR_tab <- get_OR_univar(
            dataframe = dataframe,
            dependent_var = varstrat_i,
            explanatory_vars = vars_quanti,
            check_n_levels = FALSE, # not applicable in continuous vars
            signif_digits = signif_digits,
            verbose = verbose
          )
          if (!show_OR || is.null(OR_tab)) {
            # tab_quanti_sheet <- tab_quanti_sheet # stay the same...
          } else {
            OR_tab$Modalites <- NULL
            tab_quanti_sheet <- merge(
              x = tab_quanti_sheet,
              y = OR_tab,
              by = "Variable",
              all = TRUE,
              sort = FALSE
            )
          }
        }
      }
      
      if (!global_summary) {
        col_select <- setdiff(
          names(tab_quanti_sheet),
          grep("Population_totale", names(tab_quanti_sheet), value = TRUE)
        )
        tab_quanti_sheet <- tab_quanti_sheet[, .SD, .SDcols = col_select]
      }
      
      # put message at the end
      if (do_test) {
        col_select <- c(
          setdiff(names(tab_quanti_sheet), c("message")),
          "message"
        )
        tab_quanti_sheet <- tab_quanti_sheet[, .SD, .SDcols = col_select]
      }

    }
    
    return(tab_quanti_sheet)
  })
 
  names(tab_quanti_sheet_list) <- paste0("quantitative - ", varstrat)

  
  return(tab_quanti_sheet_list)
}




#' quali_sheet
#' 
#' Compute qualitative variables sheet for Excel output
#'
#' Internal function that processes qualitative variables and prepares
#' the formatted table for Excel export, with optional statistical tests.
#'
#' @param dataframe A data.frame. Columns must be well formatted with factor or 
#'   numeric class (important). will be converted as data.table inside functions.
#' @param vars_quali A vector of characters. Names of dataframe's columns to describe.
#'   Only consider factors (subset of vars). 
#' @param varstrat A character. Name of the stratification variable,
#'   making groups to compare.
#'   Only one varstrat is expected in general.
#'   About 2 crossed varstrats : you can provide "var1*var2" to get a description 
#'   of the 2nd varstrat for each level of the 1st varstrat.
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#'   Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Integer indicating the number of 
#'   decimal places (signif) for pvalues.
#' @param prop_table_margin A vector giving the margins to split by. 
#'   Default 2. 1 indicates rows, 2 indicates columns,
#'   c(1, 2) indicates rows and columns. When x has named dimnames, 
#'   it can be a character vector selecting dimension names.
#' @param force_generate_1_when_0 A logical, Default TRUE.
#'   If TRUE, will test if the unique modality is 0 or "non" and
#'   add the level 1 or "oui" so it can be display in counts.
#'   Can be combined with simplify to only show the modality (1).
#'  This may be useful when considering the central limit theorem or small deviations. 
#' @param force_non_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality. 
#'  (so will use and show medians instead of means)
#' @param force_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  parametric test, whatever shapiro test said about normality. 
#'  (so will use and show means instead of medians)
#' @param show_metric A character, Default "auto". What is the metric to show in
#'  cell content ?
#'  "auto" = mean or median, automatic choice according shapiro test,
#'  "mean" = mean +/- sd forced, whatever shapiro said,
#'  "median" = media [Q1;Q3] forced, whatever shapiro said.
#'  Caution, if you force_non_parametric_test as TRUE, show_metric is forced 
#'  as 'median' and if you force_parametric_test as TRUE, show_metric is forced 
#'  as 'mean', to be consistent. 
#' @param global_summary A logical. Default TRUE. Do you want to get global summary.
#' @param show_OR A logical. Default FALSE. Do you want to get OR, IC and p
#'  (when varstrat is a binary fact).
#' @param show_SMD A logical, Default FALSE. If turn TRUE, add SMD column
#'  (standardized mean difference) between 2 groups.
#' @param light_contents A logical, Default TRUE. If FALSE, the information like 
#'  Nb_mesures, Valeurs manquantes et p will be repeated.
#' @param simplify A logical. Boolean indicating if one or two 
#'   lines should be displayed for binary variables.
#'   TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'   FALSE = both levels of the variables.
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail one column by group levels's order.
#' @param do_test A logical, Default TRUE will apply bivariate statistical tests.
#'  do_test = FALSE => show_SMD and show_OR trun FALSE
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#'  
#' @return list with formatted qualitative variables description
#'
#' @export
#' 
quali_sheet <- function(
    dataframe,
    vars_quali,
    varstrat,
    precision,
    signif_digits,
    prop_table_margin = 2,
    force_generate_1_when_0,
    force_non_parametric_test,
    force_parametric_test,
    show_metric,
    global_summary,
    show_OR,
    show_SMD,
    light_contents,
    simplify,
    detail_NB_measures,
    do_test, 
    verbose
) {
  # message("[quali_sheet]")
  Variable <- p <- Modalites <- Q1 <- Q3 <- Nb_mesures <- NULL
  
  ##### Classique : No crossed #####
  
  if (is.null(varstrat)) varstrat <- ""
  
  tab_quali_sheet_list <- lapply(X = varstrat, function(varstrat_i) {
    # for each variable of group varstrat #
    mm <- ifelse(
      is.null(varstrat_i) || varstrat_i %in% "",
      paste0("[quali_sheet] ", "without varstat"),
      paste0("[quali_sheet] ", "with varstat : ", varstrat_i)
    )
    if (verbose) message(mm)
    
    ###### Describe #####
    
    # get computed tables
    if (!is.null(varstrat_i) && !(varstrat_i %in% "") && is.numeric(dataframe[[varstrat_i]])) {
      ###### Varstrat continuous ######
      analyse_desc_quali <- lapply(X = vars_quali, FUN = function(quali_i_var) {
        compute_continuous_table(
          dataframe = dataframe,
          vars = varstrat_i,
          varstrat = quali_i_var,
          stats_choice = c(
            "mean", "sd", "median", "Q1", "Q3", "min", "max", "N",
            "Valeurs_manquantes", "Nb_mesures", "is_Normal"
          ),
          # all stats are excepted in excel workbook function
          precision = precision, 
          verbose = verbose
        )
      })
    } else {
      ###### Varstrat factorial or No varstrat ######
      analyse_desc_quali <- compute_factorial_table(
        dataframe = dataframe,
        vars = vars_quali,
        varstrat = varstrat_i,
        simplify = simplify, 
        prop_table_margin = prop_table_margin,
        precision = precision,
        force_generate_1_when_0 = force_generate_1_when_0, 
        verbose = verbose
      )
    }
    
    ###### Format tab sheet ###### 
    # and
    ###### Statistical tests ###### 
    
    # format outputs tab_quali_sheet
    if (is.null(varstrat_i) || varstrat_i %in% "") {
      
      ## Case : no varstrat, no test...
      pvaleur_quali <- NULL
      tab_quali_sheet <- data.table::rbindlist(
        l = lapply(
          X = seq_len(length(analyse_desc_quali)),
          FUN = function(i) {
            var_i <- names(analyse_desc_quali)[i]
            tmp_raw <- analyse_desc_quali[i]
            tmp <- data.table::as.data.table(
              analyse_desc_quali[i],
              keep.rownames = "Modalites"
            )
            names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
            tmp$Variable <- var_i
            tmp$Nb_mesures <- tmp$n[tmp$Modalites %in% "Nb_mesures"]
            tmp$Valeurs_manquantes <- tmp$n[tmp$Modalites %in% "Valeurs_manquantes"]
            tmp$`Effectif (%)` <- paste(tmp$n, tmp$p)
            tmp <- tmp[
              !Modalites %in% c("Nb_mesures", "Valeurs_manquantes"), 
              .SD, .SDcols = c(
                "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes",
                "Effectif (%)"
            )]
            
            if (light_contents) {
              tmp$Variable <- c(tmp$Variable[1], rep(NA, nrow(tmp) - 1))
              tmp$Nb_mesures <- c(tmp$Nb_mesures[1], rep(NA, nrow(tmp) - 1))
              tmp$Valeurs_manquantes <- c(
                tmp$Valeurs_manquantes[1], rep(NA, nrow(tmp) - 1)
              )
            }
            
            return(tmp)
          }
        ),
        fill = TRUE
      )
      
    } else {
      
      ## Case : Varstrat is there = Tests possible, if do_test =
      
      if (do_test) {
        
        ##### With TESTS #####
        
        if (is.numeric(dataframe[[varstrat_i]])) {
          # numeric varstrat..
          
          ##### stat test varstrat continuous #####
          tab_test <- data.table::rbindlist(lapply(X = vars_quali, FUN = function(quali_i_var) {
            if (verbose) message("[quali_sheet] test_means ", quali_i_var)
            pvaleur_quanti <- test_means(
              dataframe = dataframe,
              vars = varstrat_i,
              varstrat = quali_i_var,
              force_non_parametric_test = force_non_parametric_test,
              force_parametric_test = force_parametric_test,
              signif_digits = signif_digits,
              verbose = verbose
            )
            tab_test <- data.table::as.data.table(
              pvaleur_quanti$result,
              keep.rownames = "Variable"
            )
            tab_test$varstrat <- quali_i_var
            return(tab_test)
          }), use.names = TRUE, fill = TRUE)
        } else {
          
          ##### stat test varstrat factorial #####
          pvaleur_quali <- test_proportions(
            dataframe = dataframe,
            vars = vars_quali,
            varstrat = varstrat_i,
            signif_digits = signif_digits
          )
          tab_test <- data.table::as.data.table(
            pvaleur_quali$result,
            keep.rownames = "Variable"
          )
        }
        
      } 
      # else { # SANS TESTS # ne rien faire ... }
      
      ##### Format tab sheet #####
      
      # now we have computed tables and pvalues, reshape table outputs : tab_quali_sheet
      if (is.numeric(dataframe[[varstrat_i]])) {
        
        tab_quali_sheet <- data.table::rbindlist(
          l = lapply(
            X = seq_len(length(analyse_desc_quali)),
            FUN = function(i) {
              var_i <- vars_quali[i]
              if (verbose) message("[quali_sheet] ", var_i)
              tmp_raw <- analyse_desc_quali[[i]]
              tmp <- data.table::as.data.table(
                tmp_raw,
                keep.rownames = "Modalites"
              )
              names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
              
              tmp$Variable <- varstrat_i
              tmp$Modalites <- gsub(var_i, "", tmp$Modalites)
              tmp$Modalites[1] <- "Population_totale"
              tmp$varstrat <- var_i
              
              tmp$Valeurs_manquantes <- tmp[["Valeurs_manquantes"]]
              
              tmp <- tmp[, `:=`(
                `Mean_sd` = paste0(mean, " +/- ", sd),
                `Med_q1_q3` = paste0(median, " [", Q1, ";", Q3, "]"),
                `Min - Max` = paste0(min, " - ", max)
              )]
              # Metric selection based on testing (if do_test) = v0.1.27
              if (do_test) {
                
                cell_auto <- ifelse(
                  test = tab_test[varstrat %in% var_i, "Test"] %in%
                    c("Wilcoxon rank sum exact test (Mann-Whitney)",
                      "Kruskal-Wallis rank sum test"),
                  yes = "Med_q1_q3",
                  no = "Mean_sd" # default show means if no test
                )
                
              } else {
                # No test : use is_Normal
                is_normal_var <- tmp$is_Normal[1]
                cell_auto <- ifelse(
                  test = is.na(is_normal_var) || !is_normal_var,
                  yes = "Med_q1_q3",
                  no = "Mean_sd"
                )
              }
              
              cell_content <- ifelse(
                test = show_metric %in% "auto",
                yes = cell_auto, # selection auto according test
                no = ifelse(
                  test = show_metric %in% "mean",
                  yes = "Mean_sd",
                  no = "Med_q1_q3"
                )
              )
              
              tmp <- tmp[, .SD, .SDcols = c(
                # "Variable",
                "varstrat", "Modalites",
                "N", "Nb_mesures", "Valeurs_manquantes", 
                # "Mean_sd", "Med_q1_q3" # select one of them
                cell_content
                # , "Min - Max", "is_Normal" # not returned with varstrat
              )]
              
              tmp[Nb_mesures %in% "0", cell_content] <- "/" # if n = 0, NaN +/- NA replace by "/"
              
              names(tmp)[grep(cell_content, names(tmp))] <- varstrat_i
              
              return(tmp)
            }
          ),
          fill = TRUE
        )
        
        # Merger tab_test only of do_test = TRUE =
        if (do_test) {
          if (is.null(tab_test)) {
            stop("[quali_sheet] ERREUR INTERNE: tab_test est NULL alors que do_test = TRUE")
          }
          
          # merge computed table and stat test results
          if (light_contents) {
            tab_quali_sheet$line_fill <- tab_quali_sheet$Modalites %in% "Population_totale"
            tab_test$line_fill <- TRUE
            tab_quali_sheet <- merge(
              tab_quali_sheet, tab_test,
              by = c("varstrat", "line_fill"),
              all = TRUE,
              sort = FALSE
            )
            tab_quali_sheet$line_fill <- NULL
          } else {
            tab_quali_sheet <- merge(
              x = tab_quali_sheet,
              y = tab_test,
              by = "varstrat", 
              sort = FALSE
            )
          }
        }
        
      } else {
        
        tab_quali_sheet <- data.table::rbindlist(
          l = lapply(
            X = seq_len(length(analyse_desc_quali)),
            FUN = function(i) {
              var_i <- names(analyse_desc_quali)[i]
              if (verbose) message("[quali_sheet] ", var_i)
              tmp_raw <- analyse_desc_quali[i]
              
              tmp <- data.table::as.data.table(tmp_raw, keep.rownames = "Modalites")
              names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
              tmp$Variable <- var_i
              
              levels_i_n <- grep("^n(.*)", names(tmp), value = TRUE)[-1]
              levels_i_p <- grep("^p(.*)", names(tmp), value = TRUE)[-1]
              tmp$Nb_mesures <- as.character(tmp$Nb_mesures)
              
              levels_i_names <- gsub("^n(.*)", "\\1", levels_i_n)
              
              # --done 0.1.27 keep N as col
              tmp2_N <- tmp[
                Modalites %in% "Nb_mesures",
                .SD, .SDcols = c(
                  "Variable", paste0("n", levels_i_names)
                )
              ]
              names(tmp2_N) <- c("Variable", paste0(levels_i_names, "_N"))
              
              tmp$Nb_mesures <- ifelse(
                tmp$n[tmp$Modalites %in% "Nb_mesures"] == "0",
                "0",
                paste0(
                  tmp$n[tmp$Modalites %in% "Nb_mesures"],
                  ifelse(
                    detail_NB_measures,
                    paste0(" ", "(", paste0(
                      tmp[tmp$Modalites %in% "Nb_mesures", .SD, .SDcols = levels_i_n],
                      collapse = "+"
                    ), ")"),
                    ""
                  )
                )
              )
              tmp$Valeurs_manquantes <- ifelse(
                tmp$n[tmp$Modalites %in% "Valeurs_manquantes"] == 0,
                0,
                paste0(
                  tmp$n[tmp$Modalites %in% "Valeurs_manquantes"],
                  ifelse(
                    detail_NB_measures,
                    paste0(" ", "(", paste0(
                      tmp[tmp$Modalites %in% "Valeurs_manquantes",
                          .SD, .SDcols = levels_i_n],
                      collapse = "+"
                    ), ")"),
                    ""
                  )
                )
              )
              # tmp
             
              tmp2 <- data.table::copy(tmp)[, `:=`(
                `Population_totale` = paste0(n, " ", p)
              )][
                ,
                # c(paste0(varstrat_i, "=", levels_i_names)) := lapply(levels_i_names, FUN = function(namei) {
                c(levels_i_names) := lapply(levels_i_names, FUN = function(namei) {
                  paste(tmp[[paste0("n", namei)]], tmp[[paste0("p", namei)]])
                })
              ]
              tmp2 <- tmp2[
                !Modalites %in% c("Nb_mesures", "Valeurs_manquantes"),
                .SD, .SDcols = c(
                  "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes",
                  "Population_totale",
                  # paste0(varstrat_i, "=", levels_i_names)
                  levels_i_names
                )
              ]
              # tmp2

              tmp2 <- merge(tmp2, tmp2_N, by = "Variable")
              
              name_base <- c(
                "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes",
                "Population_totale"
              )
              name_group <- paste0(varstrat_i, "=", setdiff(names(tmp2), name_base) )
              names(tmp2) <- c(name_base, name_group)
              
              # Réorganiser les colonnes
              cols_order <- unlist(lapply(
                X = paste0(varstrat_i, "=", levels(dataframe[[varstrat_i]])),
                FUN = function(x) paste0(x, c("", "_N")))
              )
              data.table::setcolorder(x = tmp2, neworder = c(
                setdiff(names(tmp2), cols_order), cols_order
              ))
              
              if (light_contents && (nrow(tmp2) - 1) >= 0) { # only applicable if levels > 0
                tmp2$Variable_light <- c(tmp2$Variable[1], rep(NA, nrow(tmp2) - 1))
                # it is needed to keep full Variable if OR wanted
                tmp2$Nb_mesures <- c(tmp2$Nb_mesures[1], rep(NA, nrow(tmp2) - 1))
                tmp2$Valeurs_manquantes <- c(tmp2$Valeurs_manquantes[1], rep(NA, nrow(tmp2) - 1))
                
                # light content also for _N cols # 0.1.27
                # --here to valide
                idx_na <- c(FALSE, rep(TRUE, nrow(tmp2) - 1))
                tmp2[idx_na, grep(paste0(varstrat_i, ".*_N$"), names(tmp2))] <- NA
                tmp2$Variable <- tmp2$Variable_light
                tmp2$Variable_light <- NULL
              }
              
              return(tmp2)
            }
          ),
          fill = TRUE
        )
        
        ## SMD et OR if do_test
        if (do_test) {
          
          ##### Show SMD #####
          if (show_SMD && is.factor(dataframe[[varstrat_i]]) && nlevels(dataframe[[varstrat_i]]) == 2) {
            if (verbose) message("[quali_sheet] show_SMD")
            SMD_tab <- compute_SMD_table(
              dataframe = dataframe,
              vars = vars_quali,
              varstrat = varstrat_i,
              precision = 2 # no more passing digits param v0.1.25
            )
            if (!show_SMD || is.null(SMD_tab)) {
              # tab_quali_sheet <- tab_quali_sheet # stay the same...
            } else {
              tab_quali_sheet <- merge(
                x = tab_quali_sheet,
                y = SMD_tab,
                by = "Variable",
                all = TRUE,
                sort = FALSE
              )
            }
          }
          
          ##### Show OR #####
          if (show_OR && is.factor(dataframe[[varstrat_i]]) && length(levels(dataframe[[varstrat_i]])) == 2) {
            if (verbose) message("[quali_sheet] show_OR")
            OR_tab <- get_OR_univar(
              dataframe = dataframe,
              dependent_var = varstrat_i,
              explanatory_vars = vars_quali,
              check_n_levels = TRUE,
              signif_digits = signif_digits,
              verbose = verbose
            )
            if (!show_OR || is.null(OR_tab)) {
              # tab_quali_sheet <- tab_quali_sheet # stay the same...
            } else {
              tab_quali_sheet <- merge(
                x = tab_quali_sheet,
                y = OR_tab,
                by = c("Variable", "Modalites"), # need in all line here
                all = TRUE,
                sort = FALSE
              )
            }
          }
          
          # merge computed table and stat test results
          if (light_contents) {
            if ("Variable_light" %in% names(tab_quali_sheet)) {
              # it was needed to keep full Variable for OR
              tab_quali_sheet$Variable <- tab_quali_sheet$Variable_light
              tab_quali_sheet$Variable_light <- NULL
            }
            tab_quali_sheet$line_fill <- !is.na(tab_quali_sheet$Nb_mesures)
            tab_test$line_fill <- TRUE
            tab_quali_sheet <- merge(
              tab_quali_sheet, tab_test,
              by = c("Variable", "line_fill"),
              all = TRUE, sort = FALSE
            )
            tab_quali_sheet$line_fill <- NULL
          } else {
            tab_quali_sheet <- merge(
              x = tab_quali_sheet,
              y = tab_test,
              by = "Variable",
              sort = FALSE
            )
          }
          
          
          ## and re order cols, put OR at the end if col present
          if (show_OR && is.factor(dataframe[[varstrat_i]]) && length(levels(dataframe[[varstrat_i]])) == 2) {
            col_select <- c(
              setdiff(names(tab_quali_sheet), c("OR", "OR_P_valeur", "message")),
              "OR", "OR_P_valeur", "message"
            )
            tab_quali_sheet <- tab_quali_sheet[, .SD, .SDcols = col_select]
          }
        }
        
        if (!global_summary) {
          col_select <- setdiff(
            names(tab_quali_sheet),
            grep("Population_totale", names(tab_quali_sheet), value = TRUE)
          )
          tab_quali_sheet <- tab_quali_sheet[, .SD, .SDcols = col_select]
        }
      }
    }
    
    # order : Variable first
    data.table::setcolorder(x = tab_quali_sheet, neworder = c(
      "Variable", setdiff(names(tab_quali_sheet), "Variable")
    ))
    return(tab_quali_sheet)
  })
  names(tab_quali_sheet_list) <- paste0("qualitative - ", varstrat)

  
  return(tab_quali_sheet_list)
  
}


#' date_sheet
#' 
#' Compute dates variables sheet for Excel output
#'
#' Internal function that processes dates variables and prepares
#' the formatted table for Excel export
#' 
#' @param dataframe A data.frame. Columns must be well formatted with factor or 
#'   numeric class (important). will be converted as data.table inside functions.
#' @param vars_dates A vector of characters. Names of dataframe's columns to describe.
#'   Only consider date (subset of vars). 
#' @param varstrat A character. Name of the stratification variable,
#'   making groups to compare.
#'   Only one varstrat is expected in general.
#'   About 2 crossed varstrats : you can provide "var1*var2" to get a description 
#'   of the 2nd varstrat for each level of the 1st varstrat.
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#'  
#' @return list with formatted dates variables description
#'
#' @export
date_sheet <- function(
    dataframe, 
    vars_dates, 
    varstrat, 
    verbose
) {
  # v0.1.24
  if (verbose) message("[date_sheet]")
  
  ##### call date_sheet #####

  tab_date_sheet_list <- list(compute_date_table(
    dataframe = dataframe,
    vars = vars_dates,
    varstrat = varstrat,
    verbose = verbose
  ))
  names(tab_date_sheet_list) <- paste0("date - ", varstrat)

  return(tab_date_sheet_list)
}


