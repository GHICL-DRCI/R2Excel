
#' save_excel_paired_results
#'
#' Tables for paired or longitudinal data, saved in Excel file
#'
#' Create Excel file of descriptive analysis
#' Provide descriptive Excel file for factorial (qualitative) 
#'   and continuous (quantitative) data
#' Add statistical paired or longitudinal test according the time variable of stratification
#' 
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#'  Columns must be well formated with factor or numeric class (important).
#' @param file A character. Path and name of the excel results file.
#' @param vars A vector of characters. Name of columns to describe and test.
#'  Only consider numerics, factors and dates.
#' @param varstrat A characters. Not null. Names of the stratification variable, 
#'  making groups to compare (*time* or *visits* for instance).
#'  The repeated measures must be presented *in line* (for instance, "V1", "V2", "V3" 
#'  will be set on 3 lines for each individuals, in the column "visits").
#'  About 2 *crossed varstrats* : you can provide "var1*var2", and so
#'  get a description of the 2nd varstrat (var2) for each level of
#'  the 1st varstrat (var1). However, in the "paired" function, the stat test 
#'  will not be performed in the case of a crossed varstrat 
#'  (do_test will be set to FALSE) to avoid the wrong test being used if
#'   the variable order is not respected ( "visit*group" or "grpup*visit").
#' @param patient_id A character. Default "patientid". 
#'  Name of patient identifier (id) column.
#'  The repeated measures must be presented in line 
#'  (for instance, "id1" will be set on 3 line if he has 3 visits).
#' @param precision mode: "auto" (adaptive) or numeric (fixed)
#'  Integer indicating the number of decimal places (round).
#'  Default = 2.
#' @param signif_digits A integer, Default 4. Integer indicating the number of 
#'  decimal places (signif) for p-values.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines
#'  should be displayed for binary variables.
#'  TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'  FALSE = both levels of the variables.
#' @param keep_missing_line A logical, Default TRUE. 
#'  Do you want to keep the missing data count (like a level)
#' @param global_summary A logical. Default FALSE Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... 
#'  but ok if you want it, you can.
#' @param force_non_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#' @param force_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  parametric test, whatever shapiro test said about normality. 
#'  (so will use and show means instead of medians)
#'  This may be useful when considering the central limit theorem or small deviations. 
#' @param force_generate_1_when_0 A logical, Default TRUE. 
#'  If TRUE, will test if the unique modality is 0 or "non" and
#'  add the level 1 or "oui" so it can be display in counts. 
#'  Can be combined with simplify to only show the modality (1).
#' @param show_metric A character, Default "auto". What is the metric to show in 
#'  cell content ?
#'  "auto" = mean or median, automatic choice according shapiro test,
#'  "mean" = mean +/- sd forced, whatever shapiro said,
#'  "median" = media [Q1;Q3] forced, whatever shapir said.
#'  Caution, if you force_non_parametric_test as TRUE, show_metric is forced 
#'  as 'median' and if you force_parametric_test as TRUE, show_metric is forced 
#'  as 'mean', to be consistent. 
#' @param light_contents A logical, Default TRUE. 
#'  If FALSE, the information like "Nb_mesure", "Valeurs manquantes" et "p" 
#'  will be repeated.
#' @param crossedvarstrat_long_wide A character, "wide" by default, or "long" if,
#'  in case of crossed varstrat, you want each level of the first variable in a column. 
#'  "long" option can be usefull if many levels (more readable).
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail (N1 + N2 + ...) for each group.
#' @param do_test A logical, Default TRUE. 
#' To prevent the statistical test from returning a result, set the value to FALSE.
#' @param show_exact_p A logical, Default FALSE. 
#'  So if p<0.0001, the character "<0.0001" is returned.
#'  If turn TRUE, p-values will be returned will all numbers as numeric.
#' @param show_p_adj A logical, Default FALSE. 
#'  If turn TRUE, add P_adj_holm column based on p.adjust.
#' @param drop_levels A logical, Default FALSE. 
#'  If turn TRUE, apply droplevels(dataframe)
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#' @param dico_labels A data.frame, Default NULL. If data.frame provided,
#'  the first column must be the vars' name and 
#'  the 2nd column must be the labels. Other information will be ignored.
#'  
#' @return A character. Path and name of the excel results file.
#'
#' @export
#' @import data.table
#' @importFrom zoo na.locf
#' @importFrom writexl write_xlsx
#' @examples
#' \dontrun{
#' save_excel_paired_results(
#'   dataframe = modified_sleep,
#'   vars = c("extra", "extra_with_missings", "mesure1", "mesure2", "mesure3"),
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   precision = 1,
#'   global_summary = TRUE,
#'   file = file.path("tmp", "desc_paired_data1.xlsx")
#' )
#' save_excel_paired_results(
#'   dataframe = modified_sleep,
#'   vars = c("extra", "extra_with_missings", "mesure1", "fact1", "fact1_na"),
#'   varstrat = c("visites_2"),
#'   patient_id = "ID2",
#'   precision = 2,
#'   global_summary = TRUE,
#'   force_generate_1_when_0 = FALSE, # for fact tab
#'   keep_missing_line = TRUE, # for fact tab
#'   file = file.path("tmp", "desc_paired_data3.xlsx")
#' )
#' save_excel_paired_results(
#'   dataframe = modified_sleep,
#'   vars = c("extra", "extra_with_missings", "mesure1", "mesure3", "fact1", "fact1_na"),
#'   varstrat = "visites_2*fact3",
#'   patient_id = "ID2",
#'   precision = 2,
#'   global_summary = TRUE,
#'   force_generate_1_when_0 = FALSE, # for fact tab
#'   keep_missing_line = TRUE, # for fact tab
#'   file = file.path("tmp", "desc_paired_data4.xlsx")
#' )
#' save_excel_paired_results(
#'   dataframe = modified_sleep,
#'   vars = c("extra", "extra_with_missings", "mesure1", "mesure3", "fact3", "fact1_na"),
#'   varstrat = "visites_2*fact1",
#'   patient_id = "ID2",
#'   precision = 2,
#'   global_summary = TRUE,
#'   force_generate_1_when_0 = FALSE, # for fact tab
#'   keep_missing_line = TRUE, # for fact tab
#'   detail_NB_measures = TRUE,
#'   file = file.path("tmp", "desc_paired_data5.xlsx")
#' )
#' }
save_excel_paired_results <- function(
    dataframe,
    file = "paired_desc_table.xlsx",
    vars,
    varstrat,
    patient_id = "patientid",
    precision = 2,
    signif_digits = 4,
    simplify = FALSE,
    keep_missing_line = TRUE, # for fact tab
    global_summary = FALSE,
    force_non_parametric_test = FALSE,
    force_parametric_test = FALSE,
    force_generate_1_when_0 = TRUE, # for fact tab
    show_metric = "auto",
    light_contents = TRUE,
    crossedvarstrat_long_wide = "wide", #  v0.2.0
    detail_NB_measures = FALSE, # v0.1.27
    do_test = TRUE,  # v0.1.27
    show_exact_p = FALSE,
    show_p_adj = FALSE,
    drop_levels = FALSE,
    verbose = TRUE,
    dico_labels = NULL
) {
  
  if (verbose) message("[save_excel_paired_results] Starts")
  
  P_valeur <- NULL
  
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(patient_id %in% names(dataframe))
  stopifnot(!is.null(varstrat))
  stopifnot(!varstrat %in% "")
  stopifnot(length(varstrat) == 1)
  # stopifnot(digits >= 0)
  stopifnot(precision == "auto" || is.numeric(precision))
  stopifnot(signif_digits >= 0)
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(force_non_parametric_test))
  stopifnot(is.logical(force_parametric_test))
  stopifnot(sum(c(force_parametric_test, force_non_parametric_test)) <= 1) # v0.1.27
  # obviously, you can't force both 
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(is.logical(keep_missing_line))
  stopifnot(show_metric %in% c("mean", "median", "auto"))
  stopifnot(crossedvarstrat_long_wide %in% c("wide", "long"))
  stopifnot(is.logical(light_contents))
  stopifnot(is.logical(detail_NB_measures))
  stopifnot(is.logical(do_test))
  stopifnot(is.logical(drop_levels))
  stopifnot(is.logical(show_exact_p)) # dev 1.27
  stopifnot(is.logical(show_p_adj))
  
  
  if (!is.null(dico_labels)) {
    stopifnot(is.data.frame(dico_labels))
    if (verbose) {
      message(
        "[save_excel_paired_results] ", 
        "We only use the first two columns (re-)named 'Variable' and 'Label'."
      )
    }
    names(dico_labels)[1:2] <- c("Variable", "Label")
  }

  ## detect crossed varstrat :
  varstrat_splited <- strsplit(x = varstrat, split = "*", fixed = TRUE)
  if (length(unlist(varstrat_splited)) == 2) {
    varstrat <- unlist(varstrat_splited)
    crossed_varstrat <- TRUE
    if (drop_levels) dataframe[[varstrat[2]]] <- droplevels(dataframe[[varstrat[2]]]) # v0.1.22
    # droplevels if any visite no present at all --here to test
    stopifnot(nlevels(dataframe[[varstrat[2]]]) > 1)
    if (verbose) message("[save_excel_paired_results] With crossedvarstrat : No paired test possible (precaution...)")
    do_test <- FALSE
  } else {
    crossed_varstrat <- FALSE
    # because of the dropted levels check varstrat :
    if (drop_levels) dataframe[[varstrat]] <- droplevels(dataframe[[varstrat]]) # v0.1.22
    # droplevels : if any visite no present at all 
    stopifnot(nlevels(dataframe[[varstrat]]) > 1)
  }

  if (drop_levels) { # v0.1.22
    if (verbose) message("[save_excel_paired_results] droplevels(dataframe)")
    dataframe <- droplevels(dataframe)
  }

  if((force_parametric_test | force_non_parametric_test) & !do_test) {
    if (verbose) message("[save_excel_paired_results] Warning : you force test so do_test is turned TRUE")
    do_test <- TRUE
  }
  if(show_p_adj & !do_test) {
    if (verbose) message("[save_excel_paired_results] Warning : you want p_adj so do_test is turned TRUE")
    do_test <- TRUE
  }
  if(show_exact_p & !do_test) {
    if (verbose) message("[save_excel_paired_results] Warning : you want exact_p so do_test is turned TRUE")
    do_test <- TRUE
  } 
  if (force_parametric_test) {
    if (verbose) message("[save_excel_paired_results] Warning : you want force_parametric_test so show_metric is turned mean")
    show_metric <- "mean"
  }
  if (force_non_parametric_test) {
    if (verbose) message("[save_excel_paired_results] Warning : you want force_non_parametric_test so show_metric is turned median")
    show_metric <- "median"
  }
  
  vars <- setdiff(vars, varstrat)

  ## bug : data.table::setDT(dataframe) # v0.1.23
  dt <- data.table::setDT(data.table::copy(dataframe))[
    , .SD, .SDcols = c(patient_id, vars, varstrat)]
  ## remove vars (columns) with all NA # v0.1.22
  dt <- dt[, .SD, .SDcols = colSums(is.na(dt)) < nrow(dt)]
  newcols <- names(dt)
  var_setdiff <- setdiff(vars, newcols)
  if (length(var_setdiff) > 0) {
    if (verbose) {
      message(
        "[save_excel_paired_results] Warning : Column(s) ", 
        paste0(var_setdiff, collapse = ", "), 
        " removed from vars list, because all values are NA. => cf. sheet 'Variables_all_na'."
      )
    }
    vars <- setdiff(vars, var_setdiff)
    tab_na_sheet_list <- list(data.frame("Variables_all_na" = var_setdiff))
    names(tab_na_sheet_list) <- "Variables_all_na"
  } else {
    tab_na_sheet_list <- NULL
  }

  ## detect vars class
  vars_quanti <- get_numerics(dt, vars = vars)
  if (verbose && any(c("Q1", "Q3", "mean", "sd", "median", "min", "max") %in% vars_quanti)) {
    ## trouble... # reported by Klervi in v0.2.0
    message(
      "[save_excel_paired_results] Warning : ", 
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
      "[save_excel_paired_results] Warning : Following variables ", 
      paste0(vars[(!vars %in% c(vars_quanti, vars_quali, vars_dates))], collapse = ", "),
      " (from your dataset) are ignored because not factor, numeric or date type...", 
      " Remember to format variables before calling R2Excel functions."
    )
  }

  #### Quanti vars ####

  if (length(vars_quanti) == 0) {
    
    if (verbose) message("[save_excel_paired_results] There is no quantitative variables")
    tab_quanti_sheet_list <- NULL
    
  } else {
    
    if (crossed_varstrat) {
      
      sheet_leveli <- lapply(X = levels(dt[[varstrat[[1]]]]), FUN = function(leveli){
        dti <- data.table::copy(dt)
        dti <- dti[dti[[varstrat[[1]]]] %in% leveli, ]

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
          show_OR = FALSE, # not applicable,
          show_SMD = FALSE, # not applicable
          detail_NB_measures = detail_NB_measures,
          light_contents = light_contents,
          do_test = FALSE, # hard, for sure
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
      
      tab_quanti_sheet_list <- quanti_sheet_paired(
        dataframe = dt,
        vars_quanti = vars_quanti,
        varstrat = varstrat,
        patient_id = patient_id,
        precision = precision,
        signif_digits = signif_digits,
        global_summary = global_summary,
        force_non_parametric_test = force_non_parametric_test,
        force_parametric_test = force_parametric_test,
        show_metric = show_metric,
        detail_NB_measures = detail_NB_measures,
        do_test = do_test, 
        verbose = verbose
      )
    }
    
  } 

  #### Quali vars ####
  
  if (length(vars_quali) == 0) {
    
    if (verbose) message("[save_excel_paired_results] There is no qualitative variables")
    tab_quali_sheet_list <- NULL
    
  } else {
    
    if (crossed_varstrat) {
      
      qualsheet_leveli <- lapply(X = levels(dt[[varstrat[[1]]]]), FUN = function(leveli){
        dti <- data.table::copy(dt)
        dti <- dti[dti[[varstrat[[1]]]] %in% leveli, ]

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
          show_OR = FALSE, # not applicable,
          show_SMD = FALSE, # not applicable
          detail_NB_measures = detail_NB_measures,
          light_contents = light_contents,
          simplify = simplify,
          force_generate_1_when_0 = force_generate_1_when_0,
          prop_table_margin = 2,
          do_test = FALSE, # Hard, for sure 
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
      
      tab_quali_sheet_list <- quali_sheet_paired(
        dataframe = dt,
        vars_quali = vars_quali,
        varstrat = varstrat,
        precision = precision,
        signif_digits = signif_digits,
        patient_id = patient_id,
        simplify = simplify,
        global_summary = global_summary,
        force_non_parametric_test = force_non_parametric_test,
        force_parametric_test = force_parametric_test,
        force_generate_1_when_0 = force_generate_1_when_0, # for fact tab
        keep_missing_line = keep_missing_line, # for fact tab
        detail_NB_measures = detail_NB_measures,
        do_test = do_test, 
        verbose = verbose
      )
      
    }

  }


  #### show p adjusted ####
  if (show_p_adj && !crossed_varstrat) {
    # v0.1.27 plus de test en cas de cross varstrat
    if (verbose) message("[save_excel_paired_results] show_p_adj")

    ##### P adj classique #####
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
      # ][,
      #   P_valeur := as.numeric(gsub("<", "", as.character(P_valeur)))
    ][
      order(P_valeur, decreasing = FALSE), # classés du plus bas au plus haut.
    ]
    tab_pval$P_adj_holm <- signif(stats::p.adjust(
      p = tab_pval$P_valeur, method = "holm"
    ), digits = signif_digits)
    tab_pval$P_valeur <- NULL

    if (length(vars_quanti) > 0) {
      tab_quanti_sheet_tab <- merge(
        x = tab_quanti_sheet_tab, y = tab_pval, 
        by = "Variable", 
        all.x = TRUE, sort = FALSE
      )
      # order cols
      pcol <- grep("^P_valeur$", names(tab_quanti_sheet_tab))
      data.table::setcolorder(x = tab_quanti_sheet_tab, neworder = c(
        unique(c(
          names(tab_quanti_sheet_tab)[1:pcol], "P_adj_holm",
          names(tab_quanti_sheet_tab)[(pcol + 1):ncol(tab_quanti_sheet_tab)]
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
        x = tab_quali_sheet_tab, y = tab_pval,
        by = "Variable", all.x = TRUE, sort = FALSE
      )
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

  if (crossed_varstrat) {
    ## finish to formated crossed vars tabs

    ## --done v0.1.18
    name_quanti <- names(tab_quanti_sheet_list)
    name_quali <- names(tab_quali_sheet_list)
    tab_quanti_sheet_tab <- tab_quanti_sheet_list[[1]]
    tab_quali_sheet_tab <- tab_quali_sheet_list[[1]]

    # finish to format sheet list

    if (!is.null(tab_quanti_sheet_tab)) {
      
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
    
    if (!is.null(tab_quali_sheet_tab)) { 
      
      if (!is.null(dico_labels)) {
        # add label #v0.1.22
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
      }
      
      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- paste0("quali-", varstrat[1], "-", varstrat[2])
    } else {
      tab_quali_sheet_list <- NULL ## --done v0.1.18
    }
    
  } else {
    
    # just format p values

    name_quanti <- names(tab_quanti_sheet_list)
    name_quali <- names(tab_quali_sheet_list)
    tab_quanti_sheet_tab <- tab_quanti_sheet_list[[1]]
    tab_quali_sheet_tab <- tab_quali_sheet_list[[1]]

    #### format p val in tab_quanti_sheet_tab ####
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

    #### format p val in tab_quali_sheet_tab ####
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
      
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- name_quanti
    }
    if (is.null(name_quali)) {
      tab_quali_sheet_list <- NULL
    } else {
      
      if (!is.null(dico_labels)) {
        # add label #v0.1.22
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
      }
      
      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- name_quali
    }
  }

  
  #### Variables Dates ####  v0.1.24
  if (length(vars_dates) == 0) {
    if (verbose) message("[save_excel_paired_results] There is no date variables")
    tab_date_sheet_list <- NULL
  } else {
    if (crossed_varstrat) {
      levels_to_sep <- levels(dt[[varstrat[1]]])
      if (verbose) {
        message(paste0(
          "[save_excel_paired_results] date (with varstat : ", varstrat[1], ")"
        ))
      }
      tab_date_sheet_list <- list(
        data.table::rbindlist(
          lapply(X = levels_to_sep, FUN = function(level1i) {
            tab1i <- compute_date_table(
              dataframe = dt[dt[[varstrat[1]]] %in% level1i,],
              vars = vars_dates,
              varstrat = varstrat[2],
              verbose = verbose
            )
            tab1i$varstrat1 <- varstrat[1]
            tab1i$levels1 <- level1i
            data.table::setcolorder(x = tab1i, neworder = unique(c(
              "varstrat1", "levels1", names(tab1i)
            )))
            return(tab1i)
          }),
          fill = TRUE, use.names = TRUE
        )
      )
      names(tab_date_sheet_list) <- paste0("date - ", varstrat[1], "__", varstrat[2])
    } else {
      tab_date_sheet_list <- list(compute_date_table(
        dataframe = dt,
        vars = vars_dates,
        varstrat = varstrat,
        verbose = verbose
      ))
      names(tab_date_sheet_list) <- paste0("date - ", varstrat)
    }
    
  }
  
  #### Write Excel ####
  if (verbose) message("[save_excel_paired_results] Ends : write_xlsx")
  writexl::write_xlsx(
    x = c(
      tab_quanti_sheet_list,
      tab_quali_sheet_list,
      tab_date_sheet_list,
      tab_na_sheet_list
    ),
    path = file,
    col_names = TRUE
  )

  return(file)
}


#' save_excel_paired_results_filtertest
#' 
#' Tables for paired-tested-data, saved in Excel file
#'
#' Provide descriptive statistics table with a paired level, 
#' **after filtering missing data** for each variables, then re-save the output in Excel
#' (calling `save_excel_paired_results`)
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#'  Columns must be well formated with factor or numeric class (important).
#' @param file A character. Path and name of the excel results file.
#' @param vars A vector of characters. Name of columns to describe and test.
#'  Only consider numerics, factors and dates.
#' @param varstrat A characters. Not null. Names of the stratification variable, 
#'  making groups to compare (*time* or *visits* for instance).
#'  The repeated measures must be presented *in line* (for instance, "V1", "V2", "V3" 
#'  will be set on 3 lines for each individuals, in the column "visits").
#'  About 2 *crossed varstrats* : you can provide "var1*var2", and so
#'  get a description of the 2nd varstrat (var2) for each level of
#'  the 1st varstrat (var1).
#' @param patient_id A character. Default "patientid". 
#'  Name of patient identifier (id) column.
#'  The repeated measures must be presented in line 
#'  (for instance, "id1" will be set on 3 line if he has 3 visits).
#' @param precision mode: "auto" (adaptive) or numeric (fixed)
#'  Integer indicating the number of decimal places (round).
#'  Default = 2.
#' @param signif_digits A integer, Default 4. Integer indicating the number of 
#'  decimal places (signif) for p-values.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines
#'  should be displayed for binary variables.
#'  TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'  FALSE = both levels of the variables.
#' @param keep_missing_line A logical, Default TRUE. 
#'  Do you want to keep the missing data count (like a level)
#' @param global_summary A logical. Default FALSE Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... 
#'  but ok if you want it, you can.
#' @param force_non_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#' @param force_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  parametric test, whatever shapiro test said about normality. 
#'  (so will use and show means instead of medians)
#'  This may be useful when considering the central limit theorem or small deviations. 
#' @param force_generate_1_when_0 A logical, Default TRUE. 
#'  If TRUE, will test if the unique modality is 0 or "non" and
#'  add the level 1 or "oui" so it can be display in counts. 
#'  Can be combined with simplify to only show the modality (1).
#' @param show_metric A character, Default "auto". What is the metric to show in 
#'  cell content ?
#'  "auto" = mean or median, automatic choice according shapiro test,
#'  "mean" = mean +/- sd forced, whatever shapiro said,
#'  "median" = media [Q1;Q3] forced, whatever shapir said.
#'  Caution, if you force_non_parametric_test as TRUE, show_metric is forced 
#'  as 'median' and if you force_parametric_test as TRUE, show_metric is forced 
#'  as 'mean', to be consistent. 
#' @param light_contents A logical, Default TRUE. 
#'  If FALSE, the information like "Nb_mesure", "Valeurs manquantes" et "p" 
#'  will be repeated.
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail (N1 + N2 + ...) for each group.
#' @param do_test A logical, Default TRUE. 
#' To prevent the statistical test from returning a result, set the value to FALSE.
#' @param show_exact_p A logical, Default FALSE. 
#'  So if p<0.0001, the character "<0.0001" is returned.
#'  If turn TRUE, p-values will be returned will all numbers as numeric.
#' @param show_p_adj A logical, Default FALSE. 
#'  If turn TRUE, add P_adj_holm column based on p.adjust.
#' @param drop_levels A logical, Default FALSE. 
#'  If turn TRUE, apply droplevels(dataframe)
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#' @param dico_labels A data.frame, Default NULL. If data.frame provided,
#'  the first column must be the vars' name and 
#'  the 2nd column must be the labels. Other information will be ignored.
#'
#' @return A character. Path and name of the excel results file.
#'
#' @export
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom readxl excel_sheets
#' @importFrom writexl write_xlsx
#' @examples
#' \dontrun{
#' save_excel_paired_results_filtertest(
#'   dataframe = modified_sleep,
#'   vars = c("extra", "extra_with_missings", "group"),
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   file = file.path("tmp", "desc_paired_data_tested.xlsx")
#' )
#' }
save_excel_paired_results_filtertest <- function(
    dataframe,
    file = "paired_desc_table.xlsx",
    vars,
    varstrat,
    patient_id = "patientid",
    precision = 2,
    signif_digits = 4,
    simplify = FALSE,
    keep_missing_line = TRUE, # for fact tab
    global_summary = FALSE,
    force_non_parametric_test = FALSE,
    force_parametric_test = FALSE,
    force_generate_1_when_0 = TRUE, # for fact tab
    show_metric = "auto",
    light_contents = TRUE,
    detail_NB_measures = FALSE, # v0.1.27
    do_test = TRUE,  # v0.1.27
    show_exact_p = FALSE,
    show_p_adj = FALSE,
    drop_levels = FALSE,
    verbose = TRUE, 
    dico_labels = NULL
) {
  
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(patient_id %in% names(dataframe))
  stopifnot(!is.null(varstrat))
  stopifnot(!varstrat %in% "")
  stopifnot(length(varstrat) == 1)
  # stopifnot(digits >= 0)
  stopifnot(precision == "auto" || is.numeric(precision))
  stopifnot(signif_digits >= 0)
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(force_non_parametric_test))
  stopifnot(is.logical(force_parametric_test))
  stopifnot(sum(c(force_parametric_test, force_non_parametric_test)) <= 1) # v0.1.27
  # obviously, you can't force both 
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(is.logical(keep_missing_line))
  stopifnot(show_metric %in% c("mean", "median", "auto"))
  stopifnot(is.logical(light_contents))
  stopifnot(is.logical(do_test))
  stopifnot(is.logical(drop_levels))
  stopifnot(is.logical(show_exact_p))
  stopifnot(is.logical(show_p_adj))
  
  if (verbose) message("[save_excel_paired_results_filtertest] Run save_excel_paired_results for each vars (filter NA).")
  
  desc_data_tested <- lapply(
    X = vars,
    FUN = function(var_i) {
      tmp_i <- dataframe
      # get if with NA :
      detect_missing_id <- tmp_i[[patient_id]][which(is.na(tmp_i[[var_i]]))]
      # remove them
      tmp_i <- tmp_i[!(tmp_i[[patient_id]] %in% detect_missing_id), ]
      # or get id with 1 line ... (other kind of missing values, meaning individu only have 1 visit)
      detect_missing_id <- local({
        tt <- as.data.frame(table(tmp_i[[patient_id]]))
        tt$Var1[tt$Freq == 1]
      })
      # remove them
      tmp_i <- tmp_i[!(tmp_i[[patient_id]] %in% detect_missing_id), ]
      
      if (nrow(tmp_i) > 0) {
        save_excel_paired_results(
          dataframe = tmp_i,
          file = gsub(".xlsx", paste0("_", var_i, ".xlsx"), file),
          vars = var_i,
          varstrat = varstrat,
          precision = precision,
          signif_digits = signif_digits,
          patient_id = patient_id,
          simplify = simplify,
          global_summary = global_summary,
          force_non_parametric_test = force_non_parametric_test,
          force_generate_1_when_0 = force_generate_1_when_0, # for fact tab
          keep_missing_line = keep_missing_line, # for fact tab
          show_metric = show_metric,
          light_contents = light_contents,
          detail_NB_measures = detail_NB_measures,
          do_test = do_test,
          show_exact_p = show_exact_p,
          show_p_adj = show_p_adj,
          drop_levels = drop_levels,
          dico_labels = dico_labels
        )
      } else {
        if (verbose) {
          message(
            "[save_excel_paired_results_filtertest] ", var_i,
            " can not be analysed (filter NA remove all lines),",
            " variable just ignored."
          )
        }
        return(NULL)
      }
    }
  )

  if (verbose) message("[save_excel_paired_results_filtertest] Gather quanti lines")
  # quanti
  desc_data_tested_sheetquanti <- data.table::rbindlist(l = lapply(
    X = vars,
    FUN = function(var_i) {
      file_i <- gsub(".xlsx", paste0("_", var_i, ".xlsx"), file)
      if (file.exists(file_i)) {
        # read the excel table, quanti sheet
        if (grepl("^quanti", readxl::excel_sheets(file_i))) {
          sheet_name <- grep(
            "^quanti",
            readxl::excel_sheets(file_i),
            value = TRUE
          )
          tab_quanti <- readxl::read_excel(
            path = file_i,
            sheet = sheet_name
          )
          return(tab_quanti)
        } else { 
          # no quanti sheet
          return(NULL)
        }
      } else {
        # no file, var ignored
        return(NULL)
      }
    }
  ))
  if (verbose) message("[save_excel_paired_results_filtertest] Gather quali lines")
  # quali
  desc_data_tested_sheetquali <- data.table::rbindlist(l = lapply(
    X = vars,
    FUN = function(var_i) {
      file_i <- gsub(".xlsx", paste0("_", var_i, ".xlsx"), file)
      if (file.exists(file_i)) {
        # read the excel table, quanti sheet
        if (
          grepl("^quali", readxl::excel_sheets(file_i))
        ) {
          sheet_name <- grep(
            "^quali",
            readxl::excel_sheets(file_i),
            value = TRUE
          )
          tab_quali <- readxl::read_excel(
            path = file_i, 
            sheet = sheet_name
          )
        } else {
          # no quali sheet
          return(NULL)
        }
      } else {
        # no file, var ignored
        return(NULL)
      }
    }
  ))
  if (verbose) message("[save_excel_paired_results_filtertest] Gather date lines")
  # dates
  desc_data_tested_sheetdate <- data.table::rbindlist(l = lapply(
    X = vars,
    FUN = function(var_i) {
      file_i <- gsub(".xlsx", paste0("_", var_i, ".xlsx"), file)
      if (file.exists(file_i)) {
        # read the excel table, quanti sheet
        if (
          grepl("^date", readxl::excel_sheets(file_i))
        ) {
          sheet_name <- grep(
            "^date",
            readxl::excel_sheets(file_i),
            value = TRUE
          )
          tab_quali <- readxl::read_excel(
            path = file_i, 
            sheet = sheet_name
          )
        } else {
          # no date sheet
          return(NULL)
        }
      } else {
        # no file, var ignored
        return(NULL)
      }
    }
  ))
  
  
  if (verbose) message("[save_excel_paired_results_filtertest] Clear")
  clean_temp_files <- lapply(
    X = vars,
    FUN = function(var_i) {
      file_i <- gsub(".xlsx", paste0("_", var_i, ".xlsx"), file)
      if (file.exists(file_i)) {
        unlink( # if want to clean temporary file
          file_i
        )
      }
    }
  )
  
  if (verbose) message("[save_excel_paired_results_filtertest] Make list")
  # remake the lists
  if (nrow(desc_data_tested_sheetquanti) > 0) {
    tab_quanti_sheet_list <- list(desc_data_tested_sheetquanti)
    names(tab_quanti_sheet_list) <- paste0("quantitative - ", varstrat)
  } else {
    tab_quanti_sheet_list <- NULL
  }

  if (nrow(desc_data_tested_sheetquali) > 0) {
    tab_quali_sheet_list <- list(desc_data_tested_sheetquali)
    names(tab_quali_sheet_list) <- paste0("qualitative - ", varstrat)
  } else {
    tab_quali_sheet_list <- NULL
  }
  
  if (nrow(desc_data_tested_sheetdate) > 0) {
    tab_date_sheet_list <- list(desc_data_tested_sheetdate)
    names(tab_date_sheet_list) <- paste0("date - ", varstrat)
  } else {
    tab_date_sheet_list <- NULL
  }
  
  if (verbose) message("[save_excel_paired_results_filtertest] Save ", file)
  writexl::write_xlsx(
    x = c(
      tab_quanti_sheet_list,
      tab_quali_sheet_list,
      tab_date_sheet_list
    ),
    path = file
  )
  return(file)
}



#### SHEETS ####
#### SHEETS ####
#### SHEETS ####


#' quanti_sheet_paired
#' 
#' Compute quantitative variables sheet for Excel output.
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#'  Columns must be well formated with factor or numeric class (important).
#' @param vars_quanti Vector of characters. Subset of vars : vector of quantitative variable names
#' @param varstrat A characters. Not null. Names of the stratification variable, 
#'  making groups to compare (*time* or *visits* for instance).
#'  The repeated measures must be presented *in line* (for instance, "V1", "V2", "V3" 
#'  will be set on 3 lines for each individuals, in the column "visits").
#'  About 2 *crossed varstrats* : you can provide "var1*var2", and so
#'  get a description of the 2nd varstrat (var2) for each level of
#'  the 1st varstrat (var1).
#' @param patient_id A character.
#'  Name of patient identifier (id) column.
#'  The repeated measures must be presented in line 
#'  (for instance, "id1" will be set on 3 line if he has 3 visits).
#' @param precision mode: "auto" (adaptive) or numeric (fixed)
#'  Integer indicating the number of decimal places (round).
#' @param signif_digits A integer. Integer indicating the number of 
#'  decimal places (signif) for p-values.
#' @param global_summary A logical. Default FALSE Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... 
#'  but ok if you want it, you can.
#' @param force_non_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#' @param force_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  parametric test, whatever shapiro test said about normality. 
#'  (so will use and show means instead of medians)
#'  This may be useful when considering the central limit theorem or small deviations. 
#' @param show_metric A character. What is the metric to show in 
#'  cell content ?
#'  "auto" = mean or median, automatic choice according shapiro test,
#'  "mean" = mean +/- sd forced, whatever shapiro said,
#'  "median" = media [Q1;Q3] forced, whatever shapir said.
#'  Caution, if you force_non_parametric_test as TRUE, show_metric is forced 
#'  as 'median' and if you force_parametric_test as TRUE, show_metric is forced 
#'  as 'mean', to be consistent.
#' @param detail_NB_measures A logical,
#'  If turn TRUE, N will be shown with detail (N1 + N2 + ...) for each group.
#' @param do_test A logical, 
#' To prevent the statistical test from returning a result, set the value to FALSE.
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#'  
#' @return list with formatted quantitative variables description
#'
#' @export
#' 
quanti_sheet_paired <- function(
    dataframe,
    vars_quanti,
    varstrat,
    patient_id,
    precision,
    signif_digits,
    global_summary,
    force_non_parametric_test,
    force_parametric_test,
    show_metric,
    detail_NB_measures,
    do_test, 
    verbose
) {

  # stopifnot ? --here
  
  if (verbose) message("[quati_sheet_paired] ", varstrat)
  Variable <- Modalites <- Q1 <- Q3 <- Mean_sd <- Med_q1_q3 <- strat <- p <- NULL
  
  ##### Classique : for each variable of group varstrat #####
  
  tab_quanti_sheet_tab <- data.table::rbindlist(
      l = lapply(
        X = vars_quanti,
        FUN = function(variable_interest) {
          res <- compute_paired_continuous_table_and_test(
            dataframe = dataframe,
            variable_interest = variable_interest,
            varstrat = varstrat,
            precision = precision,
            signif_digits = signif_digits,
            patient_id = patient_id,
            global_summary = global_summary,
            force_non_parametric_test = force_non_parametric_test,
            force_parametric_test = force_parametric_test,
            show_metric = show_metric,
            do_test = do_test,
            verbose = verbose
          )
          return(res$line_res)
        }
      ),
      fill = TRUE
    )
  tab_quanti_sheet_tab$is_Normal <- NULL 

  # order : message at the very end
  data.table::setcolorder(
    x = tab_quanti_sheet_tab, 
    neworder = c(setdiff(names(tab_quanti_sheet_tab), "message"), "message" )
  )
  
  tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
  names(tab_quanti_sheet_list) <- paste0("quantitative - ", varstrat)
  
  return(tab_quanti_sheet_list)
}


#' quali_sheet_paired
#' 
#' Compute quali variables sheet for Excel output
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#'  Columns must be well formated with factor or numeric class (important).
#' @param vars_quali Vector of characters. Subset of vars : vector of qualitative variable names
#' @param varstrat A characters. Not null. Names of the stratification variable, 
#'  making groups to compare (*time* or *visits* for instance).
#'  The repeated measures must be presented *in line* (for instance, "V1", "V2", "V3" 
#'  will be set on 3 lines for each individuals, in the column "visits").
#'  About 2 *crossed varstrats* : you can provide "var1*var2", and so
#'  get a description of the 2nd varstrat (var2) for each level of
#'  the 1st varstrat (var1).
#' @param patient_id A character.
#'  Name of patient identifier (id) column.
#'  The repeated measures must be presented in line 
#'  (for instance, "id1" will be set on 3 line if he has 3 visits).
#' @param precision mode: "auto" (adaptive) or numeric (fixed)
#'  Integer indicating the number of decimal places (round).
#' @param signif_digits A integer. Integer indicating the number of 
#'  decimal places (signif) for p-values.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines
#'  should be displayed for binary variables.
#'  TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'  FALSE = both levels of the variables.
#' @param global_summary A logical. Default FALSE Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... 
#'  but ok if you want it, you can.
#' @param force_non_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#' @param force_parametric_test A logical. Default FALSE. 
#'  You can turn it TRUE if you want to force the use of
#'  parametric test, whatever shapiro test said about normality. 
#'  (so will use and show means instead of medians)
#'  This may be useful when considering the central limit theorem or small deviations. 
#' @param force_generate_1_when_0 A logical, Default TRUE. 
#'  If TRUE, will test if the unique modality is 0 or "non" and
#'  add the level 1 or "oui" so it can be display in counts. 
#'  Can be combined with simplify to only show the modality (1).
#' @param keep_missing_line A logical, Default TRUE. 
#'  Do you want to keep the missing data count (like a level)
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail (N1 + N2 + ...) for each group.
#' @param do_test A logical, Default TRUE. 
#' To prevent the statistical test from returning a result, set the value to FALSE.
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#'  
#' @return list with formatted quantitative variables description
#'
#' @export
#' 
quali_sheet_paired <- function(
    dataframe,
    vars_quali,
    varstrat,
    patient_id,
    precision,
    signif_digits,
    simplify,
    global_summary,
    force_non_parametric_test,
    force_parametric_test,
    force_generate_1_when_0, # for fact tab
    keep_missing_line, # for fact tab
    detail_NB_measures,
    do_test, 
    verbose
) {
  
  # stopifnot ? --here
  
  if (verbose) message("[quali_sheet_paired]")
  Variable <- Modalites <- p <- NULL
    
  ##### Classique : for each variable of group varstrat #####
  
  tab_quanti_sheet_tab <- data.table::rbindlist(
    lapply(
      X = vars_quali,
      FUN = function(variable_interest) {
        # message(variable_interest)
        res <- compute_paired_factorial_table_and_test(
          dataframe = dataframe,
          variable_interest = variable_interest,
          varstrat = varstrat,
          precision = precision,
          signif_digits = signif_digits,
          simplify = simplify,
          patient_id = patient_id,
          force_generate_1_when_0 = force_generate_1_when_0,
          keep_missing_line = keep_missing_line,
          global_summary = global_summary,
          do_test = do_test, 
          verbose = verbose
        )
        return(res$line_res)
      }
    ),
    fill = TRUE
  )

  tab_quali_sheet_list <- list(tab_quanti_sheet_tab)
  names(tab_quali_sheet_list) <- paste0("qualitative - ", varstrat[1])
  
  return(tab_quali_sheet_list)
}

