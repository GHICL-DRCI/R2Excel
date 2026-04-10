

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
#'  making groups to compare (*time* or *visites* for instance).
#'  The repeated measures must be presented *in line* (for instance, "V1", "V2", "V3" 
#'  will be set on 3 lines for each individuals).
#'  About 2 *crossed varstrats* : you can provide "var1*var2", and so
#'  get a description of the 2nd varstrat (var2) for each level of the 1st varstrat (var1).
#' @param patient_id A character. Default "patientid". 
#'  Name of patient identifier (id) column.
#'  The repeated measures must be presented in line 
#'  (for instance, "id1" will be set on 3 line if he has 3 visits)
#' @param precision mode: "auto" (adaptive) or numeric (fixed)
#'   Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of 
#'  decimal places (signif) for pvalues.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines
#'  should be displayed for binary variables.
#'  TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'  FALSE = both levels of the variables.
#' @param keep_missing_line A logical, Default TRUE. Do you want to keep the missing
#'  data count (like a level)
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
#' @param force_generate_1_when_0 A logical, Default FALSE. 
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
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail (N1 + N2 + ...) for each group.
#' @param light_contents A logical, Default TRUE. If FALSE,
#'  the information like "Nb_mesure", "Valeurs manquantes" et "p" will be repeated.
#' @param do_test A logical, Default TRUE. Do not return stat test if FALSE.
#' @param show_exact_p A logical, Default FALSE. 
#'  So if p<0.0001, the character "<0.0001" is returned.
#'  If turn TRUE, p-values will be returned will all numbers as numeric.
#' @param show_p_adj A logical, Default FALSE. If turn TRUE, add P_adj_holm column
#'  based on p.adjust.
#' @param drop_levels A logical, Default FALSE. If turn TRUE, apply droplevels(dataframe)
#' @param dico_labels A data.frame, Default NULL. If data.frame provided,
#'  the first column must be the vars' name and 
#'  the 2nd column must be the labels. Other information will be ignored.
#'
#' @return A character. Path and name of the excel results file.
#'
#' @export
#' @import data.table
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
    detail_NB_measures = FALSE, # v0.1.27
    do_test = TRUE,  # v0.1.27
    show_exact_p = FALSE,
    show_p_adj = FALSE,
    drop_levels = FALSE,
    dico_labels = NULL
) {
  
  message("[save_excel_paired_results] Start")
  
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
  stopifnot(is.logical(detail_NB_measures))
  stopifnot(is.logical(do_test))
  stopifnot(is.logical(drop_levels))
  stopifnot(is.logical(show_exact_p)) # dev 1.27
  stopifnot(is.logical(show_p_adj))
  
  if((force_parametric_test | force_non_parametric_test) & !do_test) {
    message("Warning : you force test so do_test is turned TRUE ;-)")
    do_test <- TRUE
  }
  if(show_p_adj & !do_test) {
    message("Warning : you want p_adj so do_test is turned TRUE ;-)")
    do_test <- TRUE
  }
  if(show_exact_p & !do_test) {
    message("Warning : you want exact_p so do_test is turned TRUE ;-)")
    do_test <- TRUE
  } 
  
  if (!is.null(dico_labels)) {
    stopifnot(is.data.frame(dico_labels))
    message(
      "[save_excel_paired_results] ", 
      "We only use the first two columns (re-)named 'Variable' and 'Label'."
    )
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
  } else {
    crossed_varstrat <- FALSE
    # because of the dropted levels check varstrat :
    if (drop_levels) dataframe[[varstrat]] <- droplevels(dataframe[[varstrat]]) # v0.1.22
    # droplevels : if any visite no present at all 
    stopifnot(nlevels(dataframe[[varstrat]]) > 1)
  }

  if (drop_levels) { # v0.1.22
    message(
      "[save_excel_paired_results] droplevels(dataframe)"
    )
    dataframe <- droplevels(dataframe)
  }

  if (force_non_parametric_test) {
    message(
      "[save_excel_paired_results]",
      "Because force_non_parametric_test is TRUE, ",
      "show_metric is forced as 'median'."
    )
    show_metric <- "median"
  }
  if (force_parametric_test) {
    message(
      "[save_excel_paired_results]",
      "Because force_parametric_test is TRUE, ",
      "show_metric is forced as 'mean'."
    )
    show_metric <- "mean"
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
    message(
      "[save_excel_paired_results] Warning!!! Column(s) ", 
      paste0(var_setdiff, collapse = ", "), 
      " removed from vars list, because all NA.=> cf sheet 'Variables_all_na'."
    )
    vars <- setdiff(vars, var_setdiff)
    tab_na_sheet_list <- list(data.frame("Variables_all_na" = var_setdiff))
    names(tab_na_sheet_list) <- "Variables_all_na"
  } else {
    tab_na_sheet_list <- NULL
  }

  ## detect vars class
  vars_quanti <- get_numerics(dt, vars = vars)
  vars_quali <- get_factors(dt, vars = vars)
  vars_dates <- get_dates(dt, vars = vars)

  #### Quanti vars ####
  if (length(vars_quanti) > 0) {
    
    tab_quanti_sheet_list <- quanti_sheet_paired(
      dataframe = dt,
      vars_quanti = vars_quanti,
      varstrat = varstrat,
      patient_id = patient_id,
      crossed_varstrat = crossed_varstrat,
      precision = precision,
      signif_digits = signif_digits,
      global_summary = global_summary,
      force_non_parametric_test = force_non_parametric_test,
      force_parametric_test = force_parametric_test,
      keep_missing_line = keep_missing_line,
      show_metric = show_metric,
      detail_NB_measures = detail_NB_measures,
      do_test = do_test
    )
    
  } else {
    
    message("[save_excel_paired_results] There is no quantitative variables")
    tab_quanti_sheet_list <- NULL
  
  }

  #### Quali vars ####
  
  if (length(vars_quali) > 0) {
    
    tab_quali_sheet_list <- quali_sheet_paired(
      dataframe = dt,
      vars_quali = vars_quali,
      varstrat = varstrat,
      crossed_varstrat = crossed_varstrat,
      precision = precision,
      signif_digits = signif_digits,
      patient_id = patient_id,
      simplify = simplify,
      global_summary = global_summary,
      force_non_parametric_test = force_non_parametric_test,
      force_parametric_test = force_parametric_test,
      force_generate_1_when_0 = force_generate_1_when_0, # for fact tab
      keep_missing_line = keep_missing_line, # for fact tab
      show_metric = show_metric,
      detail_NB_measures = detail_NB_measures,
      do_test = do_test
    )
    
  } else {
    message("[save_excel_paired_results] There is no qualitative variables")
    tab_quali_sheet_list <- NULL
  }


  #### show p adjusted ####
  if (show_p_adj & !crossed_varstrat) {
    # v0.1.27 plus de test en cas de cross varstrat
    message("[save_excel_paired_results] show_p_adj")

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

    ## --here v0.1.18
    name_quanti <- names(tab_quanti_sheet_list)
    name_quali <- names(tab_quali_sheet_list)
    tab_quanti_sheet_tab <- tab_quanti_sheet_list[[1]]
    tab_quali_sheet_tab <- tab_quali_sheet_list[[1]]

    ## now light_contents
    if (light_contents) {
      if (!is.null(tab_quali_sheet_tab)) { ## --here v0.1.18
        tab_quali_sheet_tab <- data.table::rbindlist(
          l = lapply(X = unique(tab_quali_sheet_tab$Variable), FUN = function(vari) {
            tmp <- tab_quali_sheet_tab[tab_quali_sheet_tab$Variable %in% vari, ]

            # light content in excel cells
            tmp$Variable <- c(as.character(tmp$Variable[1]), rep(NA, nrow(tmp) - 1))
            idx_val <- which(!is.na(tmp[, 3]))[1]
            idx_na <- setdiff(seq_len(nrow(tmp)), idx_val)
            tmp[idx_na, grep("Nb_mesures$", names(tmp))] <- NA
            tmp[idx_na, grep("Valeurs_manquantes$", names(tmp))] <- NA

            return(tmp)
          })
        )
      } # else nothing to format as light
    }

    # finish to format sheet list

    ## --here v0.1.18
    if (!is.null(tab_quanti_sheet_tab)) { ## --here v0.1.18
      names(tab_quanti_sheet_tab) <- gsub(
        "(.*)__(.*)", "\\1", names(tab_quanti_sheet_tab)
      )
      
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
      tab_quanti_sheet_list <- NULL ## --here v0.1.18
    }
    if (!is.null(tab_quali_sheet_tab)) { ## --here v0.1.18
      names(tab_quali_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quali_sheet_tab))
      
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
      tab_quali_sheet_list <- NULL ## --here v0.1.18
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
    message("[save_excel_paired_results] ", "There is no date variables")
    tab_date_sheet_list <- NULL
  } else {
    if (crossed_varstrat) {
      levels_to_sep <- levels(dt[[varstrat[1]]])
      message(paste0("[save_excel_paired_results] ", 
                     "date (with varstat : ", varstrat[1], ")"))
      
      tab_date_sheet_list <- list(
        data.table::rbindlist(
          lapply(X = levels_to_sep, FUN = function(level1i) {
            tab1i <- compute_date_table(
              dataframe = dt[dt[[varstrat[1]]] %in% level1i,],
              vars = vars_dates,
              varstrat = varstrat[2]
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
        varstrat = varstrat
      ))
      names(tab_date_sheet_list) <- paste0("date - ", varstrat)
    }
    
  }
  
  #### Write Excel ####
  message("[save_excel_paired_results] ", "write_xlsx")
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
#'  making groups to compare (*time* or *visites* for instance).
#'  The repeated measures must be presented *in line* (for instance, "V1", "V2", "V3" 
#'  will be set on 3 lines for each individuals).
#'  About 2 *crossed varstrats* : you can provide "var1*var2", and so
#'  get a description of the 2nd varstrat (var2) for each level of the 1st varstrat (var1).
#' @param patient_id A character. Default "patientid". 
#'  Name of patient identifier (id) column.
#'  The repeated measures must be presented in line 
#'  (for instance, "id1" will be set on 3 line if he has 3 visits)
#' @param precision mode: "auto" (adaptive) or numeric (fixed)
#'   Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of 
#'  decimal places (signif) for pvalues.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines
#'  should be displayed for binary variables.
#'  TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'  FALSE = both levels of the variables.
#' @param keep_missing_line A logical, Default TRUE. Do you want to keep the missing
#'  data count (like a level)
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
#' @param force_generate_1_when_0 A logical, Default FALSE. 
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
#' @param detail_NB_measures A logical, Default FALSE.
#'  If turn TRUE, N will be shown with detail (N1 + N2 + ...) for each group.
#' @param light_contents A logical, Default TRUE. If FALSE,
#'  the information like "Nb_mesure", "Valeurs manquantes" et "p" will be repeated.
#' @param do_test A logical, Default TRUE. Do not return stat test if FALSE.
#' @param show_exact_p A logical, Default FALSE. 
#'  So if p<0.0001, the character "<0.0001" is returned.
#'  If turn TRUE, p-values will be returned will all numbers as numeric.
#' @param show_p_adj A logical, Default FALSE. If turn TRUE, add P_adj_holm column
#'  based on p.adjust.
#' @param drop_levels A logical, Default FALSE. If turn TRUE, apply droplevels(dataframe)
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
#' 
save_excel_paired_results_filtertest <- function(
    dataframe,
    file = "paired_desc_table.xlsx",
    vars,
    varstrat,
    precision = 2,
    signif_digits = 4,
    patient_id = "patientid",
    simplify = FALSE,
    global_summary = FALSE,
    force_non_parametric_test = FALSE,
    force_parametric_test = FALSE,
    force_generate_1_when_0 = TRUE, # for fact tab
    keep_missing_line = TRUE, # for fact tab
    show_metric = "auto",
    light_contents = TRUE,
    detail_NB_measures = FALSE,
    do_test = FALSE,
    show_exact_p = FALSE,
    show_p_adj = FALSE,
    drop_levels = FALSE,
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
  
  message("[save_excel_paired_results_filtertest] Run save_excel_paired_results for each vars (filter NA).")
  desc_data_tested <- lapply(
    X = vars,
    FUN = function(var_i) {
      tmp_i <- dataframe
      # get if with NA :
      detect_missing_id <- tmp_i[[patient_id]][which(is.na(tmp_i[[var_i]]))]
      # remove them
      tmp_i <- tmp_i[!(tmp_i[[patient_id]] %in% detect_missing_id), ]
      # or get id with 1 line ... (other kind of missing values)
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
        message(
          "[save_excel_paired_results_filtertest] ", var_i,
          " can not be analysed (filter NA remove all lines),",
          " variable just ignored."
        )
        return(NULL)
      }
    }
  )

  message("[save_excel_paired_results_filtertest] Gather quanti lines")
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
  message("[save_excel_paired_results_filtertest] Gather quali lines")
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
  message("[save_excel_paired_results_filtertest] Gather date lines")
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
  
  
  message("[save_excel_paired_results_filtertest] Clear")
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
  
  message("[save_excel_paired_results_filtertest] Make list")
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
  
  message("[save_excel_paired_results_filtertest] save ", file)
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
#' Compute quantitative variables sheet for Excel output
#'
#' @param dataframe Data.frame containing the data
#' @param vars_quanti Character vector of quantitative variable names
#' @param varstrat Stratification variable name (can be "", NULL, or a variable name)
#' @param patient_id A character. Default "patientid". 
#'  Name of patient identifier (id) column.
#' @param crossed_varstrat Logical
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#' @param signif_digits Number of significant digits for p-values (default: 4)
#' @param force_non_parametric_test Logical, force non-parametric tests
#' @param force_parametric_test Logical, force parametric tests
#' @param show_metric Metric to display: "auto", "mean", or "median"
#' @param global_summary Logical, include global population summary
#' @param detail_NB_measures Logical, show detailed counts per level
#' @param do_test Logical, perform statistical tests (default: TRUE)
#'
#' @return list with formatted quantitative variables description
#'
#' @keywords internal
#' 
quanti_sheet_paired <- function(
    dataframe,
    vars_quanti,
    varstrat,
    patient_id,
    crossed_varstrat,
    precision,
    signif_digits,
    global_summary,
    force_non_parametric_test,
    force_parametric_test,
    keep_missing_line,
    show_metric,
    detail_NB_measures,
    do_test
) {
  
  # message("[quati_sheet_paired]")
  
  if (crossed_varstrat) {
    
    ##### crossed varstrat #####
    
    message("[save_excel_paired_results] crossed_varstrat")
    # for each level of group (varstrat[[1]]), produce a part of the table :
    levels_to_sep <- levels(dataframe[[varstrat[1]]])
    message(paste0("[save_excel_paired_results] ", "quanti (with Varstat ? ", varstrat[1], ")"))
    
    tab_all_varstrat_quanti <- data.table::rbindlist(lapply(X = levels_to_sep, function(level_i) {
      message(paste0(
        "[save_excel_paired_results] Go on varstrat ", varstrat[1], " crossed for level ", level_i, ")"
      ))
      
      dt <- dataframe[dataframe[[varstrat[1]]] %in% level_i, ]
      
      complete_continuous_tab_varstrati <- data.table::rbindlist(
        lapply(X = vars_quanti, function(variable_interest) {
          message("[save_excel_paired_results] ", variable_interest)
          res <- suppressWarnings(compute_continuous_table(
            # inside the level of one time, people are not paired : use compute_continuous_table
            dataframe = dt,
            vars = variable_interest,
            varstrat = varstrat[2], 
            precision = precision
          ))[[1]]
          
          res <- data.table::as.data.table(res, keep.rownames = "Modalites")
          res$Modalites[1] <- "Population_totale"
          res$Modalites <- gsub(varstrat[2], "", res$Modalites)
          res$Variable <- variable_interest
          res$varstrat <- varstrat[1]
          res$varstrat2 <- varstrat[2]
          res$strat <- level_i
          
          ## add NB total and Valeurs manquantes totale
          res$Nb_mesures <- as.character(res$Nb_mesures)
          res$Nb_mesures <- paste0(
            res$Nb_mesures[res$Modalites %in% "Population_totale"],
            ifelse(
              detail_NB_measures,
              paste0(" ", "(", paste0(
                res$Nb_mesures[!res$Modalites %in% "Population_totale"],
                collapse = "+"
              ), ")"),
              ""
            )
          )
          res[["Valeurs_manquantes"]] <- as.character(res[["Valeurs_manquantes"]])
          res$Valeurs_manquantes <- paste0(
            res[["Valeurs_manquantes"]][res$Modalites %in% "Population_totale"],
            ifelse(
              detail_NB_measures & res[["Valeurs_manquantes"]][res$Modalites %in% "Population_totale"] != "0",
              paste0(" ", "(", paste0(
                res$Valeurs_manquantes[!res$Modalites %in% "Population_totale"],
                collapse = "+"
              ), ")"),
              ""
            )
          )
          return(res)
        }),
        fill = TRUE
      )
      return(complete_continuous_tab_varstrati)
    }), fill = TRUE)
    
    if (!global_summary) {
      tab_all_varstrat_quanti <- tab_all_varstrat_quanti[Modalites != "Population_totale", ]
    }
    
    tab_all_varstrat_quanti$varstrat_strat <- paste0(
      tab_all_varstrat_quanti$varstrat, "==", tab_all_varstrat_quanti$strat
    )
    tab_all_varstrat_quanti <- tab_all_varstrat_quanti[, Mean_sd := paste0(
      mean, " +/- ", sd # , " (n=", Nb_mesures, ")"
    )]
    tab_all_varstrat_quanti <- tab_all_varstrat_quanti[, Med_q1_q3 := paste0(
      median, " [", Q1, ";", Q3, "]" # , " (n=", Nb_mesures, ")"
    )]
    
    ## adapt cell_content if nothing to show
    tab_all_varstrat_quanti$Mean_sd <- ifelse(
      tab_all_varstrat_quanti$Nb_mesures == "0",
      "/",
      tab_all_varstrat_quanti$Mean_sd
    )
    tab_all_varstrat_quanti$Med_q1_q3 <- ifelse(
      tab_all_varstrat_quanti$Nb_mesures == "0",
      "/",
      tab_all_varstrat_quanti$Med_q1_q3
    )
    
    if (show_metric %in% c("auto")) {
      ## go read is_Normal info
      tab_all_varstrat_quanti$cell_contents <- ifelse(
        tab_all_varstrat_quanti$is_Normal %in% 1,
        tab_all_varstrat_quanti$Mean_sd,
        tab_all_varstrat_quanti$Med_q1_q3
      )
    } else {
      if (show_metric %in% c("mean")) {
        tab_all_varstrat_quanti$cell_contents <- tab_all_varstrat_quanti$Mean_sd
      } else { # median
        tab_all_varstrat_quanti$cell_contents <- tab_all_varstrat_quanti$Med_q1_q3
      }
    }
    # format NA as "/"
    tab_all_varstrat_quanti$cell_contents <- ifelse(
      grepl("^NA", tab_all_varstrat_quanti$cell_contents),
      "/",
      tab_all_varstrat_quanti$cell_contents
    )
    
    tab_all_varstrat_quanti$varstrat2 <- paste0(
      tab_all_varstrat_quanti$varstrat2, "=", tab_all_varstrat_quanti$Modalites
    )
    
    # tab_all_varstrat_quanti
    
    tab_all_varstrat_quanti_wide <- lapply(X = levels_to_sep, function(level_i) {
      res <- data.table::dcast(
        data = tab_all_varstrat_quanti[strat %in% level_i, ],
        formula = stats::as.formula(
          "Variable + Nb_mesures + Valeurs_manquantes ~ varstrat2"
        ),
        value.var = "cell_contents"
      )
      
      names(res) <- paste0(varstrat[1], "==", level_i, "__", names(res))
      names(res)[1] <- "Variable"
      
      return(res)
    })
    
    tab_all_varstrat_quanti2 <- Reduce(
      f = function(x, y) merge(x = x, y = y, by = c("Variable"), all = TRUE, sort = FALSE),
      x = tab_all_varstrat_quanti_wide
    )
    
    first_line <- data.table::as.data.table(matrix(
      data = c(
        paste0("varstrat:", varstrat[1]),
        gsub("(.*)__(.*)", "\\2", names(tab_all_varstrat_quanti2))[-1]
      ),
      nrow = 1, byrow = TRUE,
      dimnames = list(NA, names(tab_all_varstrat_quanti2))
    ))
    
    tab_quanti_sheet_tab <- data.table::rbindlist(
      l = list(first_line, tab_all_varstrat_quanti2)
    )
    # finish to format
    # names(tab_quanti_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quanti_sheet_tab))
    tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
    names(tab_quanti_sheet_list) <- paste0("quanti-", varstrat[1], "-", varstrat[2])
    
  } else {
    
    ##### Classique : for each variable of group varstrat #####
    
    tab_quanti_sheet_tab <- data.table::rbindlist(lapply(X = varstrat, function(varstrat_i) {
      message("[quanti_sheet_paired] varstrat : ", varstrat_i)
      
      complete_continuous_tab_varstrati <- data.table::rbindlist(
        l = lapply(
          X = vars_quanti,
          FUN = function(variable_interest) {
            res <- compute_paired_continuous_table_and_test(
              dataframe = dataframe,
              variable_interest = variable_interest,
              varstrat = varstrat_i,
              precision = precision,
              signif_digits = signif_digits,
              patient_id = patient_id,
              global_summary = global_summary,
              force_non_parametric_test = force_non_parametric_test,
              force_parametric_test = force_parametric_test,
              show_metric = show_metric,
              do_test = do_test
            )
            return(res$line_res)
          }
        ),
        fill = TRUE
      )
      complete_continuous_tab_varstrati$is_Normal <- NULL 
      # no more wanted to be shown.
      return(complete_continuous_tab_varstrati)
    }), fill = TRUE)
    
    # order : message at the very end
    data.table::setcolorder(
      x = tab_quanti_sheet_tab, 
      neworder = c(
        setdiff(names(tab_quanti_sheet_tab), "message"), "message"
      )
    )
    
    tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
    names(tab_quanti_sheet_list) <- paste0("quantitative - ", varstrat[1])
  }
  
  return(tab_quanti_sheet_list)
}

#' quali_sheet_paired
#' 
#' Compute quali variables sheet for Excel output
#'
#' @param dataframe Data.frame containing the data
#' @param vars_quali Character vector of qualitative variable names
#' @param varstrat Stratification variable name (can be "", NULL, or a variable name)
#' @param patient_id A character. Default "patientid". 
#'  Name of patient identifier (id) column.
#' @param crossed_varstrat Logical
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#' @param signif_digits Number of significant digits for p-values (default: 4)
#' @param force_non_parametric_test Logical, force non-parametric tests
#' @param force_parametric_test Logical, force parametric tests
#' @param show_metric Metric to display: "auto", "mean", or "median"
#' @param global_summary Logical, include global population summary
#' @param detail_NB_measures Logical, show detailed counts per level
#' @param do_test Logical, perform statistical tests (default: TRUE)
#'
#' @return list with formatted quantitative variables description
#'
#' @keywords internal
#' 
quali_sheet_paired <- function(
    dataframe,
    vars_quali,
    varstrat,
    patient_id,
    crossed_varstrat,
    precision,
    signif_digits,
    simplify,
    global_summary,
    force_non_parametric_test,
    force_parametric_test,
    force_generate_1_when_0, # for fact tab
    keep_missing_line, # for fact tab
    show_metric,
    detail_NB_measures,
    do_test
) {
  # message("[quali_sheet_paired]")
  
  if (crossed_varstrat) {
    
    ##### crossed_varstrat #####
    message("[save_excel_paired_results] ", "crossed_varstrat")
    # for each level of group (varstrat[[1]]), produce a part of the table :
    
    levels_to_sep <- levels(dataframe[[varstrat[1]]])
    message(paste0(
      "[save_excel_paired_results] ",
      "quali (with Varstat ? ", varstrat[1], ")"
    ))
    
    tab_all_varstrat_sep <- lapply(X = levels_to_sep, function(level_i) {
      message(paste0(
        "[save_excel_paired_results] Go on varstrat ",
        varstrat[1], " crossed for level ", level_i, ")"
      ))
      
      dt <- droplevels(dataframe[dataframe[[varstrat[1]]] %in% level_i, ])
      
      analyse_desc_quali <- lapply(X = vars_quali, function(variable_interest) {
        res <- compute_factorial_table(
          dataframe = dt,
          vars = variable_interest,
          varstrat = varstrat[2],
          precision = precision,
          simplify = simplify,
          prop_table_margin = 2,
          force_generate_1_when_0 = force_generate_1_when_0
        )
        return(res)
      })
      
      tab_quali_sheet <- data.table::rbindlist(l = lapply(
        X = seq_len(length(analyse_desc_quali)),
        FUN = function(i) {
          var_i <- vars_quali[i]
          message("[save_excel_paired_results] ", var_i)
          tmp <- analyse_desc_quali[[i]]
          
          tmp <- data.table::as.data.table(tmp, keep.rownames = "Modalites")
          # v0.1.19 attention spaces in var's names generate dot instead
          names(tmp) <- gsub(
            paste0(gsub(" ", "\\.", var_i, fixed = TRUE), "\\."), "", names(tmp)
          )
          tmp$Variable <- var_i
          
          levels_i_n <- grep("^n(.*)", names(tmp), value = TRUE)[-1]
          # levels_i_p <- grep("^p(.*)", names(tmp), value = TRUE)[-1] # not used
          tmp$Nb_mesures <- paste0(
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
          tmp$Valeurs_manquantes <- ifelse(
            tmp$n[tmp$Modalites %in% "Valeurs_manquantes"] == 0,
            "0",
            paste0(
              tmp$n[tmp$Modalites %in% "Valeurs_manquantes"],
              ifelse(
                detail_NB_measures,
                paste0(" ", "(", paste0(
                  tmp[tmp$Modalites %in% "Valeurs_manquantes", .SD, .SDcols = levels_i_n],
                  collapse = "+"
                ), ")"),
                ""
              )
            )
          )
          # tmp
          levels_i_names <- gsub("^n(.*)", "\\1", levels_i_n)
          tmp2 <- tmp[, `:=`(
            Population_totale = paste0(n, " ", p)
          )][
            ,
            c(paste0(varstrat[2], "=", levels_i_names)) := lapply(levels_i_names, FUN = function(namei) {
              paste(tmp[[paste0("n", namei)]], tmp[[paste0("p", namei)]])
            })
          ]
          tmp3 <- tmp2[!Modalites %in% c("Nb_mesures", "Valeurs_manquantes"), .SD, .SDcols = c(
            "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes", "Population_totale",
            paste0(varstrat[2], "=", levels_i_names)
          )]
          
          if (nrow(tmp3) == 0) {
            # because of the droplevels, this variable is not present at all for this strat of varstrat[1]
            tmp3 <- tmp2[Modalites %in% c("Nb_mesures"), .SD, .SDcols = c(
              "Variable", "Modalites", "Nb_mesures",
              "Valeurs_manquantes", "Population_totale",
              paste0(varstrat[2], "=", levels_i_names)
            )]
            tmp3 <- tmp3[rep(1, length(levels(dataframe[[tmp3$Variable]]))), ]
            tmp3$Modalites <- levels(dataframe[[unique(tmp3$Variable)]])
          }
          
          if (!global_summary) {
            tmp3$Population_totale <- NULL
          }
          return(tmp3)
        }
      ))
      
      ## keep info of level i from varstrat[[1]]
      names(tab_quali_sheet) <- paste0(
        varstrat[1], "==", level_i, "__", names(tab_quali_sheet)
      )
      names(tab_quali_sheet)[1] <- "Variable"
      names(tab_quali_sheet)[2] <- "Modalites"
      return(tab_quali_sheet)
    })
    
    tab_all_varstrat_quali_gather <- Reduce(
      f = function(x, y) merge(x = x, y = y, by = c("Variable", "Modalites"), all = TRUE, sort = FALSE),
      x = tab_all_varstrat_sep
    )
    # order folling levels
    tab_all_varstrat_quali_gather$Variable <- factor(
      tab_all_varstrat_quali_gather$Variable, levels = vars_quali
    )
    tab_all_varstrat_quali_gather <- tab_all_varstrat_quali_gather[order(Variable), ]
    
    first_line <- data.table::as.data.table(matrix(
      data = c(
        paste0("varstrat:", varstrat[1]),
        gsub("(.*)__(.*)", "\\2", names(tab_all_varstrat_quali_gather))[-1]
      ),
      nrow = 1, byrow = TRUE,
      dimnames = list(NA, names(tab_all_varstrat_quali_gather))
    ))
    tab_quali_sheet_tab <- data.table::rbindlist(l = list(first_line, tab_all_varstrat_quali_gather))
    # finish to format
    # names(tab_quali_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quali_sheet_tab))
    tab_quali_sheet_list <- list(tab_quali_sheet_tab)
    names(tab_quali_sheet_list) <- paste0("quali-", varstrat[1], "-", varstrat[2])
  } else {
    ##### Classique : for each variable of group varstrat #####
    
    tab_quanti_sheet_tab <- data.table::rbindlist(lapply(X = varstrat, function(varstrat_i) {
      message("[quali_sheet_paired] varstrat : ", varstrat_i)
      
      complete_fact_tab_varstrati <- data.table::rbindlist(
        lapply(
          X = vars_quali,
          FUN = function(variable_interest) {
            # message(variable_interest)
            res <- compute_paired_factorial_table_and_test(
              dataframe = dataframe,
              variable_interest = variable_interest,
              varstrat = varstrat_i,
              precision = precision,
              signif_digits = signif_digits,
              simplify = simplify,
              patient_id = patient_id,
              force_generate_1_when_0 = force_generate_1_when_0,
              keep_missing_line = keep_missing_line,
              global_summary = global_summary,
              do_test = do_test
            )
            return(res$line_res)
          }
        ),
        fill = TRUE
      )
      return(complete_fact_tab_varstrati)
    }), fill = TRUE)
    
    tab_quali_sheet_list <- list(tab_quanti_sheet_tab)
    names(tab_quali_sheet_list) <- paste0("qualitative - ", varstrat[1])
  }
  
  return(tab_quali_sheet_list)
  
}

