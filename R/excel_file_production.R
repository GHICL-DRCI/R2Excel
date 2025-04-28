# Create Excel file with 2 sheets (description of qualitative and quantitative variables)

#' create Excel file of descriptive analysis
#' provide descriptive Excel file for factorial (qualitative) and continuous (quantitative) data
#' add statistical test according a variable of stratification
#'
#' @param dataframe A data.frame. Columns must be well formated with factor or numeric class (important).
#' @param file A character. Name of the Excel file or path + name of the Excel file (file name must end by ".xlsx").
#'   Default "Excel_report_description.xlsx".
#' @param vars A vector of characters. Names of dataframe's columns to describe.
#' @param varstrat A character. Default NULL. Name of the stratification variable, making groups to compare.
#'   Only one varstrat is expected in general.
#'   About 2 crossed varstrats : you can provide "var1*var2"
#'    to get a description of the 2nd varstrat for each level of the 1st varstrat.
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines should be displayed for binary variables.
#'   TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), FALSE = both levels of the variables.
#' @param prop_table_margin A vector giving the margins to split by. Default 2. 1 indicates rows, 2 indicates columns,
#'   c(1, 2) indicates rows and columns. When x has named dimnames, it can be a character vector selecting dimension names.
#' @param force_generate_1_when_0 A logical, Default TRUE. If TRUE, will test if the unique modality is 0 or "non" and
#'    add the level 1 or "oui" so it can be display in counts. Can be combined with simplify to only show the modality (1).
#' @param force_non_parametric_test A logical. Default FALSE. You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality. (so will use and show medians instead of means)
#' @param metric_show A character, Default "auto". What is the metric to show in cell content ?
#'   "auto" = mean or median, automatic choice according shapiro test,
#'   "mean" = mean +/- sd forced, whatever shapiro said,
#'   "median" = media [Q1;Q3] forced, whatever shapiro said.
#'   Caution, if you force_non_parametric_test as TRUE, metric_show is forced as 'median' to be consistent.
#' @param global_summary A logical. Default TRUE. Do you want to get global summary.
#' @param show_OR A logical. Default FALSE. Do you want to get OR, IC and p (when varstrat is a binary fact).
#' @param light_contents A logical, Default TRUE. If FALSE, the information like Nb_mesures,
#'  Valeurs manquantes et p will be repeated.
#' @param crossed_varstrat_test A logical, Default FALSE. If turn TRUE, 
#'  and detection of 2 varstrat "var1*var2" to cross, statistical test will be provided.
#' @param detail_NB_mesure_sum A logical, Default FALSE. If turn TRUE, N will be shown with detail (N1 + N2 + ...) 
#'  for each group.
#' @param show_p_adj A logical, Default FALSE. If trun TRUE, add P_adj_holm column based on p.adjust.
#' @param drop_levels A logical, Default FALSE. If trun TRUE, apply droplevels(dataframe)
#' @param dico_mapping A data.frame, Default NULL. If data.frame provided, the first column must be the vars' name and 
#'   the 2nd column must be the labels. Other information will be ignored.
#'
#' @return The name or path + name of the saved Excel file.
#' @export
#' @examples
#' \dontrun{
#' save_excel_results(
#'   dataframe = modified_state,
#'   file = "tmp/Descriptive_bivariate_analysis_1.xlsx",
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp", "Murder",
#'     "HS Grad", "Frost", "Area",
#'     "state.division", "state.region", "binary_test"
#'   ),
#'   varstrat = "election",
#'   digits = 2
#' )
#' save_excel_results(
#'   dataframe = modified_state,
#'   file = "tmp/Descriptive_bivariate_analysis_2.xlsx",
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp", "Murder",
#'     "HS Grad", "Frost", "Area",
#'     "state.division", "state.region",
#'     "special_condition", "special_measures" # trap
#'   ),
#'   varstrat = "election*binary_test",
#'   digits = 2, crossed_varstrat_test = TRUE
#' )
#' save_excel_results(
#'   dataframe = modified_state,
#'   file = "tmp/Descriptive_bivariate_analysis_3.xlsx",
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp",
#'     "state.division", "state.region", "election"
#'   ),
#'   varstrat = "Area",
#'   digits = 2
#' )
#' }
save_excel_results <- function(
    dataframe,
    file = "tmp/description.xlsx",
    vars,
    varstrat = NULL,
    digits = 2,
    signif_digits = 4,
    simplify = FALSE,
    prop_table_margin = 2,
    force_generate_1_when_0 = TRUE,
    force_non_parametric_test = FALSE,
    metric_show = "auto",
    global_summary = TRUE,
    show_OR = FALSE,
    light_contents = TRUE,
    crossed_varstrat_test = FALSE,
    detail_NB_mesure_sum = FALSE,
    show_p_adj = FALSE,
    drop_levels = FALSE,
    dico_mapping = NULL
) {
  message("[save_excel_results] ", "Starts descriptive analysesn")

  stopifnot(is.data.frame(dataframe))
  stopifnot(vars %in% names(dataframe))
  stopifnot(digits >= 0)
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(is.logical(force_non_parametric_test))
  stopifnot(metric_show %in% c("mean", "median", "auto"))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(show_OR))
  stopifnot(is.logical(light_contents))
  stopifnot(is.logical(crossed_varstrat_test))
  stopifnot(is.logical(detail_NB_mesure_sum))
  stopifnot(is.logical(show_p_adj))
  stopifnot(is.logical(drop_levels))
  if (!is.null(dico_mapping)) {
    stopifnot(is.data.frame(dico_mapping))
    message("[save_excel_results] Variable and Label must be the first 2 columns !")
    names(dico_mapping)[1:2] <- c("Variable", "Label")
    # only select the first 2 columns? should remouve others ? 
  }

  ## detect crossed varstrat :
  if (is.null(varstrat[1]) || varstrat[1] %in% "") {
    varstrat <- ""
    crossed_varstrat <- FALSE
  } else {
    varstrat_splited <- strsplit(x = varstrat, split = "*", fixed = TRUE)
    if (length(unlist(varstrat_splited)) == 2) {
      varstrat <- unlist(varstrat_splited)
      crossed_varstrat <- TRUE
      stopifnot(varstrat[2] %in% names(dataframe))

      ## --here to test
      if (drop_levels) dataframe[[varstrat[2]]] <- droplevels(dataframe[[varstrat[2]]]) # v0.1.22
      if (nlevels(dataframe[[varstrat[2]]]) == 1) {
        varstrat <- varstrat[1] # no more 2nd var strat
        crossed_varstrat <- FALSE
        message(
          "[save_excel_results] Warning!!! only 1 level in 2nd varstrat, so turn varstrat = varstrat[1]."
        )
      }
    } else {
      crossed_varstrat <- FALSE

      if (drop_levels) dataframe[[varstrat]] <- droplevels(dataframe[[varstrat]]) # v0.1.22
      # droplevels , stop to test
      if (nlevels(dataframe[[varstrat]]) == 1) {
        varstrat <- NULL # no more var strat
        message(
          "[save_excel_results] Warning!!! only 1 level in varstrat, so turn varstrat = NULL."
        )
      }
    }
    stopifnot(varstrat[1] %in% names(dataframe))
  }

  if (drop_levels) { # v0.1.22
    message(
      "[save_excel_results] droplevels(dataframe)"
    )
    dataframe <- droplevels(dataframe)
  }

  if (force_non_parametric_test) {
    message(
      "[save_excel_results] Because force_non_parametric_test is TRUE, metric_show is forced as 'median'."
    )
    metric_show <- "median"
  }

  vars <- setdiff(vars, varstrat)
  dataframe <- data.table::setDT(dataframe)
  if (is.null(varstrat[1]) || varstrat[1] %in% "") {
    dataframe <- dataframe[, .SD, .SDcols = c(vars)]
  } else {
    dataframe <- dataframe[, .SD, .SDcols = c(vars, varstrat)]
  }
  ## remove vars (columns) with all NA # v0.1.22
  dataframe <- dataframe[, .SD, .SDcols = colSums(is.na(dataframe)) < nrow(dataframe)]
  newcols <- names(dataframe)
  var_setdiff <- setdiff(vars, newcols)
  if (length(var_setdiff) > 0) {
    message("[save_excel_results] Warning!!! Column(s) ", paste0(var_setdiff, collapse = ", "), " removed from vars list, because all NA.")
    vars <- setdiff(vars, var_setdiff)
    tab_na_sheet_list <- list(data.frame("Variables_all_na" = var_setdiff))
    names(tab_na_sheet_list) <- "Variables_all_na"
  } else {
    tab_na_sheet_list <- NULL
  }

  # Separation des variables quali et quanti
  vars_quanti <- get_numerics(dataframe, vars = vars)
  vars_quali <- get_factors(dataframe, vars = vars)

  #### Variables quantitatives ####
  if (length(vars_quanti) == 0) {
    ##### skip variables quantitatives #####
    # print("Il n'y a pas de variable quantitative")
    message("[save_excel_results] ", "There is no quantitative variables")
    tab_quanti_sheet_list <- NULL
  } else {
    if (crossed_varstrat) {
      ##### crossed_varstrat #####
      message("[save_excel_results] ", "crossed_varstrat")
      # for each level of group (varstrat[[1]]), produce a part of the table :
      levels_to_sep <- levels(dataframe[[varstrat[1]]])
      message(paste0("[save_excel_results] ", "quanti (with Varstat ? ", varstrat[1], ")"))

      tab_quanti_sheet_sep <- lapply(levels_to_sep, function(level_i) {
        message(paste0(
          "[save_excel_results] Go on varstrat ", varstrat[1], " crossed for level ", level_i, ")"
        ))

        analyse_desc_quanti <- suppressWarnings(compute_continuous_table(
          dataframe = dataframe[dataframe[[varstrat[1]]] %in% level_i, ],
          vars = vars_quanti,
          varstrat = varstrat[2],
          stats_choice = c(
            "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal"
          ),
          digits = digits
        ))

        ## want stat test as option
        if (crossed_varstrat_test) {
          pvaleur_quanti <- test_means(
            dataframe = dataframe[dataframe[[varstrat[1]]] %in% level_i, ],
            vars = vars_quanti,
            varstrat = varstrat[2],
            force_non_parametric_test = force_non_parametric_test
          )
          tab_test <- data.table::as.data.table(pvaleur_quanti$result, keep.rownames = "Variable")
        }

        tab_quanti_sheet <- data.table::rbindlist(l = lapply(
          X = seq_len(length(analyse_desc_quanti)), FUN = function(i) {
            var_i <- names(analyse_desc_quanti)[i]
            message("[save_excel_results] ", var_i)
            tmp_raw <- analyse_desc_quanti[i]
            tmp <- data.table::as.data.table(tmp_raw, keep.rownames = "Modalites")
            # names(tmp) <- gsub(paste0(gsub(" ", "\\.", var_i), "\\."), "", names(tmp))
            # or instead directly rename as :
            names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
            tmp$Variable <- var_i
            tmp$Modalites[1] <- "Population_totale"

            tmp$Valeurs_manquantes <- as.character(tmp[["N_NA"]])

            tmp <- tmp[, `:=`(
              `Mean_sd` = paste0(mean, " +/- ", sd),
              `Med_q1_q3` = paste0(median, " [", Q1, ";", Q3, "]")
            )]

            tmp$cell_auto <- ifelse(
              test = tmp$is_Normal %in% c(TRUE, 1),
              yes = "Mean_sd",
              no = "Med_q1_q3"
            )
            cell_content <- ifelse( # in order of tmp$Modalites
              test = metric_show %in% "auto",
              yes = tmp$cell_auto, # selection auto according test
              no = ifelse(
                test = metric_show %in% "mean",
                yes = "Mean_sd",
                no = "Med_q1_q3"
              )
            )
            ## adapt cell_content if nothing to show
            tmp$Nb_mesures <- as.character(tmp$Nb_mesures)
            if (tmp$Nb_mesures[tmp$Modalites %in% "Population_totale"] == "0") {
              tmp$Mean_sd <- "/"
              tmp$Med_q1_q3 <- "/"
            }
            tmp$Nb_mesures <- ifelse(
              tmp$Nb_mesures[tmp$Modalites %in% "Population_totale"] == "0",
              "0",
              paste0(
                tmp$Nb_mesures[tmp$Modalites %in% "Population_totale"],
                ifelse(
                  detail_NB_mesure_sum,
                  paste0(" ", "(", paste0(
                    tmp$Nb_mesures[!tmp$Modalites %in% "Population_totale"],
                    collapse = "+"
                  ), ")"),
                  ""
                )
              )
            )
            tmp$Valeurs_manquantes <- ifelse(
              tmp$Valeurs_manquantes[tmp$Modalites %in% "Population_totale"] == "0",
              "0",
              paste0(
                tmp$Valeurs_manquantes[tmp$Modalites %in% "Population_totale"],
                ifelse(
                  detail_NB_mesure_sum,
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
              value.var = cell_content # object in order of tmp$Modalites
            )
            ## reorder cols pop total before group v0.1.19
            names(tmp2) <- gsub(varstrat[2], paste0(varstrat[2], "="), names(tmp2))
            reordercols <- unique(c(
              "Variable", "Nb_mesures", "Valeurs_manquantes", "Population_totale",
              names(tmp2)
            ))
            tmp2 <- tmp2[, .SD, .SDcols = c(reordercols)]
            return(tmp2)
          }
        ))

        ## stat test added in option
        if (crossed_varstrat_test) {
          tab_quanti_sheet <- merge(
            tab_quanti_sheet, tab_test,
            by = "Variable", all = TRUE, sort = FALSE
          )
        }

        ## keep info of level i from varstrat[[1]]
        names(tab_quanti_sheet) <- paste0(
          varstrat[1], "==", level_i, "__", names(tab_quanti_sheet)
        )
        names(tab_quanti_sheet)[1] <- "Variable"

        return(tab_quanti_sheet)
      })

      tab_quanti_sheet_tab <- Reduce(
        f = function(x, y) merge(x = x, y = y, by = "Variable", all = TRUE, sort = FALSE),
        x = tab_quanti_sheet_sep
      )

      if (!global_summary) {
        col_select <- setdiff(
          names(tab_quanti_sheet_tab),
          grep("Population_totale", names(tab_quanti_sheet_tab), value = TRUE)
        )
        tab_quanti_sheet_tab <- tab_quanti_sheet_tab[, .SD, .SDcols = col_select]
      }

      first_line <- data.table::as.data.table(matrix(
        data = c(
          paste0("varstrat:", varstrat[1]),
          gsub("(.*)__(.*)", "\\2", names(tab_quanti_sheet_tab))[-1]
        ),
        nrow = 1, byrow = TRUE,
        dimnames = list(NA, names(tab_quanti_sheet_tab))
      ))
      tab_quanti_sheet_tab <- data.table::rbindlist(l = list(first_line, tab_quanti_sheet_tab))

      # finish to format
      # names(tab_quanti_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quanti_sheet_tab))
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- paste0("quanti-", varstrat[1], "-", varstrat[2])
    } else {
      ##### for each variable of group varstrat #####

      if (is.null(varstrat)) varstrat <- ""
      tab_quanti_sheet_list <- lapply(varstrat, function(varstrat_i) {
        mm <- ifelse(
          is.null(varstrat_i) || varstrat_i %in% "",
          paste0("[save_excel_results] ", "quanti (without Varstat)"),
          paste0("[save_excel_results] ", "quanti (with Varstat ? ", varstrat_i, ")")
        )
        message(mm)

        if (!is.null(varstrat_i) && !varstrat_i %in% "" & is.numeric(dataframe[[varstrat_i]])) {
          ##### varstrat continuous #####
          analyse_desc_quanti <- compute_correlation_table(
            dataframe = dataframe,
            vars = vars_quanti,
            varstrat = varstrat_i,
            method = "detect_auto",
            digits = digits,
            signif_digits = signif_digits
          )
        } else {
          ##### varstrat factorial or no varstrat #####
          analyse_desc_quanti <- compute_continuous_table(
            dataframe = dataframe,
            vars = vars_quanti,
            varstrat = varstrat_i,
            stats_choice = c(
              "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal"
            ),
            # all stats are excepted in excel workbook function
            digits = digits
          )
        }

        ##### stats tests #####
        if (is.null(varstrat_i) || varstrat_i %in% "" || is.numeric(dataframe[[varstrat_i]])) {
          pvaleur_quanti <- NULL
          tab_quanti_sheet <- data.table::as.data.table(analyse_desc_quanti, keep.rownames = "Variable")
          if ("N_NA" %in% names(tab_quanti_sheet)) {
            tab_quanti_sheet$Valeurs_manquantes <- tab_quanti_sheet[["N_NA"]]
          }

          tab_quanti_sheet <- tab_quanti_sheet[, `:=`(
            `Moy +/- Sd` = paste0(as.character(mean), " +/- ", as.character(sd)),
            `Med [Q1;Q3]` = paste0(median, " [", Q1, ";", Q3, "]"),
            `Min - Max` = paste0(min, " - ", max)
          )]

          if (is.null(varstrat_i) || varstrat_i %in% "") {
            tab_quanti_sheet <- tab_quanti_sheet[, .SD, .SDcols = c(
              "Variable", "Nb_mesures", "Valeurs_manquantes",
              "Moy +/- Sd", "Med [Q1;Q3]", "Min - Max", "is_Normal"
            )]
          } else { # is.numeric(dataframe[[varstrat_i]])
            tab_quanti_sheet <- tab_quanti_sheet[, .SD, .SDcols = c(
              # "varstrat",  # do not show
              "Variable", "Valeurs_manquantes", "Nb_mesures",
              # "Moy +/- Sd", "Med [Q1;Q3]", "Min - Max",  # do not show
              "is_Normal",
              "correlation", "IC95", "P_valeur", "correlation_method", "message"
            )]
            # add varstrat name before correlation
            names(tab_quanti_sheet)[grep("^correlation$", names(tab_quanti_sheet))] <- paste0(
              varstrat_i, " correlation"
            )
          }
        } else {
          pvaleur_quanti <- test_means(
            dataframe = dataframe,
            vars = vars_quanti,
            varstrat = varstrat_i,
            force_non_parametric_test = force_non_parametric_test,
            digits = digits,
            signif_digits = signif_digits
          )
          tab_test <- data.table::as.data.table(pvaleur_quanti$result, keep.rownames = "Variable")

          tab_quanti_sheet <- data.table::rbindlist(l = lapply(
            X = seq_len(length(analyse_desc_quanti)), FUN = function(i) {
              var_i <- names(analyse_desc_quanti)[i]
              message("[save_excel_results] ", var_i)
              tmp_raw <- analyse_desc_quanti[i]
              tmp <- data.table::as.data.table(tmp_raw, keep.rownames = "Modalites")
              # names(tmp) <- gsub(paste0(gsub(" ", "\\.", var_i), "\\."), "", names(tmp))
              names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
              tmp$Variable <- var_i
              tmp$Modalites[1] <- "Population_totale"

              tmp$Valeurs_manquantes <- as.character(tmp[["N_NA"]])

              tmp <- tmp[, `:=`(
                `Mean_sd` = paste0(mean, " +/- ", sd),
                `Med_q1_q3` = paste0(median, " [", Q1, ";", Q3, "]")
              )]

              cell_auto <- ifelse(
                test = tab_test[Variable %in% var_i, "Test"] %in%
                  c("Wilcoxon rank sum exact test (Mann-Whitney)", "Kruskal-Wallis rank sum test"),
                yes = "Med_q1_q3",
                no = "Mean_sd" # default show means if no test
              )
              cell_content <- ifelse(
                test = metric_show %in% "auto",
                yes = cell_auto, # selection auto according test
                no = ifelse(
                  test = metric_show %in% "mean",
                  yes = "Mean_sd",
                  no = "Med_q1_q3"
                )
              )
              tmp$Nb_mesures <- as.character(tmp$Nb_mesures)
              tmp$Nb_mesures <- ifelse(
                tmp$Nb_mesures[tmp$Modalites %in% "Population_totale"] == "0",
                "0",
                paste0(
                  tmp$Nb_mesures[tmp$Modalites %in% "Population_totale"],
                  ifelse(
                    detail_NB_mesure_sum,
                    paste0(" ", "(", paste0(
                      tmp$Nb_mesures[!tmp$Modalites %in% "Population_totale"],
                      collapse = "+"
                    ), ")"),
                    ""
                  )
                )
              )
              tmp$Valeurs_manquantes <- ifelse(
                tmp$Valeurs_manquantes[tmp$Modalites %in% "Population_totale"] == "0",
                "0",
                paste0(
                  tmp$Valeurs_manquantes[tmp$Modalites %in% "Population_totale"],
                  ifelse(
                    detail_NB_mesure_sum,
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
              names(tmp2) <- gsub(varstrat_i, paste0(varstrat_i, "="), names(tmp2))
              reordercols <- unique(c(
                "Variable", "Nb_mesures", "Valeurs_manquantes", "Population_totale",
                names(tmp2)
              ))
              tmp2 <- tmp2[, .SD, .SDcols = c(reordercols)]
              return(tmp2)
            }
          ))
          tab_quanti_sheet <- merge(tab_quanti_sheet, tab_test, by = "Variable", sort = FALSE)

          if (show_OR && is.factor(dataframe[[varstrat_i]]) && length(levels(dataframe[[varstrat_i]])) == 2) {
            message("[save_excel_results] ", "show_OR")
            OR_tab <- get_OR_univar(
              dataframe = dataframe,
              dependent_var = varstrat_i,
              explanatory_vars = vars_quanti,
              check_n_levels = FALSE, # not applicable in continuous vars
              signif_digits = signif_digits
            )
            if (is.null(OR_tab)) {
              # tab_quanti_sheet <- tab_quanti_sheet # stay the same...
            } else {
              OR_tab$Modalites <- NULL
              tab_quanti_sheet <- merge(
                x = tab_quanti_sheet,
                y = OR_tab,
                by = c("Variable"),
                all = TRUE,
                sort = FALSE
              )
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
          col_select <- c(
            setdiff(names(tab_quanti_sheet), c("message")),
            "message"
          )
          tab_quanti_sheet <- tab_quanti_sheet[, .SD, .SDcols = col_select]
        }

        return(tab_quanti_sheet)
      })
      names(tab_quanti_sheet_list) <- paste0("quantitative - ", varstrat)
    }
  }

  #### Variables qualitatives ####

  if (length(vars_quali) == 0) {
    ##### skip variables qualitative #####
    # print("Il n'y a pas de variable qualitative")
    message("[save_excel_results] ", "There is no qualitative variables")
    tab_quali_sheet_list <- NULL
  } else {
    if (crossed_varstrat) {
      ##### crossed_varstrat #####
      message("[save_excel_results] ", "crossed_varstrat")
      # for each level of group (varstrat[[1]]), produce a part of the table :
      levels_to_sep <- levels(dataframe[[varstrat[1]]])
      message(paste0("[save_excel_results] ", "quali (with Varstat ? ", varstrat[1], ")"))

      tab_quali_sheet_sep <- lapply(levels_to_sep, function(level_i) {
        message(paste0(
          "[save_excel_results] Go on varstrat ", varstrat[1], " crossed for level ", level_i, ")"
        ))

        analyse_desc_quali <- compute_factorial_table(
          dataframe = droplevels(dataframe[dataframe[[varstrat[1]]] %in% level_i, ]),
          vars = vars_quali,
          varstrat = varstrat[2],
          simplify = simplify,
          prop_table_margin = prop_table_margin,
          digits = digits,
          force_generate_1_when_0 = force_generate_1_when_0
        )

        ## stat test in crossed_varstrat as option
        if (crossed_varstrat_test) {
          pvaleur_quali <- test_proportions(
            dataframe = dataframe[dataframe[[varstrat[1]]] %in% level_i, ],
            vars = vars_quali,
            varstrat = varstrat[2]
          )
          tab_test <- data.table::as.data.table(pvaleur_quali$result, keep.rownames = "Variable")
        }

        tab_quali_sheet <- data.table::rbindlist(
          l = lapply(
            X = seq_len(length(analyse_desc_quali)),
            FUN = function(i) {
              var_i <- names(analyse_desc_quali)[i]
              message("[save_excel_results] ", var_i)
              tmp_raw <- analyse_desc_quali[i]
              tmp <- data.table::as.data.table(tmp_raw, keep.rownames = "Modalites")
              # names(tmp) <- gsub(paste0(var_i, "\\."), "", names(tmp))
              names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
              tmp$Variable <- var_i

              levels_i_n <- grep("^n(.*)", names(tmp), value = TRUE)[-1]
              levels_i_p <- grep("^p(.*)", names(tmp), value = TRUE)[-1]
              tmp$Nb_mesures <- as.character(tmp$Nb_mesures)
              tmp$Nb_mesures <- ifelse(
                tmp$n[tmp$Modalites %in% "Nb_mesures"] == "0",
                "0",
                paste0(
                  tmp$n[tmp$Modalites %in% "Nb_mesures"],
                  ifelse(
                    detail_NB_mesure_sum,
                    paste0(" ", "(", paste0(
                      tmp[tmp$Modalites %in% "Nb_mesures", .SD, .SDcols = levels_i_n],
                      collapse = "+"
                    ), ")"),
                    ""
                  )
                )
              )
              tmp$Valeurs_manquantes <- ifelse(
                tmp$n[tmp$Modalites %in% "N_NA"] == 0,
                "0",
                paste0(
                  tmp$n[tmp$Modalites %in% "N_NA"],
                  ifelse(
                    detail_NB_mesure_sum,
                    paste0(" ", "(", paste0(
                      tmp[tmp$Modalites %in% "N_NA", .SD, .SDcols = levels_i_n],
                      collapse = "+"
                    ), ")"),
                    ""
                  )
                )
              )
              # tmp
              levels_i_names <- gsub("^n(.*)", "\\1", levels_i_n)
              tmp2 <- tmp[, `:=`(
                `Population_totale` = paste0(n, " ", p)
              )][
                ,
                c(paste0(varstrat[2], "=", levels_i_names)) := lapply(levels_i_names, FUN = function(namei) {
                  paste(tmp[[paste0("n", namei)]], tmp[[paste0("p", namei)]])
                })
              ]
              tmp3 <- tmp2[!Modalites %in% c("Nb_mesures", "N_NA"), .SD, .SDcols = c(
                "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes", "Population_totale",
                paste0(varstrat[2], "=", levels_i_names)
              )]

              if (nrow(tmp3) == 0) {
                # because of the droplevels, this variable is not present at all for this strat of varstrat[1]
                tmp3 <- tmp2[Modalites %in% c("Nb_mesures"), .SD, .SDcols = c(
                  "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes", "Population_totale",
                  paste0(varstrat[2], "=", levels_i_names)
                )]
                tmp3 <- tmp3[rep(1, length(levels(dataframe[[tmp3$Variable]]))), ]
                tmp3$Modalites <- levels(dataframe[[unique(tmp3$Variable)]])
              }

              ## --fix make error when merge later
              # if (light_contents && (nrow(tmp3) - 1) >= 0) { # only applicable if levels > 0
              #   tmp3$Variable <- c(tmp3$Variable[1], rep(NA, nrow(tmp3) - 1))
              #   tmp3$Nb_mesures <- c(tmp3$Nb_mesures[1], rep(NA, nrow(tmp3) - 1))
              #   tmp3$Valeurs_manquantes <- c(tmp3$Valeurs_manquantes[1], rep(NA, nrow(tmp3) - 1))
              # }

              return(tmp3)
            }
          ),
          fill = TRUE
        )

        ## stat test in crossed_varstrat as option
        if (crossed_varstrat_test) {
          # merge computed table and stat test results
          # if (light_contents) { ## --fix make error when merge later
          #   tab_quali_sheet$line_fill <- !is.na(tab_quali_sheet$Nb_mesures)
          #   tab_test$line_fill <- TRUE
          #   tab_quali_sheet <- merge(
          #     x = tab_quali_sheet, y = tab_test,
          #     by = c("Variable", "line_fill"),
          #     all = TRUE,
          #     sort = FALSE
          #   )
          #   tab_quali_sheet$line_fill <- NULL
          # } else {
          tab_quali_sheet <- merge(
            x = tab_quali_sheet, y = tab_test,
            by = "Variable",
            all = TRUE,
            sort = FALSE
          )
          # }
        }

        ## keep info of level i from varstrat[[1]]
        names(tab_quali_sheet) <- paste0(varstrat[1], "==", level_i, "__", names(tab_quali_sheet))
        names(tab_quali_sheet)[1] <- "Variable"
        names(tab_quali_sheet)[2] <- "Modalites"

        return(tab_quali_sheet)
      })

      tab_quali_sheet_tab <- Reduce(
        function(x, y) {
          merge(
            x = x, y = y,
            by = c("Variable", "Modalites"),
            all = TRUE, sort = FALSE
          )
        },
        tab_quali_sheet_sep
      )
      # order folling levels
      tab_quali_sheet_tab$Variable <- factor(tab_quali_sheet_tab$Variable, levels = vars_quali)
      tab_quali_sheet_tab <- tab_quali_sheet_tab[order(Variable), ]

      if (!global_summary) {
        col_select <- setdiff(
          names(tab_quali_sheet_tab),
          grep("Population_totale", names(tab_quali_sheet_tab), value = TRUE)
        )
        tab_quali_sheet_tab <- tab_quali_sheet_tab[, .SD, .SDcols = col_select]
      }

      first_line <- data.table::as.data.table(matrix(
        data = c(
          paste0("varstrat:", varstrat[1]),
          gsub("(.*)__(.*)", "\\2", names(tab_quali_sheet_tab))[-1]
        ),
        nrow = 1, byrow = TRUE,
        dimnames = list(NA, names(tab_quali_sheet_tab))
      ))
      tab_quali_sheet_tab <- data.table::rbindlist(l = list(first_line, tab_quali_sheet_tab))

      # finish to format
      # names(tab_quali_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quali_sheet_tab))
      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- paste0("quali-", varstrat[1], "-", varstrat[2])
    } else {
      ##### for each variable of group varstrat #####
      if (is.null(varstrat)) varstrat <- ""
      tab_quali_sheet_list <- lapply(X = varstrat, function(varstrat_i) {
        mm <- ifelse(
          is.null(varstrat_i) || varstrat_i %in% "",
          paste0("[save_excel_results] ", "quali (without Varstat)"),
          paste0("[save_excel_results] ", "quali (with Varstat ? ", varstrat_i, ")")
        )
        message(mm)

        # get computed tables
        if (!is.null(varstrat_i) && !(varstrat_i %in% "") && is.numeric(dataframe[[varstrat_i]])) {
          ##### varstrat continuous #####
          analyse_desc_quali <- lapply(X = vars_quali, FUN = function(quali_i_var) {
            compute_continuous_table(
              dataframe = dataframe,
              vars = varstrat_i,
              varstrat = quali_i_var,
              stats_choice = c(
                "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal"
              ),
              # all stats are excepted in excel workbook function
              digits = digits
            )
          })
        } else {
          ##### varstrat factorial or no varstrat #####
          analyse_desc_quali <- compute_factorial_table(
            dataframe = dataframe,
            vars = vars_quali,
            varstrat = varstrat_i,
            simplify = simplify, prop_table_margin = prop_table_margin,
            digits = digits,
            force_generate_1_when_0 = force_generate_1_when_0
          )
        }

        # format outputs tab_quali_sheet
        if (is.null(varstrat_i) || varstrat_i %in% "") {
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
                tmp$Valeurs_manquantes <- tmp$n[tmp$Modalites %in% "N_NA"]
                tmp$`Effectif (%)` <- paste(tmp$n, tmp$p)
                tmp <- tmp[!Modalites %in% c("Nb_mesures", "N_NA"), .SD, .SDcols = c(
                  "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes", "Effectif (%)"
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
          ##### stats tests #####

          if (is.numeric(dataframe[[varstrat_i]])) {
            tab_test <- data.table::rbindlist(lapply(X = vars_quali, FUN = function(quali_i_var) {
              message("[save_excel_results] test_means ", quali_i_var)
              pvaleur_quanti <- test_means(
                dataframe = dataframe,
                vars = varstrat_i,
                varstrat = quali_i_var,
                force_non_parametric_test = force_non_parametric_test,
                digits = digits,
                signif_digits = signif_digits
              )
              tab_test <- data.table::as.data.table(
                pvaleur_quanti$result,
                keep.rownames = "Variable"
              )
              tab_test$varstrat <- quali_i_var
              return(tab_test)
            }), use.names = TRUE, fill = TRUE)
          } else {
            pvaleur_quali <- test_proportions(
              dataframe = dataframe,
              vars = vars_quali,
              varstrat = varstrat_i,
              digits = digits,
              signif_digits = signif_digits
            )
            tab_test <- data.table::as.data.table(
              pvaleur_quali$result,
              keep.rownames = "Variable"
            )
          }

          # now we have computed tables and pvalues, reshape table outputs : tab_quali_sheet
          if (is.numeric(dataframe[[varstrat_i]])) {
            tab_quali_sheet <- data.table::rbindlist(
              l = lapply(
                X = seq_len(length(analyse_desc_quali)),
                FUN = function(i) {
                  var_i <- vars_quali[i]
                  message("[save_excel_results] ", var_i)
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

                  tmp$Valeurs_manquantes <- tmp[["N_NA"]]

                  tmp <- tmp[, `:=`(
                    `Mean_sd` = paste0(mean, " +/- ", sd),
                    `Med_q1_q3` = paste0(median, " [", Q1, ";", Q3, "]"),
                    `Min - Max` = paste0(min, " - ", max)
                  )]

                  cell_auto <- ifelse(
                    test = tab_test[varstrat %in% var_i, "Test"] %in%
                      c("Wilcoxon rank sum exact test (Mann-Whitney)", "Kruskal-Wallis rank sum test"),
                    yes = "Med_q1_q3",
                    no = "Mean_sd" # default show means if no test
                  )
                  cell_content <- ifelse(
                    test = metric_show %in% "auto",
                    yes = cell_auto, # selection auto according test
                    no = ifelse(
                      test = metric_show %in% "mean",
                      yes = "Mean_sd",
                      no = "Med_q1_q3"
                    )
                  )

                  tmp <- tmp[, .SD, .SDcols = c(
                    # "Variable",
                    "varstrat", "Modalites",
                    "N", "Valeurs_manquantes", "Nb_mesures",
                    # "Mean_sd", "Med_q1_q3" # select one of them
                    cell_content
                    # , "Min - Max", "is_Normal" # nor return with varstrat
                  )]

                  tmp[Nb_mesures %in% "0", cell_content] <- "/" # if n = 0, NaN +/- NA replace by "/"

                  names(tmp)[grep(cell_content, names(tmp))] <- varstrat_i

                  return(tmp)
                }
              ),
              fill = TRUE
            )

            # merge computed table and stat test results
            if (light_contents) {
              tab_quali_sheet$line_fill <- tab_quali_sheet$Modalites %in% "Population_totale"
              tab_test$line_fill <- TRUE
              tab_quali_sheet <- merge(
                tab_quali_sheet, tab_test,
                by = c(
                  # "Variable",
                  "varstrat", "line_fill"
                ),
                all = TRUE,
                sort = FALSE
              )
              tab_quali_sheet$line_fill <- NULL
            } else {
              tab_quali_sheet <- merge(
                x = tab_quali_sheet,
                y = tab_test,
                by = "varstrat", # c("Variable", "varstrat")
                sort = FALSE
              )
            }
          } else {
            tab_quali_sheet <- data.table::rbindlist(
              l = lapply(
                X = seq_len(length(analyse_desc_quali)),
                FUN = function(i) {
                  var_i <- names(analyse_desc_quali)[i]
                  message("[save_excel_results] ", var_i)
                  tmp_raw <- analyse_desc_quali[i]
                  tmp <- data.table::as.data.table(tmp_raw, keep.rownames = "Modalites")
                  names(tmp) <- c("Modalites", names(tmp_raw[[1]]))
                  tmp$Variable <- var_i

                  levels_i_n <- grep("^n(.*)", names(tmp), value = TRUE)[-1]
                  levels_i_p <- grep("^p(.*)", names(tmp), value = TRUE)[-1]
                  tmp$Nb_mesures <- as.character(tmp$Nb_mesures)
                  tmp$Nb_mesures <- ifelse(
                    tmp$n[tmp$Modalites %in% "Nb_mesures"] == "0",
                    "0",
                    paste0(
                      tmp$n[tmp$Modalites %in% "Nb_mesures"],
                      ifelse(
                        detail_NB_mesure_sum,
                        paste0(" ", "(", paste0(
                          tmp[tmp$Modalites %in% "Nb_mesures", .SD, .SDcols = levels_i_n],
                          collapse = "+"
                        ), ")"),
                        ""
                      )
                    )
                  )
                  tmp$Valeurs_manquantes <- ifelse(
                    tmp$n[tmp$Modalites %in% "N_NA"] == 0,
                    0,
                    paste0(
                      tmp$n[tmp$Modalites %in% "N_NA"],
                      ifelse(
                        detail_NB_mesure_sum,
                        paste0(" ", "(", paste0(
                          tmp[tmp$Modalites %in% "N_NA", .SD, .SDcols = levels_i_n],
                          collapse = "+"
                        ), ")"),
                        ""
                      )
                    )
                  )
                  # tmp

                  levels_i_names <- gsub("^n(.*)", "\\1", levels_i_n)
                  tmp2 <- tmp[, `:=`(
                    `Population_totale` = paste0(n, " ", p)
                  )][
                    ,
                    c(paste0(varstrat_i, "=", levels_i_names)) := lapply(levels_i_names, FUN = function(namei) {
                      paste(tmp[[paste0("n", namei)]], tmp[[paste0("p", namei)]])
                    })
                  ]
                  tmp2 <- tmp2[!Modalites %in% c("Nb_mesures", "N_NA"), .SD, .SDcols = c(
                    "Variable", "Modalites", "Nb_mesures", "Valeurs_manquantes", "Population_totale",
                    paste0(varstrat_i, "=", levels_i_names)
                  )]

                  if (light_contents && (nrow(tmp2) - 1) >= 0) { # only applicable if levels > 0
                    tmp2$Variable_light <- c(tmp2$Variable[1], rep(NA, nrow(tmp2) - 1))
                    # it is needed to keep full Variable if OR wanted
                    tmp2$Nb_mesures <- c(tmp2$Nb_mesures[1], rep(NA, nrow(tmp2) - 1))
                    tmp2$Valeurs_manquantes <- c(tmp2$Valeurs_manquantes[1], rep(NA, nrow(tmp2) - 1))
                  }

                  return(tmp2)
                }
              ),
              fill = TRUE
            )

            if (show_OR && is.factor(dataframe[[varstrat_i]]) && length(levels(dataframe[[varstrat_i]])) == 2) {
              message("[save_excel_results] ", "show_OR")
              OR_tab <- get_OR_univar(
                dataframe = dataframe,
                dependent_var = varstrat_i,
                explanatory_vars = vars_quali,
                check_n_levels = TRUE,
                signif_digits = signif_digits
              )
              if (is.null(OR_tab)) {
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
    }
  }

  #### show p adjusted ####

  if (show_p_adj && !is.null(varstrat[[1]]) && !varstrat[[1]] %in% "") {
    message("[save_excel_results] show_p_adj")
    if (crossed_varstrat) {
      ##### P adj for crossed_varstrat #####
      ## for each level of varstrat[1]
      levels_to_sep <- levels(dataframe[[varstrat[1]]])
      message("[save_excel_results] On each levels of ", varstrat[[1]])

      tab_pval_sep <- lapply(levels_to_sep, function(level_i) {
        tab_quanti <- tab_quanti_sheet_tab[, .SD, .SDcols = c(
          "Variable", grep(paste0(varstrat[[1]], "==", level_i), names(tab_quanti_sheet_tab), value = TRUE)
        )]
        tab_quali <- tab_quali_sheet_tab[, .SD, .SDcols = c(
          "Variable", "Modalites",
          grep(paste0(varstrat[[1]], "==", level_i), names(tab_quali_sheet_tab), value = TRUE)
        )]

        tab_pval <- data.table::rbindlist(l = list(
          tab_quanti[, .SD, .SDcols = c(
            "Variable",
            grep(paste0(varstrat[[1]], "==", level_i, "__P_valeur"), names(tab_quanti), value = TRUE)
          )],
          tab_quali[, .SD, .SDcols = c(
            "Variable",
            grep(paste0(varstrat[[1]], "==", level_i, "__P_valeur"), names(tab_quali), value = TRUE)
          )]
        ), use.names = TRUE, fill = TRUE)
        pval_col <- names(tab_pval)[2]
        names(tab_pval)[2] <- "P_valeur"
        tab_pval <- unique(tab_pval[
          P_valeur != "P_valeur",
        ][, P_valeur := as.numeric(P_valeur)][!is.na(P_valeur), ])[
          order(P_valeur, decreasing = FALSE), # classés du plus bas au plus haut.
        ]

        tab_pval$P_adj_holm <- signif(
          stats::p.adjust(p = tab_pval$P_valeur, method = "holm"),
          digits = signif_digits
        )
        tab_pval$P_valeur <- NULL
        new_p_col <- gsub("__P_valeur", "__P_adj_holm", pval_col)
        names(tab_pval)[2] <- new_p_col

        if (length(vars_quanti) > 0) {
          tab_quanti <- merge(
            x = tab_quanti, y = tab_pval, by = "Variable", all.x = TRUE, sort = FALSE
          )
          # add col names in line 1 :
          tab_quanti[[new_p_col]][1] <- "P_adj_holm"
          # order cols
          pcol <- grep("__P_valeur$", names(tab_quanti))
          data.table::setcolorder(x = tab_quanti, neworder = c(
            unique(c(
              names(tab_quanti)[1:pcol], new_p_col, names(tab_quanti)[(pcol + 1):ncol(tab_quanti)]
            ))
          ))
        } else {
          tab_quanti <- NULL
        }

        if (length(vars_quali) > 0) {
          tab_quali <- merge(
            x = tab_quali, y = tab_pval, by = "Variable", all.x = TRUE, sort = FALSE
          )
          # add col names in line 1 :
          tab_quali[[new_p_col]][1] <- "P_adj_holm"
          # order cols
          pcol <- grep("__P_valeur$", names(tab_quali))
          data.table::setcolorder(x = tab_quali, neworder = c(
            unique(c(
              names(tab_quali)[1:pcol], new_p_col, names(tab_quali)[(pcol + 1):ncol(tab_quali)]
            ))
          ))
        } else {
          tab_quali <- NULL
        }
        return(list("tab_quanti" = tab_quanti, "tab_quali" = tab_quali))
      })

      tab_quanti_sheet_sep <- lapply(X = tab_pval_sep, FUN = function(eli) {
        eli$tab_quanti
      })
      tab_quali_sheet_sep <- lapply(X = tab_pval_sep, FUN = function(eli) {
        eli$tab_quali
      })
      tab_quanti_sheet_tab <- Reduce(
        f = function(x, y) merge(x = x, y = y, by = "Variable", all = TRUE, sort = FALSE),
        x = tab_quanti_sheet_sep
      )
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab) ## --here v0.1.18
      names(tab_quanti_sheet_list) <- paste0("quanti-", varstrat[1], "-", varstrat[2])
      tab_quali_sheet_tab <- Reduce(
        function(x, y) {
          merge(
            x = x, y = y,
            by = c("Variable", "Modalites"),
            all = TRUE, sort = FALSE
          )
        },
        tab_quali_sheet_sep
      )
      tab_quali_sheet_list <- list(tab_quali_sheet_tab) ## --here v0.1.18
      names(tab_quali_sheet_list) <- paste0("quali-", varstrat[1], "-", varstrat[2])
    } else {
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
      ][
        order(P_valeur, decreasing = FALSE), # classés du plus bas au plus haut.
      ]
      tab_pval$P_adj_holm <- signif(stats::p.adjust(
        p = tab_pval$P_valeur, method = "holm"
      ), digits = signif_digits)
      tab_pval$P_valeur <- NULL

      if (length(vars_quanti) > 0) {
        tab_quanti_sheet_tab <- merge(
          x = tab_quanti_sheet_tab, y = tab_pval, by = "Variable", all.x = TRUE, sort = FALSE
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
          x = tab_quali_sheet_tab, y = tab_pval, by = "Variable", all.x = TRUE, sort = FALSE
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
  }

  ####  finish to format pval and sheets ####
  if (crossed_varstrat) {
    ##### crossed_varstrat #####
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

            # + add simplify ? --here
            # if (nrow(tmp) == 2 & modalites 0/1 ou oui/non )

            # light content in excel cells
            tmp$Variable <- c(as.character(tmp$Variable[1]), rep(NA, nrow(tmp) - 1))
            idx_val <- which(!is.na(tmp[, 3]))[1]
            idx_na <- setdiff(seq_len(nrow(tmp)), idx_val)
            tmp[idx_na, grep("Nb_mesures$", names(tmp))] <- NA
            tmp[idx_na, grep("Valeurs_manquantes$", names(tmp))] <- NA
            if (crossed_varstrat_test) {
              tmp[idx_na, grep("P_valeur$", names(tmp))] <- NA
              tmp[idx_na, grep("Test$", names(tmp))] <- NA
              tmp[idx_na, grep("message$", names(tmp))] <- NA
            }
            if (show_p_adj) {
              tmp[idx_na, grep("P_adj_holm$", names(tmp))] <- NA
            }
            return(tmp)
          })
        )
      } # else nothing to format as light
    }

    ##### format p val in tab_quanti_sheet_tab #####
    pval_cols_quanti <- c(
      grep("P_valeur$", names(tab_quanti_sheet_tab), value = TRUE),
      grep("P_adj_holm$", names(tab_quanti_sheet_tab), value = TRUE)
    )
    if (length(pval_cols_quanti) > 0) {
      tab_quanti_sheet_tab[-1, # do not modify the first list (subnames)
        (pval_cols_quanti) := lapply(.SD, function(icol) {
          icol_num <- as.numeric(icol)
          ifelse(
            test = icol_num < 0.0001,
            yes = "<0.0001",
            no = icol_num
          )
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
      tab_quali_sheet_tab[-1, # do not modify the first list (subnames)
        (pval_cols_quali) := lapply(.SD, function(icol) {
          icol_num <- as.numeric(icol)
          ifelse(
            test = icol_num < 0.0001,
            yes = "<0.0001",
            no = icol_num
          )
        }),
        .SDcols = pval_cols_quali
      ]
    }

    # finish to format sheet list

    if (!is.null(tab_quanti_sheet_tab)) { ## --here v0.1.18
      names(tab_quanti_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quanti_sheet_tab))

      if (!is.null(dico_mapping)) {
        # add label #v0.1.22
        tab_quanti_sheet_tab <- merge(
          x = tab_quanti_sheet_tab,
          y = dico_mapping,
          by = "Variable", all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quanti_sheet_tab, neworder = unique(c("Label", names(tab_quanti_sheet_tab)))
        )
      }

      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- paste0("quanti-", varstrat[1], "-", varstrat[2])
    } else {
      tab_quanti_sheet_list <- NULL ## --here v0.1.18
    }

    if (!is.null(tab_quali_sheet_tab)) { ## --here v0.1.18
      names(tab_quali_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quali_sheet_tab))

      if (!is.null(dico_mapping)) {
        # add label #v0.1.22
        tab_quali_sheet_tab <- merge(
          x = tab_quali_sheet_tab,
          y = dico_mapping,
          by = "Variable", all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quali_sheet_tab, neworder = unique(c("Label", names(tab_quali_sheet_tab)))
        )
      }

      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- paste0("quali-", varstrat[1], "-", varstrat[2])
    } else {
      tab_quali_sheet_list <- NULL ## --here v0.1.18
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
          ifelse(
            test = icol_num < 0.0001,
            yes = "<0.0001",
            no = icol_num
          )
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
          ifelse(
            test = icol_num < 0.0001,
            yes = "<0.0001",
            no = icol_num
          )
        }),
        .SDcols = pval_cols_quali
      ]
    }

    # remake the lists
    if (is.null(name_quanti)) {
      tab_quanti_sheet_list <- NULL
    } else {
      if (!is.null(dico_mapping)) {
        # add label #v0.1.22
        tab_quanti_sheet_tab <- merge(
          x = tab_quanti_sheet_tab,
          y = dico_mapping,
          by = "Variable", all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quanti_sheet_tab, neworder = unique(c("Label", names(tab_quanti_sheet_tab)))
        )
      }

      # remake the list
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- name_quanti
    }
    if (is.null(name_quali)) {
      tab_quali_sheet_list <- NULL
    } else {
      if (!is.null(dico_mapping)) {
        # add label
        tab_quali_sheet_tab <- merge(
          x = tab_quali_sheet_tab,
          y = dico_mapping,
          by = "Variable", all.x = TRUE,
          sort = FALSE
        )
        data.table::setcolorder(
          x = tab_quali_sheet_tab, neworder = unique(c("Label", names(tab_quali_sheet_tab)))
        )
      }

      # remake the list
      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- name_quali
    }
  }

  #### Write Excel ####
  message("[save_excel_results] ", "write_xlsx")
  writexl::write_xlsx(
    x = c(
      tab_quanti_sheet_list,
      tab_quali_sheet_list,
      tab_na_sheet_list
    ),
    path = file, col_names = TRUE
  )

  return(file)
}
