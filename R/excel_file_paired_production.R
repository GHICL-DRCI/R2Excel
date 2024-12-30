#' Save the tables for paired data in Excel file
#'
#' provide descriptive statistics table with a paired level, saved in Excel
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#'  Columns must be well formated with factor or numeric class (important).
#' @param vars A vector of characters. Name of columns to describe and test.
#' @param varstrat A characters. Not null. Names of the stratification variable, making groups to compare (*time* or *visites* for instance).
#'   The repeated measures must be presented *in line* (for instance, "V1", "V2", "V3" will be set on 3 lines for each individuals).
#'   About 2 *crossed varstrats* : you can provide "var1*var2", and so
#'    get a description of the 2nd varstrat (var2) for each level of the 1st varstrat (var1).
#'   Attention, we except var1 as the repeated variables (time or visites) and 
#'    var2 as the condition/group that will be tested with *not paired test* (if `crossed_varstrat_test` is set TRUE).
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines should be displayed for binary variables.
#'   TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), FALSE = both levels of the variables.
#' @param patient_id A character. Default "patientid". Name of identifant patient id column.
#'   the repeated measures must be presented in line (for instance, "id1" will be set on 3 line if he has 3 visits)
#' @param global_summary A logical. Default FALSE Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... but ok if you want it, you can.
#' @param force_non_parametric_test A logical. Default FALSE. You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#' @param force_generate_1_when_0 A logical, Default FALSE. If TRUE, will test if the unique modality is 0 or "non" and
#'    add the level 1 or "oui" so it can be display in counts. Can be combined with simplify to
#'    only show the modality (1).
#' @param keep_missing_line A logical, Default TRUE. Do you want to keep the missing data count (like a level)
#' @param metric_show A character, Default "auto". What is the metric to show in cell content ?
#'   "auto" = mean or median, automatic choice according shapiro test,
#'   "mean" = mean +/- sd forced, whatever shapir said,
#'   "median" = media [Q1;Q3] forced, whatever shapir said.
#'   Caution, if you force_non_paramtric_test as TRUE, metric_show is forced as 'median' to be consistent.
#' @param crossed_varstrat_test A logical, Default FALSE. If turn TRUE, and detection of 2 varstrat "var1*var2" to cross,
#'  statistical test will be provided.
#' @param detail_NB_mesure_sum A logical, Default FALSE. If turn TRUE, N will be shown with detail (N1 + N2 + ...) for each group.
#' @param light_contents A logical, Default TRUE. If FALSE,
#'   the information like Nb_mesure, Valeurs manquantes et p will be repeated.
#' @param test_more_2_levels A logicial, Default FALSE. Do not return stat test if varstrat has more than 2 levels
#' @param show_p_adj A logical, Default FALSE. If trun TRUE, add P_adj_holm column based on p.adjust.
#' @param file A character. Path and name of the excel results file.
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
#'   digits = 1,
#'   global_summary = TRUE,
#'   file = file.path("tmp", "desc_paired_data1.xlsx")
#' )
#' save_excel_paired_results(
#'   dataframe = modified_sleep,
#'   vars = c("extra", "extra_with_missings", "mesure1", "fact1", "fact1_na"),
#'   varstrat = c("visites_2"),
#'   patient_id = "ID2",
#'   digits = 2,
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
#'   digits = 2,
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
#'   digits = 2,
#'   global_summary = TRUE,
#'   force_generate_1_when_0 = FALSE, # for fact tab
#'   keep_missing_line = TRUE, # for fact tab
#'   detail_NB_mesure_sum = TRUE,
#'   crossed_varstrat_test = TRUE,
#'   file = file.path("tmp", "desc_paired_data5.xlsx")
#' )
#' }
save_excel_paired_results <- function(
  dataframe,
  vars,
  varstrat,
  digits = 2,
  signif_digits = 4,
  patient_id = "patientid",
  simplify = FALSE,
  global_summary = FALSE,
  force_non_parametric_test = FALSE,
  force_generate_1_when_0 = TRUE, # for fact tab
  keep_missing_line = TRUE, # for fact tab
  metric_show = "auto",
  light_contents = TRUE,
  crossed_varstrat_test = FALSE,
  detail_NB_mesure_sum = FALSE,
  test_more_2_levels = FALSE,
  show_p_adj = FALSE,
  file = "paired_desc_table.xlsx"
) {
  
  # dataframe = modified_sleep
  # vars = c("extra", "extra_with_missings", "mesure1", "fact1", "fact1_na")
  # varstrat = "visites_2*fact3"
  # patient_id = "ID2"
  # digits = 2
  # global_summary = TRUE
  # force_generate_1_when_0 = FALSE  # for fact tab
  # keep_missing_line = TRUE # for fact tab
  # file = file.path("tmp","E-nested_paired_desc.xlsx")
  # 
  # signif_digits = 4
  # simplify = FALSE
  # force_non_parametric_test = FALSE
  # metric_show = "auto"
  # light_contents = TRUE
  # crossed_varstrat_test = FALSE
  # detail_NB_mesure_sum = FALSE
  # test_more_2_levels = FALSE
  # show_p_adj = FALSE
  
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(patient_id %in% names(dataframe))
  stopifnot(!is.null(varstrat))
  stopifnot(!varstrat %in% "")
  stopifnot(length(varstrat) == 1)
  stopifnot(digits >= 0)
  stopifnot(signif_digits >= 0)
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(force_non_parametric_test))
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(is.logical(keep_missing_line))
  stopifnot(metric_show %in% c("mean", "median", "auto"))
  stopifnot(is.logical(light_contents))
  stopifnot(is.logical(crossed_varstrat_test))
  stopifnot(is.logical(test_more_2_levels))

  ## detect crossed varstrat :
  varstrat_splited <- strsplit(x = varstrat, split = "*", fixed = TRUE)
  if (length(unlist(varstrat_splited)) == 2) {
    varstrat <- unlist(varstrat_splited)
    crossed_varstrat <- TRUE
  } else {
    crossed_varstrat <- FALSE
  }

  if (force_non_parametric_test) {
    message(
      "[save_excel_paired_results]",
      "Because force_non_parametric_test is TRUE, metric_show is forced as 'median'."
    )
    metric_show <- "median"
  }

  vars <- setdiff(vars, varstrat)
  vars_quanti <- get_numerics(dataframe, vars = vars)
  vars_quali <- get_factors(dataframe, vars = vars)

  #### Quanti vars ####
  if (length(vars_quanti) > 0) {
    if (crossed_varstrat) {
      ##### crossed_varstrat #####
      message("[save_excel_paired_results] ", "crossed_varstrat")
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
              varstrat = varstrat[2]
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
                detail_NB_mesure_sum,
                paste0(" ", "(", paste0(
                  res$Nb_mesures[!res$Modalites %in% "Population_totale"],
                  collapse = "+"
                ), ")"),
                ""
              )
            )

            res$Valeurs_manquantes <- paste0(
              res[["NA"]][res$Modalites %in% "Population_totale"],
              ifelse(
                detail_NB_mesure_sum & res[["NA"]][res$Modalites %in% "Population_totale"] != 0,
                paste0(" ", "(", paste0(
                  res$Nb_mesures[!res$Modalites %in% "Population_totale"],
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
        round(mean, digits), " +/- ", round(sd, digits) # , " (n=", Nb_mesures, ")"
      )]
      tab_all_varstrat_quanti <- tab_all_varstrat_quanti[, Med_q1_q3 := paste0(
        round(median, digits), " [", round(Q1, digits), ";", round(Q3, digits), "]" # , " (n=", Nb_mesures, ")"
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

      if (metric_show %in% c("auto")) {
        ## go read is_Normal info
        tab_all_varstrat_quanti$cell_contents <- ifelse(
          tab_all_varstrat_quanti$is_Normal %in% 1,
          tab_all_varstrat_quanti$Mean_sd,
          tab_all_varstrat_quanti$Med_q1_q3
        )
      } else {
        if (metric_show %in% c("mean")) {
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

      # tab_all_varstrat_quanti_wide <- lapply(X = levels_to_sep, function(level_i) {
      #   res <- data.table::dcast(
      #     data = tab_all_varstrat_quanti[strat %in% level_i, ],
      #     formula = stats::as.formula("Variable ~ varstrat2"),
      #     value.var = "cell_contents"
      #   )
      #   names(res) <- paste0(varstrat[1], "==", level_i, "__", names(res))
      #   names(res)[1] <- "Variable"
      #   return(res)
      # })
      ## --fix 
      tab_all_varstrat_quanti_wide <- lapply(X = levels_to_sep, function(level_i) {
        res <- data.table::dcast(
          data = tab_all_varstrat_quanti[strat %in% level_i, ],
          formula = stats::as.formula(
            "Variable + Nb_mesures + Valeurs_manquantes ~ varstrat2"
          ),
          value.var = "cell_contents"
        )

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

        ## stat test added in option
        if (crossed_varstrat_test) {
          res <- merge(res, tab_test, by = "Variable", all = TRUE, sort = FALSE)
        }

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

      tab_quanti_sheet_tab <- data.table::rbindlist(l = list(first_line, tab_all_varstrat_quanti2))
      # finish to format
      # names(tab_quanti_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quanti_sheet_tab))
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- paste0("quanti-", varstrat[1], "-", varstrat[2])
    } else {
      ##### for each variable of group varstrat #####

      tab_quanti_sheet_tab <- data.table::rbindlist(lapply(X = varstrat, function(varstrat_i) {
        message("[save_excel_paired_results] varstrat ", varstrat_i)

        complete_continuous_tab_varstrati <- data.table::rbindlist(
          l = lapply(
            X = vars_quanti,
            FUN = function(variable_interest) {
              res <- compute_paired_continuous_table_and_test(
                dataframe = dataframe,
                variable_interest = variable_interest,
                varstrat = varstrat_i,
                digits = digits,
                signif_digits = signif_digits,
                patient_id = patient_id,
                global_summary = global_summary,
                force_non_parametric_test = force_non_parametric_test,
                metric_show = metric_show,
                test_more_2_levels = test_more_2_levels
              )
              return(res$line_res)
            }
          ),
          fill = TRUE
        )
        complete_continuous_tab_varstrati$is_Normal <- NULL # no more wanted to be shown.
        return(complete_continuous_tab_varstrati)
      }), fill = TRUE)

      # order : message at the very end
      data.table::setcolorder(x = tab_quanti_sheet_tab, neworder = c(
        setdiff(names(tab_quanti_sheet_tab), "message"), "message"
      ))

      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- paste0("quantitative - ", varstrat[1])
    }
  } else {
    message("[save_excel_paired_results] There is no quantitative variables")
    tab_quanti_sheet_list <- NULL
  }

  #### Quali vars ####
  if (length(vars_quali) > 0) {
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
            digits = digits,
            simplify = simplify,
            prop_table_margin = 2,
            force_generate_1_when_0 = force_generate_1_when_0
          )
          return(res)
        })

        ## stat test in crossed_varstrat as option
        if (crossed_varstrat_test) {
          pvaleur_quali <- test_proportions(
            dataframe = dt,
            vars = vars_quali,
            varstrat = varstrat[2]
          )
          tab_test <- data.table::as.data.table(
            pvaleur_quali$result,
            keep.rownames = "Variable"
          )
        }

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
                detail_NB_mesure_sum,
                paste0(" ", "(", paste0(
                  tmp[tmp$Modalites %in% "Nb_mesures", .SD, .SDcols = levels_i_n],
                  collapse = "+"
                ), ")"),
                ""
              )
            )
            tmp$Valeurs_manquantes <- ifelse(
              tmp$n[tmp$Modalites %in% "NA"] == 0,
              "0",
              paste0(
                tmp$n[tmp$Modalites %in% "NA"],
                ifelse(
                  detail_NB_mesure_sum,
                  paste0(" ", "(", paste0(
                    tmp[tmp$Modalites %in% "NA", .SD, .SDcols = levels_i_n],
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
            tmp3 <- tmp2[!Modalites %in% c("Nb_mesures", "NA"), .SD, .SDcols = c(
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

        ## stat test in crossed_varstrat as option
        if (crossed_varstrat_test) {
          tab_quali_sheet <- merge(
            x = tab_quali_sheet, y = tab_test,
            by = "Variable",
            all = TRUE,
            sort = FALSE
          )
        }

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
      tab_all_varstrat_quali_gather$Variable <- factor(tab_all_varstrat_quali_gather$Variable, levels = vars_quali)
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
      ##### for each variable of group varstrat #####

      tab_quanti_sheet_tab <- data.table::rbindlist(lapply(X = varstrat, function(varstrat_i) {
        message("[save_excel_paired_results] varstrat ", varstrat_i)

        complete_fact_tab_varstrati <- data.table::rbindlist(
          lapply(
            X = vars_quali,
            FUN = function(variable_interest) {
              res <- compute_paired_factorial_table_and_test(
                dataframe = dataframe,
                variable_interest = variable_interest,
                varstrat = varstrat_i,
                digits = digits,
                signif_digits = signif_digits,
                simplify = simplify,
                patient_id = patient_id,
                force_generate_1_when_0 = force_generate_1_when_0,
                keep_missing_line = keep_missing_line,
                global_summary = global_summary,
                test_more_2_levels = test_more_2_levels
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
    
  } else {
    message("[save_excel_paired_results] There is no qualitative variables")
    tab_quali_sheet_list <- NULL
  }


  #### show p adjusted ####
  if (show_p_adj) {
    message("[save_excel_paired_results] show_p_adj")

    if (crossed_varstrat) {
      ##### P adj for crossed_varstrat #####
      ## for each level of varstrat[1]
      levels_to_sep <- levels(dataframe[[varstrat[1]]])
      message("[save_excel_paired_results] On each levels of ", varstrat[[1]])

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

        tab_pval$P_adj_holm <- signif(stats::p.adjust(
          p = tab_pval$P_valeur, method = "holm"
        ), digits = signif_digits)
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
              names(tab_quanti)[1:pcol], new_p_col,
              names(tab_quanti)[(pcol + 1):ncol(tab_quanti)]
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
              names(tab_quali)[1:pcol], new_p_col,
              names(tab_quali)[(pcol + 1):ncol(tab_quali)]
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
          x = tab_quanti_sheet_tab, y = tab_pval, by = "Variable", all.x = TRUE, sort = FALSE
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
          x = tab_quali_sheet_tab, y = tab_pval, by = "Variable", all.x = TRUE, sort = FALSE
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

    #### format p val in tab_quanti_sheet_tab ####
    pval_cols_quanti <- c(
      grep("P_valeur$", names(tab_quanti_sheet_tab), value = TRUE),
      grep("P_adj_holm$", names(tab_quanti_sheet_tab), value = TRUE)
    )
    if (length(pval_cols_quanti) > 0) {
      tab_quanti_sheet_tab <- tab_quanti_sheet_tab[-1,
        (pval_cols_quanti) := lapply(.SD, function(icol) {
          ifelse(
            test = as.numeric(icol) < 0.0001,
            yes = "<0.0001",
            no = icol
          )
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
      tab_quali_sheet_tab <- tab_quali_sheet_tab[-1,
        (pval_cols_quali) := lapply(.SD, function(icol) {
          ifelse(
            test = as.numeric(icol) < 0.0001,
            yes = "<0.0001",
            no = icol
          )
        }),
        .SDcols = pval_cols_quali
      ]
    }

    # finish to format sheet list

    ## --here v0.1.18
    if (!is.null(tab_quanti_sheet_tab)) { ## --here v0.1.18
      names(tab_quanti_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quanti_sheet_tab))
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- paste0("quanti-", varstrat[1], "-", varstrat[2])
    } else {
      tab_quanti_sheet_list <- NULL ## --here v0.1.18
    }
    if (!is.null(tab_quali_sheet_tab)) { ## --here v0.1.18
      names(tab_quali_sheet_tab) <- gsub("(.*)__(.*)", "\\1", names(tab_quali_sheet_tab))
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
          val <- ifelse(
            test = as.numeric(icol) < 0.0001, ## --here to check if icol ever as <0.0001 ?
            yes = "<0.0001",
            no = icol
          )
          return(val)
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
          ifelse(
            test = as.numeric(icol) < 0.0001, ## --here to check if icol ever as <0.0001 ?
            yes = "<0.0001",
            no = icol
          )
        }),
        .SDcols = pval_cols_quali
      ]
    }

    # remake the lists
    if (is.null(name_quanti)) {
      tab_quanti_sheet_list <- NULL
    } else {
      tab_quanti_sheet_list <- list(tab_quanti_sheet_tab)
      names(tab_quanti_sheet_list) <- name_quanti
    }
    if (is.null(name_quali)) {
      tab_quali_sheet_list <- NULL
    } else {
      tab_quali_sheet_list <- list(tab_quali_sheet_tab)
      names(tab_quali_sheet_list) <- name_quali
    }
  }

  #### Write Excel ####
  message("[save_excel_paired_results] ", "write_xlsx")
  writexl::write_xlsx(
    x = c(
      tab_quanti_sheet_list,
      tab_quali_sheet_list
    ),
    path = file,
    col_names = TRUE
  )

  return(file)
}
