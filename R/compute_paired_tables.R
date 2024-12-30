# Core function of the pkg, compute descriptive tables for paired data

#' compute continuous table for paired data
#'
#' provide descriptive statistics table for continuous data with a paired level (time or visites)
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#' @param variable_interest A character. Name of dataframe's continuous columns to describe and test.
#' @param varstrat A character. Name of the stratification variable, making groups to compare (time or visites).
#'   the repeated measures must be presented in line (for instance, "V1", "V2", "V3" will be set on 3 line for each individuals)
#' @param digits A integer, Default 1. Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#' @param patient_id A character., Default "patientid". Name of identifiant patient id column.
#'   the repeated measures must be presented in line (for instance, "id1" will be set on 3 line if he has 3 visits)
#' @param global_summary A logical. Default TRUE. Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... but ok if you want it, you can.
#' @param force_non_parametric_test A logical. Default FALSE. You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#' @param metric_show A character, Default "auto". What is the metric to show in cell content ?
#'   "auto" = mean or median, automatic choice according statistic test,
#'   "mean" = mean +/- sd forced, whatever shapiro said,
#'   "median" = media [Q1;Q3] forced, whatever shapiro said.
#' @param test_more_2_levels A logicial, Default FALSE. Do not return stat test if varstrat has more than 2 levels
#'
#' @return a list of 2 elements :
#'   line_res : the data.frame with one line containing needed results
#'   test_result : the test object if more detail wanted about the stat test.
#'
#' @export
#' @import data.table
#' @import Skillings.Mack
#' @import rstatix
#' @importFrom data.table `:=`
#' @importFrom data.table `.N`
#' @importFrom data.table `.SD`
#' @examples
#' \dontrun{
#' ## Paired t-test
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra",
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   digits = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra_with_missings",
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   digits = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra_with_missings",
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   digits = 2, signif_digits = 2,
#'   global_summary = TRUE
#' )
#' ## Anova on paired data
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra",
#'   varstrat = "visites_4",
#'   patient_id = "ID",
#'   digits = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra",
#'   varstrat = "visites_5",
#'   patient_id = "ID",
#'   digits = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' ## test no normal measure : Paired Wilcoxon signed-rank test
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "mesure3",
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   digits = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' }

compute_paired_continuous_table_and_test <- function(
  dataframe,
  variable_interest,
  varstrat,
  digits = 2,
  signif_digits = 4,
  patient_id = "patientid",
  global_summary = TRUE,
  force_non_parametric_test = FALSE,
  metric_show = "auto",
  test_more_2_levels = FALSE
) {

  `.SD` <- `.` <- `.N` <- `:=` <- NULL
  `Q1` <- `Q3` <- cell_content <- IDENT_PAT <- value <- n_group <- NULL

  # `:=` <- data.table::`:=`

  message("[compute_paired_continuous_table_and_test] Compute tab for ", variable_interest)
  stopifnot(length(variable_interest) == 1)
  stopifnot(variable_interest %in% names(dataframe))
  stopifnot(is.numeric(dataframe[[variable_interest]]))
  stopifnot(varstrat %in% names(dataframe))
  stopifnot(is.factor(dataframe[[varstrat]]))
  stopifnot(patient_id %in% names(dataframe))
  stopifnot(digits >= 0)
  stopifnot(signif_digits >= 0)
  stopifnot(metric_show %in% c("mean", "median", "auto"))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(force_non_parametric_test))
  stopifnot(is.logical(test_more_2_levels))

  dt <- data.table::setDT(dataframe)

  N_init <- nrow(dt)
  dt <- dt[, .SD, .SDcols = c(variable_interest, varstrat, patient_id)]
  N_missingval <- sum(is.na(dt[[variable_interest]])) # in total
  sumup <- unique(
    data.table::copy(dt)[,
      `:=`(
        mean = mean(.SD[[1]], na.rm = TRUE),
        sd = stats::sd(.SD[[1]], na.rm = TRUE),
        median = stats::median(.SD[[1]], na.rm = TRUE),
        Q1 = stats::quantile(.SD[[1]], na.rm = TRUE)["25%"],
        Q3 = stats::quantile(.SD[[1]], na.rm = TRUE)["75%"],
        n_missing = sum(is.na(.SD[[1]])),
        n_group = .N
      ),
      by = varstrat,
      .SDcols = variable_interest
    ][
      , .SD,
      .SDcols = -c(patient_id, variable_interest)
    ]
  )[, n_group := n_group - n_missing]
  # sumup

  resN <- data.table::transpose(sumup[, .SD, .SDcols = c(varstrat, "n_group")], make.names = TRUE)
  names(resN) <- paste0(names(resN), "_N")

  if (all(resN %in% 0)) {
    # v0.1.19 : case when the numeric vars is all NA. dcast can't be applied
    N_individuals <- N_init - N_missingval
    test_result <- list()
    test_result$p.value <- NA
    test_used <- "/"
    ongoing_message <- "Variable present have no data (all NA), no desc, no test applicable"
    message("[compute_paired_continuous_table_and_test] ", ongoing_message)

    tab <- data.table::data.table(
      Variable = variable_interest,
      # Nb_mesures_init = N_init, # no more wanted
      Valeurs_manquantes = N_missingval,
      N_individuals = N_individuals, # NB mesures reelles
      resN,
      is_Normal = NA,
      P_valeur = NA,
      Test = test_used,
      message = ongoing_message
    )
    # exit 
    return(list(
      "line_res" = tab,
      "test_result" = test_result
    ))
  } 
  
  # else, ok continue...
  dt_wide <- data.table::dcast(
    data = stats::na.omit(
      dt[, .SD, .SDcols = c(patient_id, varstrat, variable_interest)]
    ),
    formula = stats::as.formula(paste0(
      "`", patient_id, "`", " ~ ", "`", varstrat, "`"
    )),
    value.var = variable_interest
  )
  # dt_wide

  if (ncol(dt_wide) == 2) {
    ## test case where ncol(dt_wide) == 2 => it means it only remaines 1 visit + ID col.
    ## only do the desc, nothing to test...

    ongoing_message <- "Variable present only in 1 visit/group, no paired test applicable"
    N_individuals <- N_init - N_missingval
    test_result <- list()
    test_result$p.value <- NA
    test_used <- "/"
    normal_data <- apply(
      X = dt_wide[, .SD, .SDcols = base::setdiff(names(dt_wide), patient_id)],
      MARGIN = 2, # cols
      FUN = function(xcol) {
        stats::shapiro.test(stats::na.omit(xcol))$`p.value` > 0.05
      }
    )
    if ((normal_data && metric_show %in% "auto") || metric_show %in% "mean") {
      sumup <- sumup[, cell_content := ifelse(
        is.na(mean) | is.nan(mean),
        "/",
        paste0(
          round(mean, digits), " +/- ", round(sd, digits)
          # , " (n=", n_group, ")"
        )
      )]
      if (global_summary) {
        Population_totale <- paste0(
          round(mean(dt[[variable_interest]], na.rm = TRUE), digits),
          " +/- ", round(stats::sd(dt[[variable_interest]], na.rm = TRUE), digits)# ,
          # " (n=", sum(!is.na(dt[[variable_interest]])), ")"
        )
      }
    }
    if ((!normal_data && metric_show %in% "auto") || metric_show %in% "median") {
      sumup <- sumup[, cell_content := ifelse(
        is.na(median) | is.nan(median),
        "/",
        paste0(
          round(median, digits), " [", round(Q1, digits), ";", round(Q3, digits), "]"
          # , " (n=", n_group, ")"
        )
      )]
      if (global_summary) {
        Population_totale <- paste0(
          round(median(dt[[variable_interest]], na.rm = TRUE), digits),
          " [", round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["25%"], digits), " ; ",
          round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["75%"], digits), "]"#,
          # " (n=", sum(!is.na(dt[[variable_interest]])), ")"
        )
      }
    }
  } else {
    ongoing_message <- ""

    if (force_non_parametric_test) {
      message(
        "[compute_paired_continuous_table_and_test]",
        "Because force_non_parametric_test is TRUE, metric_show is forced as 'median'."
      )
      metric_show <- "median"
    }

    if (nlevels(dt[[varstrat]]) == 2) {
      
      #### 2 levels ####

      message("[compute_paired_continuous_table_and_test] 2 levels in ", varstrat)
      dt_wide <- stats::na.omit(dt_wide)
      N_individuals <- nrow(dt_wide) # here n pairs

      vec1 <- unlist(dt_wide[, 2], use.names = FALSE)
      vec2 <- unlist(dt_wide[, 3], use.names = FALSE)
      diff <- vec2 - vec1 ## --here v0.1.18

      ## describ diff
      Difference_description <- paste0(
        "mean = ",
        round(mean(diff, na.rm = TRUE), digits),
        " +/- ", round(stats::sd(diff, na.rm = TRUE), digits),
        " ",
        "median = ",
        round(median(diff, na.rm = TRUE), digits),
        " [", round(stats::quantile(diff, na.rm = TRUE)["25%"], digits), " ; ",
        round(stats::quantile(diff, na.rm = TRUE)["75%"], digits), "]"
      )

      # normal_data <- shapiro.test(diff)$`p.value` > 0.05
      has_issues_norm <- try(tools::assertCondition(
        normal_data <- stats::shapiro.test(diff)$`p.value` > 0.05
      ), silent = TRUE)
      has_issues_norm <- has_issues_norm[
        sapply(has_issues_norm, function(el) {
          length(base::intersect(class(el), c("warning", "error"))) != 0
        })
      ]

      if (length(has_issues_norm) == 0 && normal_data && !force_non_parametric_test) { # If no error and normality ok

        #### Paired T test ####
        # Test t de Student pour données appariées (si les données sont normalement distribuées)
        test_result <- stats::t.test(vec2, vec1, paired = TRUE)
        test_used <- "Paired t-test"
        if (metric_show %in% c("auto", "mean")) {
          sumup <- sumup[, cell_content := paste0(
            round(mean, digits), " +/- ", round(sd, digits) # , " (n=", n_group, ")"
          )]
          Difference_description <- gsub("mean = (.*) median = (.*)", "\\1", Difference_description)
        }
        if (metric_show %in% c("median")) {
          message(
            "[compute_paired_continuous_table_and_test]",
            " /!\\ Caution median forced in cell content. It may cause a disconnection between metrics and tests /!\\"
          )
          sumup <- sumup[, cell_content := paste0(
            round(median, digits), " [", round(Q1, digits), ";", round(Q3, digits), "]" # , " (n=", n_group, ")"
          )]
          Difference_description <- gsub("mean = (.*) median = (.*)", "\\2", Difference_description)
        }

        if (global_summary) {
          if (metric_show %in% c("auto", "mean")) {
            Population_totale <- paste0(
              round(mean(dt[[variable_interest]], na.rm = TRUE), digits),
              " +/- ", round(stats::sd(dt[[variable_interest]], na.rm = TRUE), digits)# ,
              # " (n=", sum(!is.na(dt[[variable_interest]])), ")"
            )
          }
          if (metric_show %in% c("median")) {
            Population_totale <- paste0(
              round(median(dt[[variable_interest]], na.rm = TRUE), digits),
              " [", round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["25%"], digits), " ; ",
              round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["75%"], digits), "]",
              " (n=", length(unique(dt[[patient_id]])), ")"
            )
          }
        }
      } else {
        #### Wilcoxon signed ranks test ####

        # Test des rangs signés de Wilcoxon 
        # si les données ne sont pas normalement distribuées
        # ou si force_non_parametric_test
        # ou si shapiro test has error

        if (length(has_issues_norm) != 0) {
          msg_captured_norm <- paste(
            unique(sapply(
              X = has_issues_norm, # from shapiro
              FUN = function(el) {
                paste0("[", class(el)[2], "] ", el$message)
              }
            )),
            collapse = ";"
          )
          normal_data <- FALSE
        } else {
          msg_captured_norm <- ""
        }

        # test_result <- wilcox.test(vec1, vec2, paired = TRUE)
        has_issues_w <- try(tools::assertCondition(
          test_result <- stats::wilcox.test(vec2, vec1, paired = TRUE)
        ), silent = TRUE)
        has_issues_w <- has_issues_w[
          sapply(has_issues_w, function(el) {
            length(base::intersect(class(el), c("warning", "error"))) != 0
          })
        ]
        if (length(has_issues_w) == 0) {
          # wilcox.test no error # nothing to keep for wilcoxon msg
          msg_captured_wilcox <- ""
        } else {
          msg_captured_wilcox <- paste(
            unique(sapply(
              X = has_issues_w, # from wilcoxon
              FUN = function(el) {
                paste0("[", class(el)[2], "] ", el$message)
              }
            )),
            collapse = ";"
          )
        }
        if (force_non_parametric_test && normal_data) {
          ongoing_message <- "force_non_parametric_test.\n"
        } else {
          ongoing_message <- ""
        }
        ongoing_message <- paste0(ongoing_message, msg_captured_wilcox, msg_captured_norm)

        test_used <- "Wilcoxon signed-rank test (paired data)"

        if (force_non_parametric_test || metric_show %in% c("auto", "median")) {
          sumup <- sumup[, cell_content := paste0(
            round(median, digits), " [", round(Q1, digits), ";", round(Q3, digits), "]" # , " (n=", n_group, ")"
          )]
          Difference_description <- gsub("mean = (.*) median = (.*)", "\\2", Difference_description)
        } else {
          # metric_show %in% c("mean")
          message(
            "[compute_paired_continuous_table_and_test]",
            " /!\\ Caution mean forced in cell content. It may cause a disconnection between metrics and tests /!\\"
          )
          ongoing_message <- paste0(ongoing_message, "!Caution mean forced in cell content. It may cause a disconnection between metrics and tests!")
          sumup <- sumup[, cell_content := paste0(
            round(mean, digits), " +/- ", round(sd, digits) # , " (n=", n_group, ")"
          )]
          Difference_description <- gsub("mean = (.*) median = (.*)", "\\1", Difference_description)
        }

        if (global_summary) {
          if (force_non_parametric_test || metric_show %in% c("auto", "median")) {
            Population_totale <- paste0(
              round(median(dt[[variable_interest]], na.rm = TRUE), digits),
              " [", round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["25%"], digits), " ; ",
              round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["75%"], digits), "]"#,
              # " (n=", sum(!is.na(dt[[variable_interest]])), ")"
            )
          }
          if (metric_show %in% c("mean")) {
            Population_totale <- paste0(
              round(mean(dt[[variable_interest]], na.rm = TRUE), digits),
              " +/- ", round(stats::sd(dt[[variable_interest]], na.rm = TRUE), digits),
              " (n=", length(unique(dt[[patient_id]])), ")"
            )
          }
        }
      }
    } else {
      #### More than 2 levels ####

      ## do not apply stat test any more # version 0.1.14
      ## use Anova on paired data if measure are normal for each time ?
      ## else use Quade test or Skilling-Mack

      has_issues_norm <- try(tools::assertCondition(
        normal_data <- apply(
          X = dt_wide[, .SD, .SDcols = base::setdiff(names(dt_wide), patient_id)],
          MARGIN = 2, # cols
          FUN = function(xcol) {
            stats::shapiro.test(stats::na.omit(xcol))$`p.value` > 0.05
          }
        )
      ), silent = TRUE)
      has_issues_norm <- has_issues_norm[
        sapply(has_issues_norm, function(el) {
          length(base::intersect(class(el), c("warning", "error"))) != 0
        })
      ]
      if (length(has_issues_norm) == 0) {
        # no error when test normality
        msg_captured_norm <- ""
      } else {
        normal_data <- FALSE
        msg_captured_norm <- paste(
          unique(sapply(
            X = has_issues_norm, # from shapiro
            FUN = function(el) {
              paste0("[", class(el)[2], "] ", el$message)
            }
          )),
          collapse = ";"
        )
      }
      
      dt_long <- data.table::melt(data = dt_wide, id.vars = patient_id)
      dt_long[[patient_id]] <- as.factor(dt_long[[patient_id]])
      dt_long$variable <- as.factor(dt_long$variable) # visite times

      any_na_in_dt <- apply(
        X = dt_wide[, .SD, .SDcols = base::setdiff(names(dt_wide), patient_id)],
        MARGIN = 2, # cols
        FUN = function(xcol) {
          any(is.na(xcol))
        }
      )
      not_complete_design <- any(any_na_in_dt)

      if (all(normal_data) && !not_complete_design && !force_non_parametric_test) {
        normal_data <- TRUE
        N_individuals <- length(unique(dt_long[[patient_id]]))

        if (test_more_2_levels) {
          #### Anova on paired data ####
          test_result <- rstatix::anova_test(
            data = dt_long, dv = "value", wid = dplyr::all_of(patient_id), within = "variable"
          )
          # les degrés de liberté du numérateur (DFn) et du dénominateur (DFd), respectivement ;
          # F Indique que nous comparons à une distribution F (test F) => la valeur statistique F obtenue
          # p spécifie la p-value
          # ges est la taille de l’effet généralisé (taille de la variabilité due au facteur intra-sujets)
          test_result$p.value <- test_result$p
          test_used <- "Repeated measures ANOVA: within-Subjects designs"
          ongoing_message <- "WARNING : dev of Repeated measures ANOVA not validated yet !!"
        } else {
          test_result <- list()
          test_result$p.value <- NA
          test_used <- "/"
          ongoing_message <- "Repeated measures ANOVA can be explored with option test_more_2_levels."
        }

        if (metric_show %in% c("auto", "mean")) {
          sumup <- sumup[, cell_content := paste0(
            round(mean, digits), " +/- ", round(sd, digits) # , " (n=", n_group, ")"
          )]
        }
        if (metric_show %in% c("median")) {
          message(
            "[compute_paired_continuous_table_and_test]",
            " /!\\ Caution median forced in cell content. It may cause a disconnection between metrics and tests /!\\"
          )
          ongoing_message <- paste0(ongoing_message, "!Caution median forced in cell content. It may cause a disconnection between metrics and tests!")
          
          sumup <- sumup[, cell_content := paste0(
            round(median, digits), " [", round(Q1, digits), ";", round(Q3, digits), "]"
          )]
        }

        if (global_summary) {
          message(
            "[compute_paired_continuous_table_and_test] ",
            "Caution! global_summary on longitudinal data is not really relevant... but ok if you want it"
          )

          if (metric_show %in% c("auto", "mean")) {
            Population_totale <- paste0(
              round(mean(dt[[variable_interest]], na.rm = TRUE), digits),
              " +/- ", round(stats::sd(dt[[variable_interest]], na.rm = TRUE), digits)
            )
          }
          if (metric_show %in% c("median")) {
            Population_totale <- paste0(
              round(stats::median(dt[[variable_interest]], na.rm = TRUE), digits),
              " [", round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["25%"], digits), " ; ",
              round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["75%"], digits), "]"
            )
          }
        }

        ## Valeurs aberrantes
        # identify_outliers()
        res_outliers <- rstatix::identify_outliers(dt_long, variable = "value")
        if (nrow(res_outliers) > 0) {
          msg <- paste0(
            "Detection of ", nrow(res_outliers), " points outliers on ", variable_interest, ".\n",
            "Check individuals with id ", paste0(unique(res_outliers[[patient_id]]), collapse = ",")
          )
          message("[compute_paired_continuous_table_and_test] ", msg)
          ongoing_message <- paste0(ongoing_message, msg)
        }

        ## Hypothèse de sphéricité (automatiquement vérifiée dans rstatix::anova_test()
        ## Le test de Mauchly est utilisé en interne pour évaluer l’hypothèse de sphéricité.
        ## --here to dev : capture message from rstatix::anova_test to get test de Mauchly (message or error?)
      } else {
        # when :
        # not all(normal_data) or not_complete_design or force_non_parametric_test

        normal_data <- all(normal_data)

        # Quade test if complete design or Skilling-Mack otherwise

        if (!not_complete_design) {
          N_individuals <- length(unique(dt_long[[patient_id]]))

          if (test_more_2_levels) {
            #### Quade ####
            # Quade test if we do have a complet design
            quade_form <- stats::as.formula(paste0(
              "value ~ variable | ", "`", patient_id, "`"
            ))
            test_result <- stats::quade.test(quade_form, data = dt_long)
            test_used <- "Quade Test"
            ongoing_message <- "WARNING : dev of Quade test not validated yet !!"
          } else {
            test_result <- list()
            test_result$p.value <- NA
            test_used <- "/"
            ongoing_message <- "Quade Test can be explored with option test_more_2_levels."
          }
          
        } else {
          ongoing_message <- paste0(paste0(
            c("force_non_parametric_test", "Normal data", "not complete design")[
              c(force_non_parametric_test, normal_data, not_complete_design)
            ],
            collapse = " with "
          ), ".")

          if (test_more_2_levels) {
            #### Skilling-Mack ####
            # Skilling-Mack test
            # Le test de Skilling-Mack s'utilise à la place de Quade en cas de blocs incomplets
            # attention pour Skilling-Mack : doit voir apparaitre tous les groupes pour chaque patient,
            # même ceux qui ont des valeurs manquantes
            # ne pas supprimer les lignes, mais mettre un NA
            # ici "dt_long" a bien les info NA

            ## to avoid Erreur dans Ski.Mack(...) : "Block#n has only one observation. Please remove this block"
            count_block <- dt_long[, .(Nobs = sum(is.na(value))), by = eval(patient_id)]
            ## in Ski.Mack, remove patient with only one obs <=> with nlevels-1 missing data
            remove_patient_id <- count_block[[patient_id]][count_block$Nobs == nlevels(dt_long[["variable"]]) - 1]
            if (length(remove_patient_id) > 0) {
              ongoing_message <- paste0(
                ongoing_message,
                "\nRemove ", length(remove_patient_id), " patients with only 1 observation.\n"
              )
            }
            dt_long_ski <- dt_long[!dt_long[[patient_id]] %in% remove_patient_id, ]
            dt_long_ski <- dt_long_ski[
              ,
              IDENT_PAT := factor(
                dt_long_ski[[patient_id]],
                levels = sort(unique(dt_long_ski[[patient_id]])),
                labels = sort(unique(dt_long_ski[[patient_id]]))
              )
            ]

            # capture.output used to capture element and be able to return them in report later
            Ski_result <- utils::capture.output(Skillings.Mack::Ski.Mack(
              y = dt_long_ski$value,
              groups = dt_long_ski$variable, # times = visites
              blocks = dt_long_ski$IDENT_PAT,
              simulate.p.value = TRUE, B = 10000
            ))
            # Ski_result[[2]]
            # Ski_result[[3]]
            # Ski_result[[4]]

            test_result <- list(
              full_capture = Ski_result,
              p.value = as.numeric(gsub(" ", "", unlist(strsplit(Ski_result[[2]], split = "p-value ="))[[2]])),
              nblocks_patients = length(unique(dt_long_ski$IDENT_PAT)),
              ngroups_times = length(unique(dt_long_ski$variable)), # = visites
              df = Ski_result[[3]],
              simulation = Ski_result[[4]]
            )
            test_used <- "Skillings-Mack test"
            N_individuals <- length(unique(dt_long_ski$IDENT_PAT))
            ongoing_message <- paste0(ongoing_message, "WARNING : dev of Skillings.Mack not validated yet !!")
          } else {
            N_individuals <- length(unique(dt_long[[patient_id]]))

            test_result <- list()
            test_result$p.value <- NA
            test_used <- "/"
            ongoing_message <- paste0(
              ongoing_message,
              "Skillings-Mack test can be explored with option test_more_2_levels."
            )
          }

        }
        
        if (metric_show %in% c("auto", "median")) {
          sumup <- sumup[, cell_content := paste0(
            round(median, digits), " [", round(Q1, digits), ";", round(Q3, digits), "]" # , " (n=", n_group, ")"
          )]
        }
        if (metric_show %in% c("mean")) {
          message(
            "[compute_paired_continuous_table_and_test]",
            " /!\\ Caution mean forced in cell content. It may cause a disconnection between metrics and tests /!\\"
          )
          ongoing_message <- paste0(ongoing_message, "!Caution mean forced in cell content. It may cause a disconnection between metrics and tests!")
          sumup <- sumup[, cell_content := paste0(
            round(mean, digits), " +/- ", round(sd, digits) # , " (n=", n_group, ")"
          )]
        }
        
        if (global_summary) {
          message(
            "[compute_paired_continuous_table_and_test] ",
            "Caution! global_summary on longitudinal data is not really relevant... but ok if you want it"
          )
          
          if (metric_show %in% c("auto", "median")) {
            Population_totale <- paste0(
              round(median(dt[[variable_interest]], na.rm = TRUE), digits),
              " [", round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["25%"], digits), " ; ",
              round(stats::quantile(dt[[variable_interest]], na.rm = TRUE)["75%"], digits), "]"# ,
              # " (n=", sum(!is.na(dt[[variable_interest]])), ")"
            )
          }
          if (metric_show %in% c("mean")) {
            Population_totale <- paste0(
              round(mean(dt[[variable_interest]], na.rm = TRUE), digits),
              " +/- ", round(stats::sd(dt[[variable_interest]], na.rm = TRUE), digits)# ,
              # " (n=", sum(!is.na(dt[[variable_interest]])), ")"
            )
          }
        }
        
      }
      ongoing_message <- paste0(msg_captured_norm, ongoing_message)
    }
  }
  message("[compute_paired_continuous_table_and_test] ", test_used)

  ## format tab results
  res <- data.frame(
    Variable = variable_interest,
    # Nb_mesures_init = N_init, # no more wanted
    Valeurs_manquantes = N_missingval,
    N_individuals = N_individuals # NB mesures reelles
  )
  if (global_summary) {
    res <- cbind(
      res, Population_totale
    )
  }

  tab <- cbind(
    res,
    data.table::transpose(sumup[, .SD, .SDcols = c(varstrat, "cell_content")], make.names = varstrat),
    resN # intercaller les N for each visite
  )
  # reorder
  cols_visits <- unlist(lapply(X = sumup[[varstrat]], FUN = function(x) paste0(x, c("", "_N"))))
  data.table::setcolorder(x = tab, neworder = c(
    setdiff(names(tab), cols_visits), cols_visits
  ))

  if (nlevels(dt[[varstrat]]) == 2 && ncol(dt_wide) != 2) {
    tab <- cbind(
      tab, Difference_description
    )
  }

  P_valeur <- signif(test_result$p.value, digits = signif_digits)
  # ifelse(
  #   test_result$p.value < 0.0001, "<0.0001", signif(test_result$p.value, digits = signif_digits)
  # )
  if (is.nan(P_valeur)) {
    P_valeur <- NA
    test_used <- "/"
    ongoing_message <- paste0("Test non applicable.", ongoing_message)
  }
  
  tab <- cbind(
    tab,
    data.table::data.table(
      is_Normal = normal_data,
      P_valeur = P_valeur,
      Test = test_used,
      message = ongoing_message
    )
  )

  return(list(
    "line_res" = tab,
    "test_result" = test_result
  ))
}

#' compute factorial table for paired data
#'
#' provide descriptive statistics table for factorial data with a paired level (time or visites)
#'
#' The McNemar test is a statistical test used to compare qualitative data sets.
#' That is to say, for comparing frequencies or percentages on matched data.
#' This test is particularly suitable for situations where you have matched measurements (the same patients)
#'  before and after an intervention => only applicable on 2x2 contingency table.
#'   example of Assumptions:
#'   H0: The proportion of paracetamol given intravenously is the same before and after the campaign. Vs
#'   H1: The proportion of paracetamol given intravenously is different after campaign compared to before.
#'
#' for more than 2x2 tc : Test d'homogénéité marginale --todev
#' Ce test s'utilise lorsque la variable à étudier a plus de 2 modalités et que les groupes sont appariés
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#' @param variable_interest A character. Name of dataframe's factorial columns to describe and test.
#' @param varstrat A character. Name of the stratification variable, making groups to compare (time or visites).
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two lines should be displayed for binary variables.
#'   TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), FALSE = both levels of the variables.
#' @param patient_id A character., Default "patientid". Name of identifant patient id column.
#' @param force_generate_1_when_0 A logical, Default TRUE. If TRUE, will test if the unique modality is 0 or "non" and
#'    add the level 1 or "oui" so it can be display in counts. Can be combined with simplify to only show the modality (1).
#' @param keep_missing_line A logical, Default TRUE. Do you want to keep the missing data count (like a level)
#' @param global_summary A logical. Default FALSE Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... but ok if you want it, you can.
#' @param test_more_2_levels A logicial, Default FALSE. Do not return stat test if varstrat has more than 2 levels
#' @return a list of 2 elements :
#'   line_res : the data.frame with one line containing needed results
#'   test_result : the test object if more detail wanted about the stat test.
#'
#' @export
#' @import data.table
#' @import coin
#' @importFrom data.table `:=`
#' @importFrom data.table `.N`
#' @importFrom data.table `.SD`
#' @examples
#' \dontrun{
#' ## Mc Nemar test
#' compute_paired_factorial_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "fact1",
#'   varstrat = "visites_2",
#'   digits = 1,
#'   patient_id = "ID2",
#'   force_generate_1_when_0 = FALSE,
#'   keep_missing_line = TRUE
#' )
#' compute_paired_factorial_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "fact1_na",
#'   varstrat = "visites_2",
#'   digits = 1,
#'   patient_id = "ID2",
#'   force_generate_1_when_0 = FALSE,
#'   keep_missing_line = TRUE
#' )
#' compute_paired_factorial_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "fact3",
#'   varstrat = "visites_2",
#'   digits = 1,
#'   patient_id = "ID2",
#'   force_generate_1_when_0 = FALSE,
#'   keep_missing_line = TRUE
#' )
#' # dataframe = modified_sleep;
#' # variable_interest = "fact3";
#' # varstrat = "visites_2";
#' # digits = 1;
#' # patient_id = "ID2";
#' # force_generate_1_when_0 = FALSE;
#' # keep_missing_line = TRUE;
#' }
compute_paired_factorial_table_and_test <- function(
  dataframe,
  variable_interest,
  varstrat,
  digits = 2,
  signif_digits = 4,
  simplify = FALSE,
  patient_id = "patientid",
  force_generate_1_when_0 = TRUE,
  keep_missing_line = TRUE,
  global_summary = FALSE,
  test_more_2_levels = FALSE
) {
  `.SD` <- `.` <- `.N` <- `:=` <- NULL
  cell_content <- IDENT_PAT <- value <- n_group_mod <- Nb_mesures <- Modalites <- NULL

  message("[compute_paired_factorial_table_and_test] Compute tab for ", variable_interest)
  stopifnot(length(variable_interest) == 1)

  dt <- data.table::setDT(dataframe)

  stopifnot(variable_interest %in% names(dt))
  stopifnot(varstrat %in% names(dt))
  stopifnot(is.factor(dt[[variable_interest]]))
  stopifnot(is.factor(dt[[varstrat]]))
  stopifnot(!all(is.na(dt[[varstrat]]))) # no missing data in varstart
  stopifnot(patient_id %in% names(dt))
  stopifnot(digits >= 0)
  stopifnot(signif_digits >= 0)
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(is.logical(keep_missing_line))

  N_init <- nrow(dt)
  dt <- dt[, .SD, .SDcols = c(variable_interest, varstrat, patient_id)]
  N_missingval <- sum(is.na(dt)) # in total
  # dt <- na.omit(dt) # do not apply, so missing values are count

  varstrat_levels <- levels(dt[[varstrat]])
  # varstrat_nlevels <- nlevels(dt[[varstrat]])
  var_nlevels <- nlevels(dt[[variable_interest]])
  var_levels <- levels(dt[[variable_interest]])

  if (force_generate_1_when_0 && var_nlevels == 1 && (var_levels == 0 || tolower(var_levels) == "non")) {
    message("[compute_paired_factorial_table_and_test] force_generate_1_when_0 for ", variable_interest)
    new_level <- ifelse(
      var_levels %in% 0,
      1,
      "Oui"
    )
    var_levels <- c(var_levels, new_level)
    dt[[variable_interest]] <- factor(dt[[variable_interest]], levels = var_levels, labels = var_levels)
    var_nlevels <- nlevels(dt[[variable_interest]])
  }

  ## count, N and p ##
  sumup <- data.table::as.data.table(table(dt[[varstrat]], dt[[variable_interest]], useNA = "always"))
  names(sumup) <- c(varstrat, variable_interest, "n_group_mod")
  sumup <- sumup[!is.na(sumup[[varstrat]]), ]

  sumup <- sumup[, `:=`(
    Nb_mesures = sum(n_group_mod)
  ),
  by = variable_interest
  ][, `:=`(
    N_by_visit = sum(n_group_mod) # ,
    # p =  paste( ## this p use NA lines
    #   "(", round(100 * n_group_mod / sum(n_group_mod), digits), "%)",
    #   sep = ""
    # )
  ), by = varstrat]

  ## fix N_by_visit for levels, without N missing values
  sumup$is_level_line <- !is.na(sumup[[variable_interest]])
  sumup <- sumup[, `:=`(
    N_by_visit_level = sum(n_group_mod)
  ),
  by = c(varstrat, "is_level_line")
  ]
  sumup$N_by_visit <- ifelse(
    !is.na(sumup[[variable_interest]]),
    sumup$N_by_visit_level,
    sumup$N_by_visit
  )
  sumup$N_by_visit_level <- NULL
  sumup$is_level_line <- NULL

  # compute P without NA lines ie. only on levels

  ## --error
  # sumup <- sumup[
  #   sumup[[variable_interest]] %in% var_levels,
  #   `:=`(
  #     p = ifelse(
  #       sum(n_group_mod) == 0,
  #       "",
  #       paste(
  #         "(", round(100 * n_group_mod / sum(n_group_mod), digits), "%)",
  #         sep = ""
  #       )
  #     )
  #   ),
  #   by = varstrat
  # ]
  ## --fixed
  sumup <- sumup[
    sumup[[variable_interest]] %in% var_levels,
    `:=`(
      p = ifelse(
        sum(n_group_mod) == 0,
        "",
        paste(
          "(", round(100 * n_group_mod / N_by_visit, digits), "%)",
          sep = ""
        )
      )
    ),
    by = c(varstrat, variable_interest)
  ]

  sumup$cell_content <- ifelse(
    test = is.na(sumup[[variable_interest]]),
    sumup[["n_group_mod"]],
    paste0(sumup[["n_group_mod"]], " ", sumup[["p"]])
  )

  res <- data.table::dcast(
    data = sumup,
    stats::as.formula(paste0(
      "`", variable_interest, "`", " + Nb_mesures ~ ", "`", varstrat, "`"
    )),
    value.var = "cell_content"
  )

  resN <- data.table::dcast(
    data = sumup,
    stats::as.formula(paste0(
      "`", variable_interest, "`", " ~ ", "`", varstrat, "`"
    )),
    value.var = "N_by_visit"
  )
  names(resN)[-1] <- paste0(names(resN)[-1], "_N")

  # merge resN et res
  effectif_tab <- merge(res, resN, by = variable_interest, sort = FALSE)
  names(effectif_tab)[1] <- "Modalites"

  cols_visits <- unlist(lapply(varstrat_levels, FUN = function(x) paste0(x, c("", "_N"))))
  data.table::setcolorder(x = effectif_tab, neworder = c(
    "Modalites", "Nb_mesures", cols_visits
  ))

  effectif_tab[is.na(Modalites), "Modalites"] <- "Missing data"
  if (!keep_missing_line) effectif_tab <- effectif_tab[!Modalites %in% "Missing data"]
  # effectif_tab

  if (global_summary) {
    sumup_global <- data.table::as.data.table(table(dt[[variable_interest]], useNA = "always"))
    names(sumup_global) <- c(variable_interest, "n_group_mod")
    sumup_global <- sumup_global[!is.na(sumup_global[[variable_interest]]), ]

    sumup_global <- sumup_global[, `:=`(
      Population_total = paste0(
        n_group_mod,
        " (", round(100 * n_group_mod / sum(sumup_global[["n_group_mod"]], na.rm = TRUE), digits), "%)"
      )
    )][
      ,
      n_group_mod := NULL
    ]
    names(sumup_global)[1] <- "Modalites"

    effectif_tab <- merge(x = effectif_tab, y = sumup_global, by = "Modalites", all = TRUE, sort = FALSE)
    # order
    data.table::setcolorder(x = effectif_tab, neworder = c(
      unique(c("Modalites", "Nb_mesures", "Population_total", names(effectif_tab)))
    ))
  }

  #### Stat test part ####
  message("[compute_paired_factorial_table_and_test] Go for stat test")

  # table of observed counts
  # mcnemar_dt <- dt %>% pivot_wider(id_cols = patient_id, names_from = varstrat, values_from = variable_interest)
  # data.table style
  mcnemar_dt <- data.table::dcast(
    data = stats::na.omit(
      dt[, .SD, .SDcols = c(patient_id, varstrat, variable_interest)]
    ),
    formula = stats::as.formula(paste0(
      "`",patient_id, "`", " ~ ", "`", varstrat, "`"
    )),
    value.var = variable_interest
  )

  dt_applicable <- na.omit(dt)
  if (length(unique(dt_applicable[[varstrat]])) == 1) {
    ongoing_message <- "Variable present only in 1 visit/group, no paired test applicable"
    message("[compute_paired_factorial_table_and_test] ", ongoing_message)
    test_used <- "/"
    test_result <- list(
      "p.value" = NA,
      method = test_used,
      message = ongoing_message
    )
  } else {
    # effobs <- table(dt[, .SD, .SDcols = c(variable_interest, varstrat)])
    effobs_mcnemar <- table(
      data.frame(
        mcnemar_dt[, .SD, .SDcols = varstrat_levels[1]],
        mcnemar_dt[, .SD, .SDcols = varstrat_levels[2]]
      )
    )
    # efftheo <- as.table(round(rowSums(effobs) %*% t(colSums(effobs)) / sum(effobs), 2))
    # names(dimnames(efftheo)) <- names(dimnames(effobs))
    # rownames(efftheo) <- rownames(effobs)
    # colnames(efftheo) <- colnames(effobs)

    if (all(dim(effobs_mcnemar) %in% 2) && ncol(mcnemar_dt) == 3) {
      # only 2 labels for var i and 2 levels for varstrat = ok : McNemar test

      #### McNemar test ####
      test_result <- stats::mcnemar.test(effobs_mcnemar, correct = TRUE)
      if (effobs_mcnemar[2, 1] + effobs_mcnemar[1, 2] < 10) {
        ongoing_message <- "Less than 10 discordant pairs"
      } else {
        ongoing_message <- ""
      }
      test_used <- "McNemar's Chi-squared test"
      message("[compute_paired_factorial_table_and_test] ", test_used)
    } else {
      #### Marginal Homogeneity Test ####

      # Q de cochran ? if more than 2 modalités
      # or
      # # Marginal Homogeneity Tests
      # # La table est en long, par exemple une variable visite (x),
      # # une variable catégorielle (y) et une variable patient_id (block)
      # dt_mh <- stats::na.omit(dt[, .SD, .SDcols = c(variable_interest, varstrat, patient_id)])
      # format as factor the 3 variables ? (inclusing patient id) -here to check

      dt_mh <- base::droplevels(dt)
      check_mh_dt <- as.data.frame(table(dt_mh[[varstrat]]))
      check_mh <- all(check_mh_dt$Freq == check_mh_dt$Freq[1]) ## complete block design

      if (check_mh) {
        ## need a balanced design = same visite available for all sample

        if (nrow(check_mh_dt) >= 2 && test_more_2_levels) {
          test_used <- "Marginal Homogeneity Test"
          ongoing_message <- "WARNING : dev of Marginal Homogeneity Test not validated yet !!"
          raw_mh_result <- coin::mh_test(
            formula = stats::as.formula(paste0(
              "`", variable_interest, "`", " ~ ", "`", varstrat, "`", " | ", "`", patient_id, "`"
            )),
            data = dt_mh
          )
          test_result <- list(
            raw_result = raw_mh_result,
            "p.value" = coin::pvalue(raw_mh_result),
            message = ongoing_message
          )
          message("[compute_paired_factorial_table_and_test] ", test_used)
        } else {
          ongoing_message <- "Marginal Homogeneity Test can be explored with option test_more_2_levels."
          test_used <- "/"
          test_result <- list(
            "p.value" = NA,
            method = test_used,
            message = ongoing_message
          )
        }
      } else {
        ongoing_message <- "Not balanced design, Marginal Homogeneity Test not applicable"
        ##  ‘x’ is not a balanced factor
        test_used <- "/"
        test_result <- list(
          "p.value" = NA,
          method = test_used,
          message = ongoing_message
        )
      }
    }
  }

  #### format tab results ####
  res <- data.frame(
    Variable = variable_interest,
    # Nb_mesures_init = N_init, # no more wanted
    Valeurs_manquantes = N_missingval
  )

  tab_test <- data.table::data.table(
    P_valeur = signif(test_result$p.value, digits = signif_digits),
    # ifelse(
    #   test_result$p.value < 0.0001, "<0.0001", signif(test_result$p.value, digits = signif_digits)
    # ),
    Test = test_used,
    message = ongoing_message
  )


  if (simplify) {
    # on effectif_tab
    if (!is.null(effectif_tab) && nrow(effectif_tab) == 2) {
      if (
        all(c("0", "1") %in% effectif_tab$Modalites) |
          all(c("oui", "non") %in% tolower(effectif_tab$Modalites))
      ) {
        if (any(grepl("1", effectif_tab$Modalites))) {
          effectif_tab <- effectif_tab[effectif_tab$Modalites %in% 1, ]
        } else {
          # ignore.case about oui
          row_select <- grep("OUI", effectif_tab$Modalites, ignore.case = TRUE)
          effectif_tab <- effectif_tab[row_select, ]
        }
      }
    }
  }

  res <- data.table::rbindlist(l = list(
    res,
    as.data.frame(matrix(data = NA, nrow = nrow(effectif_tab) - 1, ncol = ncol(res)))
  ), use.names = FALSE)
  tab_test <- data.table::rbindlist(l = list(
    tab_test,
    as.data.frame(matrix(data = NA, nrow = nrow(effectif_tab) - 1, ncol = ncol(tab_test)))
  ), use.names = FALSE)

  effectif_tab$Nb_mesures <- NULL # no more wanted
  tab <- cbind(
    res,
    effectif_tab,
    tab_test
  )
  # order
  data.table::setcolorder(x = tab, neworder = c(
    unique(c("Variable","Modalites", names(tab))) # first Var and modalities
  ))
  
  return(list(
    "line_res" = tab,
    "test_result" = test_result
  ))
}
