# Statistical tests

#### Check condition with tests ####

#' check Cochran rule
#'
#' Check theoretical sample size in table
#'
#' @param efftheo A table of theoretical counts
#' @param seuil A numeric between 0 and 100. Default 80\%.
#'   Percentage of cell supposed get a count greater than effcochran (Eg. 80\% should have count above 5.)
#' @param effcritique A integer. Default 1. Integer of the minimal count for a given cell
#' @param effcochran A integer. Default 5. Integer if the minimal count for maximum seuil in \% of cells
#' @param verbose A logical, Default TRUE. Print the conclusion message.
#'
#' @return A logical. TRUE means the Cochran rule is respected, otherwise FALSE.
#' @export
#' @examples
#' \dontrun{
#' effobs <- table(modified_state[, c("state.region", "election")])
#' # tableau des effectifs theoriques
#' efftheo <- as.table(round(rowSums(effobs) %*% t(colSums(effobs)) / sum(effobs), 2))
#' names(dimnames(efftheo)) <- names(dimnames(effobs))
#' rownames(efftheo) <- rownames(effobs)
#' colnames(efftheo) <- colnames(effobs)
#' check_cochran(
#'   efftheo = efftheo
#' )
#' }
check_cochran <- function(
  efftheo,
  seuil = 80,
  effcritique = 1,
  effcochran = 5,
  verbose = TRUE
) {
  # stops
  stopifnot(is.table(efftheo))
  stopifnot(effcritique >= 1) # why not freeze effcritique <- 1 ? --question
  stopifnot(effcochran >= 5) # why not freeze effcochran <- 5 ? --question

  # pourcentage of cells with counts > 5 (effcochran) and < 1 (effcritique)
  pct_sup5 <- round(sum(efftheo > effcochran) / (length(efftheo)) * 100, digits = 2)
  pct_inf1 <- round(sum(efftheo < effcritique) / (length(efftheo)) * 100, digits = 2)

  # If all cell have counts > 5 (effcochran)
  if (pct_sup5 == 100) {
    cochran_result <- TRUE
    ind_effbas <- NA
    msg <- paste(
      "The Cochran's rule **is** respected.\n",
      "All theoretical counts are strictly greater than ", effcochran, ".\n\n",
      sep = ""
    )
  }

  # If any cell have counts < 1 (effcritique)
  if (pct_inf1 != 0) {
    cochran_result <- FALSE
    ind_effbas <- which(efftheo < effcritique, arr.ind = TRUE)
    msg <- paste(
      "The Cochran's rule is **not** respected.\n",
      pct_sup5, "%", " theoretical counts are greater than ", effcochran, ".\n",
      "Cells corresponding to following modalities have theoretical counts strictly lower than ",
      effcritique, " :\n\n",
      sep = ""
    )
  }

  # If any cell have not counts < 1 (effcritique) but
  # less than seuil% (80%) respect count > 5 (effcochran)
  # ie. more than 1-seuil (20%) of cells have count <= 5 (effcochran)
  if (pct_inf1 == 0 && pct_sup5 < seuil) {
    cochran_result <- FALSE
    ind_effbas <- which(efftheo >= 1 & efftheo <= 5, arr.ind = TRUE)
    msg <- paste(
      "The Cochran's rule is **not** respected.\n",
      pct_sup5, "%", " theoretical counts are greater than ", effcochran, ".\n",
      "Cells corresponding to following modalities have theoretical counts between ",
      effcritique, "and", effcochran, " :\n\n",
      sep = ""
    )
  }

  # If any cell have not counts < 1 (effcritique) and
  # more than seuil% (80%) respect count > 5 (effcochran)
  if (pct_inf1 == 0 && pct_sup5 >= seuil && pct_sup5 < 100) {
    cochran_result <- TRUE
    ind_effbas <- which(efftheo >= 1 & efftheo <= 5, arr.ind = TRUE)
    msg <- paste(
      "The Cochran's rule **is** respected.\n",
      pct_sup5, "%", " theoretical counts are greater than ", effcochran, ".\n",
      "Cells corresponding to following modalities have theoretical counts between ",
      effcritique, "and", effcochran, " :\n\n",
      sep = ""
    )
  }

  # show modalities with problems. priority <1 then <5
  if (verbose) cat(msg)
  if (!is.na(ind_effbas[[1]]) && verbose) {
    colnames(ind_effbas) <- c(names(dimnames(efftheo))[1], names(dimnames(efftheo))[2])
    rownames(ind_effbas) <- NULL
    ind_effbas[, names(dimnames(efftheo))[1]] <- rownames(efftheo)[
      as.integer(ind_effbas[, names(dimnames(efftheo))[1]])
    ]
    ind_effbas[, names(dimnames(efftheo))[2]] <- colnames(efftheo)[
      as.integer(ind_effbas[, names(dimnames(efftheo))[2]])
    ]
    print(ind_effbas)
  }

  # return TRUE if cochran's rule is satisfied, else FALSE
  return(cochran_result)
}

## --here to do
# check_shapiro <- function()


#' check Fisher good application
#'
#' In the situation where one variable (Var1) has missing data for one of the levels of another variable (Var2),
#' this leads to zeros in some cell of the contingency table.
#'
#' Structural zeros: If these zeros are due to a structural impossibility
#' (that is, they are expected by the nature of the data, for example,
#' if a Var1 category does not exist for a Var2 category),
#' the exact Fisher test may not be appropriate because it assumes that all combinations are possible.
#'
#' Zeros observed: If these zeros are not structural but simply observed
#' (that is, they result from sampling and not from a theoretical constraint),
#' Fisher’s exact test can be used.
#' However, the results should be interpreted with caution.
#' The test is still mathematically correct, but zeros may indicate a problem with the data
#' (for example, a lack of information or sampling bias).
#'
#' In the 2 cases (structural zeros or observed zeros) we will prefer not to display the fisher statistic here and
#' indicates that the correct application of the fisher test is not "valid".
#'
#' @param tc A table of contingency count.
#'
#' @return A logical. TRUE means the fisher "rule" is respected, otherwise FALSE.
#' @export
check_fisher <- function(tc) {
  return(!(all(tc[, 1] %in% 0) | all(tc[, 2] %in% 0)))
}

#### Apply statistic tests ####

#' test proportions
#'
#' Test contingency tables.
#' Deploy the tests Khi 2 and fisher for proportions with a variable of reference
#'
#' @param dataframe A data.frame containing data.
#' @param vars A vector of characters. Names of dataframe's factorial columns to describe.
#' @param varstrat A character. Name of the stratification variable, making groups to compare.
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#'
#' @return A list of 3 objects
#'   results = data.frame with test, p-values, explicit decision for each vars
#'   details = list with details, various objects.
#'   selection = vector of variables with p-values < 0.2
#' @export
#' @import stats
#' @examples
#' \dontrun{
#' test_proportions(
#'   dataframe = modified_state[, c("state.division", "state.region", "election")],
#'   vars = c("state.region", "state.division"),
#'   varstrat = "election"
#' )
#' }
test_proportions <- function(
  dataframe,
  vars = setdiff(colnames(dataframe), varstrat),
  varstrat,
  digits = 2,
  signif_digits = 4
) {
  dataframe <- as.data.frame(dataframe)
  ## stops
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(all(varstrat %in% names(dataframe)))

  # init resulting dataframe containing used tests, p-valeurs and conclusion
  vars_factor <- get_factors(dataframe, vars)
  prop_equal <- as.data.frame(matrix(NA, nrow = length(vars_factor), ncol = 3))
  rownames(prop_equal) <- vars_factor
  colnames(prop_equal) <- c("P_valeur", "Test", "message")

  # list resulting of detailed tests
  detailtest <- vector("list", length(vars_factor))
  names(detailtest) <- vars_factor

  for (vari in vars_factor) {
    detailtest[[vari]] <- vector("list", 4)
    names(detailtest[[vari]]) <- c("observations", "observed", "theorical", "Test")

    # count individuals (in)exploitables for test
    detailtest[[vari]][["observations"]] <- matrix(
      c(
        sum(apply(is.na(dataframe[, c(vari, varstrat)]), 1, any)),
        sum(apply(!is.na(dataframe[, c(vari, varstrat)]), 1, all))
      )
    )
    rownames(detailtest[[vari]][["observations"]]) <- c("Number of NA lines : ", "Number of used lines : ")
    colnames(detailtest[[vari]][["observations"]]) <- ""

    # table of observed counts
    effobs <- table(dataframe[, c(vari, varstrat)])
    detailtest[[vari]][["observed"]] <- effobs
    if (sum(effobs) == 0) next

    # table of theoretical counts
    efftheo <- as.table(round(rowSums(effobs) %*% t(colSums(effobs)) / sum(effobs), 2))
    names(dimnames(efftheo)) <- names(dimnames(effobs))
    rownames(efftheo) <- rownames(effobs)
    colnames(efftheo) <- colnames(effobs)
    detailtest[[vari]][["theorical"]] <- efftheo

    # cochran's rule check
    cochran_condition <- check_cochran(efftheo, verbose = FALSE)

    # show results of test according the respect of cochran's rule
    if (cochran_condition) {
      khi2 <- stats::chisq.test(effobs)
      prop_equal[vari, "message"] <- "Cochran condition is validated."
      prop_equal[vari, "Test"] <- "khi2 (Pearson's Chi-squared test)" # Pearson's Chi-squared test
      prop_equal[vari, "P_valeur"] <- signif(x = khi2$p.value, digits = signif_digits)
      # prop_equal[vari, "decision"] <- ifelse(prop_equal[vari, "P_valeur"] < 0.05, "dependance", "")
      detailtest[[vari]][["Test"]] <- khi2
    }

    if (!cochran_condition) {
      fisher_condition <- check_fisher(tc = effobs)
      if (fisher_condition) {
        prop_equal[vari, "Test"] <- "Fisher's Exact Test for Count Data" # Fisher's Exact Test for Count Data

        has_issues <- try(tools::assertCondition(
          fisher <- stats::fisher.test(effobs, workspace = 1000000)
        ), silent = TRUE)
        has_issues <- has_issues[
          sapply(has_issues, function(el) {
            length(base::intersect(class(el), c("warning", "error"))) != 0
          })
        ]
        if (length(has_issues) == 0) { # If no error
          prop_equal[vari, "P_valeur"] <- signif(x = fisher$p.value, digits = signif_digits)
          # prop_equal[vari, "decision"] <- ifelse(prop_equal[vari, "P_valeur"] < 0.05, "dependance", "")
          detailtest[[vari]][["Test"]] <- fisher
          prop_equal[vari, "message"] <- "Cochran condition is not validated."
        } else { # capture error
          msg_captured <- paste(
            unique(sapply(
              X = has_issues,
              FUN = function(el) {
                paste0("[", class(el)[2], "] ", el$message)
              }
            )),
            collapse = ";"
          )
          message("[test_proportions] ", msg_captured)
          prop_equal[vari, "message"] <- paste0(msg_captured, ".\nCochran condition is not validated.")

          if (any(unique(sapply(X = has_issues, FUN = function(el) {
            class(el)[2]
          })) != "warning")) {
            # it means we have some errors or other type ?
            # nothing should be done with fisher obj
          } else {
            ## if only warning keep statistic from fisher obj ? --here
            prop_equal[vari, "P_valeur"] <- signif(x = fisher$p.value, digits = signif_digits)
            # prop_equal[vari, "decision"] <- ifelse(prop_equal[vari, "P_valeur"] < 0.05, "dependance", "")
            detailtest[[vari]][["Test"]] <- fisher
          }
        }
      } else {
        prop_equal[vari, "Test"] <- "/" # Fisher's Exact Test for Count Data possible but not deployed here
        # prop_equal[vari, "decision"] <- "/"
        prop_equal[vari, "message"] <- paste0(
          "Structural zero observed.\n",
          "Cochran condition is not validated so fisher test could be possible but not shown here."
        )
      }
    }
  }

  # variables with p-valeur < 0.2
  selection <- rownames(prop_equal)[which(prop_equal[, "P_valeur"] <= 0.2)]

  return(list(results = prop_equal, details = detailtest, selection = selection))
}

#' test means
#'
#' Test equality of means.
#' Deploy the tests Wilcoxon-Mann-Whitney or kruskal-Wallis for difference of means with a variable of groups for non normal data.
#' Otherwise Deploy the tests Student or anova.
#'
#' @param dataframe A data.frame containing data.
#' @param vars A vector of characters. Names of dataframe's factorial columns to describe.
#' @param varstrat A character. Name of the stratification variable, making groups to compare.
#' @param force_non_parametric_test A logical. Default FALSE. You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#'
#' @return A list of 3 objects
#'   results = data.frame with test, P-values, explicit decision for each vars
#'   details = list with details, various objects.
#'   selection = vector of variables with p-values < 0.2
#' @export
#' @import stats
#' @examples
#' \dontrun{
#' test_means(
#'   dataframe = modified_state[, c("Population", "Income", "election")],
#'   vars = c("Population", "Income"),
#'   varstrat = "election"
#' )
#' }
test_means <- function(
  dataframe,
  vars = setdiff(colnames(dataframe), varstrat),
  varstrat,
  force_non_parametric_test = FALSE,
  digits = 2,
  signif_digits = 4
) {
  dataframe <- as.data.frame(dataframe)
  ## stops
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(all(varstrat %in% names(dataframe)))

  varstrat_levels <- levels(dataframe[, varstrat])

  # check minimum of 4 patients per group
  if (any(table(dataframe[, c(varstrat)]) < 4)) {
    message("[test_means] L'effectif est < 4 dans au moins l un des groupes ! No test possible")
    results <- data.frame(
      Variable = vars,
      P_valeur = NA,
      Test = "/",
      message = "Error : L'effectif est < 4 dans au moins l un des groupes ! Pas de test possible"
    )
    results$Variable <- NULL
    rownames(results) <- vars
    colnames(results) <- c("P_valeur", "Test", "message")
    res <- list(
      results = results,
      details = list(),
      selection = NA_character_
    )
    return(res)
  }
  ## Evolution : tester l'effectif pour chaque variable aussi.

  # check well more than one level
  if (nlevels(dataframe[, c(varstrat)]) <= 1) {
    message("[test_means] varstrat have less than 2 levels ! No test possible")
    results <- data.frame(
      P_valeur = NA,
      Test = "/",
      message = "Error : varstrat have less than 2 levels ! No test possible"
    )
    rownames(results) <- vars[1]
    colnames(results) <- c("P_valeur", "Test", "message")
    res <- list(
      results = results,
      details = list(),
      selection = NA_character_
    )
    return(res)
  }

  # init resulting dataframe containing used tests, p-valeur and test conclusion
  vars_numeric <- get_numerics(dataframe, vars)
  mean_equal <- as.data.frame(matrix(NA, nrow = length(vars_numeric), ncol = 3))
  rownames(mean_equal) <- vars_numeric
  colnames(mean_equal) <- c("P_valeur", "Test", "message")

  # liste of detailed test results
  detailtest <- vector("list", length(vars_numeric))
  names(detailtest) <- vars_numeric

  if (force_non_parametric_test) message("[test_means] force_non_parametric_test")

  for (vari in vars_numeric) {
    message("[test_means] ", vari, " by ", varstrat)
    detailtest[[vari]] <- vector("list", 3)
    names(detailtest[[vari]]) <- c("observations", "statistics", "Test")

    # Number of individuals (in)exploitables for test
    detailtest[[vari]][["observations"]] <- matrix(
      c(
        sum(apply(is.na(dataframe[, c(vari, varstrat)]), 1, any)),
        sum(apply(!is.na(dataframe[, c(vari, varstrat)]), 1, all))
      )
    )
    rownames(detailtest[[vari]][["observations"]]) <- c("Number of NA lines : ", "Number of used lines : ")
    colnames(detailtest[[vari]][["observations"]]) <- ""

    # statistics table
    summarizedstats <- as.data.frame(matrix(NA, nrow = nlevels(dataframe[, varstrat]) + 1, ncol = 6))
    aggr_results <- stats::aggregate(dataframe[, vari], list(dataframe[, varstrat]), summary, simplify = FALSE)
    for (l in (1:nlevels(dataframe[, varstrat]))) {
      summarizedstats[l, ] <- aggr_results[, 2][[l]][1:6]
    }
    summarizedstats[nlevels(dataframe[, varstrat]) + 1, ] <- summary(dataframe[, vari])[1:6]

    rownames(summarizedstats) <- c(varstrat_levels, "Total")
    colnames(summarizedstats) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")
    detailtest[[vari]][["statistics"]] <- summarizedstats

    # --dev test (any(table(dataframe[ vari , c(varstrat)]) < 4)) for each variables ... (later)

    # Check normality in each group
    has_issues <- try(tools::assertCondition(
      group_normality <- vapply(X = 1:nlevels(dataframe[, varstrat]), FUN = function(g) {
        stats::shapiro.test(dataframe[[vari]][dataframe[[varstrat]] %in% varstrat_levels[g]])$p.value
      }, FUN.VALUE = 1)
    ), silent = TRUE)
    has_issues <- has_issues[
      sapply(has_issues, function(el) {
        length(base::intersect(class(el), c("warning", "error"))) != 0
      })
    ]

    if (length(has_issues) == 0) { # If no error

      # test equality

      # if no normality in one group
      if (any(group_normality < 0.05) || force_non_parametric_test) {
        if (nlevels(dataframe[, varstrat]) == 2) {
          captured_wilcox_message <- try(tools::assertCondition(
            wilcox <- wilcox.test(dataframe[, vari] ~ dataframe[, varstrat])
          ), silent = TRUE)
          captured_wilcox_message <- captured_wilcox_message[
            sapply(captured_wilcox_message, function(el) {
              length(base::intersect(class(el), c("warning", "error"))) != 0
            })
          ]
          if (all(group_normality >= 0.05) && force_non_parametric_test) {
            mean_equal[vari, "message"] <- "force_non_parametric_test.\n"
          } else {
            mean_equal[vari, "message"] <- ""
          }
          mean_equal[vari, "message"] <- paste0(
            mean_equal[vari, "message"],
            "Make sure you have symetric distributions (wilcoxon test).\n",
            sum(group_normality < 0.05), " group(s) not normal."
          )
          if (length(captured_wilcox_message) > 0) {
            captured_wilcox_message <- paste(
              unique(sapply(
                X = captured_wilcox_message,
                FUN = function(el) {
                  paste0("[", class(el)[2], "] ", el$message)
                }
              )),
              collapse = ";"
            )
            mean_equal[vari, "message"] <- paste0(
              mean_equal[vari, "message"], "\n",
              captured_wilcox_message
            )
          }
          mean_equal[vari, "Test"] <- "Wilcoxon rank sum exact test (Mann-Whitney)" # Wilcoxon-Mann-Whitney
          mean_equal[vari, "P_valeur"] <- signif(x = wilcox$p.value, digits = signif_digits)
          # mean_equal[vari, "decision"] <- ifelse(mean_equal[vari, "P_valeur"] < 0.05, "difference", "")
          detailtest[[vari]][["Test"]] <- wilcox
        } else {
          kruskal <- stats::kruskal.test(dataframe[, vari] ~ dataframe[, varstrat])
          if (all(group_normality >= 0.05) && force_non_parametric_test) {
            mean_equal[vari, "message"] <- "force_non_parametric_test.\n"
          } else {
            mean_equal[vari, "message"] <- ""
          }
          mean_equal[vari, "message"] <- paste0(mean_equal[vari, "message"], sum(group_normality < 0.05), " group(s) not normal.")
          mean_equal[vari, "Test"] <- "Kruskal-Wallis rank sum test"
          mean_equal[vari, "P_valeur"] <- signif(x = kruskal$p.value, digits = signif_digits)
          # mean_equal[vari, "decision"] <- ifelse(mean_equal[vari, "P_valeur"] < 0.05, "difference", "")
          detailtest[[vari]][["Test"]] <- kruskal
        }
      } else { # if normality in all groups

        effectif_by_group <- vapply(X = 1:nlevels(dataframe[, varstrat]), FUN = function(g) {
          length(dataframe[[vari]][dataframe[[varstrat]] %in% varstrat_levels[g]])
        }, FUN.VALUE = 1)
        if (any(effectif_by_group < 15)) { # min 15 as param ?
          # just a message to warn, normality is eval on few points...
          msg <- paste0("Warning:Normality assessed on few points in a group (n min = ", min(effectif_by_group), ").")
          message("[test_means] ", vari, " ", msg)
          mean_equal[vari, "message"] <- msg
        } else {
          mean_equal[vari, "message"] <- ""
        }

        if (nlevels(dataframe[, varstrat]) == 2) {
          equalvariance <- (stats::var.test(dataframe[, vari] ~ dataframe[, varstrat])$p.value >= 0.05)
          student <- stats::t.test(dataframe[, vari] ~ dataframe[, varstrat], var.equal = equalvariance)
          mean_equal[vari, "message"] <- paste0(mean_equal[vari, "message"], "\nAll groups normal.")
          mean_equal[vari, "Test"] <- "Student T-test"
          mean_equal[vari, "P_valeur"] <- signif(x = student$p.value, digits = signif_digits)
          # mean_equal[vari, "decision"] <- ifelse(mean_equal[vari, "P_valeur"] < 0.05, "difference", "")
          detailtest[[vari]][["Test"]] <- student
        } else {
          equalvariance <- (stats::bartlett.test(dataframe[, vari] ~ dataframe[, varstrat])$p.value >= 0.05)
          anova_obj <- stats::oneway.test(dataframe[, vari] ~ dataframe[, varstrat], var.equal = equalvariance)
          mean_equal[vari, "message"] <- paste0(mean_equal[vari, "message"], "\nAll groups normal.")
          mean_equal[vari, "Test"] <- "Anova One-Way"
          mean_equal[vari, "P_valeur"] <- signif(x = anova_obj$p.value, digits = signif_digits)
          # mean_equal[vari, "decision"] <- ifelse(mean_equal[vari, "P_valeur"] < 0.05, "difference", "")
          detailtest[[vari]][["Test"]] <- anova_obj
        }
      }
    } else { # capture error occured during shapiro
      msg_captured <- paste(
        unique(sapply(
          X = has_issues,
          FUN = function(el) {
            paste0("[", class(el)[2], "] ", el$message)
          }
        )),
        collapse = ";"
      )
      message("[test_means] ", msg_captured)
      mean_equal[vari, "message"] <- msg_captured

      # mean_equal[vari, "message"] <- "Test not done : not enough data. "
      # mean_equal[vari, "Test"] <- "no test"
      # mean_equal[vari, "P_valeur"] <- "/"
      # mean_equal[vari, "decision"] <- "/"
      # detailtest[[vari]][["Test"]] <- "/"
    }
  }

  ## Warning message to focus on the symetric distributions in wilcoxon test
  ## warning in consol not wanted, ever save in mean_equal[vari, "message"] ...
  # if (any(mean_equal[, "Test"] == "Wilcoxon rank sum exact test (Mann-Whitney)", na.rm = TRUE)) {
  #   msg_warning <- paste0(
  #     "[test_means] ",
  #     "Make sur you have symetric distributions for following variable(s) : \n ",
  #     vector_to_character(vars_numeric[mean_equal[, "Test"] == "wilcox"], ", "), "\n"
  #   )
  #   warning(msg_warning)
  # }

  # variables dont la p-valeur < 0.2
  selection <- rownames(mean_equal)[which(mean_equal[, "P_valeur"] <= 0.2)]

  return(list(
    results = mean_equal, details = detailtest, selection = selection
  ))
}


#### Get estimate tested ####

#' estimate difference of means
#'
#' Compute difference of means and its IC between groups
#'
#' @param dataframe A data.frame containing data.
#' @param vars A vector of characters. Names of dataframe's factorial columns to describe.
#' @param varstrat A character. Name of the stratification variable, making groups to compare.
#' @param digits A integer, Default 1. Integer indicating the number of decimal places (round).
#' @param force_non_parametric_test A logical. Default FALSE. You can turn it TRUE if you want to force the use of
#'  non parametric test, whatever shapiro test said about normality.
#'
#' @return A data.frame,
#'   in rows the vars and in column the stat used, the difference values and the IC95%
#' @export
#' @import dplyr
#' @importFrom simpleboot two.boot
#' @importFrom boot boot.ci
#' @examples
#' \dontrun{
#' estimate_diff_mean(
#'   dataframe = modified_state[, c("Population", "Income", "election")],
#'   vars = c("Population", "Income"),
#'   varstrat = "election"
#' )
#' }
estimate_diff_mean <- function(
  dataframe,
  vars,
  varstrat,
  digits = 1,
  force_non_parametric_test = FALSE
) {
  `%>%` <- magrittr::`%>%`

  # On vérifie qu'on a bien que 2 modalités dans varstrat, et que c'est un facteur
  stopifnot(varstrat %in% names(dataframe))
  stopifnot(vars %in% names(dataframe))
  dataframe <- as.data.frame(dataframe)
  if (!is.factor(dataframe[, varstrat])) {
    stop("La variable de groupe n'est pas un facteur")
  }
  if (nlevels(dataframe[, varstrat]) != 2) {
    stop("La variable de groupe n'a pas 2 modalites")
  }

  # test normality in 2 groups
  normalite <- dataframe %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(varstrat))) %>%
    dplyr::summarise_at(vars, ~ stats::shapiro.test(.)$p.value)

  varstrat_levels <- levels(dataframe[, varstrat])

  if (force_non_parametric_test) message("[estimate_diff_mean] force_non_parametric_test")

  tab <- do.call("rbind", lapply(vars, function(vari) {
    effectif_by_group <- vapply(
      X = 1:nlevels(dataframe[, varstrat]),
      FUN = function(g) {
        length(dataframe[[vari]][dataframe[[varstrat]] %in% varstrat_levels[g]])
      },
      FUN.VALUE = 1
    )
    if (any(effectif_by_group < 15)) { # min 15 as param ?
      # just a message to warn, normality is eval on few points...
      msg <- paste0("Warning:Normality assessed on few points in a group (n min = ", min(effectif_by_group), ").")
      message("[estimate_diff_mean] ", vari, " ", msg)
    }

    # nom de la variable, pour mettre dans le tableau
    nom <- ifelse(
      is.null(attr(dataframe[, vari], "label")),
      vari,
      attr(dataframe[, vari], "label")
    )
    # nom <- vari ??? directement? --here

    # si normale
    if (all(normalite[, vari] > 0.05) && !force_non_parametric_test) {
      # La différence peut être obtenue avec la fonction t.test
      test <- stats::t.test(
        stats::as.formula(paste0("`", vari, "`", " ~ ", "`", varstrat, "`")),
        data = dataframe
      )

      res <- data.frame(
        variable = nom,
        Stat = "Moyenne",
        Difference = round(test$estimate[2] - test$estimate[1], digits = digits),
        `IC95%` = paste0(
          "[", paste(round(test$conf.int, digits = digits), collapse = " ; "), "]"
        ),
        check.names = FALSE, row.names = NULL
      )
    } else {
      # Sinon, difference des medianes
      # rééchantillonnage
      medb <- simpleboot::two.boot(
        sample1 = stats::na.omit(dataframe[dataframe[, varstrat] == levels(dataframe[, varstrat])[2], vari]),
        sample2 = stats::na.omit(dataframe[dataframe[, varstrat] == levels(dataframe[, varstrat])[1], vari]),
        FUN = median, R = 10000
      )

      # IC
      ic <- boot::boot.ci(
        boot.out = medb,
        conf = 0.95,
        type = "perc"
        # percent = The intervals calculated using the bootstrap percentile method.
      )

      res <- data.frame(
        variable = nom, Stat = "Mediane",
        Difference = round(medb$t0, digits = digits),
        `IC95%` = paste0(
          "[", paste(round(ic$percent[, 4:5], digits = digits), collapse = " ; "),
          "]"
        ),
        check.names = FALSE, row.names = NULL
      )

      return(res)
    }
  }))
  return(tab)
}
