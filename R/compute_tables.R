# Core function of the pkg, compute descriptive tables

#' compute continuous table
#'
#' provide descriptive statistics table for continuous data
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.frame.
#' @param vars A vector of characters. Names of dataframe's continuous columns to describe.
#' @param varstrat A character. Default NULL. Name of the stratification variable, making groups to compare.
#' @param stats_choice A vector of characters. Default provide all usual statistics to describe continuous variables,
#'  namely 'c("mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal")'
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#'
#' @return A final table or a list of tables if varstrat not null.
#' 
#' @export
#' @examples
#' \dontrun{
#' compute_continuous_table(
#'   dataframe = modified_state,
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp", "Murder",
#'     "HS Grad", "Frost", "Area"
#'   ),
#'   varstrat = "election",
#'   digits = 2
#' )
#' }
compute_continuous_table <- function(
    dataframe,
    vars = setdiff(colnames(dataframe), varstrat),
    varstrat = NULL,
    stats_choice = c(
      "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal"
    ),
    digits = 2
) {
  dataframe <- as.data.frame(dataframe)
  ## stops
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(all(stats_choice %in% c(
    "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal"
  )))
  stopifnot(digits >= 0)

  # detection of numeric varibles
  vars_numeric <- get_numerics(dataframe, vars)
  if (!all(vars %in% vars_numeric)) {
    message("[compute_continuous_table] Warning, some of selected vars were ignored (not numeric).")
  }

  if (is.null(varstrat) || varstrat %in% "") { # univariate analyse

    # init resulting dataframe with stats
    statistics <- as.data.frame(matrix(NA, nrow = length(vars_numeric), ncol = 11))
    rownames(statistics) <- vars_numeric
    colnames(statistics) <- c(
      "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal"
    )

    # compute stats for each variables
    for (vari in vars_numeric) {
      tmp_shapi <- stats::na.omit(dataframe[, vari])
      if (length(unique(tmp_shapi)) == 1 || length(tmp_shapi) < 3) {
        shapiro_conclu <- NA # shapiro not applicable if all values are the same
      } else {
        # capture shapi error, so able to skip it
        has_issues <- try(tools::assertCondition(
          shapiro_obj <- stats::shapiro.test(tmp_shapi)
        ), silent = TRUE)
        has_issues <- has_issues[
          sapply(has_issues, function(el) {
            length(base::intersect(class(el), c("warning", "error"))) != 0
          })
        ]
        if (length(has_issues) == 0) { # No error
          shapiro_conclu <- as.logical(shapiro_obj$p.value > 0.05)
        } else {
          shapiro_conclu <- NA # shapi error
        }
      }

      statistics[vari, ] <- c(
        round(x = c(
          mean(dataframe[[vari]], na.rm = TRUE),
          sd(dataframe[[vari]], na.rm = TRUE),
          stats::median(dataframe[[vari]], na.rm = TRUE),
          stats::quantile(dataframe[[vari]], 0.25, na.rm = TRUE),
          stats::quantile(dataframe[[vari]], 0.75, na.rm = TRUE),
          # min(dataframe[[vari]], na.rm = TRUE), # --here  Message d'avis : -Inf est renvoyé si aucune données
          # max(dataframe[[vari]], na.rm = TRUE), # --here  Message d'avis : -Inf est renvoyé si aucune données
          # fix message d'avis
          ifelse(
            all(is.na(dataframe[[vari]])),
            NA, # --here  Message d'avis : -Inf est renvoyé si aucune données
            min(dataframe[[vari]], na.rm = TRUE)
          ),
          ifelse(
            all(is.na(dataframe[[vari]])),
            NA, # --here  Message d'avis : -Inf est renvoyé si aucune données
            max(dataframe[[vari]], na.rm = TRUE)
          ),
          length(dataframe[[vari]]), # N
          sum(is.na(dataframe[[vari]])), # N_NA
          sum(is.na(dataframe[[vari]]) == FALSE) # Nb mesures
        ), digits = digits),
        shapiro_conclu
      )
    }

    final_table <- statistics[, stats_choice]
  } else { # bivariate analyse

    # message("[compute_continuous_table] varstrat : ", varstrat)
    stopifnot(all(varstrat %in% names(dataframe)))

    # get levels of varstrat
    varstrat_levels <- levels(dataframe[, varstrat])

    # init resulting list of dataframe with stats
    statistics <- vector("list", length(vars_numeric))
    names(statistics) <- vars_numeric

    # compute stats for each variables
    for (vari in vars_numeric) {
      # init resulting dataframe with stats
      statistics[[vari]] <- as.data.frame(matrix(NA, nrow = length(varstrat_levels) + 1, ncol = 11))
      rownames(statistics[[vari]]) <- c("", varstrat_levels)
      colnames(statistics[[vari]]) <- c(
        "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "N_NA", "Nb_mesures", "is_Normal"
      )

      # compute stats for each variables per level of varstrat
      for (level in varstrat_levels) {
        tmp_shapi <- stats::na.omit(dataframe[which(dataframe[, varstrat] == level), vari])

        if (length(unique(tmp_shapi)) == 1 || length(tmp_shapi) < 3) {
          shapiro_conclu <- NA # shapiro not applicable if all values are the same
        } else {
          # capture shapi error, so able to skip it
          has_issues <- try(tools::assertCondition(
            shapiro_obj <- stats::shapiro.test(tmp_shapi)
          ), silent = TRUE)
          has_issues <- has_issues[
            sapply(has_issues, function(el) {
              length(base::intersect(class(el), c("warning", "error"))) != 0
            })
          ]
          if (length(has_issues) == 0) { # No error
            shapiro_conclu <- as.logical(shapiro_obj$p.value > 0.05)
          } else {
            shapiro_conclu <- NA # shapi error
          }
        }

        statistics[[vari]][level, ] <- c(
          round(x = c(
            mean(dataframe[which(dataframe[, varstrat] == level), vari], na.rm = TRUE),
            stats::sd(dataframe[which(dataframe[, varstrat] == level), vari], na.rm = TRUE),
            stats::median(dataframe[which(dataframe[, varstrat] == level), vari], na.rm = TRUE),
            stats::quantile(dataframe[which(dataframe[, varstrat] == level), vari], 0.25, na.rm = TRUE),
            stats::quantile(dataframe[which(dataframe[, varstrat] == level), vari], 0.75, na.rm = TRUE),
            ifelse(
              all(is.na(dataframe[which(dataframe[, varstrat] == level), vari])),
              NA, # --here  Message d'avis : -Inf est renvoyé si aucune données
              min(dataframe[which(dataframe[, varstrat] == level), vari], na.rm = TRUE)
            ),
            ifelse(
              all(is.na(dataframe[which(dataframe[, varstrat] == level), vari])),
              NA, # --here  Message d'avis : -Inf est renvoyé si aucune données
              max(dataframe[which(dataframe[, varstrat] == level), vari], na.rm = TRUE)
            ),
            length(dataframe[which(dataframe[, varstrat] == level), vari]),
            sum(is.na(dataframe[which(dataframe[, varstrat] == level), vari])),
            sum(is.na(dataframe[which(dataframe[, varstrat] == level), vari]) == FALSE)
          ), digits = digits),
          shapiro_conclu
        )
      }

      #### Global pop ####

      # compute stats for each variables in global pop
      tmp_shapi <- stats::na.omit(dataframe[, vari])
      if (length(unique(tmp_shapi)) == 1 || length(tmp_shapi) < 3) { # v0.1.24 : add length(tmp_shapi) < 3
        shapiro_conclu <- NA # shapiro not applicable if all values are the same
      } else {
        # capture shapi error, so able to skip it
        has_issues <- try(tools::assertCondition(
          shapiro_obj <- stats::shapiro.test(tmp_shapi)
        ), silent = TRUE)
        has_issues <- has_issues[
          sapply(has_issues, function(el) {
            length(base::intersect(class(el), c("warning", "error"))) != 0
          })
        ]
        if (length(has_issues) == 0) { # No error
          shapiro_conclu <- as.logical(shapiro_obj$p.value > 0.05)
        } else {
          shapiro_conclu <- NA # shapi error
        }
      }
      statistics[[vari]][1, ] <- c(
        round(x = c(
          mean(dataframe[[vari]], na.rm = TRUE),
          stats::sd(dataframe[[vari]], na.rm = TRUE),
          stats::median(dataframe[[vari]], na.rm = TRUE),
          stats::quantile(dataframe[[vari]], 0.25, na.rm = TRUE),
          stats::quantile(dataframe[[vari]], 0.75, na.rm = TRUE),
          # min(dataframe[[vari]], na.rm = TRUE), # --here  Message d'avis : -Inf est renvoyé si aucune données
          # max(dataframe[[vari]], na.rm = TRUE), # --here  Message d'avis : -Inf est renvoyé si aucune données
          ifelse(
            all(is.na(dataframe[[vari]])),
            NA, # --here  Message d'avis : -Inf est renvoyé si aucune données
            min(dataframe[[vari]], na.rm = TRUE)
          ),
          ifelse(
            all(is.na(dataframe[[vari]])),
            NA, # --here  Message d'avis : -Inf est renvoyé si aucune données
            max(dataframe[[vari]], na.rm = TRUE)
          ),
          length(dataframe[[vari]]),
          sum(is.na(dataframe[[vari]])),
          sum(is.na(dataframe[[vari]]) == FALSE)
        ), digits = digits),
        shapiro_conclu
      )

      rownames(statistics[[vari]]) <- c("", paste(varstrat, varstrat_levels, sep = ""))
    }

    final_table <- lapply(statistics, function(dataframe) {
      subset(dataframe, select = stats_choice)
    })
  }

  return(final_table)
}


#' compute correlation table
#'
#' provide descriptive statistics table for continuous data and its correlation with a continuous varstrat
#'
#' @param dataframe A data.frame, tibble or data.table will be converted into data.table.
#' @param vars A vector of characters. Names of dataframe's continuous columns to describe.
#' @param varstrat A character. Always needed. Name of the continuous var to compute the correlation with.
#' @param method_corr A character. Default "detect_auto" meaning the linear link will be detected if the R2 > 0.5.
#'   Otherwise, method to compute correlation is wanted "pearson", "spearman", "kendall".
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues.
#'
#' @return A final table of stat and correlation.
#' 
#' @import data.table
#' @import stats
#' @import tools
#' @importFrom RVAideMemoire spearman.ci
#' @export
#' @examples
#' \dontrun{
#' compute_correlation_table(
#'   dataframe = modified_state,
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp",
#'     "HS Grad", "Frost"
#'   ),
#'   varstrat = "Area",
#'   method_corr = "detect_auto",
#'   digits = 2
#' )
#' }
compute_correlation_table <- function(
    dataframe,
    vars = setdiff(colnames(dataframe), varstrat),
    varstrat = varstrat, # needed
    method_corr = "detect_auto",
    digits = 2,
    signif_digits = 4
) {
  dataframe <- data.table::as.data.table(dataframe)

  ## stops
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(!is.null(varstrat))
  stopifnot(all(varstrat %in% names(dataframe)))
  stopifnot(is.numeric(dataframe[[varstrat]]))
  stopifnot(digits >= 0)
  stopifnot(signif_digits > 0)
  stopifnot(method_corr %in% c("detect_auto", "pearson", "spearman", "kendall"))

  # detection of numeric varibles
  vars_numeric <- get_numerics(dataframe, vars)
  if (!all(vars %in% vars_numeric)) {
    message("[compute_correlation_table] Warning, some of selected vars were ignored (not numeric).")
  }

  # compute stats corr for each variables
  statistics_tab <- lapply(X = vars_numeric, function(vari) {
    message("[compute_correlation_table] ", vari)

    N_missingval <- sum(is.na(dataframe[[vari]])) # in total

    tmp_dt <- stats::na.omit(dataframe[, .SD, .SDcols = c(vari, varstrat)])

    if (length(unique(tmp_dt[[vari]])) == 1) {
      shapiro_conclu <- NA # shapiro not applicable if all values are the same
    } else {
      # capture shapi error, so able to skip it
      has_issues <- try(tools::assertCondition(
        shapiro_obj <- stats::shapiro.test(tmp_dt[[vari]])
      ), silent = TRUE)
      has_issues <- has_issues[
        sapply(has_issues, function(el) {
          length(base::intersect(class(el), c("warning", "error"))) != 0
        })
      ]
      if (length(has_issues) == 0) { # No error
        shapiro_conclu <- as.logical(shapiro_obj$p.value > 0.05) # about vari
      } else {
        shapiro_conclu <- NA # shapi error
      }
    }

    if (length(unique(tmp_dt[[varstrat]])) == 1) {
      varstrat_is_normal <- NA # shapiro not applicable if all values are the same
    } else {
      # capture shapi error, so able to skip it
      has_issues <- try(tools::assertCondition(
        shapiro_obj <- stats::shapiro.test(tmp_dt[[varstrat]])
      ), silent = TRUE)
      has_issues <- has_issues[
        sapply(has_issues, function(el) {
          length(base::intersect(class(el), c("warning", "error"))) != 0
        })
      ]
      if (length(has_issues) == 0) { # No error
        varstrat_is_normal <- as.logical(shapiro_obj$p.value > 0.05) # about varstrat
      } else {
        varstrat_is_normal <- NA # shapi error
      }
    }

    if (method_corr %in% "detect_auto") {
      if (isTRUE(shapiro_conclu) & isTRUE(varstrat_is_normal)) { # if vari normal & varstrat_is_normal
        method_corr <- "pearson"
      } else {
        method_corr <- "spearman"
      }
      ## detection of linear link
      ## reg <- stats::lm(formula = as.formula(paste0("`", varstrat, "`", "~", "`", vari, "`")), data = tmp_dt)
      ## --here improvment suggestion : also detect linear link ? summary(reg)[["r.squared"]] > 0.5
    }
    message("[compute_correlation_table] correlation ", method_corr)
    has_issues <- try(tools::assertCondition(
      corr_obj <- stats::cor.test(
        x = tmp_dt[[vari]], y = tmp_dt[[varstrat]],
        alternative = "two.sided",
        method = method_corr
      )
    ), silent = TRUE)
    has_issues <- has_issues[
      sapply(has_issues, function(el) {
        length(base::intersect(class(el), c("warning", "error"))) != 0
      })
    ]

    if (length(has_issues) == 0) {
      msg <- ""
    } else {
      msg <- paste(
        unique(sapply(
          X = has_issues, # from shapiro
          FUN = function(el) {
            paste0("[", class(el)[2], "] ", el$message)
          }
        )),
        collapse = ";"
      )
    }
    # corr_obj$estimate

    if (method_corr %in% c("spearman", "kendall")) {
      set.seed(42)
      ic95_obj <- RVAideMemoire::spearman.ci(
        tmp_dt[[vari]], tmp_dt[[varstrat]],
        conf.level = 0.95, nrep = 1000
      )
      IC95 <- paste0(
        "[", round(ic95_obj$conf.int[["Inf"]], digits = digits), " , ",
        round(ic95_obj$conf.int[["Sup"]], digits = digits), "]"
      )
      if (method_corr %in% "kendall") msg <- paste0(msg, " spearman.ci")
    } else {
      IC95 <- paste0("[", paste0(round(corr_obj$conf.int, digits = digits), collapse = " , "), "]")
    }

    sumup <- unique(
      tmp_dt[, `:=`(
        varstrat = varstrat,
        Variable = vari,
        Valeurs_manquantes = N_missingval,
        Nb_mesures = .N,
        mean = round(x = mean(.SD[[1]], na.rm = TRUE), digits = digits),
        sd = round(stats::sd(.SD[[1]], na.rm = TRUE), digits = digits),
        median = round(stats::median(.SD[[1]], na.rm = TRUE), digits = digits),
        Q1 = round(stats::quantile(.SD[[1]], na.rm = TRUE)["25%"], digits = digits),
        Q3 = round(stats::quantile(.SD[[1]], na.rm = TRUE)["75%"], digits = digits),
        min = round(min(.SD[[1]], na.rm = TRUE), digits = digits),
        max = round(max(.SD[[1]], na.rm = TRUE), digits = digits),
        is_Normal = shapiro_conclu, # var i is normal
        varstrat_is_Normal = varstrat_is_normal, # var strat is normal
        correlation = round(corr_obj$estimate, digits = digits),
        IC95 = IC95,
        P_valeur = signif(corr_obj$p.value, digits = signif_digits),
        correlation_method = method_corr, # corr_obj$method
        message = msg
      ),
      .SDcols = vari
      ][
        , .SD,
        .SDcols = -c(varstrat, vari)
      ]
    )

    return(sumup)
  })

  final_table <- data.table::rbindlist(statistics_tab)

  return(final_table)
}


#' compute factorial table
#'
#' provide descriptive statistics table for factorial data
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.frame.
#' @param vars A vector of characters. Names of dataframe's factorial columns to describe.
#' @param varstrat A character. Default NULL. Name of the stratification variable,
#'  making groups to compare.
#' @param simplify A logical. Default TRUE. Reduce the yes/no  (or 1/0) modalities 
#' to display only the yes (1).
#' @param prop_table_margin A vector giving the margins to split by. Default 2. 
#'  1 indicates rows, 2 indicates columns,
#'  c(1, 2) indicates rows and columns. When x has named dimnames, 
#'  it can be a character vector selecting dimension names.
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' @param force_generate_1_when_0 A logical, Default FALSE. If TRUE, 
#'  will test if the unique modality is 0 or "non" and
#'  add the level 1 or "oui" so it can be display in counts. 
#'  Can be combined with simplify to only show the modality (1).
#'    
#' @return A list of tables with counts and percentage.
#' 
#' @export
#' @examples
#' \dontrun{
#' compute_factorial_table(
#'   dataframe = modified_state,
#'   vars = c("state.division", "state.region", "binary_test"),
#'   varstrat = "election",
#'   digits = 2,
#'   simplify = FALSE
#' )
#' vars_wanted <- c("yes_no_french_question", "all_count_zero")
#' varstrat_wanted <- "election"
#' res_fact_simpl0_tabs <- compute_factorial_table(
#'   dataframe = modified_state,
#'   vars = vars_wanted,
#'   varstrat = varstrat_wanted,
#'   digits = 2,
#'   simplify = TRUE,
#'   force_generate_1_when_0 = TRUE
#' )
#' }
compute_factorial_table <- function(
    dataframe,
    vars = setdiff(colnames(dataframe), varstrat),
    varstrat = NULL,
    simplify = TRUE,
    prop_table_margin = 2,
    digits = 2,
    force_generate_1_when_0 = FALSE
) {
  dataframe <- as.data.frame(dataframe)
  ## stops
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(digits >= 0)

  # get factorial variables
  vars_factor <- get_factors(dataframe, vars)
  if (!all(vars %in% vars_factor)) {
    message("[compute_factorial_table] Warning, some of selected vars were ignored (not factors).")
  }

  # init resulting list of dataframe for each variable
  effectifs <- vector("list", length(vars_factor))
  names(effectifs) <- vars_factor

  if (is.null(varstrat) || varstrat %in% "") { # univariate analyse

    # loop on variable
    for (vari in vars_factor) {
      # level and number of levels for this vari
      var_levels <- levels(dataframe[, vari])
      var_nlevels <- nlevels(dataframe[, vari])

      if (force_generate_1_when_0 && var_nlevels == 1 && (var_levels == 0 || tolower(var_levels) == "non")) {
        message("[compute_factorial_table] force_generate_1_when_0 for ", vari)
        new_level <- ifelse(
          var_levels %in% 0,
          1,
          "Oui"
        )
        var_levels <- c(var_levels, new_level)
        dataframe[[vari]] <- factor(dataframe[[vari]], levels = var_levels, labels = var_levels)
        var_nlevels <- nlevels(dataframe[, vari])
      }

      # init resulting dataframe for this variable
      effectifs[[vari]] <- as.data.frame(matrix(NA, ncol = 2, nrow = var_nlevels + 2))
      rownames(effectifs[[vari]]) <- c(var_levels, "N_NA", "Nb_mesures")
      colnames(effectifs[[vari]]) <- c("n", "p")

      # counts and proportions of modalities
      effectifs[[vari]][var_levels, "n"] <- table(dataframe[, vari])
      pt <- prop.table(table(dataframe[, vari]))
      pt[is.nan(pt)] <- 0
      effectifs[[vari]][var_levels, "p"] <- paste(
        "(", round(100 * pt, digits), "%)",
        sep = ""
      )

      # counts et proportions des NA
      effectifs[[vari]][var_nlevels + 1, "n"] <- sum(is.na(dataframe[, vari]))
      effectifs[[vari]][var_nlevels + 1, "p"] <- "/"

      # number of observations
      effectifs[[vari]][var_nlevels + 2, "n"] <- sum(is.na(dataframe[, vari]) == FALSE)
      effectifs[[vari]][var_nlevels + 2, "p"] <- "/"
    }

    if (simplify) {
      final_table <- lapply(X = effectifs, FUN = function(vari_tab) {
        if (!is.null(vari_tab) && nrow(vari_tab) == 4) {
          if (
            all(c("0", "1") %in% rownames(vari_tab)) |
              all(c("oui", "non") %in% tolower(rownames(vari_tab)))
          ) {
            if (any(grepl("1", rownames(vari_tab)))) {
              vari_tab <- vari_tab[c("1", "N_NA", "Nb_mesures"), ]
            } else {
              # ignore.case about oui
              row_select <- grep("OUI", rownames(vari_tab), ignore.case = TRUE, value = TRUE)
              vari_tab <- vari_tab[c(row_select, "N_NA", "Nb_mesures"), ]
            }
          }
        }
        return(vari_tab)
      })
    } else { # do not simplify
      final_table <- effectifs
    }
  } else { # bivariate analyse

    # possible only on factorial varstrat
    # message("[compute_factorial_table] bivariate analyse : ", varstrat)
    # message(paste0(names(dataframe), collapse = ", "))
    stopifnot(varstrat %in% names(dataframe))
    stopifnot(is.factor(dataframe[, varstrat]))

    # table, modalitiess and numbre of modalities for varstrat
    # eff <- table(dataframe[, varstrat]) # lintr : may be not used
    varstrat_levels <- levels(dataframe[, varstrat])
    varstrat_nlevels <- nlevels(dataframe[, varstrat])

    # iterations
    for (vari in vars_factor) {
      var_levels <- levels(dataframe[, vari])
      var_nlevels <- nlevels(dataframe[, vari])

      if (force_generate_1_when_0 && var_nlevels == 1 && (var_levels == 0 || tolower(var_levels) == "non")) {
        message("[compute_factorial_table] force_generate_1_when_0 for ", vari)
        new_level <- ifelse(
          var_levels %in% 0,
          1,
          "Oui"
        )
        var_levels <- c(var_levels, new_level)
        dataframe[[vari]] <- factor(dataframe[[vari]], levels = var_levels, labels = var_levels)
        var_nlevels <- nlevels(dataframe[, vari])
      }

      # init resulting dataframe for this variable
      effectifs[[vari]] <- as.data.frame(matrix(
        NA,
        ncol = 2 * varstrat_nlevels + 2, nrow = var_nlevels + 2
      ))
      rownames(effectifs[[vari]]) <- c(var_levels, "N_NA", "Nb_mesures")
      colnames(effectifs[[vari]]) <- c(
        # "n", "p", paste(c("n", "p"), sort(rep(varstrat_levels, 2)), sep = "") ## sort to remove (v0.1.3)
        "n", "p", paste(c("n", "p"), rep(varstrat_levels, each = 2), sep = "")
      )

      # counts and proportions of modalities
      effectifs[[vari]][var_levels, "n"] <- table(dataframe[, vari])
      pt <- prop.table(table(dataframe[, vari]))
      pt[is.nan(pt)] <- 0
      effectifs[[vari]][var_levels, "p"] <- paste(
        "(", round(100 * pt, digits), "%)",
        sep = ""
      )

      # counts and proportions NA
      effectifs[[vari]][var_nlevels + 1, "n"] <- sum(is.na(dataframe[, vari]))
      effectifs[[vari]][var_nlevels + 1, "p"] <- "/"

      # number of observations
      effectifs[[vari]][var_nlevels + 2, "n"] <- sum(is.na(dataframe[, vari]) == FALSE)
      effectifs[[vari]][var_nlevels + 2, "p"] <- "/"

      ## then counts according the varstart

      # counts and proportions of modalities per level of varstrat
      effectifs[[vari]][var_levels, paste("n", varstrat_levels, sep = "")] <- table(
        dataframe[, c(vari, varstrat)]
      )
      pt_varstrat <- prop.table(table(dataframe[, c(vari, varstrat)]), prop_table_margin)
      pt_varstrat[is.nan(pt_varstrat)] <- 0
      effectifs[[vari]][var_levels, paste("p", varstrat_levels, sep = "")] <- paste(
        "(", round(100 * pt_varstrat, digits), "%)",
        sep = ""
      )

      # counts and proportions NA per level of varstrat
      effectifs[[vari]][var_nlevels + 1, paste("n", varstrat_levels, sep = "")] <- table(
        dataframe[which(is.na(dataframe[, vari])), varstrat]
      )
      effectifs[[vari]][var_nlevels + 1, paste("p", varstrat_levels, sep = "")] <- "/"

      # number of observations per level of varstrat
      effectifs[[vari]][var_nlevels + 2, paste("n", varstrat_levels, sep = "")] <- table(
        dataframe[which(is.na(dataframe[, vari]) == FALSE), varstrat]
      )
      effectifs[[vari]][var_nlevels + 2, paste("p", varstrat_levels, sep = "")] <- "/"
    }
    if (simplify) {
      final_table <- lapply(X = effectifs, FUN = function(vari_tab) {
        if (!is.null(vari_tab) && nrow(vari_tab) == 4) {
          if (
            all(c("0", "1") %in% rownames(vari_tab)) |
              all(c("oui", "non") %in% tolower(rownames(vari_tab)))
          ) {
            if (any(grepl("1", rownames(vari_tab)))) {
              vari_tab <- vari_tab[c("1", "N_NA", "Nb_mesures"), ]
            } else {
              # ignore.case about oui
              row_select <- grep("OUI", rownames(vari_tab), ignore.case = TRUE, value = TRUE)
              vari_tab <- vari_tab[c(row_select, "N_NA", "Nb_mesures"), ]
            }
          }
        }
        return(vari_tab)
      })
    } else { # do not simplify
      final_table <- effectifs
    }
  }

  return(final_table)
}


#' compute SMD
#'
#' standardized mean difference
#' 
#' Interpretation of SMD values : The generally accepted thresholds for interpreting effect size are :  
#' SMD < 0.1: negligible difference,  
#' 0.1 ≤ SMD < 0.2: small difference,  
#' 0.2 ≤ SMD < 0.5: moderate difference,  
#' SMD ≥ 0.5: large difference.  
#' These thresholds help to assess the balance between groups, especially after matching or adjustment procedures.
#'
#' @param dataframe A data.frame. tibble or data.table
#' @param vars A vector of characters. Names of dataframe's numeric columns.
#' @param varstrat A character. Default NULL. Name of the stratification variable, making groups to compare.
#' @param digits A integer, Default 2. Integer indicating the number of decimal places (round).
#' 
#' @return A table of SMD values for each vars.
#' 
#' @export
#' @examples
#' \dontrun{
#' compute_SMD_table(
#'   dataframe = modified_state,
#'   vars = c(
#'     "Population", "Income", "Illiteracy", "Life Exp", "Murder",
#'     "HS Grad", "Frost", "Area"
#'   ),
#'   varstrat = "election",
#'   digits = 2
#'  )
#' }
compute_SMD_table <- function(
  dataframe, 
  vars, 
  varstrat,
  digits = 2
) {

  ## stops
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(varstrat %in% names(dataframe))
  stopifnot(digits >= 0)
  
  stopifnot(nlevels(dataframe[[varstrat]]) == 2)

  # detection of numeric varibles
  vars_numeric <- get_numerics(dataframe, vars)
  if (!all(vars %in% vars_numeric)) {
    message("[compute_SMD_table] Warning, some of selected vars were ignored (not numeric).")
  }
  
  # compute stats corr for each variables
  SMD_tab <- data.table::rbindlist(lapply(X = vars_numeric, function(vari) {
    x <- dataframe[[vari]]
    g <- dataframe[[varstrat]]
    g_levels <- levels(dataframe[[varstrat]])
    
    SMD_val <- (
        mean(x[g %in% g_levels[1]], na.rm = TRUE) -
          mean(x[g %in% g_levels[2]], na.rm = TRUE)
      ) / (
        sqrt(
          (
            var(x[g %in% g_levels[1]], na.rm = TRUE) +
              var(x[g %in% g_levels[2]], na.rm = TRUE)
          ) / 2
        )
      )
    return(
      data.table(
        Variable = vari,
        SMD = round(x = SMD_val, digits = digits)
      )
    )
  }), use.names = TRUE, fill = TRUE)
  
  return(SMD_tab)
}
