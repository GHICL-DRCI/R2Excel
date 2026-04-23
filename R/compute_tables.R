# Core function of the pkg, compute descriptive tables


#### quantitative vars ####

#' compute_single_var_level_stats
#' 
#' Compute descriptive statistics for a single continuous variable (unit function)
#'
#' Internal function that calculates all descriptive statistics for one continuous
#' variable, with optional filtering by stratification level.
#'
#' @param var_name Variable name (character)
#' @param dataframe Data.frame containing the data
#' @param level Stratification level (NULL for global population)
#' @param varstrat Stratification variable name (NULL if none)
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#'
#' @return Data.frame with one row containing: mean, sd, median, Q1, Q3,
#'   min, max, N, Valeurs_manquantes, Nb_mesures, is_Normal
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- data.frame(
#' age = c(25, 30, 35, 40), # Entiers -> 0 décimale
#' poids = c(70.5, 75.2, 68.9, 72.1), # 1 décimale
#' taille = c(1.75, 1.82, 1.68, 1.79), # 2 décimales
#' biomarqueur = c(0.0125, 0.0138, 0.0142, 0.0129) # 4 décimales
#' )
#' compute_single_var_level_stats(
#'   dataframe = data,
#'   var_name = "age",
#'   precision = "auto"
#' )
#' compute_single_var_level_stats(
#'  dataframe = data,
#'  var_name = "biomarqueur",
#'  precision = "auto"
#' )
#' compute_single_var_level_stats(
#'   dataframe = data,
#'   var_name = "biomarqueur",
#'   precision = NULL
#' )
#' }
compute_single_var_level_stats <- function(
    dataframe,
    var_name,
    level = NULL, 
    varstrat = NULL,
    precision = "auto"
) {
  # message("[compute_single_var_level_stats]")
  # Pour une variable : Données décrites avec différentes précisions
  
  stopifnot(precision == "auto" || is.numeric(precision))
  
  # Extraction des données selon le niveau
  if (is.null(level)) {
    x <- dataframe[[var_name]]
  } else {
    x <- dataframe[dataframe[[varstrat]] == level, var_name]
  }
  
  # Déterminer les digits à utiliser
  if (precision %in% "auto") {
    base_decimals <- detect_decimal_places(x)
    digits_central <- compute_precision_digits(x, "central", base_decimals)
    digits_sd <- compute_precision_digits(x, "sd", base_decimals, max_decimals = 3)
  } else {
    digits_central <- precision
    digits_sd <- precision
  }
  
  # Calcul du test de Shapiro
  tmp_shapi <- stats::na.omit(x)
  if (length(unique(tmp_shapi)) == 1 || length(tmp_shapi) < 3) {
    shapiro_conclu <- NA
  } else {
    normality_result <- check_normality(tmp_shapi, return_messages = TRUE)
    shapiro_conclu <- if (nchar(normality_result$message) > 0) {
      NA  # Erreur lors du test
    } else {
      normality_result$is_normal
    }
  }
  
  # Calcul de toutes les statistiques avec précision adaptée
  stats_vec <- c(
    mean = round(mean(x, na.rm = TRUE), digits_central),
    sd = round(stats::sd(x, na.rm = TRUE), digits_sd),
    median = round(stats::median(x, na.rm = TRUE), digits_central),
    Q1 = round(stats::quantile(x, 0.25, na.rm = TRUE), digits_central)[[1]],
    Q3 = round(stats::quantile(x, 0.75, na.rm = TRUE), digits_central)[[1]],
    min = round(ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE)), digits_central),
    max = round(ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE)), digits_central),
    N = length(x),
    Valeurs_manquantes = sum(is.na(x)),
    Nb_mesures = sum(!is.na(x)),
    is_Normal = shapiro_conclu
  )
  
  # Nom de ligne
  if (is.null(level)) {
    row_name <- if (is.null(varstrat) || varstrat %in% "") var_name else ""
  } else {
    row_name <- paste0(varstrat, level)
  }
  
  # Retourner un data.frame d'une ligne
  df_result <- as.data.frame(t(stats_vec), stringsAsFactors = FALSE)
  rownames(df_result) <- row_name
  return(df_result)
}



#' compute continuous table
#'
#' provide descriptive statistics table for continuous data
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.frame.
#' @param vars A vector of characters. Names of dataframe's continuous columns to describe.
#' @param varstrat A character. Default NULL. Name of the stratification variable,
#'  making groups to compare.
#' @param stats_choice A vector of characters. Default provide all usual statistics
#'  to describe continuous variables,
#'  namely 'c("mean", "sd", "median", "Q1", "Q3", "min", "max", "N", 
#'  "Valeurs_manquantes", "Nb_mesures", "is_Normal")'
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
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
#'   precision = 2
#' )
#' }
compute_continuous_table <- function(
    dataframe,
    vars = setdiff(colnames(dataframe), varstrat),
    varstrat = NULL,
    stats_choice = c(
      "mean", "sd", "median", "Q1", "Q3", "min", "max", "N",
      "Valeurs_manquantes", "Nb_mesures", "is_Normal"
    ),
    precision = "auto"  # new v0.1.27
) {
  message("[compute_continuous_table]")
  dataframe <- as.data.frame(dataframe)
  
  ## Validations
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(all(stats_choice %in% c(
    "mean", "sd", "median", "Q1", "Q3", "min", "max", "N", 
    "Valeurs_manquantes", "Nb_mesures", "is_Normal"
  )))
  stopifnot(precision == "auto" || is.numeric(precision))
  
  # Détection des variables numériques
  vars_numeric <- get_numerics(dataframe, vars)
  if (!all(vars %in% vars_numeric)) {
    message(
      paste0(
        "[compute_continuous_table] ",
        "Warning, some of selected vars were ignored (not numeric)."
      )
    )
  }
  
  # =
  # CAS 1 : Analyse univariée (sans varstrat)
  # =
  if (is.null(varstrat) || varstrat %in% "") {
    statistics_list <- lapply(
      X = vars_numeric,
      FUN = compute_single_var_level_stats, 
      dataframe = dataframe, 
      level = NULL,
      varstrat = NULL,
      precision = precision
    )
    sumup <- do.call(rbind, statistics_list)
    
    # Sélection des statistiques demandées
    final_table <- sumup[, stats_choice, drop = FALSE]
    
    return(final_table)
  }
  
  # =
  # CAS 2 : Analyse bivariée (avec varstrat)
  # =
  stopifnot(varstrat %in% names(dataframe))
  
  # Récupération des niveaux de varstrat
  varstrat_levels <- levels(dataframe[[varstrat]])
  
  # Analyse descriptive bivariée via lapply
  statistics <- lapply(vars_numeric, function(var_name) {
    # Stats pour la population globale (première ligne)
    global_stats <- compute_single_var_level_stats(
      var_name = var_name,
      dataframe = dataframe,
      level = NULL,
      varstrat = NULL,
      precision = precision
    )
    
    # Stats pour chaque niveau de stratification
    level_stats_list <- lapply(varstrat_levels, function(level) {
      compute_single_var_level_stats(
        var_name = var_name,
        dataframe = dataframe,
        level = level,
        varstrat = varstrat,
        precision = precision
      )
    })
    
    # Combiner global + tous les niveaux
    all_stats <- do.call(rbind, c(list(global_stats), level_stats_list))
    
    return(all_stats)
  })
  names(statistics) <- vars_numeric
  
  # Sélection des statistiques demandées
  final_table <- lapply(statistics, function(df) {
    df[, stats_choice, drop = FALSE]
  })
  
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
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#' @param signif_digits A integer, 
#'   Default 4. Integer indicating the number of decimal places (signif) for pvalues.
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
#'   precision = 2
#' )
#' }
compute_correlation_table <- function(
    dataframe,
    vars = setdiff(colnames(dataframe), varstrat),
    varstrat = varstrat, # needed
    method_corr = "detect_auto",
    precision = 2,
    signif_digits = 4
) {
  message("[compute_correlation_table]")
  
  dt <- data.table::setDT(data.table::copy(dataframe))
  
  ## stops
  stopifnot(all(vars %in% names(dt)))
  stopifnot(!(is.null(varstrat) || (varstrat %in% "")))
  stopifnot(all(varstrat %in% names(dt)))
  stopifnot(is.numeric(dt[[varstrat]]))
  stopifnot(precision == "auto" || is.numeric(precision))
  stopifnot(signif_digits > 0)
  stopifnot(method_corr %in% c("detect_auto", "pearson", "spearman", "kendall"))
  
  # detection of numeric varibles
  vars_numeric <- get_numerics(dt, vars)
  if (!all(vars %in% vars_numeric)) {
    message("[compute_correlation_table] Warning, ",
            "some of selected vars were ignored (not numeric).")
  }
  
  # compute stats corr for each variables
  statistics_tab <- lapply(X = vars_numeric, function(vari) {
    message("[compute_correlation_table] ", vari)
    
    N_missingval <- sum(is.na(dt[[vari]])) # in total
    
    tmp_dt <- stats::na.omit(dt[, .SD, .SDcols = c(vari, varstrat)])
    
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
    
    # Déterminer les digits à utiliser
    if (precision %in% "auto") {
      base_decimals <- detect_decimal_places(tmp_dt[[vari]])
      digits <- base_decimals + 1 
    } else {
      digits <- precision
    }
    
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
      IC95 <- paste0(
        "[",
        paste0(round(corr_obj$conf.int, digits = digits), collapse = " , "),
        "]"
      )
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
        Q1 = round(stats::quantile(.SD[[1]], na.rm = TRUE)["25%"], digits = digits)[[1]],
        Q3 = round(stats::quantile(.SD[[1]], na.rm = TRUE)["75%"], digits = digits)[[1]],
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



#### qualitative vars ####

#' compute_single_factor_stats
#'
#' Unit function for a factorial variable
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.frame.
#' @param vars A vector of characters. Names of dataframe's factorial columns to describe.
#' @param varstrat A character. Default NULL. Name of the stratification variable,
#'  making groups to compare.
#' @param varstrat_levels  --here 
#' @param simplify A logical. Default TRUE. Reduce the yes/no  (or 1/0) modalities 
#' to display only the yes (1).
#' @param prop_table_margin A vector giving the margins to split by. Default 2. 
#'  1 indicates rows, 2 indicates columns,
#'  c(1, 2) indicates rows and columns. When x has named dimnames, 
#'  it can be a character vector selecting dimension names.
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#' @param force_generate_1_when_0 A logical, Default FALSE. If TRUE, 
#'  will test if the unique modality is 0 or "non" and
#'  add the level 1 or "oui" so it can be display in counts. 
#'  Can be combined with simplify to only show the modality (1).
#'  
#' 
#' @return a table of n and p for each levels
#'  + Nb measures and N of missing values (Valeurs_manquantes)
#'  
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   gender = as.factor(c("F", "M", "F", "F", "M")),
#'   ttt = as.factor(c("a", "a", "a", "b", NA)),
#'   success = as.factor(c(1, 1, 1, NA, NA))
#' )
#' compute_single_factor_stats(dataframe = data, var_name = "gender")
#' compute_single_factor_stats(dataframe = data, var_name = "success")
#' compute_single_factor_stats(
#'   dataframe = data, var_name = "ttt", 
#'   varstrat = "gender" , varstrat_levels = levels(data$gender)
#' )
#' }
compute_single_factor_stats <- function(
  dataframe,
  var_name, 
  varstrat = NULL, 
  varstrat_levels = NULL,
  prop_table_margin = 2, 
  precision = 1,
  force_generate_1_when_0 = FALSE
) {
  # message("[compute_single_factor_stats]")
  
  # Gestion du force_generate_1_when_0
  var_levels <- levels(dataframe[[var_name]])
  var_nlevels <- nlevels(dataframe[[var_name]])
  
  if (
    force_generate_1_when_0 && var_nlevels == 1 && 
      (var_levels == 0 || tolower(var_levels) == "non")
  ) {
    # message("[compute_single_factor_stats] force_generate_1_when_0 for ", var_name)
    new_level <- ifelse(var_levels %in% 0, 1, "Oui")
    var_levels <- c(var_levels, new_level)
    dataframe[[var_name]] <- factor(
      dataframe[[var_name]], levels = var_levels, labels = var_levels
    )
    var_nlevels <- nlevels(dataframe[[var_name]])
  }
  
  # Déterminer les colonnes nécessaires
  if (is.null(varstrat) || varstrat %in% "") {
    # Cas univarié = no varstrat
    col_names <- c("n", "p")
    ncols <- 2
  } else {
    # Cas bivarié = varstrat
    col_names <- c(
      "n", "p", 
      paste(c("n", "p"), rep(varstrat_levels, each = 2), sep = "")
    )
    ncols <- 2 * length(varstrat_levels) + 2
  }
  
  # Init du dataframe de résultats
  result <- as.data.frame(matrix(NA, ncol = ncols, nrow = var_nlevels + 2))
  rownames(result) <- c(var_levels, "Valeurs_manquantes", "Nb_mesures")
  colnames(result) <- col_names
  
  # =
  # Statistiques globales (colonnes "n" et "p")
  # =
  
  # Déterminer les digits à utiliser
  if (precision %in% "auto") {
    digits <- 1 
  } else {
    digits <- precision
  }
  
  result[var_levels, "n"] <- table(dataframe[[var_name]])
  pt <- prop.table(table(dataframe[[var_name]]))
  pt[is.nan(pt)] <- 0
  result[var_levels, "p"] <- paste0("(", round(100 * pt, digits), "%)")
  
  result["Valeurs_manquantes", "n"] <- sum(is.na(dataframe[[var_name]]))
  result["Valeurs_manquantes", "p"] <- "/"
  
  result["Nb_mesures", "n"] <- sum(!is.na(dataframe[[var_name]]))
  result["Nb_mesures", "p"] <- "/"
  
  # =
  # Statistiques par niveau de varstrat (si applicable)
  # =
  if (!(is.null(varstrat) || varstrat %in% "")) {
    # Effectifs et proportions par niveau de varstrat
    cross_tab <- table(dataframe[[var_name]], dataframe[[varstrat]])
    result[var_levels, paste0("n", varstrat_levels)] <- cross_tab
    
    pt_varstrat <- prop.table(cross_tab, prop_table_margin)
    pt_varstrat[is.nan(pt_varstrat)] <- 0
    result[var_levels, paste0("p", varstrat_levels)] <- paste0(
      "(", round(100 * pt_varstrat, digits), "%)"
    )
    
    # NA par niveau de varstrat
    na_by_strat <- table(dataframe[is.na(dataframe[[var_name]]), varstrat])
    result["Valeurs_manquantes", paste0("n", varstrat_levels)] <- na_by_strat
    result["Valeurs_manquantes", paste0("p", varstrat_levels)] <- "/"
    
    # Nb mesures par niveau de varstrat
    nonna_by_strat <- table(dataframe[!is.na(dataframe[[var_name]]), varstrat])
    result["Nb_mesures", paste0("n", varstrat_levels)] <- nonna_by_strat
    result["Nb_mesures", paste0("p", varstrat_levels)] <- "/"
  }
  
  return(result)
}


# Fonction de simplification (pour variables binaires)
#' simplify_binary_table
#' @param vari_tab A data.frame. 
#'  
#' @return vari_tab simplifyed
#'  
#' @export
simplify_binary_table <- function(
  vari_tab
) {
  if (!is.null(vari_tab) && nrow(vari_tab) == 4) {
    if (all(c("0", "1") %in% rownames(vari_tab)) ||
        all(c("oui", "non") %in% tolower(rownames(vari_tab)))) {
      
      if (any(grepl("1", rownames(vari_tab)))) {
        vari_tab <- vari_tab[c("1", "Valeurs_manquantes", "Nb_mesures"), ]
      } else {
        # ignore.case pour "oui"
        row_select <- grep("OUI", rownames(vari_tab), ignore.case = TRUE, value = TRUE)
        vari_tab <- vari_tab[c(row_select, "Valeurs_manquantes", "Nb_mesures"), ]
      }
    }
  }
  return(vari_tab)
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
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
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
#' data <- data.frame(
#'   gender = as.factor(c("F", "M", "F", "F", "M")),
#'   ttt = as.factor(c("a", "a", "a", "b", NA)),
#'   success = as.factor(c(1, 1, 1, NA, NA))
#' )
#' compute_factorial_table(dataframe = data, vars = names(data))
#' 
#' compute_factorial_table(
#'   dataframe = modified_state,
#'   vars = c("state.division", "state.region", "binary_test"),
#'   varstrat = "election",
#'   precision = 2,
#'   simplify = FALSE
#' )
#' vars_wanted <- c("yes_no_french_question", "all_count_zero")
#' varstrat_wanted <- "election"
#' res_fact_simpl0_tabs <- compute_factorial_table(
#'   dataframe = modified_state,
#'   vars = vars_wanted,
#'   varstrat = varstrat_wanted,
#'   precision = 2,
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
    precision = 1,
    force_generate_1_when_0 = FALSE
) {
  message("[compute_factorial_table]")
  dataframe <- as.data.frame(dataframe)
  
  ## Validations
  stopifnot(all(vars %in% names(dataframe)))
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(force_generate_1_when_0))
  # stopifnot(digits >= 0)
  stopifnot(precision == "auto" || is.numeric(precision))
  if (force_generate_1_when_0) message("[compute_factorial_table] force_generate_1_when_0")

  # Récupération des variables factorielles
  vars_factor <- get_factors(dataframe, vars)
  if (!all(vars %in% vars_factor)) {
    message("[compute_factorial_table] Warning, some of selected vars were ignored (not factors).")
  }
  
  # =
  # CAS 1 : Analyse univariée (sans varstrat)
  # =
  if (is.null(varstrat) || varstrat %in% "") {
    effectifs <- lapply(vars_factor, function(var_name) {
      compute_single_factor_stats(
        var_name = var_name,
        dataframe = dataframe,
        varstrat = NULL,
        varstrat_levels = NULL,
        prop_table_margin = prop_table_margin,
        precision = precision,
        force_generate_1_when_0 = force_generate_1_when_0
      )
    })
    names(effectifs) <- vars_factor
    
    # Simplification si demandée
    if (simplify) {
      final_table <- lapply(effectifs, simplify_binary_table)
    } else {
      final_table <- effectifs
    }
    
    return(final_table)
  }
  
  # =
  # CAS 2 : Analyse bivariée (avec varstrat)
  # =
  stopifnot(varstrat %in% names(dataframe))
  stopifnot(is.factor(dataframe[[varstrat]]))
  
  varstrat_levels <- levels(dataframe[[varstrat]])
  
  effectifs <- lapply(vars_factor, function(var_name) {
    compute_single_factor_stats(
      var_name = var_name,
      dataframe = dataframe,
      varstrat = varstrat,
      varstrat_levels = varstrat_levels,
      prop_table_margin = prop_table_margin,
      precision = precision,
      force_generate_1_when_0 = force_generate_1_when_0
    )
  })
  names(effectifs) <- vars_factor
  
  # Simplification si demandée
  if (simplify) {
    final_table <- lapply(effectifs, simplify_binary_table)
  } else {
    final_table <- effectifs
  }
  
  return(final_table)
}

