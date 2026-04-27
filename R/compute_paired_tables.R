# Core function of the pkg, compute descriptive tables for paired data

# =
#### Numerics ####
# =

# =
# Fonctions utilitaires
# =


# Formater le contenu de cellule selon la métrique
#' format_cell_content
#' @param stats A data.frame 
#' @param metric A character
#'  
#' @return cell formated
#'  
#' @keywords internal
format_cell_content <- function(
  stats, 
  metric = "mean"
) {
  # message("[format_cell_content]")
  if (metric == "mean") {
    if (is.na(stats$mean) || is.nan(stats$mean)) {
      return("/")
    }
    return(paste0(stats$mean, " +/- ", stats$sd))
  } else {
    # median
    if (is.na(stats$median) || is.nan(stats$median)) {
      return("/")
    }
    return(paste0(
      stats$median," [", stats$Q1, ";", stats$Q3, "]"
    ))
  }
}

# Formater la description de population globale
#' format_global_summary
#' @param dataframe A data.frame
#' @param variable A character
#' @param metric A character
#' @param patient_id A character or NULL
#' @param digits_central A integer
#' @param digits_sd A integer
#'  
#' @return global summary formated
#'  
#' @keywords internal
format_global_summary <- function(
  dataframe,
  variable,
  metric = "mean",
  patient_id = NULL,
  digits_central,
  digits_sd
) {
  # message("[format_global_summary]")
  if (metric == "mean") {
    result <- paste0(
      round(
        mean(dataframe[[variable]], na.rm = TRUE),
        digits = digits_central),
      " +/- ", round(
        stats::sd(dataframe[[variable]], na.rm = TRUE),
        digits = digits_sd)
    )
  } else {
    result <- paste0(
      round(stats::median(
        dataframe[[variable]], na.rm = TRUE), digits = digits_central),
      " [", round(
        stats::quantile(dataframe[[variable]], 0.25, na.rm = TRUE),
        digits = digits_central), " ; ",
      round(
        stats::quantile(dataframe[[variable]], 0.75, na.rm = TRUE),
        digits = digits_central), "]"
    )
  }
  
  if (!is.null(patient_id)) {
    result <- paste0(result, " (n=", length(unique(dataframe[[patient_id]])), ")")
  }
  
  return(result)
}

# =
# Fonction pour le cas : 2 niveaux
# avec gestion variable absente d'un niveau
# =
#' handle_two_levels
#' @param dt_wide A data.table wide format
#' @param variable_interest A character
#' @param varstrat A character
#' @param patient_id A character
#' @param digits_central A integer
#' @param digits_sd A integer
#' @param show_metric A character
#' @param force_parametric_test A logical
#' @param force_non_parametric_test A logical
#' @param global_summary A logical
#' @param dt A data.table
#'  
#' @return a two level description
#'  
#' @keywords internal
handle_two_levels <- function(
  dt_wide, 
  variable_interest, 
  varstrat,
  patient_id,
  digits_central,
  digits_sd,
  show_metric,
  force_parametric_test, 
  force_non_parametric_test, 
  global_summary, 
  dt
) {
  
  message("[handle_two_levels] 2 levels in ", varstrat)
  
  # === VÉRIFICATION : La variable est-elle présente dans les 2 niveaux ? ===
  if (ncol(dt_wide) != 3) {
    # La variable n'est présente que dans 1 seul niveau
    ongoing_message <- "Variable present only in 1 level of varstrat, no paired test applicable"
    message("[handle_two_levels] ", ongoing_message)
    
    N_individuals <- nrow(dt_wide)
    test_result <- list(p.value = NA)
    test_used <- "/"
    normal_data <- NA
    
    # Déterminer quelle métrique utiliser
    metric_to_show <- if (show_metric %in% c("auto", "median")) {
      "median"
    } else {
      "mean"
    }
    
    Population_totale <- if (global_summary) {
      format_global_summary(
        dataframe = dt,
        variable = variable_interest,
        metric = metric_to_show, 
        digits_central = digits_central,
        digits_sd = digits_sd
      )
    } else {
      NULL
    }
    
    return(list(
      test_result = test_result,
      test_used = test_used,
      ongoing_message = ongoing_message,
      normal_data = normal_data,
      N_individuals = N_individuals,
      metric_to_show = metric_to_show,
      Population_totale = Population_totale,
      Difference_description = NULL
    ))
  }
  
  # === CODE EXISTANT (seulement si ncol == 3) ===
  dt_wide <- stats::na.omit(dt_wide)
  N_individuals <- nrow(dt_wide)
  
  vec1 <- unlist(dt_wide[, 2], use.names = FALSE)
  vec2 <- unlist(dt_wide[, 3], use.names = FALSE)
  diff <- vec2 - vec1
  
  # Description de la différence
  Difference_description <- paste0(
    "mean = ", round(mean(diff, na.rm = TRUE), digits = digits_central),
    " +/- ", round(stats::sd(diff, na.rm = TRUE), digits = digits_sd),
    " ",
    "median = ", round(stats::median(diff, na.rm = TRUE), digits = digits_central),
    " [", round(stats::quantile(diff, 0.25, na.rm = TRUE), digits = digits_central), " ; ",
    round(stats::quantile(diff, 0.75, na.rm = TRUE), digits = digits_central), "]"
  )
  
  # Test de normalité sur la différence
  normality_check <- check_normality(diff, return_messages = TRUE)
  normal_data <- normality_check$is_normal
  msg_captured_norm <- normality_check$message
  
  # Décision du test
  use_parametric <- (normal_data || force_parametric_test) && !force_non_parametric_test
  
  if (use_parametric) {
    # ========== Paired t-test ==========
    test_result <- stats::t.test(vec2, vec1, paired = TRUE)
    test_used <- "Paired t-test"
    
    # Message si forcé
    ongoing_message <- if (force_parametric_test && !normal_data) {
      "force_parametric_test.\n"
    } else {
      ""
    }
    
    # Déterminer la métrique à afficher
    if (show_metric %in% c("auto", "mean")) {
      metric_to_show <- "mean"
      Difference_description <- gsub(
        "mean = (.*) median = (.*)", "\\1", Difference_description)
    } else {
      metric_to_show <- "median"
      message(
        "[handle_two_levels]",
        " /!\\ Caution median forced in cell content. ",
        "It may cause a disconnection between metrics and tests /!\\"
      )
      Difference_description <- gsub(
        "mean = (.*) median = (.*)", "\\2", Difference_description)
    }
    
  } else {
    # ========== Wilcoxon signed-rank test ==========
    
    # Exécuter le test avec gestion d'erreurs
    has_issues_w <- try(tools::assertCondition(
      test_result <- stats::wilcox.test(vec2, vec1, paired = TRUE)
    ), silent = TRUE)
    
    has_issues_w <- has_issues_w[
      sapply(has_issues_w, function(el) {
        length(base::intersect(class(el), c("warning", "error"))) != 0
      })
    ]
    
    msg_captured_wilcox <- if (length(has_issues_w) == 0) {
      ""
    } else {
      paste(
        unique(sapply(has_issues_w, function(el) {
          paste0("[", class(el)[2], "] ", el$message)
        })),
        collapse = ";"
      )
    }
    
    test_used <- "Wilcoxon signed-rank test (paired data)"
    
    # Message si forcé
    ongoing_message <- if (force_non_parametric_test && normal_data) {
      "force_non_parametric_test.\n"
    } else {
      ""
    }
    ongoing_message <- paste0(
      ongoing_message, msg_captured_wilcox, msg_captured_norm)
    
    # Déterminer la métrique à afficher
    if (force_non_parametric_test || show_metric %in% c("auto", "median")) {
      metric_to_show <- "median"
      Difference_description <- gsub(
        "mean = (.*) median = (.*)", "\\2", Difference_description)
    } else {
      metric_to_show <- "mean"
      message(
        "[handle_two_levels]",
        " /!\\ Caution mean forced in cell content. ",
        "It may cause a disconnection between metrics and tests /!\\"
      )
      ongoing_message <- paste0(
        ongoing_message,
        "!Caution mean forced in cell content. ",
        "It may cause a disconnection between metrics and tests!"
      )
      Difference_description <- gsub(
        "mean = (.*) median = (.*)", "\\1", Difference_description)
    }
  }
  
  # Préparer la population totale si demandée
  Population_totale <- if (global_summary) {
    format_global_summary(
      dataframe = dt, 
      variable = variable_interest, 
      metric = metric_to_show,
      digits_central = digits_central,
      digits_sd = digits_sd,
      patient_id = if (metric_to_show == "median") patient_id else NULL 
    )
    
  } else {
    NULL
  }
  
  return(list(
    test_result = test_result,
    test_used = test_used,
    ongoing_message = ongoing_message,
    normal_data = normal_data,
    N_individuals = N_individuals,
    metric_to_show = metric_to_show,
    Population_totale = Population_totale,
    Difference_description = Difference_description
  ))
}

# =
# Fonction pour le cas : Plus de 2 niveaux
# =
#' handle_multiple_levels
#' @param dt_wide A data.table wide format
#' @param variable_interest A character
#' @param varstrat A character
#' @param patient_id A character
#' @param digits_central A integer
#' @param digits_sd A integer
#' @param show_metric A character
#' @param force_parametric_test A logical
#' @param force_non_parametric_test A logical
#' @param global_summary A logical
#' @param do_test A logical
#' @param dt A data.table
#'  
#' @return a description for more than 2 levels
#'  
#' @keywords internal
handle_multiple_levels <- function(
  dt_wide, 
  variable_interest, 
  varstrat, 
  patient_id,
  digits_central,
  digits_sd,
  show_metric, 
  force_parametric_test,
  force_non_parametric_test,
  global_summary,
  do_test,
  dt
) {
  message("[handle_multiple_levels] in ", varstrat)
  Nobs <- value <- NULL
  
  # Convertir dt_wide en data.table si ce n'est pas déjà le cas
  if (!data.table::is.data.table(dt_wide)) {
    dt_wide <- data.table::setDT(dt_wide)
  }
  
  # Test de normalité sur chaque colonne
  cols_to_test <- setdiff(names(dt_wide), patient_id)
  
  # Initialiser
  normal_data <- rep(FALSE, length(cols_to_test))
  names(normal_data) <- cols_to_test
  msg_captured_norm <- ""
  
  for (col in cols_to_test) {
    x <- stats::na.omit(dt_wide[[col]])
    
    if (length(x) < 3 || length(unique(x)) == 1) {
      normal_data[col] <- FALSE
      next
    }
    
    # Test de normalité avec gestion d'erreurs
    normality_result <- check_normality(x, return_messages = TRUE)
    normal_data[col] <- normality_result$is_normal
    
    if (nchar(normality_result$message) > 0) {
      msg_captured_norm <- paste0(
        msg_captured_norm,
        if (nchar(msg_captured_norm) > 0) "; " else "",
        normality_result$message
      )
    }
  }
  
  # Créer format long
  dt_long <- data.table::melt(data = dt_wide, id.vars = patient_id)
  # NE PAS convertir patient_id en facteur - garder tel quel
  dt_long$variable <- as.factor(dt_long$variable)
  
  # Vérifier si design complet
  any_na_in_dt <- sapply(cols_to_test, function(col) {
    any(is.na(dt_wide[[col]]))
  })
  not_complete_design <- any(any_na_in_dt)
  
  # Décision du test
  use_parametric <- (all(normal_data) || force_parametric_test) && 
    !not_complete_design && !force_non_parametric_test
  
  if (use_parametric) {
    # ========== ANOVA repeated measures ==========
    normal_data <- unique(normal_data)
    N_individuals <- length(unique(dt_long[[patient_id]]))
    
    if (do_test) {
      # Convertir patient_id en facteur SEULEMENT pour l'ANOVA
      dt_long_anova <- data.table::copy(dt_long)
      dt_long_anova[[patient_id]] <- as.factor(dt_long_anova[[patient_id]])
      
      test_result_tmp <- rstatix::anova_test(
        data = dt_long_anova, dv = "value",
        wid = dplyr::all_of(patient_id), within = "variable"
      )
      # test_result_table <- rstatix::get_anova_table(x = test_result_tmp)
      ## WArning test_result_tmp doesn't have always the same slotnames...
      if("ANOVA" %in% names(test_result_tmp)) {
        test_result_df <- test_result_tmp$ANOVA
      } else {
        test_result_df <- as.data.frame(test_result_tmp)
      }

      test_result <- list(
        test_name = "ANOVA Table (type III tests)",
        test = test_result_tmp,
        # wanted ? not sure
        # DFn = as.numeric(test_result_tmp[["DFn"]]), 
        # DFd = as.numeric(test_result_tmp[["DFd"]]), 
        # Fstat = as.numeric(test_result_tmp[["F"]]), 
        # ges = as.numeric(test_result_tmp[["ges"]]), 
        # fix v0.1.27
        p.value = as.numeric(test_result_df[["p"]]) 
      )
      test_used <- "Repeated measures ANOVA: within-Subjects designs"
      ongoing_message <- "Repeated measures ANOVA to valid !"
    } else {
      test_result <- list(p.value = NA)
      test_used <- "/"
      ongoing_message <- "Repeated measures ANOVA can be explored with option do_test."
    }
    
    if (force_parametric_test) {
      ongoing_message <- paste0(ongoing_message, "\nforce_parametric_test.")
    }
    
    # Métrique à afficher
    metric_to_show <- if (show_metric %in% c("auto", "mean")) {
      "mean"
    } else {
      message(
        "[handle_multiple_levels]",
        " /!\\ Caution median forced in cell content. ",
        "It may cause a disconnection between metrics and tests /!\\"
      )
      ongoing_message <- paste0(
        ongoing_message,
        "!Caution median forced in cell content. ",
        "It may cause a disconnection between metrics and tests!"
      )
      "median"
    }
    
    # Détection des outliers
    res_outliers <- rstatix::identify_outliers(dt_long, variable = "value")
    if (nrow(res_outliers) > 0) {
      msg <- paste0(
        "Detection of ", nrow(res_outliers), " points outliers on ",
        variable_interest, ".\n",
        "Check individuals with id ",
        paste0(unique(res_outliers[[patient_id]]), collapse = ",")
      )
      message("[handle_multiple_levels] ", msg)
      ongoing_message <- paste0(ongoing_message, msg)
    }
    
  } else {
    # ========== Tests non-paramétriques ==========
    normal_data <- all(normal_data)
    
    # Messages explicatifs
    ongoing_message <- paste0(
      paste0(
        c("force_non_parametric_test", "Normal data", "not complete design")[
          c(force_non_parametric_test, normal_data, not_complete_design)
        ],
        collapse = " with "
      ), 
      "."
    )
    
    if (!not_complete_design) {
      # ----- Quade test (design complet) -----
      N_individuals <- length(unique(dt_long[[patient_id]]))
      
      if (do_test) {
        quade_form <- stats::as.formula(paste0(
          "value ~ variable | ", "`", patient_id, "`"
        ))
        test_result <- stats::quade.test(quade_form, data = dt_long)
        test_used <- "Quade Test"
        ongoing_message <- "Quade test to valid"
      } else {
        test_result <- list(p.value = NA)
        test_used <- "/"
        ongoing_message <- "Quade Test can be explored with option do_test."
      }
      
    } else {
      # ----- Skillings-Mack test (design incomplet) -----
      
      if (do_test) {
        # Retirer les patients avec une seule observation
        count_block <- dt_long[, .(Nobs = sum(is.na(value))), by = c(patient_id)]
        
        # Identifier les patients à retirer (ceux avec seulement 1 observation)
        n_levels_var <- nlevels(dt_long[["variable"]])
        remove_patient_id <- count_block[Nobs == n_levels_var - 1][[patient_id]]
        
        if (length(remove_patient_id) > 0) {
          ongoing_message <- paste0(
            ongoing_message,
            "\nRemove ", length(remove_patient_id),
            " patients with only 1 observation.\n"
          )
        }
        
        # Filtrer : convertir en data.frame standard, filtrer, reconvertir
        dt_long_ski <- as.data.frame(dt_long)
        dt_long_ski <- dt_long_ski[!dt_long_ski[[patient_id]] %in% remove_patient_id, ]
        dt_long_ski <- data.table::as.data.table(dt_long_ski)
        
        # Créer IDENT_PAT
        unique_ids <- sort(unique(dt_long_ski[[patient_id]]))
        dt_long_ski$IDENT_PAT <- factor(
          dt_long_ski[[patient_id]],
          levels = unique_ids,
          labels = unique_ids
        )
        
        Ski_result <- utils::capture.output(Skillings.Mack::Ski.Mack(
          y = dt_long_ski$value,
          groups = dt_long_ski$variable,
          blocks = dt_long_ski$IDENT_PAT,
          simulate.p.value = TRUE, B = 10000
        ))
        
        test_result <- list(
          full_capture = Ski_result,
          p.value = as.numeric(
            gsub(" ", "", unlist(strsplit(Ski_result[[2]], split = "p-value ="))[[2]])
          ),
          nblocks_patients = length(unique(dt_long_ski$IDENT_PAT)),
          ngroups_times = length(unique(dt_long_ski$variable)),
          df = Ski_result[[3]],
          simulation = Ski_result[[4]]
        )
        test_used <- "Skillings-Mack test"
        N_individuals <- length(unique(dt_long_ski$IDENT_PAT))
        ongoing_message <- paste0(ongoing_message, "Skillings.Mack to valid")
        
      } else {
        N_individuals <- length(unique(dt_long[[patient_id]]))
        test_result <- list(p.value = NA)
        test_used <- "/"
        ongoing_message <- paste0(
          ongoing_message,
          "Skillings-Mack test can be explored with option do_test."
        )
      }
    }
    
    # Métrique à afficher
    metric_to_show <- if (show_metric %in% c("auto", "median")) {
      "median"
    } else {
      message(
        "[handle_multiple_levels]",
        " /!\\ Caution mean forced in cell content. ",
        "It may cause a disconnection between metrics and tests /!\\"
      )
      ongoing_message <- paste0(
        ongoing_message,
        "!Caution mean forced in cell content. ",
        "It may cause a disconnection between metrics and tests!"
      )
      "mean"
    }
  }
  
  ongoing_message <- paste0(msg_captured_norm, ongoing_message)
  
  # Préparer la population totale si demandée
  if (global_summary) {
    message(
      "[handle_multiple_levels] ",
      "Caution! global_summary on longitudinal data is not really relevant... ",
      "but ok if you want it"
    )
    Population_totale <- format_global_summary(
      dataframe = dt,
      variable = variable_interest,
      metric = metric_to_show,
      digits_central = digits_central,
      digits_sd = digits_sd
    )
  } else {
    Population_totale <- NULL
  }
  
  return(list(
    test_result = test_result,
    test_used = test_used,
    ongoing_message = ongoing_message,
    normal_data = normal_data,
    N_individuals = N_individuals,
    metric_to_show = metric_to_show,
    Population_totale = Population_totale,
    Difference_description = NULL
  ))
}


#' compute continuous table for paired data
#'
#' provide descriptive statistics table for continuous data with a paired level (time or visites)
#' 
#' Cases handled :
#' + Variable with all NA values → no test  
#' + Variable present in only 1 level → no paired test  
#' + 2 levels of varstrat:  
#'   + Normal data (diff) → Paired t-test  
#'   + Non-normal data → Wilcoxon signed-rank test  
#' + More than 2 levels:  
#'   + Complete design + normality → Repeated measures ANOVA  
#'   + Complete design + non-normality → Quade test  
#'   + Incomplete design → Skillings-Mack test  
#'  
#' @param dataframe A data.frame. tibble or data.table will be converted into data.table.
#' @param variable_interest A character. Name of dataframe's continuous columns to describe and test.
#' @param varstrat A character. Name of the stratification variable, 
#'  making groups to compare (time or visites).
#'  The repeated measures must be presented in line 
#'  (for instance, "V1", "V2", "V3" will be set on 3 line for each individuals)
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#'   Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of 
#'  decimal places (signif) for pvalues.
#' @param patient_id A character., Default "patientid". Name of identifiant patient id column.
#'  the repeated measures must be presented in line 
#'  (for instance, "id1" will be set on 3 line if he has 3 visits)
#' @param global_summary A logical. Default TRUE. Do you want to get global summary.
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
#' @param show_metric A character, Default "auto". What is the metric to show in cell content ?
#'  "auto" = mean or median, automatic choice according statistic test,
#'  "mean" = mean +/- sd forced, whatever shapiro said,
#'  "median" = media [Q1;Q3] forced, whatever shapiro said.
#'  Caution, if you force_non_parametric_test as TRUE, show_metric is forced 
#'  as 'median' and if you force_parametric_test as TRUE, show_metric is forced 
#'  as 'mean', to be consistent. 
#' @param do_test A logicial, Default FALSE, do not return stat test. Turn TRUE if wanted.
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
#'   precision = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra_with_missings",
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   precision = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra_with_missings",
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   precision = 2, signif_digits = 2,
#'   global_summary = TRUE
#' )
#' ## Anova on paired data
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra",
#'   varstrat = "visites_4",
#'   patient_id = "ID",
#'   precision = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "extra",
#'   varstrat = "visites_5",
#'   patient_id = "ID",
#'   precision = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' ## test no normal measure : Paired Wilcoxon signed-rank test
#' compute_paired_continuous_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "mesure3",
#'   varstrat = "visites_2",
#'   patient_id = "ID2",
#'   precision = 2, signif_digits = 2,
#'   global_summary = FALSE
#' )
#' }
compute_paired_continuous_table_and_test <- function(
  dataframe,
  variable_interest,
  varstrat,
  precision = 2,
  signif_digits = 4,
  patient_id = "patientid",
  global_summary = TRUE,
  force_non_parametric_test = FALSE,
  force_parametric_test = FALSE,
  show_metric = "auto",
  do_test = FALSE
) {
  
  `.SD` <- `.` <- `.N` <- `:=` <- NULL
  `Q1` <- `Q3` <- cell_content <- IDENT_PAT <- value <- n_group <- NULL
  n_missing <- NULL
  
  message("[compute_paired_continuous_table] ", variable_interest)
  
  # ========== Validations ==========
  stopifnot(length(variable_interest) == 1)
  stopifnot(variable_interest %in% names(dataframe))
  stopifnot(is.numeric(dataframe[[variable_interest]]))
  stopifnot(varstrat %in% names(dataframe))
  stopifnot(is.factor(dataframe[[varstrat]]))
  stopifnot(patient_id %in% names(dataframe))
  # stopifnot(digits >= 0)
  stopifnot(precision == "auto" || is.numeric(precision))
  stopifnot(is.numeric(signif_digits))
  stopifnot(signif_digits >= 0)
  stopifnot(show_metric %in% c("mean", "median", "auto"))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(force_non_parametric_test))
  stopifnot(is.logical(force_parametric_test))
  stopifnot(sum(c(force_parametric_test, force_non_parametric_test)) <= 1)
  stopifnot(is.logical(do_test))
  
  # ========== Préparation des données ==========
  dt <- data.table::setDT(data.table::copy(dataframe))
  N_init <- nrow(dt)
  dt <- dt[, .SD, .SDcols = c(variable_interest, varstrat, patient_id)]
  N_missingval <- sum(is.na(dt[[variable_interest]]))
  
  
  # Déterminer les digits à utiliser
  if (precision %in% "auto") {
    base_decimals <- detect_decimal_places(dt[[variable_interest]])
    digits_central <- compute_precision_digits(
      x = dt[[variable_interest]], stat_type = "central",
      base_decimals = base_decimals
    )
    digits_sd <- compute_precision_digits(
      x = dt[[variable_interest]], stat_type = "sd", 
      base_decimals = base_decimals, max_decimals = 3
    )
  } else {
    digits_central <- precision
    digits_sd <- precision
  }
  
  
  # Calcul des statistiques par groupe
  sumup <- unique(
    data.table::copy(dt)[,
       `:=`(
         mean = round(mean(.SD[[1]], na.rm = TRUE), digits = digits_central),
         sd = round(stats::sd(.SD[[1]], na.rm = TRUE), digits = digits_sd),
         median = round(stats::median(.SD[[1]], na.rm = TRUE), digits = digits_central),
         Q1 = round(stats::quantile(.SD[[1]], na.rm = TRUE)["25%"], digits = digits_central),
         Q3 = round(stats::quantile(.SD[[1]], na.rm = TRUE)["75%"], digits = digits_central),
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
  
  resN <- data.table::transpose(
    sumup[, .SD, .SDcols = c(varstrat, "n_group")], make.names = TRUE
  )
  names(resN) <- paste0(names(resN), "_N")
  
  # ========== CAS 1 : Toutes les valeurs sont NA ==========
  if (all(resN == 0)) {
    N_individuals <- length(unique(dt[[patient_id]])) #N_init - N_missingval
    test_result <- list(p.value = NA)
    test_used <- "/"
    ongoing_message <- "Variable present have no data (all NA), no desc, no test applicable"
    message("[compute_paired_continuous_table_and_test] ", ongoing_message)
    
    tab <- data.table::data.table(
      Variable = variable_interest,
      Valeurs_manquantes = N_missingval,
      N_individuals = N_individuals,
      resN,
      is_Normal = NA,
      P_valeur = NA,
      Test = test_used,
      message = ongoing_message
    )
    
    return(list(line_res = tab, test_result = test_result))
  }
  
  # ========== Préparer format wide ==========
  dt_wide <- data.table::dcast(
    data = stats::na.omit(dt[, .SD, .SDcols = c(patient_id, varstrat, variable_interest)]),
    formula = stats::as.formula(paste0("`", patient_id, "` ~ `", varstrat, "`")),
    value.var = variable_interest
  )
  
  # ========== CAS 2 : Une seule visite/groupe ==========
  if (ncol(dt_wide) == 2) {
    ongoing_message <- "Variable present only in 1 visit/group, no paired test applicable"
    N_individuals <- length(unique(dt_wide[[patient_id]])) # N_init - N_missingval
    test_result <- list(p.value = NA)
    test_used <- "/"
    
    # # Test de normalité
    # normal_data <- apply(
    #   X = dt_wide[, .SD, .SDcols = base::setdiff(names(dt_wide), patient_id)],
    #   MARGIN = 2,
    #   FUN = function(xcol) {
    #     stats::shapiro.test(stats::na.omit(xcol))$p.value > 0.05
    #   }
    # )
    cols_to_check <- base::setdiff(names(dt_wide), patient_id)
    normal_data <- sapply(cols_to_check, function(col) {
      x <- stats::na.omit(dt_wide[[col]])
      if (length(x) < 3 || length(unique(x)) == 1) {
        return(FALSE)
      }
      check_normality(x, return_messages = FALSE)
    })
    
    # Décider de la métrique
    metric_to_show <- if ((normal_data && show_metric == "auto") || show_metric == "mean") {
      "mean"
    } else {
      "median"
    }
    
    # Formater les cellules
    for (i in 1:nrow(sumup)) {
      sumup[i, cell_content := format_cell_content(
        stats = sumup[i, ], metric = metric_to_show
      )]
    }
    
    # Population totale
    Population_totale <- if (global_summary) {
      format_global_summary(
        dataframe = dt, 
        variable = variable_interest,
        metric = metric_to_show, 
        digits_central = digits_central, 
        digits_sd = digits_sd
      )
    } else {
      NULL
    }

    Difference_description <- NULL
    
  } 
  
  # ========== NOUVEAU CAS v 0.1.27 : Toutes les valeurs sont identiques ==========
  cols_to_check <- setdiff(names(dt_wide), patient_id)
  all_values <- unlist(dt_wide[, .SD, .SDcols = cols_to_check], use.names = FALSE)
  all_values_clean <- all_values[!is.na(all_values)]
  
  if (length(unique(all_values_clean)) <= 1) {
    ongoing_message <- "toutes les valeurs de 'x' sont identiques"
    message("[compute_paired_continuous_table_and_test] ", ongoing_message)
    
    N_individuals <- length(unique(dt_wide[[patient_id]])) # nrow(dt_wide)
    test_result <- list(p.value = NA)
    test_used <- "/"
    normal_data <- NA
    metric_to_show <- "median"
    
    # Valeur unique
    unique_value <- if (length(all_values_clean) > 0) {
      unique(all_values_clean)[1]
    } else {
      0
    }
    
    # Formater les cellules
    for (i in 1:nrow(sumup)) {
      sumup[i, cell_content := paste0(
        unique_value, " [", unique_value, ";", unique_value, "]"
      )]
    }
    
    Population_totale <- if (global_summary) {
      paste0(
        unique_value, " [", unique_value, " ; ", unique_value, "]"
      )
    } else {
      NULL
    }
    
    Difference_description <- NULL
    
  } else { 
    
    # ========== CAS 3 & 4 : Tests appariés ==========
    
    # Adapter show_metric si forcé
    if (force_non_parametric_test) {
      message(
        "[compute_paired_continuous_table_and_test]",
        "Because force_non_parametric_test is TRUE, show_metric is forced as 'median'."
      )
      show_metric <- "median"
    }
    if (force_parametric_test) {
      message(
        "[compute_paired_continuous_table_and_test]",
        "Because force_parametric_test is TRUE, show_metric is forced as 'mean'."
      )
      show_metric <- "mean"
    }
    
    if (nlevels(dt[[varstrat]]) == 2) {
      # ========== CAS 3 : 2 niveaux ==========
      result <- handle_two_levels(
        dt_wide = dt_wide,
        variable_interest = variable_interest,
        varstrat = varstrat,
        patient_id = patient_id,
        digits_central = digits_central,
        digits_sd = digits_sd,
        show_metric = show_metric,
        force_parametric_test = force_parametric_test,
        force_non_parametric_test = force_non_parametric_test,
        global_summary = global_summary,
        dt = dt
      )
    } else {
      # ========== CAS 4 : Plus de 2 niveaux ==========
      ## --here see in v0.1.28
      result <- handle_multiple_levels(
        dt_wide  = dt_wide,
        variable_interest = variable_interest, 
        varstrat = varstrat, 
        patient_id = patient_id,
        digits_central = digits_central,
        digits_sd = digits_sd,
        show_metric = show_metric, 
        force_parametric_test = force_parametric_test,
        force_non_parametric_test = force_non_parametric_test,
        global_summary = global_summary,
        do_test = do_test,
        dt = dt
      )

    }
    
    # Extraire les résultats
    test_result <- result$test_result
    test_used <- result$test_used
    ongoing_message <- result$ongoing_message
    normal_data <- result$normal_data
    N_individuals <- result$N_individuals
    metric_to_show <- result$metric_to_show
    Population_totale <- result$Population_totale
    Difference_description <- result$Difference_description
    
    # Formater les cellules
    for (i in 1:nrow(sumup)) {
      sumup[i, cell_content := format_cell_content(
        stats = sumup[i, ], metric = metric_to_show
      )]
    }
  }
  
  # message("[compute_paired_continuous_table_and_test] ", test_used)
  
  # ========== Construire le tableau final ==========
  res <- data.frame(
    Variable = variable_interest,
    Valeurs_manquantes = N_missingval,
    N_individuals = N_individuals
      ## N_individuals : no more useful ? because of N col for each levels 
        ## => bellow add of resN V 0.1.27
  )
  
  if (global_summary && !is.null(Population_totale)) {
    res <- cbind(res, Population_totale)
  }
  
  tab <- cbind(
    res,
    data.table::transpose(
      sumup[, .SD, .SDcols = c(varstrat, "cell_content")], 
      make.names = varstrat
    ),
    resN
  )
  
  # # Réorganiser les colonnes
  # cols_visits <- unlist(lapply(X = sumup[[varstrat]], FUN = function(x) paste0(x, c("", "_N"))))
  # data.table::setcolorder(x = tab, neworder = c(
  #   setdiff(names(tab), cols_visits), cols_visits
  # ))
  
  # === CORRECTION v0.1.27 : Réorganiser selon l'ordre des levels de varstrat ===
  varstrat_levels_ordered <- levels(dt[[varstrat]])
  cols_visits <- unlist(
    lapply(varstrat_levels_ordered, FUN = function(x) paste0(x, c("", "_N")))
  )
  
  # Colonnes de base (avant les visites)
  base_cols <- intersect(c(
    "Variable", "Valeurs_manquantes", 
    "N_individuals", # keep ? v0.1.27
    "Population_totale"
  ), names(tab))
  # Colonnes de fin (après les visites)
  end_cols <- intersect(c(
    "Difference_description", "is_Normal", "P_valeur", "Test", "message"
  ), names(tab))
  # Réorganiser
  data.table::setcolorder(x = tab, neworder = c(
    base_cols, cols_visits, end_cols
  ))
  
  # Ajouter Difference_description si applicable
  if (!is.null(Difference_description)) {
    tab <- cbind(tab, Difference_description)
  }
  
  # Formater p-value
  ## --here 
  if (is.nan(test_result$p.value) | !is.numeric(test_result$p.value)) {
    # check numeric class # fix unit test test-compute_paired_tables.R:171
    P_valeur <- NA
    test_used <- "/"
    ongoing_message <- paste0("Test non applicable.", ongoing_message)
  } else {
    P_valeur <- signif(test_result$p.value, digits = signif_digits)
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
    line_res = tab,
    test_result = test_result
  ))
}



# =
#### Factors ####
# =

# =
# Fonctions utilitaires pour variables factorielles appariées
# =

#' Gérer force_generate_1_when_0
#' 
#' handle_force_generate
#' 
#' @param dt A data.frame 
#' @param variable_interest A character
#' @param force_generate_1_when_0 A logical
#'  
#' @return Liste avec var_levels et var_nlevels mis à jour, et dt modifié
#'  
#' @keywords internal
handle_force_generate <- function(
  dt, 
  variable_interest, 
  force_generate_1_when_0
) {
  var_nlevels <- nlevels(dt[[variable_interest]])
  var_levels <- levels(dt[[variable_interest]])
  
  if (force_generate_1_when_0 && var_nlevels == 1 && 
      (var_levels == 0 || tolower(var_levels) == "non")) {
    
    message(
      "[handle_force_generate] force_generate_1_when_0 for ",
      variable_interest
    )
    
    new_level <- ifelse(var_levels %in% 0, 1, "Oui")
    var_levels <- c(var_levels, new_level)
    dt[[variable_interest]] <- factor(
      dt[[variable_interest]], 
      levels = var_levels, 
      labels = var_levels
    )
    var_nlevels <- nlevels(dt[[variable_interest]])
  }
  
  return(list(
    dt = dt,
    var_levels = var_levels,
    var_nlevels = var_nlevels
  ))
}

#' Calculer le tableau de fréquences avec effectifs et pourcentages
#' 
#' compute_frequency_table
#' 
#' @param dt A data.frame 
#' @param variable_interest A character
#' @param varstrat A character
#' @param varstrat_levels A character
#' @param digits An integer
#'  
#' @return table of frequencies
#'  
#' @keywords internal
compute_frequency_table <- function(
  dt,
  variable_interest,
  varstrat,
  varstrat_levels, 
  digits = 1
) {
  # message("[compute_frequency_table]")
  n_group_mod <- Nb_mesures <- N_by_visit <- N_by_visit_level <- p <- NULL
  
  # Comptages
  sumup <- data.table::as.data.table(table(
    dt[[varstrat]], 
    dt[[variable_interest]], 
    useNA = "always"
  ))
  names(sumup) <- c(varstrat, variable_interest, "n_group_mod")
  
  # === SOLUTION ROBUSTE : Conversion temporaire en data.frame ===
  sumup <- as.data.frame(sumup)
  sumup <- sumup[!is.na(sumup[[varstrat]]), ]
  sumup <- data.table::as.data.table(sumup)
  
  # Calculer Nb_mesures et N_by_visit
  sumup[, Nb_mesures := sum(n_group_mod), by = variable_interest]
  sumup[, N_by_visit := sum(n_group_mod), by = varstrat]
  
  # Fixer N_by_visit pour les niveaux (sans lignes NA)
  sumup$is_level_line <- !is.na(sumup[[variable_interest]])
  sumup[, N_by_visit_level := sum(n_group_mod), by = c(varstrat, "is_level_line")]
  
  sumup$N_by_visit <- ifelse(
    !is.na(sumup[[variable_interest]]),
    sumup$N_by_visit_level,
    sumup$N_by_visit
  )
  sumup$N_by_visit_level <- NULL
  sumup$is_level_line <- NULL
  
  # Calculer les pourcentages
  var_levels <- levels(dt[[variable_interest]])
  
  # Conversion temporaire pour filtrer
  sumup_df <- as.data.frame(sumup)
  rows_to_update <- sumup_df[[variable_interest]] %in% var_levels
  
  if (any(rows_to_update)) {
    # Utiliser les indices numériques pour la mise à jour
    row_indices <- which(rows_to_update)
    
    sumup[row_indices,
          p := ifelse(
            sum(n_group_mod) == 0,
            "",
            paste0("(", round(100 * n_group_mod / N_by_visit, digits = digits), "%)")
          ),
          by = c(varstrat, variable_interest)
    ]
  }
  
  # Formater le contenu des cellules
  sumup$cell_content <- ifelse(
    test = is.na(sumup[[variable_interest]]),
    sumup[["n_group_mod"]],
    paste0(sumup[["n_group_mod"]], " ", sumup[["p"]])
  )
  
  return(sumup)
}

#' Créer le tableau d'effectifs formaté
#' 
#' format_effectif_table
#' 
#' @param sumup A data.frame 
#' @param variable_interest A character
#' @param varstrat A character
#' @param varstrat_levels A character
#' @param keep_missing_line A logical
#'  
#' @return formated table of frequencies
#'  
#' @keywords internal
format_effectif_table <- function(
  sumup,
  variable_interest, 
  varstrat, 
  varstrat_levels, 
  keep_missing_line
) {
  
  # message("[format_effectif_table]")
  Modalites <- NULL
  
  # Tableau principal
  res <- data.table::dcast(
    data = sumup,
    stats::as.formula(paste0("`", variable_interest, "` + Nb_mesures ~ `", varstrat, "`")),
    value.var = "cell_content"
  )
  
  # Tableau des N
  resN <- data.table::dcast(
    data = sumup,
    stats::as.formula(paste0("`", variable_interest, "` ~ `", varstrat, "`")),
    value.var = "N_by_visit"
  )
  names(resN)[-1] <- paste0(names(resN)[-1], "_N")
  
  # Fusion
  effectif_tab <- merge(res, resN, by = variable_interest, sort = FALSE)
  names(effectif_tab)[1] <- "Modalites"
  
  # Réorganiser les colonnes
  cols_visits <- unlist(lapply(varstrat_levels, FUN = function(x) paste0(x, c("", "_N"))))
  data.table::setcolorder(
    x = effectif_tab, 
    neworder = c("Modalites", "Nb_mesures", cols_visits)
  )
  
  # Gérer les données manquantes
  effectif_tab[is.na(Modalites), "Modalites"] <- "Missing data"
  if (!keep_missing_line) {
    effectif_tab <- effectif_tab[!Modalites %in% "Missing data"]
  }
  
  return(effectif_tab)
}

#' Ajouter la colonne Population_total si demandée
#' 
#' add_global_summary
#' 
#' @param effectif_tab A data.frame 
#' @param dt A data.frame 
#' @param variable_interest A character
#' @param digits An integer
#' 
#' @return formated table of frequencies
#'  
#' @keywords internal
add_global_summary <- function(
  effectif_tab,
  dt, 
  variable_interest, 
  digits = 1
) {
  # message("[add_global_summary]")
  
  Population_total <- n_group_mod <- NULL
  
  sumup_global <- data.table::as.data.table(table(dt[[variable_interest]], useNA = "always"))
  names(sumup_global) <- c(variable_interest, "n_group_mod")
  sumup_global <- sumup_global[!is.na(sumup_global[[variable_interest]]), ]
  
  sumup_global <- sumup_global[, `:=`(
    Population_total = paste0(
      n_group_mod,
      " (", round(100 * n_group_mod / sum(sumup_global[["n_group_mod"]], na.rm = TRUE),
                  digits = digits), "%)"
    )
  )][, n_group_mod := NULL]
  
  names(sumup_global)[1] <- "Modalites"
  
  effectif_tab <- merge(
    x = effectif_tab, 
    y = sumup_global, 
    by = "Modalites", 
    all = TRUE, 
    sort = FALSE
  )
  
  # Réorganiser
  data.table::setcolorder(
    x = effectif_tab, 
    neworder = c(
      unique(c("Modalites", "Nb_mesures", "Population_total", names(effectif_tab)))
    )
  )
  
  return(effectif_tab)
}

#' Effectuer le test de McNemar
#' 
#' perform_mcnemar_test
#' 
#' @param mcnemar_dt A data.frame 
#' @param varstrat_levels A character
#' @param variable_interest A character
#' 
#' @return Eval McNemar test
#'  
#' @keywords internal
perform_mcnemar_test <- function(
  mcnemar_dt,
  varstrat_levels,
  variable_interest
) {
  message("[perform_mcnemar_test]")
  # Créer la table pour McNemar
  effobs_mcnemar <- table(
    data.frame(
      mcnemar_dt[, .SD, .SDcols = varstrat_levels[1]],
      mcnemar_dt[, .SD, .SDcols = varstrat_levels[2]]
    )
  )
  
  # Vérifier les conditions du test
  if (!all(dim(effobs_mcnemar) %in% 2) || ncol(mcnemar_dt) != 3) {
    return(NULL)  # Conditions non remplies
  }
  
  # Test de McNemar
  test_result <- stats::mcnemar.test(effobs_mcnemar, correct = TRUE)
  
  # Message si peu de paires discordantes
  ongoing_message <- if (effobs_mcnemar[2, 1] + effobs_mcnemar[1, 2] < 10) {
    "Less than 10 discordant pairs"
  } else {
    ""
  }
  
  test_used <- "McNemar's Chi-squared test"
  # message("[perform_mcnemar_test] ", test_used)
  
  return(list(
    test_result = test_result,
    test_used = test_used,
    ongoing_message = ongoing_message
  ))
}

#' Effectuer le test d'homogénéité marginale
#' 
#' perform_marginal_homogeneity_test
#' 
#' @param dt A data.frame 
#' @param variable_interest A character
#' @param varstrat A character
#' @param patient_id A character
#' 
#' @return Eval marginal homogeneity test
#'  
#' @keywords internal
perform_marginal_homogeneity_test <- function(
  dt,
  variable_interest, 
  varstrat,
  patient_id
) {
  message("[perform_marginal_homogeneity_test]")
  dt_mh <- base::droplevels(dt)
  check_mh_dt <- as.data.frame(table(dt_mh[[varstrat]]))
  check_mh <- all(check_mh_dt$Freq == check_mh_dt$Freq[1])  # Design complet ?
  
  if (!check_mh) {
    return(list(
      test_result = list(p.value = NA, method = "/"),
      test_used = "/",
      ongoing_message = "Not balanced design, Marginal Homogeneity Test not applicable"
    ))
  }
  
  if (nrow(check_mh_dt) < 2) {
    return(list(
      test_result = list(p.value = NA, method = "/"),
      test_used = "/",
      ongoing_message = "nrow(check_mh_dt) < 2, Marginal Homogeneity Test not applicable."
    ))
  }
  
  # === CORRECTION : Convertir patient_id en facteur ===
  if (!is.factor(dt_mh[[patient_id]])) {
    dt_mh[[patient_id]] <- as.factor(dt_mh[[patient_id]])
  }
  
  # Test d'homogénéité marginale
  test_used <- "Marginal Homogeneity Test"
  ongoing_message <- "Marginal Homogeneity Test to valid"
  
  # Exécuter le test avec gestion d'erreurs
  has_issues_hm <- try(tools::assertCondition( # correction v0.1.27
    raw_mh_result <- coin::mh_test(
      formula = stats::as.formula(paste0(
        "`", variable_interest, "` ~ `", varstrat, "` | `", patient_id, "`"
      )),
      data = dt_mh
    )
  ), silent = TRUE)
  
  has_issues_hm <- has_issues_hm[
    sapply(has_issues_hm, function(el) {
      length(base::intersect(class(el), c("warning", "error"))) != 0
    })
  ]
  
  msg_captured_hm <- if (length(has_issues_hm) == 0) {
    ""
  } else {
    paste(
      unique(sapply(has_issues_hm, function(el) {
        paste0("[", class(el)[2], "] ", el$message)
      })),
      collapse = ";"
    )
  }
  
  if (grepl("error", msg_captured_hm)) {
    raw_mh_result <- NA
    p_value_hm <- NA
    ongoing_message <- paste0(ongoing_message, ";", msg_captured_hm)
  } else {
    p_value_hm <- coin::pvalue(raw_mh_result)
  }

  test_result <- list(
    raw_result = raw_mh_result,
    p.value = p_value_hm,
    message = ongoing_message
  )
  
  # message("[perform_marginal_homogeneity_test] ", test_used)
  
  return(list(
    test_result = test_result,
    test_used = test_used,
    ongoing_message = ongoing_message
  ))
}

#' Choisir et effectuer le test statistique approprié
#' 
#' perform_paired_factorial_test
#' 
#' @param dt A data.frame 
#' @param variable_interest A character
#' @param varstrat A character
#' @param patient_id A character
#' @param varstrat_levels A character
#' 
#' @return choose the appropriate paired test
#'  
#' @keywords internal
perform_paired_factorial_test <- function(
  dt, 
  variable_interest, 
  varstrat,
  patient_id,
  varstrat_levels
) {
  # message("[perform_paired_factorial_test]")
  # Préparer les données pour McNemar
  mcnemar_dt <- data.table::dcast(
    data = stats::na.omit(dt[, .SD, .SDcols = c(patient_id, varstrat, variable_interest)]),
    formula = stats::as.formula(paste0("`", patient_id, "` ~ `", varstrat, "`")),
    value.var = variable_interest
  )
  
  # Vérifier si le test est applicable
  dt_applicable <- stats::na.omit(dt)
  if (length(unique(dt_applicable[[varstrat]])) == 1) {
    ongoing_message <- "Variable present only in 1 visit/group, no paired test applicable"
    message("[perform_paired_factorial_test] ", ongoing_message)
    
    return(list(
      test_result = list(p.value = NA, method = "/", message = ongoing_message),
      test_used = "/",
      ongoing_message = ongoing_message
    ))
  }
  
  if (length(varstrat_levels) == 2) {
    # Tenter le test de McNemar
    mcnemar_result <- perform_mcnemar_test(
      mcnemar_dt = mcnemar_dt,
      varstrat_levels = varstrat_levels, 
      variable_interest = variable_interest
    )
    
    if (!is.null(mcnemar_result)) {
      return(mcnemar_result)
    }
  }
  
  # Sinon, test d'homogénéité marginale
  test_homogeneite_marginal <- perform_marginal_homogeneity_test(
    dt = dt, 
    variable_interest = variable_interest, 
    varstrat = varstrat, 
    patient_id = patient_id
  )
  return(test_homogeneite_marginal)
}

#' Simplifier le tableau si demandé (variables binaires)
#' 
#' simplify_factorial_table
#' 
#' @param effectif_tab A data.frame 

#' @return simplified table
#'  
#' @keywords internal
simplify_factorial_table <- function(effectif_tab) {
  # message("[simplify_factorial_table]")
  if (is.null(effectif_tab)) {
    return(effectif_tab)
  }
  
  is_binary <- all(c("0", "1") %in% effectif_tab$Modalites) ||
    all(c("oui", "non") %in% tolower(effectif_tab$Modalites))
  
  if (!is_binary) {
    return(effectif_tab)
  }
  
  # Garder seulement la ligne "1" ou "Oui"
  if (any(grepl("1", effectif_tab$Modalites))) {
    effectif_tab <- effectif_tab[effectif_tab$Modalites %in% 1, ]
  } else {
    row_select <- grep("OUI", effectif_tab$Modalites, ignore.case = TRUE)
    effectif_tab <- effectif_tab[row_select, ]
  }
  
  return(effectif_tab)
}

#' compute factorial table for paired data
#'
#' provide descriptive statistics table for factorial data with a paired level 
#'   (time or visites)
#'
#' The McNemar test is a statistical test used to compare qualitative data sets.
#' That is to say, for comparing frequencies or percentages on matched data.
#' This test is particularly suitable for situations where you have matched 
#'   measurements (the same patients) before and after an intervention => 
#'   only applicable on 2x2 contingency table.
#'   
#'   example of Assumptions:
#'   H0: The proportion of paracetamol given intravenously is the same before 
#'     and after the campaign. Vs
#'   H1: The proportion of paracetamol given intravenously is different after 
#'     campaign compared to before.
#'
#' for more than 2x2 tc : Test d'homogénéité marginale 
#' Ce test s'utilise lorsque la variable à étudier a plus de 2 modalités et 
#'   que les groupes sont appariés
#'
#' @param dataframe A data.frame. tibble or data.table will be converted 
#'   into data.table.
#' @param variable_interest A character. Name of dataframe's factorial 
#'   columns to describe and test.
#' @param varstrat A character. Name of the stratification variable, 
#'   making groups to compare (time or visits).
#' @param precision Precision mode: "auto" (adaptive) or numeric (fixed)
#'   Integer indicating the number of decimal places (round).
#' @param signif_digits A integer, Default 4. Integer indicating the number of 
#'   decimal places (signif) for p-values.
#' @param simplify A logical. Default FALSE. Boolean indicating if one or two 
#'   lines should be displayed for binary variables.
#'   TRUE = only the 2nd level of the variables (if 0/1 variable : only 1), 
#'   FALSE = both levels of the variables.
#' @param patient_id A character., Default "patientid". Name of username 
#'   patient id column.
#' @param force_generate_1_when_0 A logical, Default TRUE. If TRUE, will test
#'    if the unique modality is 0 or "non" and
#'    add the level 1 or "oui" so it can be display in counts. Can be combined 
#'    with simplify to only show the modality (1).
#' @param keep_missing_line A logical, Default TRUE. Do you want to keep the 
#'   missing data count (like a level)
#' @param global_summary A logical. Default FALSE Do you want to get global summary.
#'  Caution! global_summary on longitudinal data is not really relevant... 
#'  but ok if you want it, you can.
#' @param do_test A logical, Default FALSE, do not return stat test. 
#'   Turn TRUE if wanted.
#' 
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
#'   precision = 1,
#'   patient_id = "ID2",
#'   force_generate_1_when_0 = FALSE,
#'   keep_missing_line = TRUE
#' )
#' compute_paired_factorial_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "fact1_na",
#'   varstrat = "visites_2",
#'   precision = 1,
#'   patient_id = "ID2",
#'   force_generate_1_when_0 = FALSE,
#'   keep_missing_line = TRUE
#' )
#' compute_paired_factorial_table_and_test(
#'   dataframe = modified_sleep,
#'   variable_interest = "fact3",
#'   varstrat = "visites_2",
#'   precision = 1,
#'   patient_id = "ID2",
#'   force_generate_1_when_0 = FALSE,
#'   keep_missing_line = TRUE
#' )
#' # dataframe = modified_sleep;
#' # variable_interest = "fact3";
#' # varstrat = "visites_2";
#' # precision = 1;
#' # patient_id = "ID2";
#' # force_generate_1_when_0 = FALSE;
#' # keep_missing_line = TRUE;
#' }
compute_paired_factorial_table_and_test <- function(
  dataframe,
  variable_interest,
  varstrat,
  precision = 2,
  signif_digits = 4,
  simplify = FALSE,
  patient_id = "patientid",
  force_generate_1_when_0 = TRUE,
  keep_missing_line = TRUE,
  global_summary = FALSE,
  do_test = FALSE
) {
  
  `.SD` <- `.` <- `.N` <- `:=` <- NULL
  cell_content <- IDENT_PAT <- value <- n_group_mod <- Nb_mesures <- Modalites <- NULL
  V1 <- V2 <- V3 <- NULL
  
  message(
    "[compute_paired_factorial_table] ", #"Compute tab for ",
    variable_interest
  )
  
  # ========== Validations ==========
  stopifnot(length(variable_interest) == 1)
  
  dt <- data.table::setDT(data.table::copy(dataframe))
  
  stopifnot(variable_interest %in% names(dt))
  stopifnot(varstrat %in% names(dt))
  stopifnot(is.factor(dt[[variable_interest]]))
  stopifnot(is.factor(dt[[varstrat]]))
  stopifnot(!all(is.na(dt[[varstrat]])))
  stopifnot(patient_id %in% names(dt))
  # stopifnot(digits >= 0)
  stopifnot(precision == "auto" || is.numeric(precision))
  stopifnot(signif_digits >= 0)
  stopifnot(is.logical(simplify))
  stopifnot(is.logical(force_generate_1_when_0))
  stopifnot(is.logical(keep_missing_line))
  stopifnot(is.logical(global_summary))
  stopifnot(is.logical(do_test))
  
  # ========== Préparation des données ==========
  N_init <- nrow(dt)
  dt <- dt[, .SD, .SDcols = c(variable_interest, varstrat, patient_id)]
  N_missingval <- sum(is.na(dt))
  
  varstrat_levels <- levels(dt[[varstrat]])
  
  # Gérer force_generate_1_when_0
  force_result <- handle_force_generate(
    dt = dt, 
    variable_interest = variable_interest,
    force_generate_1_when_0 = force_generate_1_when_0
  )
  dt <- force_result$dt
  var_levels <- force_result$var_levels
  var_nlevels <- force_result$var_nlevels
  
  # Déterminer les digits à utiliser
  if (precision %in% "auto") {
    digits <- 1 
  } else {
    digits <- precision
  }
  
  # ========== Calcul du tableau de fréquences ==========
  sumup <- compute_frequency_table(
    dt = dt, 
    variable_interest = variable_interest,
    varstrat = varstrat,
    varstrat_levels = varstrat_levels,
    digits = digits
  )
  
  # ========== Formater le tableau d'effectifs ==========
  effectif_tab <- format_effectif_table(
    sumup = sumup,
    variable_interest = variable_interest, 
    varstrat = varstrat,
    varstrat_levels = varstrat_levels,
    keep_missing_line = keep_missing_line
  )
  
  # ========== Ajouter Population_total si demandé ==========
  if (global_summary) {
    effectif_tab <- add_global_summary(
      effectif_tab = effectif_tab, 
      dt = dt,
      variable_interest = variable_interest, 
      digits = digits
    )
  }
  
  # ========== Simplification si demandée ==========
  if (simplify) {
    effectif_tab <- simplify_factorial_table(
      effectif_tab = effectif_tab
    )
  }
  
  if (do_test) {
    # ========== Tests statistiques ==========
    message("[compute_paired_factorial_table_and_test] Go for stat test")
    
    test_results <- perform_paired_factorial_test(
      dt = dt, 
      variable_interest = variable_interest, 
      varstrat = varstrat, 
      patient_id = patient_id,
      varstrat_levels = varstrat_levels
    )
  
    test_result <- test_results$test_result
    test_used <- test_results$test_used
    ongoing_message <- test_results$ongoing_message
    
    tab_test <- data.table::data.table(
      P_valeur = as.numeric(signif(test_result$p.value, digits = signif_digits)), # V1
      Test = test_used, # V2 
      message = ongoing_message # V3
    )
    tab_test <- data.table::rbindlist(
      l = list(
        tab_test,
        as.data.table(matrix(
          data = NA,
          nrow = nrow(effectif_tab) - 1, ncol = ncol(tab_test)
        ))[, V1 := as.numeric(V1)][, V2 := as.character(V2)][, V3 := as.character(V3)]
      ), 
      use.names = FALSE
    )
    
  } else {
    # tab_test <- data.table::data.table(
    #   P_valeur = NA, 
    #   Test = "/",
    #   message = ""
    # )
    test_result <- NA
  }
  
  # ========== Formater le tableau final ==========
  res <- data.frame(
    Variable = variable_interest,
    Valeurs_manquantes = N_missingval, 
    N_individuals = length(unique(dt[[patient_id]]))
  )
  res <- data.table::rbindlist(
    l = list(
      res,
      as.data.frame(matrix(data = NA, nrow = nrow(effectif_tab) - 1, ncol = ncol(res)))
    ), 
    use.names = FALSE
  )
  
  # Retirer Nb_mesures
  effectif_tab$Nb_mesures <- NULL
  
  # Assembler le tableau final
  if (do_test) {
    tab <- cbind(res, effectif_tab, tab_test)
  } else {
    tab <- cbind(res, effectif_tab)
  }
  
  # Réorganiser les colonnes
  data.table::setcolorder(
    x = tab, 
    neworder = c(unique(c("Variable", "Modalites", names(tab))))
  )
  
  return(list(
    line_res = tab,
    test_result = test_result
  ))
}

