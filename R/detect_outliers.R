#### Detection outliers ####

#' Detect extreme values in a numeric data.table
#' 
#' Tukey Method will use [Q1 - threshold * IQR  ;  Q3 + threshold * IQR]
#' Zscore Method will use [mean - threshold * SD  ;  mean + threshold * SD]
#' 
#' You can use either classic threshold 
#'   (1.5 for Tukey (classic boxplot), 3 for Zscore (3 SD))
#'   nor apply your own threshold. 
#'
#' @param dt A data.frame or data.table with numeric columns only
#' @param vars Character vector of variable names to check.
#'   Default: all numeric columns.
#' @param method Method to detect outliers: "Tukey" or "Zscore"
#' @param threshold Multiplier for the bounds.
#'   Default: 1.5 for Tukey (classic boxplot), 3 for Zscore (3 SD).
#' @param summary Logical. If TRUE, returns a summary table (one row per
#'   variable). If FALSE, returns all outlier rows. Default: FALSE.
#' @param verbose A logical, Default TRUE. Show message. 
#'  Do you want to work in silence? Turn it FALSE.
#'  
#' @return A data.table with outlier details or summary, NULL if none found.
#'   Check the "Row_index" line to identify the row in the original data set (dt)
#'    with an extreme value.
#' 
#' @export
#' @examples
#' \dontrun{
#' dt <- data.table::data.table(
#'   PAM     = c(rnorm(43, 85, 10), 145, 150),
#'   LACTATE = c(rnorm(44, 1.8, 0.5), 8.2, NA),
#'   FC      = c(rnorm(44, 75, 10), 180, NA)
#' )
#'
#' # Tukey method (default threshold 1.5)
#' detect_outliers(dt, vars = names(dt), method = "Tukey")
#'
#' # Zscore method (default threshold 3)
#' detect_outliers(dt, vars = names(dt), method = "Zscore")
#'
#' # Summary only
#' detect_outliers(dt, vars = names(dt), method = "Tukey", summary = TRUE)
#' 
#' # Tukey avec threshold custom (boxplot étendu : 3 au lieu de 1.5)
#' detect_outliers(dt, vars = names(dt), method = "Tukey", threshold = 3)
#' 
#' }
detect_outliers <- function(
    dt,
    vars,
    method = c("Tukey", "Zscore"),
    threshold = NULL,
    summary = FALSE, 
    verbose = TRUE
) {
  if (verbose) message("[detect_outliers]")
  
  # Validation
  method <- match.arg(method)
  # Threshold par défaut selon la méthode
  if (is.null(threshold)) {
    threshold <- switch(
      method,
      "Tukey"  = 1.5,
      "Zscore" = 3
    )
  }
  stopifnot(is.numeric(threshold))
  stopifnot(is.logical(summary))
  stopifnot(is.logical(verbose))
  
  dt <- data.table::setDT(data.table::copy(dt))
  
  # Vérifier que les variables existent et sont numériques
  vars_invalid <- vars[!vars %in% names(dt)]
  if (length(vars_invalid) > 0) {
    stop(
      "[detect_outliers] Variables not found in dt: ",
      paste(vars_invalid, collapse = ", ")
    )
  }
  vars_non_numeric <- vars[!sapply(dt[, .SD, .SDcols = vars], is.numeric)]
  if (length(vars_non_numeric) > 0) {
    stop(
      "[detect_outliers] Non-numeric variables: ",
      paste(vars_non_numeric, collapse = ", ")
    )
  }
  
  # Fonction interne pour une variable
  detect_one <- function(
    var_name
  ) {
    
    x       <- dt[[var_name]]
    x_clean <- x[!is.na(x)]
    n_total <- length(x_clean)
    
    if (n_total < 3) {
      if (verbose) {
        # warning(
        message(paste0(
          "[detect_outliers] Variable '", var_name, "' has less than 3 ",
          "non-missing values, skipping."
        ))
      }
      return(NULL)
    }
    
    # === Calcul des bornes selon la méthode ===
    if (method == "Tukey") {
      
      # Tukey : Q1 - threshold*IQR ; Q3 + threshold*IQR
      q1    <- stats::quantile(x_clean, 0.25)
      q3    <- stats::quantile(x_clean, 0.75)
      iqr_x <- q3 - q1
      lower <- q1 - threshold * iqr_x
      upper <- q3 + threshold * iqr_x
      
      bound_info <- data.table::data.table(
        Q1          = round(q1, 3),
        Q3          = round(q3, 3),
        IQR         = round(iqr_x, 3),
        Lower_bound = round(lower, 3),
        Upper_bound = round(upper, 3)
      )
      
    } else {
      
      # Zscore : mean - threshold*SD  ;  mean + threshold*SD
      mean_x <- mean(x_clean)
      sd_x   <- stats::sd(x_clean)
      lower  <- mean_x - threshold * sd_x
      upper  <- mean_x + threshold * sd_x
      
      bound_info <- data.table::data.table(
        Mean        = round(mean_x, 3),
        SD          = round(sd_x, 3),
        Lower_bound = round(lower, 3),
        Upper_bound = round(upper, 3)
      )
    }
    
    # Identifier les outliers
    is_outlier <- !is.na(x) & (x < lower | x > upper)
    n_outliers <- sum(is_outlier)
    
    if (summary) {
      # Mode résumé : 1 ligne par variable
      return(data.table::data.table(
        Variable   = var_name,
        Method     = method,
        Threshold  = threshold,
        N_total    = n_total,
        N_outliers = n_outliers,
        Pct        = round(100 * n_outliers / n_total, 1),
        bound_info
      ))
    } else {
      # Mode détail : 1 ligne par outlier
      if (n_outliers == 0) return(NULL)
      
      return(data.table::data.table(
        Variable   = var_name,
        Method     = method,
        Threshold  = threshold,
        Row_index  = which(is_outlier),
        Value      = x[is_outlier],
        bound_info
      ))
    }
  }
  
  # Appliquer sur toutes les variables
  result <- data.table::rbindlist(
    lapply(vars, detect_one),
    fill = TRUE
  )
  
  if (nrow(result) == 0) {
    if (verbose) message("[detect_outliers] No outliers detected with method '", method, "'.")
    return(NULL)
  }
  
  return(result)
}


