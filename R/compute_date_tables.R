
#' compute date table
#'
#' provide descriptive statistics table for dates
#'
#' @param dataframe A data.frame. tibble or data.table will be converted into data.frame.
#' @param vars A vector of characters. Names of dataframe's date columns to describe.
#'   only consider "Date", "POSIXct" or "POSIXt" format. 
#' @param varstrat A character. Default NULL. Name of the stratification variable,
#'  making groups to compare.
#'  
#' @return A list of tables with nb measures, mean, med, min and max of dates.
#' 
#' @export
#' @examples
#' \dontrun{
#'dt <- data.frame(
#'  date1 = as.Date(c("2025-07-30", "2025-07-29", "2025-07-28", "2025-07-30", "2025-07-29", "2025-07-28")), 
#'  date2 = as.POSIXct(c("2024-06-20", "2024-06-10", "2024-06-01", "2025-07-30", "2025-07-29", "2025-07-28")), 
#'  fact = as.factor(c(rep("A", 3), rep("B", 3)))
#')
#' get_dates(dataframe = dt, vars = names(dt))
#' compute_date_table(
#'   dataframe = dt,
#'   vars = names(dt)
#' )
#' compute_date_table(
#'   dataframe = dt,
#'   vars = names(dt), 
#'   varstrat = "fact"
#' )
#' }
compute_date_table <- function(
  dataframe,
  vars = setdiff(colnames(dataframe), varstrat),
  varstrat = NULL
) {
  # v0.1.24
  # message("[compute_date_table]")
  dataframe <- as.data.frame(dataframe)
  ## stops
  stopifnot(all(vars %in% names(dataframe)))

  # get factorial variables
  vars_dates <- get_dates(dataframe, vars)
  if (!all(vars %in% vars_dates)) {
    message("[compute_date_table] Warning, some of selected vars were ignored (not dates).")
    message(
      "[compute_date_table] Only describ date (Date, POSIXct or POSIXt) : ",
      paste(vars_dates,collapse = ",")
    )
  }
  
  
  if (is.null(varstrat) || varstrat %in% "") { # univariate analyse
    message("[compute_date_table] without varstrat")
    desc_date_final <- data.table::rbindlist(
      lapply(X = vars_dates, FUN = function(date_coli) {
        # date_coli <- "DDN"
        date_i <- as.Date(dataframe[[date_coli]], format="%d-%m-%Y") 
        # only keep date, remove hours
        nb_m <- sum(!is.na(date_i))
        date_i <- date_i[!is.na(date_i)]
        line_res <- unlist(list(
          date_coli, # Variable
          nb_m, # Nb_mesures
          # V3=Min.  V4=1st Qu.  V5=Median  V6=Mean  V7=3rd Qu.  V8=Max.
          as.character(summary(date_i))
        ))
        desc_date <- as.data.frame(t(line_res))
        names(desc_date)[1:2] <- c("Variable", "Nb_mesures")
        desc_date$Moy <- desc_date$V6
        desc_date$`Med [Q1;Q3]` <- paste0(
          desc_date$V5, " ",
          "[", desc_date$V4, ";", desc_date$V7, "]"
        )
        desc_date$`Min - Max` <- paste0(
          desc_date$V3, " - ",
          desc_date$V8
        )
        desc_date <- data.table::setDT(desc_date)[
          , .SD, .SDcols = c(
            "Variable", "Nb_mesures", "Moy", "Med [Q1;Q3]", "Min - Max"
          )
        ]
        return(desc_date)
      }),
      fill = TRUE, use.names = TRUE
    )
  
    
  } else { 
    # bivariate analyse
    message("[compute_date_table] with varstrat ", varstrat)
    # possible only on factorial varstrat
    stopifnot(varstrat %in% names(dataframe))
    stopifnot(is.factor(dataframe[, varstrat]))
    
    # table, modalitiess and numbre of modalities for varstrat
    varstrat_levels <- levels(dataframe[, varstrat])
    varstrat_nlevels <- nlevels(dataframe[, varstrat])
    
    desc_date_final <- data.table::rbindlist(
      lapply(X = varstrat_levels, FUN = function(leveli) {
        
        dataframe_i <- dataframe[dataframe[[varstrat]] %in% leveli,]
        desc_date <- data.table::rbindlist(
          lapply(X = vars_dates, FUN = function(date_coli) {
            date_i <- as.Date(dataframe_i[[date_coli]], format="%d-%m-%Y") 
            # only keep date, remove hours
            nb_m <- sum(!is.na(date_i))
            date_i <- date_i[!is.na(date_i)]
            line_res <- unlist(list(
              date_coli, # Variable
              nb_m, # Nb_mesures
              # V3=Min.  V4=1st Qu.  V5=Median  V6=Mean  V7=3rd Qu.  V8=Max.
              as.character(summary(date_i))
            ))
            desc_date <- as.data.frame(t(line_res))
            names(desc_date)[1:2] <- c("Variable", "Nb_mesures")
            desc_date$Moy <- desc_date$V6
            desc_date$`Med [Q1;Q3]` <- paste0(
              desc_date$V5, " ",
              "[", desc_date$V4, ";", desc_date$V7, "]"
            )
            desc_date$`Min - Max` <- paste0(
              desc_date$V3, " - ",
              desc_date$V8
            )
            desc_date <- data.table::setDT(desc_date)[
              , .SD, .SDcols = c(
                "Variable", "Nb_mesures", "Moy", "Med [Q1;Q3]", "Min - Max"
              )
            ]
            return(desc_date)
          }),
          fill = TRUE, use.names = TRUE
        )
        desc_date$varstrat <- varstrat
        desc_date$levels <- leveli
        return(desc_date)
      }), 
      fill = TRUE, use.names = TRUE
    )
    data.table::setcolorder(x = desc_date_final, neworder = unique(c(
      "varstrat", "levels", names(desc_date_final)
    )))
    # desc_date_final
  }
  
  return(desc_date_final)
}
