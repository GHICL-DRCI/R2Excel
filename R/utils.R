# Some useful functions

#' get_factors
#' 
#' get factors : provide the names of factorial columns
#'
#' @param dataframe A data.frame.
#' @param vars A vector of characters. Names of dataframe to test.
#'
#' @return A vector of characters : the names of factorial columns
#' 
#' @export
#' @examples
#' \dontrun{
#' get_factors(
#'  dataframe = modified_state,
#'  vars = c("state.division", "state.region", "Income", "Illiteracy")
#' )
#' }
get_factors <- function(
  dataframe, vars = colnames(dataframe)
) {
  return(vars[sapply(as.data.frame(dataframe)[, vars, drop = FALSE], is.factor)])
}

#' get_numerics
#' 
#' get numerics : provide the names of numeric columns
#'
#' @param dataframe A data.frame.
#' @param vars A vector of characters. Names of dataframe to test.
#'
#' @return A vector of characters : the names of numeric columns
#' 
#' @export
#' @examples
#' \dontrun{
#' get_numerics(
#'   dataframe = modified_state,
#'   vars = c("state.division", "state.region", "Income", "Illiteracy")
#' )
#' }
get_numerics <- function(
  dataframe, vars = colnames(dataframe)
) {
  return(vars[sapply(as.data.frame(dataframe)[, vars, drop = FALSE], is.numeric)])
}

#' get_characters
#' 
#' get characters : provide the names of character columns
#'
#' @param dataframe A data.frame.
#' @param vars A vector of characters. Names of dataframe to test.
#'
#' @return A vector of characters : the names of character columns
#' 
#' @export
#' @examples
#' \dontrun{
#' get_characters(
#'   dataframe = modified_state,
#'   vars = c("state.division", "state.region", "Income", "Illiteracy")
#' )
#' }
get_characters <- function(
  dataframe, 
  vars = colnames(dataframe)
) {
  return(vars[sapply(as.data.frame(dataframe)[, vars, drop = FALSE], is.character)])
}

#' get_dates
#' 
#' get dates : provide the names of date columns
#'
#' @param dataframe A data.frame.
#' @param vars A vector of characters. Names of dataframe to test.
#'
#' @return A vector of dates : the names of date columns
#' 
#' @export
get_dates <- function(
  dataframe, 
  vars = colnames(dataframe)
) {
  return(
    vars[
      unlist(lapply(X = vars, FUN = function(d_coli) {
        di <- dataframe[[d_coli]]
        any(class(di) %in% c("POSIXct", "POSIXt", "Date"))
      }))
    ]
  )
}

#' vector_to_character
#'
#' transformation of a vector into a text (character)
#'
#' @param vector A vector wanted to be transformed.
#' @param sep A character string to separate the terms. Not NA_character_.
#'  Default ",".
#' @param NULL_value A character null. Default character(0).
#'  Return when not applicable.
#'
#' @return A character
#' @export
#' @examples
#' \dontrun{ 
#'   vector_to_character(vector = c("mean = 2", "median = 3", "max = 4")) 
#' }
vector_to_character <- function(
  vector,
  sep = ",",
  NULL_value = character(0)
) {
  if (is.null(vector)) {  # if NULL
    return(NULL_value)
  }
  # else ...
  # remove NA
  vector <- na.omit(vector)
  if (length(vector) == 0) { # if lenght 0
    return(NULL_value)
  }
  # or if length 1
  if (length(vector) == 1) {
    # return(vector[1])
    return(gsub("\\s+", "", vector[1], perl = TRUE))
  }

  # when vector length length >1
  # character <- vector[1]
  # for (ind in 2:length(vector)) {
  #   character <- paste(character, gsub("\\s+", "", vector[ind], perl = T), sep = sep)
  # }
  character <- paste0(gsub("\\s+", "", vector, perl = TRUE), collapse = sep)
  return(character)
}



#' detect_decimal_places
#'
#' Function to detect the number of decimal places in a vector
#'
#' @param x a vector
#' 
#' @return a numeric
#' 
#' @export
#' @examples
#' \dontrun{
#' detect_decimal_places(
#'   c(3.5,3.6,1.5,9)
#' ) # 1
#' detect_decimal_places(
#'   c(3.5526,3.6458,1.512,9.2)
#' ) # 4
#' }
detect_decimal_places <- function(x) {
  # message("[detect_decimal_places]")
  # Remove NA
  x_clean <- x[!is.na(x)]
  
  # if empty vector, return 0
  if (length(x_clean) == 0) {
    return(0)
  }
  
  # Convert to strings to analyse the decimal places
  x_char <- format(x_clean, scientific = FALSE, trim = TRUE)
  
  # Function to count the number of decimal places in a value
  count_decimals <- function(val_char) {
    # Separate with a decimal point
    parts <- strsplit(val_char, "\\.")[[1]]
    
    # If there is no decimal point, use 0
    if (length(parts) == 1) {
      return(0)
    }
    
    # Alternatively, 
    # count the digits after the decimal point, omitting any trailing zeros
    decimal_part <- parts[2]
    # Remove trailing zeros
    decimal_part_trimmed <- sub("0+$", "", decimal_part)
    return(nchar(decimal_part_trimmed))
  }
  
  # Count each value and take the maximum
  decimal_counts <- sapply(x_char, count_decimals, USE.NAMES = FALSE)
  max_decimals <- max(decimal_counts, na.rm = TRUE)
  
  return(max_decimals)
}


#' compute_precision_digits
#'
#' Function to calculate digits according to precision
#' 
#' Because we have established a rule within our quality approach to standardize 
#' the accuracy of the figures provided in our reports. 
#' The rule states that: 
#' The mean and median will be rounded to one decimal place higher than the 
#' accuracy of the original value. 
#' The standard deviation will be rounded to 2 decimal places above the 
#' precision of the original value, up to a maximum of 3 decimal places. 
#'
#' @param x a vector
#' @param stat_type a character
#' @param base_decimals a integer
#' @param max_decimals a integer
#' 
#' @return a numeric
#' 
#' @export
#' @examples
#' \dontrun{
#' compute_precision_digits(
#'   c(3.5,3.6,1.5,9), stat_type = "central", base_decimals = 1
#' )
#' compute_precision_digits(
#'   c(3.5526,3.6458,1.512,9.2), stat_type = "central", base_decimals = 4
#' )
#' compute_precision_digits(
#'   c(3.5526,3.6458,1.512,9.2), stat_type = "sd", base_decimals = 4
#' )
#' }
compute_precision_digits <- function(
  x, 
  stat_type = c("central", "sd"), 
  base_decimals,
  max_decimals = 3
) {

  stat_type <- match.arg(stat_type)
  
  if (stat_type == "central") {
    # mean, median, Q1, Q3, min, max : +1 dec
    digits <- base_decimals + 1
  } else if (stat_type == "sd") {
    # sd : +2 dec, max 3
    digits <- min(base_decimals + 2, max_decimals)
  }
  
  return(digits)
}



#' check_normality
#' 
#' Testing for normality with error handling
#'
#' @param x Numeric vector to test
#' @param return_messages Logical, Should error messages be returned?
#' 
#' @return Logical (normality) or
#'  list with is_normal the logical and its message
#' 
#' @export
check_normality <- function(
    x,
    return_messages = FALSE
) {
  
  has_issues <- try(tools::assertCondition( # capture warning and error
    shap_result <- stats::shapiro.test(x)
  ), silent = TRUE)
  
  has_issues <- has_issues[
    sapply(has_issues, function(el) {
      length(base::intersect(class(el), c("warning", "error"))) != 0
    })
  ]
  
  # test if there is no issues 
  if (length(has_issues) == 0) {
    # data are normal if shapiro test p val > 0.05
    is_normal <- shap_result$p.value > 0.05
    message_captured <- "" # no message
  } else {
    # As a safety measure, if a warning or error occurs,
    # we do not retrieve the test result; 
    is_normal <- FALSE
    # instead, we return the message
    message_captured <- paste(
      unique(sapply(has_issues, function(el) {
        paste0("[", class(el)[2], "] ", el$message)
      })),
      collapse = ";"
    )
  }
  
  if (return_messages) {
    return(list(is_normal = is_normal, message = message_captured))
  } else {
    return(is_normal)
  }
}