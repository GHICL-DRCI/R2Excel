# Some usefull functions

#' get factors
#'
#' provide the names of factorial columns
#'
#' @param dataframe A data.frame.
#' @param vars A vector of characters. Names of dataframe to test.
#'
#' @return A vector of characters : the names of factorial columns
#' @export
#' @examples
#' \dontrun{
# get_factors(
#   dataframe = modified_state,
#   vars = c("state.division", "state.region", "Income", "Illiteracy")
# )
#' }
get_factors <- function(
  dataframe, vars = colnames(dataframe)
) {
  return(vars[sapply(as.data.frame(dataframe)[, vars, drop = FALSE], is.factor)])
}

#' get numerics
#'
#' provide the names of numeric columns
#'
#' @param dataframe A data.frame.
#' @param vars A vector of characters. Names of dataframe to test.
#'
#' @return A vector of characters : the names of numeric columns
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

#' get characters
#'
#' provide the names of numeric columns
#'
#' @param dataframe A data.frame.
#' @param vars A vector of characters. Names of dataframe to test.
#'
#' @return A vector of characters : the names of character columns
#' @export
#' @examples
#' \dontrun{
#' get_numerics(
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


#' vector to character
#'
#' transformation of a vector into a text
#'
#' @param vector A vector wanted to be transformed.
#' @param sep A character string to separate the terms. Not NA_character_. Default ",".
#' @param NULL_value A character null. Default character(0). Return when not applicabe.
#'
#' @return A character
#' @export
#' @examples
#' \dontrun{ ector_to_character(vector = c("mean = 2", "median = 3", "max = 4")) }
vector_to_character <- function(
  vector,
  sep = ",",
  NULL_value = character(0)
) {
  # if NULL
  if (is.null(vector)) {
    return(NULL_value)
  }

  # remove NA
  vector <- na.omit(vector)
  # if lenght 0
  if (length(vector) == 0) {
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
