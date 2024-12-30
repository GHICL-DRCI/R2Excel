#' modified_state data
#'
#' Based on datasets::states* datasets, with somes modification, especially
#' the add of a binary test variable, add ... (see bellow)
#'
#' @format ## `modified_state`
#' A data frame with 50 rows and ... columns:
#' \describe{
#'   \item{Population, Income, Illiteracy, Life Exp, Murder, HS Grad, Frost, Area}{Numerics, Population size, Income in USD, ... }
#'   \item{state.division, state.region, election}{Factors, States qualitative informations...}
#'   \item{binary_test}{Factor, binary information}
#'   \item{yes_no_french_question}{Factor, binary yes no information}
#'   \item{all_count_zero}{Factor, but with only one modality "0"}
#'   \item{var_ab_NAN, special_condition}{Factor, completely NA for one level of election (red)}
#'   \item{special_measures}{Numerics, completely NA for one level of election (red)}
#'   \item{var_conti_trap, var_quali_trap, zero_levels}{trap variables to test errors in functions}
#' }
#' @source <https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/state>
"modified_state"


#' modified_sleep data
#'
#' Based on stats::sleep dataset, with somes modification, especially
#' the add of a visites variable.
#'
#' @format ## `modified_sleep`
#' A data frame with 40 rows and 6 columns:
#' \describe{
#'   \item{extra, Income, extra_with_missings, mesure1, mesure2, var1}{Numerics}
#'   \item{group, fact1, fact1_na, fact2, fact3, var2}{Factors}
#'   \item{visites_2, visites_4, visites_5}{Factors, visites factors}
#'   \item{ID, ID2, ID_group}{Factor, to identifiy individuals information}
#'   \item{all_zero_values}{Numerics, trap variables to test errors in functions}
#' }
"modified_sleep"
