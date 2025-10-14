#' compute SMD  
#'
#' Standardized mean difference  
#' 
#' Interpretation of SMD values :  
#' The generally accepted thresholds for interpreting effect size are :  
#' |SMD| < 0.1: negligible difference,  
#' 0.1 ≤ |SMD| < 0.2: "Imbalance was usually defined as "stddiff" greater than 0.1 or 0.2 (which means the small effect size)."  
#' 0.2 ≤ |SMD| < 0.5: between small and moderate difference,    
#' 0.5 ≤ |SMD| < 0.8: between moderate and large difference,  
#' |SMD| ≥ 0.8: large difference.  
#' 
#' These thresholds help to assess the balance between groups,  
#'  especially after matching or adjustment procedures.  
#'
#' Computation follows {stddiff} r pkg implementation 
#' (https://cran.r-project.org/web/packages/stddiff/index.html)  
#' 
#' Reading of thresholds are based on :  
#'   - Cohen, J. (1988).  Cohen J. Statistical Power Analysis for the Behavioral Sciences (2nd ed). Lawrence Erlbaum Associates Publishers: Hillsdale, NJ.   
#'   - also quoted in https://www.utstat.toronto.edu/brunner/oldclass/378f16/readings/CohenPower.pdf (page 40 – num 57/579)  
#'   - also quoted in SAS documentation : https://support.sas.com/resources/papers/proceedings12/335-2012.pdf  
#' 
#' Why SMD ?  
#' In the context clinical trials, the use of statistical tests to assess
#' baseline comparability is illogical, as noted by Altman in 1985:  
#' "… performing a significance test to compare baseline variables is to assess 
#' the probability of something having occurred by chance when we know that it did
#' occur by chance. Such a procedure is clearly absurd."  
#' Statisticians and clinical research methodologists have maintained that baseline 
#' significance testing should not be performed, with Austin noting in 2010:  
#' "With few exceptions, the statistical literature is uniform in its agreement 
#' on the inappropriateness of using hypothesis testing to compare the 
#' distribution of baseline covariates between treated and untreated 
#' subjects in randomised controlled trials." 
#' (https://www.sciencedirect.com/science/article/pii/S1836955315000806?via%3Dihub)
#' 
#' 
#' @param dataframe A data.frame. tibble or data.table
#' @param vars A vector of characters. Names of dataframe's columns.
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
#'     "HS Grad", "Frost", "Area", "yes_no_french_question", "binary_test",
#'     "state.division", "state.region"
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
  stopifnot(is.factor(dataframe[[varstrat]]))
  stopifnot(digits >= 0)
  
  stopifnot(nlevels(dataframe[[varstrat]]) == 2)
  
  # detection of variables
  vars_numeric <- get_numerics(dataframe, vars)
  vars_fact <- get_factors(dataframe, vars)
  
  SMD_tab <- data.table::rbindlist(lapply(X = vars, function(vari){
    if (vari %in% vars_numeric) {
      return(smd_num(
        dataframe,
        varstrat = varstrat,
        vars = vari
      ))
    }
    if (vari %in% vars_fact) {
      return(smd_cat(
        dataframe,
        varstrat = varstrat,
        vars = vari
      ))
    }
  }), fill = TRUE, use.names = TRUE)
  SMD_tab$SMD <- round(SMD_tab$SMD, digits = digits)
  return(SMD_tab)
}



#' Standardized mean difference for numeric (continuous) variables
#' 
#' see more details of interpretations and use cases in `compute_SMD_table` documentation.
#' 
#' @param dataframe A data.frame. tibble or data.table
#' @param vars A vector of characters. Names of dataframe's columns.
#' @param varstrat A character. Default NULL. Name of the stratification variable, making groups to compare.
#' 
#' @return A table of SMD values for each vars.
#' 
#' @export
#' @examples
#' \dontrun{
#' smd_num(
#'   modified_state,
#'   varstrat = "election", 
#'   vars = c(
#'     "Life Exp", "Frost", "Area"
#'   ) 
#' )
#' }
smd_num <- function(dataframe, varstrat, vars) {
  # stopifnot numeric ? to add  --here
  SMD_tab <- data.table::rbindlist(lapply(X = vars, function(vari) {
    x <- dataframe[[vari]]
    stopifnot(is.numeric(x))
    g <- dataframe[[varstrat]]
    g_levels <- levels(dataframe[[varstrat]])
    
    stddiff <- (
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
        SMD = stddiff
      )
    )
  }), use.names = TRUE, fill = TRUE)
  return(SMD_tab)
}


#' Standardized mean difference for qualitative variables
#' 
#' see more details of interpretations and use cases in `compute_SMD_table` documentation.
#' 
#' @param dataframe A data.frame. tibble or data.table
#' @param vars A vector of characters. Names of dataframe's columns.
#' @param varstrat A character. Default NULL. Name of the stratification variable, making groups to compare.
#' 
#' @return A table of SMD values for each vars.
#' 
#' @export
#' @examples
#' \dontrun{
#' smd_cat(
#'   modified_state,
#'   varstrat = "election", 
#'   vars = c(
#'     "yes_no_french_question", "binary_test",
#'     "state.division", "state.region"
#'   ) 
#' )
#' }
smd_cat <- function (dataframe, varstrat, vars) {
  # stopifnot quali = factors ? to add  --here
  dataframe <- as.data.frame(dataframe)
  
  SMD_tab <- data.table::rbindlist(
    lapply(vars, function(vari) {
      stopifnot(is.factor(dataframe[[vari]]))
      temp <- dataframe[, c(varstrat, vari)]
      temp <- temp[complete.cases(temp), ]
      
      # table des proportions par niveau
      tbl <- table(temp[[vari]], temp[[varstrat]])
      prop <- prop.table(tbl, 2)
      t <- prop[-1,1]  # suppose ref = 2e niveau
      c <- prop[-1,2]
      
      k <- length(t)
      # matrice S construite selon Yang & Dalton (2012)
      s_mat <- matrix(0, nrow = k, ncol = k)
      for (i in seq_len(k)) {
        for (j in seq_len(k)) {
          s_mat[i,j] <- if (i == j) {
            0.5 * (t[i]*(1 - t[i]) + c[i]*(1 - c[i]))
          } else {
            -0.5 * (t[i]*t[j] + c[i]*c[j])
          }
        }
      }
      
      has_issues <- try(tools::assertCondition(
        S_inv <- solve(s_mat)
      ), silent = TRUE)
      has_issues <- has_issues[
        sapply(has_issues, function(el) {
          length(base::intersect(class(el), c("warning", "error"))) != 0
        })
      ]
      if (length(has_issues) == 0) { 
        # No error
        tc1 <- t - c
        stddiff <- sqrt(as.numeric(t(tc1) %*% S_inv %*% tc1))
      } else {
        message("[smd_cat] Error : can not solve(s_mat)")
        stddiff <- NA 
      }
      
      data.table(
        Variable = vari, SMD = stddiff
      )
    }),
    use.names = TRUE, fill = TRUE
  )
  return(SMD_tab)
}


#' Standardized mean difference for binary variables
#' 
#' see more details of interpretations and use cases in `compute_SMD_table` documentation.
#' 
#' FYI : Not called in `compute_SMD_table` because `smd_bin` provides same results than `smd_cat` in binary cases.
#' 
#' @param dataframe A data.frame. tibble or data.table
#' @param vars A vector of characters. Names of dataframe's columns.
#' @param varstrat A character. Default NULL. Name of the stratification variable, making groups to compare.
#' 
#' @return A table of SMD values for each vars.
#' 
#' @export
#' @examples
#' \dontrun{
#' smd_bin(
#'   modified_state,
#'   varstrat = "election", 
#'   vars = c(
#'     "yes_no_french_question", "binary_test"
#'   ) 
#' )
#' }
smd_bin <- function(dataframe, varstrat, vars) {
  # stopifnot all binary vars to add --here
  
  dataframe <- as.data.frame(dataframe)
  SMD_tab <- data.table::rbindlist(lapply(vars, function(vari) {
    temp <- na.omit(dataframe[, c(varstrat, vari)])
    
    temp[, 2] <- as.numeric(temp[, 2])
    p1 <- mean(temp[, 2][which(temp[, 1] == levels(temp[, 1])[2])]) - 1
    p2 <- mean(temp[, 2][which(temp[, 1] == levels(temp[, 1])[1])]) - 1
    stddiff <- abs(p1 - p2)/sqrt((p1 * (1 - p1) + p2 * (1 - p2))/2)
    return(
      data.table(
        Variable = vari,
        SMD = stddiff
      )     
    )
  }), fill = TRUE, use.names = TRUE)
  return(SMD_tab)
}
