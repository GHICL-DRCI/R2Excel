message("test compute tables - done v0.1.27") # classique

#### continuous tables ####

vars_wanted <- c(
  "Population", "Income", "Illiteracy", "Life Exp", "Murder",
  "HS Grad", "Frost", "Area"
)
varstrat_wanted <- "election"

res_conti_tabs <- compute_continuous_table(
  dataframe = modified_state,
  vars = vars_wanted,
   varstrat = varstrat_wanted,
   precision = 2
)

test_that("test multi conti list", {
  expect_true(is.list(res_conti_tabs))
  expect_equal(length(res_conti_tabs), length(vars_wanted))
})

test_that("test dim in conti tab", {
  expect_equal(nrow(res_conti_tabs[[1]]), 3)
  expect_equal(ncol(res_conti_tabs[[1]]), 11)
  expect_identical(
    rownames(res_conti_tabs[[1]]),
    c("Population", paste0(varstrat_wanted, unique(modified_state$election)))
  )
  expect_identical(
    colnames(res_conti_tabs[[1]]),
    c("mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "Valeurs_manquantes",
      "Nb_mesures", "is_Normal") # default
  )
})

test_that("test values in conti tab", {
  expect_equal(
    res_conti_tabs[[1]][1, "mean"],
    round(mean(modified_state[[vars_wanted[1]]]), 2)
  )
  #  0.1.27 round digit 1
  expect_equal(
    res_conti_tabs[[1]][1, "sd"],
    sd(modified_state[[vars_wanted[1]]]),
    tolerance = 0.1
  )
  expect_false( # round 1 digits
    res_conti_tabs[[1]][1, "sd"] == round(sd(modified_state[[vars_wanted[1]]]), 1)
  )

  # Q1 et Q3
  expect_equal(
    res_conti_tabs[[1]][1, "Q1"],
    unname(quantile(modified_state[[vars_wanted[1]]])["25%"]),
    tolerance = 0.1
  )
  expect_equal(
    res_conti_tabs[[1]][1, "Q3"],
    unname(quantile(modified_state[[vars_wanted[1]]])["75%"]),
    tolerance = 0.1
  )

  expect_equal(
    res_conti_tabs[[1]]["electionred", "mean"],
    round(mean(modified_state[[vars_wanted[1]]][modified_state$election %in% "red"]), 2),
    tolerance = 0
  )
  expect_equal(
    res_conti_tabs[[1]]["electionred", "sd"],
    round(sd(modified_state[[vars_wanted[1]]][modified_state$election %in% "red"]), 2),
    tolerance = 0
  )
  expect_equal(
    res_conti_tabs[[1]]["electionred", "Valeurs_manquantes"],
    sum(is.na(modified_state[[vars_wanted[1]]][modified_state$election %in% "red"])),
    tolerance = 0
  )
  expect_equal(
    res_conti_tabs[[1]]["electionblue", "median"],
    round(median(modified_state[[vars_wanted[1]]][modified_state$election %in% "blue"]), 2),
    tolerance = 0
  )
  expect_equal(
    res_conti_tabs[[1]]["electionblue", "min"],
    round(min(modified_state[[vars_wanted[1]]][modified_state$election %in% "blue"]), 2),
    tolerance = 0
  )
  expect_equal(
    res_conti_tabs[[1]]["electionblue", "max"],
    round(max(modified_state[[vars_wanted[1]]][modified_state$election %in% "blue"]), 2),
    tolerance = 0
  )

})

test_that("test NAs in conti tab", {
  # in Frost
  expect_equal(
    res_conti_tabs[["Frost"]][1, "Valeurs_manquantes"],
    sum(is.na(modified_state[["Frost"]]))
  )
  expect_equal(
    res_conti_tabs[["Frost"]][1, "Nb_mesures"],
    sum(!is.na(modified_state[["Frost"]]))
  )
})

res_conti_tabs_subset <- compute_continuous_table(
  dataframe = modified_state,
  vars = vars_wanted[1],
  varstrat = varstrat_wanted,
  stats_choice = c("mean", "sd", "N"),
  precision = 2
)

test_that("test dim in conti subset tab", {
  expect_equal(length(res_conti_tabs_subset), 1)
  expect_equal(nrow(res_conti_tabs_subset[[1]]), 3) # pop globla + red + blue
  expect_equal(ncol(res_conti_tabs_subset[[1]]), 3) # "mean", "sd", "N"
  expect_identical(
    rownames(res_conti_tabs_subset[[1]]),
    c("Population", paste0(varstrat_wanted, unique(modified_state$election)))
  )
  expect_identical(
    colnames(res_conti_tabs_subset[[1]]),
    c("mean", "sd", "N")
  )
})



# Données avec différentes précisions
data <- data.frame(
  age = c(25, 30, 35, 40),           # Entiers -> 0 décimale
  poids = c(70.5, 75.2, 68.9, 72.1), # 1 décimale
  taille = c(1.75, 1.82, 1.68, 1.79), # 2 décimales
  biomarqueur = c(0.0125, 0.0138, 0.0142, 0.0129) # 4 décimales
)

# Avec precision auto
result_auto <- compute_continuous_table(
  dataframe = data,
  precision = "auto"
)
# result_auto
# age -> moyenne à 1 décimale, sd à 2 (max 3)
# poids -> moyenne à 2 décimales, sd à 3
# taille -> moyenne à 3 décimales, sd à 3 (max atteint)
# biomarqueur -> moyenne à 5 décimales, sd à 3 (max atteint)

# Avec digits fixe (mode classique)
result_fixed <- compute_continuous_table(
  dataframe = data,
  precision = 2
)
# result_fixed

test_that("test precision", {
  expect_equal(
    result_auto$mean,
    c(32.5, 71.67, 1.760, 0.01335)
  )
  expect_equal(
    result_auto$sd,
    c(6.450, 2.689, 0.061, 0.001)
  )

  expect_equal(
    result_fixed$mean,
    c(32.50, 71.67, 1.76, 0.01)
  )
  expect_equal(
    result_fixed$sd,
    c(6.45, 2.69, 0.06, 0.00)
  )

})


#### shapi ####

# v0.1.22 test shapiro to skip
# shapiro.test not executable with less than 3 point (in overall population, in bivar mode)
modified_state$twovalues <- c(3.3, 4.4, rep(NA, nrow(modified_state)-2))
res_shapi <- compute_continuous_table(
  dataframe = modified_state,
  vars = c("Population", "twovalues"),
  varstrat = "election"
)
# mentione as "warning" :
# message d'avis
modified_state$twovalues <- NULL
test_that("test shapi no error", {
  expect_equal(length(res_shapi), 2)
  expect_equal(nrow(res_shapi[[1]]), 3)
  expect_equal(nrow(res_shapi[[2]]), 3)
  expect_equal(ncol(res_shapi[[1]]), 11) 
  expect_equal(ncol(res_shapi[[2]]), 11) 
  expect_true(all(res_shapi[[1]]$is_Normal %in% 0))
  expect_true(all(is.na(res_shapi[[2]]$is_Normal)))
})


#### factorial tables ####

vars_wanted <- c("state.division", "state.region", "binary_test")
varstrat_wanted <- "election"
res_fact_tabs <- compute_factorial_table(
  dataframe = modified_state,
  vars = vars_wanted,
  varstrat = varstrat_wanted,
  precision = 2,
  simplify = FALSE
)

test_that("test multi fact list", {
  expect_true(is.list(res_fact_tabs))
  expect_equal(length(res_fact_tabs), length(vars_wanted))
})

test_that("test dim in fact tab", {
  expect_equal(
    nrow(res_fact_tabs[[1]]),
    length(unique(modified_state[[vars_wanted[1]]])) + 2 # NA and NB mesures
  )
  expect_equal(
    ncol(res_fact_tabs[[1]]), 6
  )
  expect_identical(
    rownames(res_fact_tabs[[1]]),
    c(levels(modified_state[[vars_wanted[1]]]), "Valeurs_manquantes", "Nb_mesures")
  )
  expect_true(
    all(
      colnames(res_fact_tabs[[1]]) %in%
      apply(expand.grid(c("n", "p"), c("", levels(modified_state$election))), 1, paste0, collapse = "")
    )
  )
})

# res_fact_tabs[[1]]
test_that("test values in conti tab", {
  expect_equal(
    res_fact_tabs[[1]][1, "n"],
    sum(modified_state[[vars_wanted[1]]] %in% levels(modified_state[[vars_wanted[1]]])[1])
  )
  expect_equal(
    res_fact_tabs[[1]][1, "p"],
    paste0("(", sum(modified_state[[vars_wanted[1]]] %in% levels(modified_state[[vars_wanted[1]]])[1]) /
      length(modified_state[[vars_wanted[1]]]) * 100, "%)"
    )
  )
  expect_false(
    res_fact_tabs[[1]][2, "n"] ==
    sum(modified_state[[vars_wanted[1]]] %in% levels(modified_state[[vars_wanted[1]]])[1])
  )
  expect_equal(
    res_fact_tabs[[1]]["Nb_mesure", "n"],
    length(modified_state[[vars_wanted[1]]])
  )

  # blue
  expect_equal(
    res_fact_tabs[[1]][3, "nblue"],
    sum(modified_state[modified_state$election %in% "blue", ][[vars_wanted[1]]] %in% levels(modified_state[[vars_wanted[1]]])[3])
  )
  expect_equal(
    res_fact_tabs[[1]][3, "pblue"],
    paste0("(", round(
      sum(modified_state[modified_state$election %in% "blue", ][[vars_wanted[1]]] %in%
            levels(modified_state[[vars_wanted[1]]])[3]) /
             length(modified_state[modified_state$election %in% "blue", ][[vars_wanted[1]]]) * 100,
      digits = 2
      ), "%)"
    )
  )
  # red
  expect_equal(
    res_fact_tabs[[1]][3, "nred"],
    sum(modified_state[modified_state$election %in% "red", ][[vars_wanted[1]]] %in% levels(modified_state[[vars_wanted[1]]])[3])
  )
  expect_equal(
    res_fact_tabs[[1]][3, "pred"],
    paste0("(", round(
      sum(modified_state[modified_state$election %in% "red", ][[vars_wanted[1]]] %in%
            levels(modified_state[[vars_wanted[1]]])[3]) /
        length(modified_state[modified_state$election %in% "red", ][[vars_wanted[1]]]) * 100,
      digits = 2
    ), "%)"
    )
  )
})

#### factorial tables simplified ####

vars_wanted <- c("state.division", "state.region", "binary_test", "yes_no_french_question")
varstrat_wanted <- "election"
res_fact_simpl_tabs <- compute_factorial_table(
  dataframe = modified_state,
  vars = vars_wanted,
  varstrat = varstrat_wanted,
  precision = 2,
  simplify = TRUE
)
# res_fact_simpl_tabs

test_that("test simplified fact", {
  expect_true(is.list(res_fact_simpl_tabs))
  expect_equal(length(res_fact_simpl_tabs), length(vars_wanted))
  expect_true(all(res_fact_simpl_tabs$state.division == res_fact_tabs$state.division))
  expect_true(all(res_fact_simpl_tabs$state.region == res_fact_tabs$state.region))
  expect_false(nrow(res_fact_simpl_tabs$binary_test) == nrow(res_fact_tabs$binary_test))
  expect_true( (nrow(res_fact_simpl_tabs$binary_test) + 1) == nrow(res_fact_tabs$binary_test))
  expect_equal(nrow(res_fact_simpl_tabs$yes_no_french_question), 3)
})

#### factorial tables simplified zero ####

vars_wanted <- c("yes_no_french_question", "all_count_zero")
varstrat_wanted <- "election"

res_fact_simpl0_tabs <- compute_factorial_table(
  dataframe = modified_state,
  vars = vars_wanted,
  varstrat = varstrat_wanted,
  precision = 2,
  simplify = TRUE,
  force_generate_1_when_0 = TRUE
)
# res_fact_simpl0_tabs

test_that("test simplified zero fact", {
  expect_true(is.list(res_fact_simpl0_tabs))
  expect_equal(length(res_fact_simpl0_tabs), length(vars_wanted))
  # check modalité 1 for all_count_zero
  expect_true(
    all(res_fact_simpl0_tabs$yes_no_french_question == res_fact_simpl_tabs$yes_no_french_question)
  )
  expect_true(grepl("1", rownames(res_fact_simpl0_tabs$all_count_zero)[1]))
})

res_fact_simpl0_tabs_2 <- compute_factorial_table(
  dataframe = modified_state,
  vars = vars_wanted,
  varstrat = varstrat_wanted,
  precision = 2,
  simplify = FALSE,
  force_generate_1_when_0 = TRUE
)
# res_fact_simpl0_tabs_2

#### test NAN when modality never seen in one level of varstrat ####

with(modified_state, table(var_ab_NAN, election))
res_fact_NAN_tab <- compute_factorial_table(
  dataframe = modified_state,
  vars = c("yes_no_french_question", "var_ab_NAN", "election"),
  varstrat = varstrat_wanted,
  precision = 2,
  simplify = FALSE,
  force_generate_1_when_0 = FALSE
)
# res_fact_NAN_tab
test_that("test NAN % fact", {
  expect_false(all(grepl("(NaN%)", res_fact_NAN_tab$var_ab_NAN$pred)))
  expect_true(
    all(res_fact_NAN_tab$var_ab_NAN[c("a", "b"), "pred"] %in% c("(0%)", "(0%)"))
  )
})

#### Correlation table ####

corr_tab <- compute_correlation_table(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp",
    "HS Grad", "Frost"
  ),
  varstrat = "Area",
  method_corr = "detect_auto",
  precision = 2
)
test_that("test correlation tab", {
  expect_equal(nrow(corr_tab), 6)
  tmp <- corr_tab[
    (!corr_tab$is_Normal | !corr_tab$varstrat_is_Normal),
  ]
  expect_true(all(tmp$correlation_method %in% "spearman"))

  val1 <- cor(modified_state$Area, modified_state$Population, method = "spearman")
  expect_equal(
    round(val1, 2),
    corr_tab$correlation[corr_tab$varstrat %in% "Area" & corr_tab$Variable %in% "Population"]
  )

})


#### SMD table ####

SMD_tab <- compute_SMD_table(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area"
  ),
  varstrat = "election",
  precision = 2
)

SMD_tab2 <- compute_SMD_table(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Area"
  ),
  varstrat = "binary_test",
  precision = 3
)

test_that("smd computation", {
  expect_equal(
    nrow(SMD_tab),
    length(c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area"
  )))
  expect_equal(
    nrow(SMD_tab2), 3
  )
  expect_equal(
    SMD_tab2$SMD,
    c(-0.088, 0.321, 0.266)
  )
  
  ## compute by hand (same than in compute_SMD_table function)
  x <- modified_state$Murder
  g <- modified_state$election
  level_elec <- levels(g)
  smd_murder <- (
    mean(x[g %in% level_elec[1]], na.rm = TRUE) -
      mean(x[g %in% level_elec[2]], na.rm = TRUE)
  ) / (
    sqrt(
      (
        var(x[g %in% level_elec[1]], na.rm = TRUE) +
          var(x[g %in% level_elec[2]], na.rm = TRUE)
      ) / 2
    )
  )
  expect_equal(
    round(smd_murder, 2), 
    SMD_tab$SMD[SMD_tab$Variable %in% "Murder"]
  )
  
})


test_that("error no smd ", {
  expect_error(
    compute_SMD_table(
      dataframe = modified_state,
      vars = c(
        "Population", "Income", "Illiteracy", "Life Exp", "Murder",
        "HS Grad", "Frost", "Area"
      ),
      varstrat = "special_condition",
      precision = 2
    )
  )
})



# =
# Tests pour compute_continuous_table()
# =

test_that("compute_continuous_table returns correct structure", {
  df <- data.frame(
    var1 = c(10, 20, 30, 40, 50),
    var2 = c(1, 2, 3, 4, 5)
  )
  
  result <- compute_continuous_table(dataframe = df, vars = "var1", precision = 2)
  
  expect_true(is.list(result))
  expect_true(all(
    c("mean", "sd", "median", "Q1", "Q3", 
      "Valeurs_manquantes", "Nb_mesures") %in% names(result)))
  expect_true(all(sapply(result, is.numeric)))
})

test_that("compute_continuous_table calculates correct statistics", {
  df <- data.frame(
    var1 = c(10, 20, 30, 40, 50)
  )
  
  result <- compute_continuous_table(dataframe = df, vars = "var1", precision = 2)
  
  expect_equal(result$mean, 30)
  expect_equal(result$median, 30)
  expect_equal(result$Q1[[1]], 20)
  expect_equal(result$Q3[[1]], 40)
  expect_equal(result$Valeurs_manquantes, 0)
  expect_equal(result$Nb_mesures, 5)
})

test_that("compute_group_summary handles missing values", {
  df <- data.frame(
    var1 = c(10, 20, NA, 40, 50, NA)
  )
  
  result <- compute_continuous_table(dataframe = df, vars = "var1", precision = 2)
  
  expect_equal(result$mean, 30)  # (10+20+40+50)/4
  expect_equal(result$Valeurs_manquantes, 2)
  expect_equal(result$Nb_mesures, 4)
})

test_that("compute_group_summary respects digits parameter", {
  df <- data.frame(
    var1 = c(10.12345, 20.67890, 30.11111)
  )
  
  result_2dig <- compute_continuous_table(dataframe = df, vars = "var1", precision = 2)
  result_4dig <- compute_continuous_table(dataframe = df, vars = "var1", precision = 4)
  
  expect_equal(result_2dig$mean, 20.3)
  expect_equal(result_4dig$mean,  20.3045)
})

test_that("compute_group_summary handles all NA = NULL", {
  df <- data.frame(
    var1 = c(NA, NA, NA)
  )
  
  result <- compute_continuous_table(dataframe = df, vars = "var1", precision = 2)
  
  expect_true(is.null(result))
})
