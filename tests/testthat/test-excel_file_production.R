dir.create("tmp", showWarnings = FALSE)

#### Test errors ####

# 1
test_that("error No such file or directory ", {
  expect_error(save_excel_results(
    dataframe = modified_state,
    file = file.path("tmp_not_creat", "03-test_final_novarstrat.xlsx"), # No such file or directory
    vars = c(
      "Population", "Income", "Illiteracy", "Life Exp", "Murder",
      "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
    ),
    varstrat = NULL,
    digits = 3,
    simplify = FALSE
  ))
})

#### Test stopifnot ####

# 2
test_that("error stop if not", {
  expect_error(save_excel_results()) # missing dataframe and vars
})


#### Excel file production ####

path03 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "03-test_final_novarstrat.xlsx"),
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = NULL,
  digits = 3,
  simplify = FALSE
)
tab_quali_3 <- readxl::read_excel(
  path = file.path("tmp", "03-test_final_novarstrat.xlsx"), sheet = "qualitative - "
)
tab_quanti_3 <- readxl::read_excel(
  path = file.path("tmp", "03-test_final_novarstrat.xlsx"), sheet = "quantitative - "
)

analyse_desc_quali_3 <- compute_factorial_table(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = NULL, digits = 3, simplify = FALSE
)
analyse_desc_quanti_3 <- compute_continuous_table(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = NULL, digits = 3
)

test_that("final_novarstrat_file", {

  expect_true(file.exists(file.path("tmp", "03-test_final_novarstrat.xlsx")))
  expect_equal(nrow(tab_quanti_3), length(get_numerics(modified_state, vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ))))
  expect_equal(ncol(tab_quanti_3), 7)
  expect_equal(
    tab_quanti_3$`Moy +/- Sd`[tab_quanti_3$Variable %in% "Area"],
    paste(round(mean(modified_state$Area, na.rm = TRUE), 3), "+/-", round(sd(modified_state$Area, na.rm = TRUE), 3))
  )
  expect_equal(length(na.omit(unique(tab_quali_3$Variable))), length(get_factors(modified_state, vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ))))
  expect_equal(ncol(tab_quali_3), 5)
  expect_equal(
    tab_quali_3$`Effectif (%)`[tab_quali_3$Modalites %in% "1"],
    paste0(sum(modified_state$binary_test %in% 1), " (", sum(modified_state$binary_test %in% 1) / nrow(modified_state) * 100, "%)")
  )

  expect_equal(length(analyse_desc_quali_3), length(unique(na.omit(tab_quali_3$Variable))))
  expect_equal(nrow(analyse_desc_quanti_3), nrow(tab_quanti_3))

})

#### Excel file production simplified (4) ####

path04 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "04-test_final_novarstrat_simplified.xlsx"),
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = NULL,
  digits = 3,
  simplify = TRUE
)
tab_quali_simplified_4 <- readxl::read_excel(
  path = file.path("tmp", "04-test_final_novarstrat_simplified.xlsx"), sheet = "qualitative - "
)
tab_quanti_simplified_4 <- readxl::read_excel(
  path = file.path("tmp", "04-test_final_novarstrat_simplified.xlsx"), sheet = "quantitative - "
)

test_that("final_novarstrat_file_simplified_4", {

  expect_true(file.exists(file.path("tmp", "04-test_final_novarstrat_simplified.xlsx")))
  expect_equal(nrow(tab_quali_3), nrow(tab_quali_simplified_4) + 1)
  expect_equal(
    tab_quali_simplified_4$`Effectif (%)`[tab_quali_simplified_4$Variable %in% "binary_test" & tab_quali_simplified_4$Modalites %in% "1"],
    paste0(sum(modified_state$binary_test %in% 1), " (", sum(modified_state$binary_test %in% 1) / nrow(modified_state) * 100, "%)")
  )
  expect_true(all(tab_quali_simplified_4[, -c(1,2)] == tab_quali_3[tab_quali_3$Modalites != "0", -c(1,2)], na.rm = TRUE))
  expect_equal(length(unique(tab_quali_simplified_4$Variable)), length(unique(tab_quali_3$Variable)))
})


#### Excel file production varstrat (5) ####

path05 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "05-test_final_varstrat.xlsx"),
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = "election",
  digits = 3,
  simplify = FALSE,
  prop_table_margin = 2
)
tab_quali_strat_5 <- readxl::read_excel(
  path = file.path("tmp", "05-test_final_varstrat.xlsx"), sheet = "qualitative - election"
)
tab_quanti_strat_5 <- readxl::read_excel(
  path = file.path("tmp", "05-test_final_varstrat.xlsx"), sheet = "quantitative - election"
)

analyse_desc_quanti_5 <- compute_continuous_table(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = "election",
  digits = 3
)
analyse_desc_quali_5 <- compute_factorial_table(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = "election",
  digits = 3,
  simplify = FALSE,
  prop_table_margin = 2
)

test_prop_5 <- test_proportions(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = "election",
  digits = 3
)
test_mean_5 <- test_means(
  dataframe = modified_state,
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = "election",
  digits = 3
)

test_that("final_varstrat_file", {

  expect_true(file.exists(file.path("tmp", "05-test_final_varstrat.xlsx")))
  expect_equal(nrow(tab_quali_3), nrow(tab_quali_strat_5))
  expect_equal(nrow(tab_quanti_3), nrow(tab_quanti_strat_5))
  expect_equal(ncol(tab_quali_strat_5), 10)
  expect_equal(ncol(tab_quanti_strat_5), 9)
  expect_equal(
    tab_quanti_strat_5$Population_totale[tab_quanti_strat_5$Variable %in% "Murder"],
    tab_quanti_3$`Moy +/- Sd`[tab_quanti_3$Variable %in% "Murder"]
  )
  # tab_quanti_3$`Med [Q1-Q3]`[tab_quanti_3$Variable %in% "Murder"] # if not normal --to check
  expect_equal(
    tab_quali_strat_5$Population_totale[tab_quali_strat_5$Modalites %in% "Northeast"],
    tab_quali_3$`Effectif (%)`[tab_quali_3$Modalites %in% "Northeast"]
  )

  ## test pvalues

  # tab_quanti_strat_5[, c("Variable", "P_valeur", "Test", "message")]
  tmp_test_mean_5 <- test_mean_5$results
  tmp_test_mean_5$Variable <- row.names(tmp_test_mean_5)
  tmp_test_mean_5$P_valeur <- signif(tmp_test_mean_5$P_valeur, 4)
  expect_true(all(
    tibble::as_tibble(tmp_test_mean_5[order(tmp_test_mean_5$Variable), c("Variable", "P_valeur", "Test", "message")]) ==
    tab_quanti_strat_5[order(tab_quanti_strat_5$Variable), c("Variable", "P_valeur", "Test", "message")]
  ))

  tmp_test_prop_5 <- test_prop_5$results
  tmp_test_prop_5$Variable <- row.names(tmp_test_prop_5)
  tmp_test_prop_5$P_valeur <- signif(tmp_test_prop_5$P_valeur, 4)
  expect_true(all(
    tibble::as_tibble(tmp_test_prop_5[, c("Variable", "P_valeur", "Test", "message")]) ==
      tab_quali_strat_5[!is.na(tab_quali_strat_5$Variable), c("Variable", "P_valeur", "Test", "message")]
  ))

  tmp_quanti_5 <- analyse_desc_quanti_5$Income["electionred",]
  expect_equal(
    paste0(tmp_quanti_5$mean, " +/- ", tmp_quanti_5$sd),
    # paste0(tmp_quanti_5$median, " [", tmp_quanti_5$Q1, " ; ", tmp_quanti_5$Q3, "]"), # if not normal --tocheck
    tab_quanti_strat_5[tab_quanti_strat_5$Variable %in% "Income", "election=red"][[1]]
  )

  tmp_quali_5 <- analyse_desc_quali_5$state.region["Northeast", ]
  expect_equal(
    paste(tmp_quali_5$nblue, tmp_quali_5$pblue),
    tab_quali_strat_5[tab_quali_strat_5$Modalites %in% "Northeast", "election=blue"][[1]]
  )
})

#### test 6 ####
# about some content...


#### Test generation of 1 when only 0 (7) ####

path07 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "07-test_final_generatelevel.xlsx"),
  vars = c(
    "Population", "yes_no_french_question", "all_count_zero"
  ),
  varstrat = "election",
  digits = 3,
  simplify = TRUE,
  force_generate_1_when_0 = TRUE
)
tab_quali_7 <- readxl::read_excel(
  path = file.path("tmp", "07-test_final_generatelevel.xlsx"), sheet = "qualitative - election"
)
test_that("final_2varstrat_file", {
  expect_true(file.exists(file.path("tmp", "07-test_final_generatelevel.xlsx")))
  expect_true(tab_quali_7$Variable[2] == "all_count_zero")
  expect_true(tab_quali_7$Modalites[2] == "1")
})

#### Check order in Excel, compare to order in computed table (08) ####

test_that("check order", {
  
  vars_quanti_order = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area"
  )
  expect_equal(
    vars_quanti_order, tab_quanti_strat_5$Variable
  )
  vars_quali_order = c(
    "state.division", "state.region", "binary_test"
  )
  expect_equal(
    vars_quali_order, as.vector(na.omit(tab_quali_strat_5$Variable))
  )

  vars = c(
    "yes_no_french_question", "all_count_zero" # quali order
  )
  expect_equal(
    vars, tab_quali_7$Variable
  )

})

#### show OR ####

# ## complex example ## 
# load("X://EC/1. OUVERTES/A. Promotion GHICL/1. COLCHICORT/3. Biostatistiques - Data-Management/1. DM/2. Extractions/20220803/bases.COLCHICORT.20240619.LNK.RData")
# data.PP <- base[which(base$IDPATIENT %in% base.PP$IDPATIENT), ]
# # setwd("X:/DRCI Methodologie/BIOSTATISTIQUES/R2Excel/tests/")
# # dir.create("tmp")
# save_excel_results(
#   data.PP,
#   file = "tmp/08-bivarie.repTTT.testOR.xlsx",
#   vars = names(data.PP)[-c(1, 3, 4)],
#   varstrat = "H24.crit.comp.eff",
#   simplify = TRUE,
#   show_OR = TRUE
# )

#### Test cross vars ####

path02 <- save_excel_results(
  dataframe = modified_state,
  file = "tmp/Descriptive_bivariate_analysis_2.xlsx",
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area",
    "state.division", "state.region",
    "special_condition", "special_measures" # trap
  ),
  varstrat = "election*binary_test",
  digits = 2,
  crossed_varstrat_test = TRUE,
  detail_NB_mesure_sum = TRUE
)
#  add test if no quali vers  :
path03 <- save_excel_results(
  dataframe = modified_state,
  file = "tmp/Descriptive_bivariate_analysis_3.xlsx",
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area"
  ),
  varstrat = "election*binary_test",
  digits = 2,
  crossed_varstrat_test = TRUE,
  detail_NB_mesure_sum = TRUE
)

test_that("cross vars", {
  expect_true(file.exists(path02))
  expect_true(file.exists(path03)) # no more error v0.1.18
})

#### Test P adj ####

path04 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "04-test_padj.xlsx"),
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area", "state.division", "state.region", "binary_test"
  ),
  varstrat = "election",
  digits = 3,
  simplify = FALSE,
  prop_table_margin = 2,
  light_contents = TRUE,
  global_summary = FALSE,
  show_p_adj = TRUE
)
sheet_padj <- readxl::read_excel(
  file.path("tmp", "04-test_padj.xlsx"), sheet = "quantitative - election"
)

test_that("test order p adj", {
  expect_true(file.exists(path04))
  expect_equal(  
    names(sheet_padj), # test order
    c("Variable", "Nb_mesures", "Valeurs_manquantes", 
     "election=blue", "election=red",
     "P_valeur", "P_adj_holm",
     "Test", "message"
    )
  )
})

path05 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "05-test_padj_cross.xlsx"),
  vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost", "Area",
    "state.division", "state.region",
    "special_condition", "special_measures" # trap
  ),
  varstrat = "election*binary_test",
  digits = 2,
  crossed_varstrat_test = TRUE, # want P_valeur
  detail_NB_mesure_sum = TRUE,
  light_contents = TRUE,
  global_summary = FALSE,
  show_p_adj = TRUE # want P_adj_holm
)
sheet_cross_padj <- readxl::read_excel(
  file.path("tmp", "05-test_padj_cross.xlsx"),
  sheet = "quanti-election-binary_test"
)

test_that("test p adj", {
  expect_true(file.exists(path05))
  expect_true(
    all(c("P_valeur", "P_adj_holm") %in% sheet_cross_padj[1, ])
  )
})

#### drop_levels ####

tmp06 <- modified_state[binary_test %in% 1, ] # keep only 1 values over the 2 levels
path06_TRUE <- save_excel_results(
  dataframe = tmp06,
  file = file.path("tmp", "06-yesdroplevels.xlsx"),
  vars = c(
    # "Population", "Income", 
    # "state.region",
    "binary_test"
  ),
  varstrat = NULL, 
  drop_levels = TRUE
)
path06_FALSE <- save_excel_results(
  dataframe = tmp06,
  file = file.path("tmp", "06-nodroplevels.xlsx"),
  vars = c(
    "binary_test"
  ),
  varstrat = NULL, 
  drop_levels = FALSE
)
sheet_yes <- readxl::read_excel(
  file.path("tmp", "06-yesdroplevels.xlsx")
)
sheet_no <- readxl::read_excel(
  file.path("tmp", "06-nodroplevels.xlsx")
)
path06_TRUE_varstrat <- save_excel_results(
  dataframe = tmp06,
  file = file.path("tmp", "06-yesdroplevels_varstrat.xlsx"),
  vars = c(
    "Population", "Income", 
    "state.region"
  ),
  varstrat = "binary_test", 
  drop_levels = TRUE
)
sheet_yes_varstrat <- readxl::read_excel(
  file.path("tmp", "06-yesdroplevels_varstrat.xlsx")
)


test_that("test drop_levels", {
  expect_true(nrow(sheet_yes) + 1 == nrow(sheet_no))
  expect_true(ncol(sheet_yes_varstrat)==7)
  expect_true(expect_true(nrow(sheet_yes_varstrat)==2))
})

#### Variables_all_na ####
modified_state$all_na_var <- NA
path07 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "07-allna.xlsx"),
  vars = c(
    "zero_levels", "all_na_var", "Population"
  ),
  varstrat = "binary_test"
)
modified_state$all_na_var <- NULL
sheet_07 <- readxl::read_excel(
  file.path("tmp", "07-allna.xlsx")
)
sheet_07na <- readxl::read_excel(
  file.path("tmp", "07-allna.xlsx"), sheet = "Variables_all_na"
)
sheet_names <- readxl::excel_sheets(file.path("tmp", "07-allna.xlsx"))
test_that("test all na", {
  expect_true(nrow(sheet_07) == 1)
  expect_true(sheet_07$Variable == "Population")
  expect_equal(sheet_names, c("quantitative - binary_test", "Variables_all_na"))
  expect_equal(sheet_07na$Variables_all_na, c("zero_levels", "all_na_var"))
})

#### dico vars = label ####
path08 <- save_excel_results(
  dataframe = modified_state,
  file = file.path("tmp", "08-labels.xlsx"),
  vars = c(
    "Population", "Illiteracy", "Income",
    "state.division", "state.region"
  ),
  varstrat = "binary_test",
  dico_mapping = data.frame(
    "Vars" = c("Population", "Income", "state.division", "state.region"), 
    "labels" = c("Population size (nb)", "Income (in dollars)", "State Division ???", "state region (cad point)")
  )
)
sheet_08quanti <- readxl::read_excel(
  file.path("tmp", "08-labels.xlsx"),
  sheet = "quantitative - binary_test"
)
sheet_08quali <- readxl::read_excel(
  file.path("tmp", "08-labels.xlsx"),
  sheet = "qualitative - binary_test"
)

test_that("test labels", {
  expect_true(names(sheet_08quanti)[1] == "Label")
  expect_true(sheet_08quanti$Label[1] == "Population size (nb)" & sheet_08quanti$Variable[1] == "Population")
  expect_true(is.na(sheet_08quanti$Label[2])  & sheet_08quanti$Variable[2] == "Illiteracy")
  expect_true(sheet_08quali$Label[1] == "State Division ???" & sheet_08quali$Variable[1] == "state.division")
  expect_equal(is.na(sheet_08quali$Label), is.na(sheet_08quali$Variable))
})


#### end ####
# clear tmp test folder
unlink("tmp", recursive = TRUE)
