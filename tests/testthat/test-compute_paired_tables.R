#### continuous paired tables ####

## Paired t-test
test_pttest1 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  global_summary = FALSE
)
test_pttest1_poptot <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  global_summary = TRUE
)

test_that("test paired t test", {
  expect_true(is.list(test_pttest1))
  expect_equal(length(test_pttest1), 2)
  expect_true(
    all(names(test_pttest1) %in% c("line_res", "test_result"))
  )
  expect_true(
    test_pttest1$line_res$Test %in% "Paired t-test"
  )
  ## eval content of the table (means and test)
  expect_equal(ncol(test_pttest1$line_res) + 1, ncol(test_pttest1_poptot$line_res))

  expect_equal(
    as.numeric(gsub("(.*) \\+\\/\\- (.*)", "\\1", test_pttest1$line_res$Difference_description)),
    as.numeric(round(test_pttest1$test_result$estimate, 2))
  )
  expect_equal(
    as.numeric(round(test_pttest1$test_result$estimate, 2)),
    -1.26
  )
  expect_equal(
    as.numeric(round(test_pttest1$test_result$estimate, 2)),
    as.numeric(round(test_pttest1_poptot$test_result$estimate, 2))
  )
  expect_equal(
    test_pttest1$line_res$Valeurs_manquantes,
    sum(is.na(modified_sleep$extra))
  )
  expect_equal(
    test_pttest1$line_res$N_individuals,
    length(unique(modified_sleep$ID2))
  )
  expect_equal(
    test_pttest1$line_res$temps1_N,
    sum(modified_sleep$visites_2 %in% "temps1")
  )
  expect_equal(
    test_pttest1$line_res$temps2_N,
    sum(modified_sleep$visites_2 %in% "temps2")
  )
  expect_true(test_pttest1_poptot$line_res$is_Normal)
  idx_pop <- grep("Population_totale", names(test_pttest1_poptot$line_res))
  expect_equal(
    test_pttest1$line_res,
    test_pttest1_poptot$line_res[, -idx_pop]
  )
})

## Paired t-test with missing data
test_pttest2 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra_with_missings",
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  global_summary = FALSE
  # signif_digits = 4 # default
)
test_pttest2_signif <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra_with_missings",
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  global_summary = FALSE,
  signif_digits = 2
)

test_that("test paired t test with missing", {
  expect_true(is.list(test_pttest2))
  expect_equal(length(test_pttest2), 2)
  expect_true(
    all(names(test_pttest2) %in% c("line_res", "test_result"))
  )
  expect_true(
    test_pttest2$line_res$Test %in% "Paired t-test"
  )
  expect_equal(
    test_pttest2$line_res$Valeurs_manquantes, 3
  )
  expect_equal(
    test_pttest2$line_res$Valeurs_manquantes,
    sum(is.na(modified_sleep$extra_with_missings))
  )
  expect_false("Population_totale" %in% names(test_pttest2$line_res))
  expect_equal(ncol(test_pttest2$line_res), 12)
  expect_equal(
    as.numeric(gsub("(.*) \\+\\/\\- (.*)", "\\1", test_pttest2$line_res$Difference_description)),
    as.numeric(round(test_pttest2$test_result$estimate, 2))
  )
  expect_equal(
    as.numeric(round(test_pttest2$test_result$estimate, 2)),
    -1.47
  )
  expect_equal(
    test_pttest2$line_res$N_individuals,
    17
  )
  expect_equal(
    test_pttest2$line_res$temps1_N,
    sum(modified_sleep$visites_2 %in% "temps1" & !is.na(modified_sleep$extra_with_missings))
  )
  expect_equal(
    test_pttest2$line_res$temps2_N,
    sum(modified_sleep$visites_2 %in% "temps2" & !is.na(modified_sleep$extra_with_missings))
  )
  expect_true(test_pttest2$line_res$is_Normal)
  expect_equal(
    signif(test_pttest2$line_res$P_valeur, digits = 2),
    test_pttest2_signif$line_res$P_valeur
  )
})


## test not normal measure : Paired Wilcoxon signed-rank test
test_pwilcox <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "mesure3",
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  global_summary = FALSE
)
test_that("test Paired Wilcoxon signed-rank test", {
  expect_true(is.list(test_pwilcox))
  expect_equal(length(test_pwilcox), 2)
  expect_true(
    all(names(test_pwilcox) %in% c("line_res", "test_result"))
  )
  ## eval content of the table (means and test)
  expect_true(
    test_pwilcox$line_res$Test %in% "Wilcoxon signed-rank test (paired data)"
  )
  expect_equal(test_pwilcox$line_res$Valeurs_manquantes, 0)
  expect_equal(test_pwilcox$line_res$N_individuals, 20)
  expect_equal(test_pwilcox$line_res$Difference_description, "-16892 [-30396.5 ; 7794.75]")
})


## Anova on paired data
test_panova_notdone <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_4",
  patient_id = "ID",
  digits = 2,
  global_summary = FALSE
)
test_panova_done <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_4",
  patient_id = "ID",
  digits = 2,
  global_summary = FALSE,
  test_more_2_levels = TRUE
)
test_that("test Paired Anova", {
  expect_true(is.list(test_panova_notdone))
  expect_true(is.list(test_panova_done))
  expect_equal(length(test_panova_notdone), 2)
  expect_equal(length(test_panova_done), 2)
  expect_true(
    all(names(test_panova_notdone) %in% c("line_res", "test_result"))
  )
  expect_true(
    all(names(test_panova_done) %in% c("line_res", "test_result"))
  )
  ## eval content of the table (means and test)
  expect_true(test_panova_notdone$line_res$Test == "/")
  expect_true(is.na(test_panova_notdone$line_res$P_valeur))
  expect_true(
    test_panova_done$line_res$Test %in% "Repeated measures ANOVA: within-Subjects designs"
  )
  expect_true(test_panova_done$line_res$P_valeur == "0.001")
  expect_equal(
    test_panova_done$line_res$N_individuals,
    10
  )
  expect_true(test_panova_done$line_res$is_Normal)
  expect_equal(
    as.numeric(gsub("(.*) \\+\\/\\- (.*)", "\\1",test_panova_done$line_res$visiteM1)),
    mean(modified_sleep$extra[modified_sleep$visites_4 %in% "visiteM1"])
  )
  expect_equal(
    round(as.numeric(gsub("(.*) \\+\\/\\- (.*)", "\\2",test_panova_done$line_res$visiteM2)), 2),
    round(sd(modified_sleep$extra[modified_sleep$visites_4 %in% "visiteM2"]), 2)
  )
})

## test Quade on paired data
# continuous measure, not normal
test_quade_notdone <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "mesure2",
  varstrat = "visites_4",
  patient_id = "ID",
  digits = 2,
  global_summary = FALSE
  # test_more_2_levels = FALSE # so quade test not done by defautl
)
test_quade_done <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "mesure2",
  varstrat = "visites_4",
  patient_id = "ID",
  digits = 2,
  global_summary = TRUE, 
  test_more_2_levels = TRUE # so quade test done
)
test_that("test Quade", {
  expect_true(is.list(test_quade_notdone))
  expect_equal(length(test_quade_notdone), 2)
  expect_true(
    all(names(test_quade_notdone) %in% c("line_res", "test_result"))
  )
  expect_true(
    test_quade_notdone$line_res$Test %in% "/"
  )
  expect_true(is.list(test_quade_done))
  expect_equal(length(test_quade_done), 2)
  ## eval content of the table (means and test)
  expect_true(
    all(names(test_quade_done) %in% c("line_res", "test_result"))
  )
  expect_true(
    test_quade_done$line_res$Test %in% "Quade Test"
  )
  expect_true(all( # complete design
    test_quade_done$line_res[, grep("_N$", names(test_quade_done$line_res))] ==
      test_quade_done$line_res[, grep("_N$", names(test_quade_done$line_res))[1]]
  ))
  expect_equal(test_quade_done$line_res$P_valeur, 0.2485)
  expect_equal(test_quade_done$line_res$visiteM3, "1.4 [1.1;1.8]")
  expect_equal(test_quade_done$line_res$visiteM4, "1 [0.72;1.3]")
})

## SkillingsMack on paired data
test_SkillingsMack_notdone <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_5",
  patient_id = "ID",
  digits = 2,
  global_summary = FALSE
)
test_SkillingsMack_done <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_5",
  patient_id = "ID",
  digits = 2,
  global_summary = FALSE,
  test_more_2_levels = TRUE
)
test_that("test SkillingsMack", {
  expect_true(is.list(test_SkillingsMack_notdone))
  expect_equal(length(test_SkillingsMack_notdone), 2)
  expect_true(
    all(names(test_SkillingsMack_notdone) %in% c("line_res", "test_result"))
  )
  expect_true(
    test_SkillingsMack_notdone$line_res$Test %in% "/"
  )
  expect_true(is.list(test_SkillingsMack_done))
  expect_equal(length(test_SkillingsMack_done), 2)
  ## eval content of the table (means and test)
  expect_true(
    all(names(test_SkillingsMack_done) %in% c("line_res", "test_result"))
  )
  expect_true(
    test_SkillingsMack_done$line_res$Test %in% "Skillings-Mack test"
  )
  expect_false(all( # incomplete design
    test_SkillingsMack_done$line_res[, grep("_N$", names(test_SkillingsMack_done$line_res))] ==
      test_SkillingsMack_done$line_res[, grep("_N$", names(test_SkillingsMack_done$line_res))[1]]
  ))
  expect_equal(test_SkillingsMack_done$line_res$P_valeur, 0.0283)
  expect_equal(test_SkillingsMack_done$line_res$visiteD3, "0.05 [-0.03;0.26]")
  expect_equal(test_SkillingsMack_done$line_res$visiteD4, "-0.04 [-0.24;-0.02]")
})

#### factorial paired tables ####

## Mc Nemar test
macnemar1 <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact1",
  varstrat = "visites_2",
  digits = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = TRUE
)
# macnemar1
test_that("test McNemar1", {
  expect_true(is.list(macnemar1))
  expect_equal(length(macnemar1), 2)
  expect_true(
    all(names(macnemar1) %in% c("line_res", "test_result"))
  )
  expect_true(
    macnemar1$line_res$Test[1] %in% "McNemar's Chi-squared test"
  )
  expect_equal(
    macnemar1$line_res$Valeurs_manquantes[1], 0
  )
  expect_equal(nrow(macnemar1$line_res), 3)
  expect_equal(ncol(macnemar1$line_res), 10)
  expect_true( # check order
    all(names(macnemar1$line_res)[1:2] %in% c("Variable", "Modalites"))
  )
  ## --here more : test values
  
})
macnemar2_na <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact1_na",
  varstrat = "visites_2",
  digits = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = TRUE
)
# macnemar2_na
test_that("test McNemar2", {
  expect_true(is.list(macnemar2_na))
  expect_equal(length(macnemar2_na), 2)
  expect_true(
    all(names(macnemar2_na) %in% c("line_res", "test_result"))
  )
  expect_true(
    macnemar2_na$line_res$Test[1] %in% "McNemar's Chi-squared test"
  )
  expect_equal(
    macnemar2_na$line_res$Valeurs_manquantes[1], 1
  )
  expect_true(
    all(macnemar2_na$line_res$temps1_N[2:3]==19)
  )
  expect_equal(nrow(macnemar2_na$line_res), 3)
  expect_equal(ncol(macnemar2_na$line_res), 10)
})
macnemar3_missingline <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact3",
  varstrat = "visites_2",
  digits = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = FALSE
)
macnemar3_missingline
test_that("test McNemar3", {
  expect_true(is.list(macnemar3_missingline))
  expect_equal(length(macnemar3_missingline), 2)
  expect_true(
    all(names(macnemar3_missingline) %in% c("line_res", "test_result"))
  )
  expect_true(
    macnemar3_missingline$line_res$Test[1] %in% "McNemar's Chi-squared test"
  )
  expect_equal(
    macnemar3_missingline$line_res$Valeurs_manquantes[1],
    sum(is.na(modified_sleep$fact3))
  )
  expect_true(
    all(macnemar3_missingline$line_res$temps1_N[1:2]==15)
  )
  expect_equal(nrow(macnemar3_missingline$line_res), 2)
  expect_equal(ncol(macnemar3_missingline$line_res), 10)
})
macnemar4_simplify <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact3",
  varstrat = "visites_2",
  digits = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = FALSE,
  simplify = TRUE
)
macnemar4_simplify
test_that("test McNemar4", {
  expect_true(is.list(macnemar4_simplify))
  expect_equal(length(macnemar4_simplify), 2)
  expect_true(
    all(names(macnemar4_simplify) %in% c("line_res", "test_result"))
  )
  expect_true(
    macnemar4_simplify$line_res$Test[1] %in% "McNemar's Chi-squared test"
  )
  expect_equal(
    macnemar4_simplify$line_res$Valeurs_manquantes[1],
    sum(is.na(modified_sleep$fact3))
  )
  expect_true(
    macnemar4_simplify$line_res$temps1_N==15
  )
  expect_equal(nrow(macnemar4_simplify$line_res), 1)
  expect_equal(ncol(macnemar4_simplify$line_res), 10)
})
macnemar5_global <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact3",
  varstrat = "visites_2",
  digits = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = FALSE,
  global_summary = TRUE
)
# macnemar5_global
test_that("test McNemar5", {
  expect_true(is.list(macnemar5_global))
  expect_equal(length(macnemar5_global), 2)
  expect_true(
    all(names(macnemar5_global) %in% c("line_res", "test_result"))
  )
  expect_true(
    macnemar5_global$line_res$Test[1] %in% "McNemar's Chi-squared test"
  )
  expect_equal(
    macnemar5_global$line_res$Valeurs_manquantes[1],
    sum(is.na(modified_sleep$fact3))
  )
  expect_true(
    all(macnemar5_global$line_res$temps1_N==15)
  )
  expect_equal(nrow(macnemar5_global$line_res), 2)
  expect_equal(ncol(macnemar5_global$line_res), 11)
  expect_true( # check order
    all(names(macnemar5_global$line_res)[1:2] %in% c("Variable", "Modalites"))
  )
})

# macnemar6_mh <- compute_paired_factorial_table_and_test(
#   dataframe = modified_sleep,
#   variable_interest = "--here val more than 2 levels",
#   varstrat = "visites_2",
#   digits = 1,
#   patient_id = "ID2",
#   force_generate_1_when_0 = FALSE,
#   keep_missing_line = FALSE,
#   global_summary = TRUE,
#   test_more_2_levels = TRUE
# )


#### Traps ####

##### desc in paired data but one var not present at the 2nd time (or for the paired group) #####

tab7 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "var1",
  varstrat = "group",
  digits = 3,
  signif_digits = 3,
  patient_id = "ID_group",
  global_summary = TRUE,
  force_non_parametric_test = FALSE,
  metric_show = "auto",
  test_more_2_levels = FALSE
)
tab8 <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "var2",
  varstrat = "group",
  digits = 3,
  signif_digits = 3,
  simplify = FALSE,
  patient_id = "ID_group",
  force_generate_1_when_0 = TRUE,
  keep_missing_line = TRUE,
  global_summary = TRUE,
  test_more_2_levels = FALSE
)
test_that("test paired 7-8 one group", {
  expect_true(is.list(tab7))
  expect_true(is.list(tab8))
  expect_true(
    all(names(tab7) %in% c("line_res", "test_result"))
  )
  expect_true(
    all(names(tab8) %in% c("line_res", "test_result"))
  )
  expect_true(tab7$line_res$Test[1] %in% "/")
  expect_true(tab8$line_res$Test[1] %in% "/")

  expect_equal(tab7$line_res$`1`, "1.5 [1;2]")
  expect_equal(tab7$line_res$`2`, "/")
  expect_equal(tab8$line_res$`1`[2], "5 (50%)")
  expect_equal(tab8$line_res$`2`[2], "0 ")

  expect_equal(tab7$line_res$message[1], "Variable present only in 1 visit/group, no paired test applicable")
  expect_equal(tab7$line_res$message[1], tab8$line_res$message[1])
  
  expect_equal(tab7$line_res$Valeurs_manquantes, sum(is.na(modified_sleep$var1)))
  expect_equal(tab7$line_res$N_individuals, sum(!is.na(modified_sleep$var1)))
  expect_equal(tab8$line_res$Valeurs_manquantes[1], sum(is.na(modified_sleep$var2)))
  expect_equal(
    as.numeric(tab8$line_res$`1`[1]),
    sum(is.na(modified_sleep$var2[modified_sleep$group %in% "1"]))
  )
  expect_equal(
    as.numeric(tab8$line_res$`2`[1]),
    sum(is.na(modified_sleep$var2[modified_sleep$group %in% "2"]))
  )
})

##### Empty conti vars #####
modified_sleep$empty_conti_var <- NA_real_
tab9 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "empty_conti_var",
  varstrat = "group",
  patient_id = "ID_group",
  global_summary = TRUE
)
test_that("test empty vars", {
  expect_true(is.list(tab9))
  expect_true(
    all(names(tab9) %in% c("line_res", "test_result"))
  )
  expect_true(tab9$line_res$Test[1] %in% "/")
  expect_true(tab9$line_res$Variable[1] %in% "empty_conti_var")
  expect_equal(tab9$line_res$message[1], "Variable present have no data (all NA), no desc, no test applicable")
})


##### all zero conti vars #####
# shapiro must have an error: "toutes les valeurs de 'x' sont identiques"

tab10 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "all_zero_values",
  varstrat = "group",
  patient_id = "ID_group",
  global_summary = TRUE
)
tab11 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "all_zero_values",
  varstrat = "visites_4",
  patient_id = "ID2",
  global_summary = TRUE,
  test_more_2_levels = TRUE
)
tab12 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "all_zero_values",
  varstrat = "visites_5",
  patient_id = "ID2",
  global_summary = TRUE,
  test_more_2_levels = TRUE
)

test_that("test all zero vars", {
  expect_true(is.list(tab10))
  expect_true(
    all(names(tab10) %in% c("line_res", "test_result"))
  )
  expect_true(tab10$line_res$Test[1] %in% "/")
  expect_true(tab10$line_res$Variable[1] %in% "all_zero_values")
  expect_false(tab10$line_res$is_Normal[1])
  expect_true(grepl(pattern = "Test non applicable", tab10$line_res$message[1]))
  expect_true(grepl(pattern = "toutes les valeurs de 'x' sont identiques", tab10$line_res$message[1]))
  
  expect_true(is.list(tab11))
  expect_true(
    all(names(tab11) %in% c("line_res", "test_result"))
  )
  expect_true(tab11$line_res$Test[1] %in% "Skillings-Mack test") # works with all zero
  expect_true(tab11$line_res$Variable[1] %in% "all_zero_values")
  expect_false(tab11$line_res$is_Normal[1])
  expect_true(grepl(pattern = "toutes les valeurs de 'x' sont identiques", tab11$line_res$message[1]))

  expect_true(is.list(tab12))
  expect_true(
    all(names(tab12) %in% c("line_res", "test_result"))
  )
  expect_true(tab12$line_res$Test[1] %in% "Skillings-Mack test") # works with all zero
  expect_true(tab12$line_res$Variable[1] %in% "all_zero_values")
  expect_false(tab12$line_res$is_Normal[1])
  expect_true(grepl(pattern = "toutes les valeurs de 'x' sont identiques", tab12$line_res$message[1]))
})
