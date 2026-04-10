message("test compute paired tables - done v0.1.27")

#### continuous paired tables ####

## Paired t-test
test_pttest1 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_2",
  patient_id = "ID2",
  precision = 2,
  global_summary = FALSE
)
test_pttest1_poptot <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_2",
  patient_id = "ID2",
  precision = 2,
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
  precision = 2,
  global_summary = FALSE
  # signif_digits = 4 # default
)
test_pttest2_signif <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra_with_missings",
  varstrat = "visites_2",
  patient_id = "ID2",
  precision = 2,
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
  precision = 2,
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
  precision = 2,
  global_summary = FALSE
)
test_panova_done <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_4",
  patient_id = "ID",
  precision = 2,
  global_summary = FALSE,
  do_test = TRUE
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
  precision = 2,
  global_summary = FALSE
  # do_test = FALSE # so quade test not done by defautl
)
test_quade_done <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "mesure2",
  varstrat = "visites_4",
  patient_id = "ID",
  precision = 2,
  global_summary = TRUE, 
  do_test = TRUE # so quade test done
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
  precision = 2,
  global_summary = FALSE
)
test_SkillingsMack_done <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "extra",
  varstrat = "visites_5",
  patient_id = "ID",
  precision = 2,
  global_summary = FALSE,
  do_test = TRUE
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

#### add new unit test on utils functions v 0.1.27 ####


# =
# Tests pour format_cell_content()
# =

test_that("format_cell_content formats mean correctly", {
  stats <- list(mean = 25.5, sd = 3.2, median = 24.8, Q1 = 22.5, Q3 = 27.3)
  
  result <- format_cell_content(stats, metric = "mean")
  
  expect_equal(result, "25.5 +/- 3.2")
})

test_that("format_cell_content formats median correctly", {
  stats <- list(mean = 25.5, sd = 3.2, median = 24.8, Q1 = 22.5, Q3 = 27.3)
  
  result <- format_cell_content(stats, metric = "median")
  
  expect_equal(result, "24.8 [22.5;27.3]")
})

test_that("format_cell_content handles NA values for mean", {
  stats <- list(mean = NA, sd = 3.2, median = 24.8, Q1 = 22.5, Q3 = 27.3)
  
  result <- format_cell_content(stats, metric = "mean")
  
  expect_equal(result, "/")
})

test_that("format_cell_content handles NaN values for mean", {
  stats <- list(mean = NaN, sd = 3.2, median = 24.8, Q1 = 22.5, Q3 = 27.3)
  
  result <- format_cell_content(stats, metric = "mean")
  
  expect_equal(result, "/")
})

test_that("format_cell_content handles NA values for median", {
  stats <- list(mean = 25.5, sd = 3.2, median = NA, Q1 = 22.5, Q3 = 27.3)
  
  result <- format_cell_content(stats, metric = "median")
  
  expect_equal(result, "/")
})

# =
# Tests pour format_global_summary()
# =

test_that("format_global_summary formats mean correctly", {
  df <- data.frame(
    var1 = c(10, 20, 30, 40, 50),
    patient_id = c("P1", "P2", "P3", "P4", "P5")
  )
  
  result <- format_global_summary(
    dataframe = df, variable = "var1", metric = "mean", digits_central = 2, digits_sd = 2
  )
  
  expect_equal(result, "30 +/- 15.81")
})

test_that("format_global_summary formats median correctly", {
  df <- data.frame(
    var1 = c(10, 20, 30, 40, 50),
    patient_id = c("P1", "P2", "P3", "P4", "P5")
  )
  
  result <- format_global_summary(df, "var1", metric = "median", digits_central = 2, digits_sd = 2)
  
  expect_equal(result, "30 [20 ; 40]")
})

test_that("format_global_summary includes patient count when requested", {
  df <- data.frame(
    var1 = c(10, 20, 30, 40, 50, 60),
    patient_id = c("P1", "P2", "P3", "P1", "P2", "P3")
  )
  
  result_with_n <- format_global_summary(df, "var1", metric = "mean", digits_central = 2, digits_sd = 2, patient_id = "patient_id")
  result_without_n <- format_global_summary(df, "var1", metric = "mean", digits_central = 2, digits_sd = 2, patient_id = NULL)
  
  expect_true(grepl("\\(n=3\\)$", result_with_n))
  expect_false(grepl("\\(n=", result_without_n))
})

test_that("format_global_summary handles NA values", {
  df <- data.frame(
    var1 = c(10, 20, NA, 40, 50)
  )
  
  result_mean <- format_global_summary(df, "var1", metric = "mean", digits_central = 2, digits_sd = 2)
  result_median <- format_global_summary(df, "var1", metric = "median", digits_central = 2, digits_sd = 2)
  
  expect_equal(result_mean, "30 +/- 18.26")
  # unname(quantile(df$var1, na.rm = T)[c(2,4)])
  expect_equal(result_median, "30 [17.5 ; 42.5]")
})

test_that("format_global_summary respects digits parameter", {
  df <- data.frame(
    var1 = c(10.123, 20.456, 30.789)
  )
  
  result_2dig <- format_global_summary(df, "var1", metric = "mean", digits_central = 2, digits_sd = 2)
  result_4dig <- format_global_summary(df, "var1", metric = "mean", digits_central = 4, digits_sd = 4)
  
  # Vérifier que le nombre de décimales est respecté
  expect_true(grepl("\\d+\\.\\d{2} \\+/- \\d+\\.\\d{2}", result_2dig))
  expect_true(
    grepl(
      paste0(
      "\\d+\\.\\d{",min(4, detect_decimal_places(df$var1)),"} \\+/-",
      " \\d+\\.\\d{",min(4, detect_decimal_places(df$var1)),"}"
      ),
      result_4dig
  ))
})

# =
# Tests pour handle_two_levels()
# =

test_that("handle_two_levels executes paired t-test for normal data", {
  
  # Créer des données avec 2 niveaux et différences normales
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:20),
    temps1 = rnorm(20, mean = 50, sd = 10),
    temps2 = rnorm(20, mean = 55, sd = 10)
  )
  
  dt <- data.table::data.table(
    patient_id = rep(paste0("P", 1:20), 2),
    varstrat = factor(rep(c("temps1", "temps2"), each = 20)),
    var1 = c(dt_wide$temps1, dt_wide$temps2)
  )
  
  result <- handle_two_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = TRUE,
    dt = dt
  )
  
  expect_true(is.list(result))
  expect_true(all(c("test_result", "test_used", "ongoing_message", "normal_data", 
                    "N_individuals", "metric_to_show", "Population_totale", 
                    "Difference_description") %in% names(result)))
  expect_equal(result$test_used, "Paired t-test")
  expect_equal(result$N_individuals, 20)
  expect_true(result$metric_to_show %in% c("mean", "median"))
  expect_true(!is.null(result$Difference_description))
})

test_that("handle_two_levels executes Wilcoxon for non-normal data", {
  # Créer des données avec différences non-normales
  set.seed(123)
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:20),
    temps1 = rexp(20, rate = 0.1),  # Distribution exponentielle
    temps2 = rexp(20, rate = 0.1) + 10
  )
  
  dt <- data.table::data.table(
    patient_id = rep(paste0("P", 1:20), 2),
    varstrat = factor(rep(c("temps1", "temps2"), each = 20)),
    var1 = c(dt_wide$temps1, dt_wide$temps2)
  )
  
  result <- handle_two_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    dt = dt
  )
  
  expect_equal(result$test_used, "Wilcoxon signed-rank test (paired data)")
  expect_true(result$metric_to_show %in% c("mean", "median"))
})

test_that("handle_two_levels respects force_parametric_test", {
  # Données non-normales mais forcer le test paramétrique
  set.seed(123)
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:20),
    temps1 = rexp(20, rate = 0.1),
    temps2 = rexp(20, rate = 0.1) + 10
  )
  
  dt <- data.table::data.table(
    patient_id = rep(paste0("P", 1:20), 2),
    varstrat = factor(rep(c("temps1", "temps2"), each = 20)),
    var1 = c(dt_wide$temps1, dt_wide$temps2)
  )
  
  result <- handle_two_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = TRUE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    dt = dt
  )
  
  expect_equal(result$test_used, "Paired t-test")
  expect_true(grepl("force_parametric_test", result$ongoing_message))
})

test_that("handle_two_levels respects force_non_parametric_test", {
  # Données normales mais forcer le test non-paramétrique
  set.seed(123)
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:20),
    temps1 = rnorm(20, mean = 50, sd = 10),
    temps2 = rnorm(20, mean = 55, sd = 10)
  )
  
  dt <- data.table::data.table(
    patient_id = rep(paste0("P", 1:20), 2),
    varstrat = factor(rep(c("temps1", "temps2"), each = 20)),
    var1 = c(dt_wide$temps1, dt_wide$temps2)
  )
  
  result <- handle_two_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = TRUE,
    global_summary = FALSE,
    dt = dt
  )
  
  expect_equal(result$test_used, "Wilcoxon signed-rank test (paired data)")
  expect_true(grepl("force_non_parametric_test", result$ongoing_message))
})

test_that("handle_two_levels formats Difference_description correctly", {
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:10),
    temps1 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    temps2 = c(15, 25, 35, 45, 55, 65, 75, 85, 95, 105)
  )
  
  dt <- data.table::data.table(
    patient_id = rep(paste0("P", 1:10), 2),
    varstrat = factor(rep(c("temps1", "temps2"), each = 10)),
    var1 = c(dt_wide$temps1, dt_wide$temps2)
  )
  
  result <- handle_two_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    dt = dt
  )
  
  # La différence devrait être de 5 (temps2 - temps1)
  expect_true(
    grepl("5 [5 ; 5]", result$Difference_description)
  )
  expect_true(!is.null(result$Difference_description))
})

# =
# Tests pour handle_multiple_levels()
# =

test_that("handle_multiple_levels detects complete vs incomplete design", {
  # Design complet
  dt_wide_complete <- data.table::data.table(
    patient_id = paste0("P", 1:10),
    temps1 = rnorm(10, 50, 10),
    temps2 = rnorm(10, 55, 10),
    temps3 = rnorm(10, 60, 10)
  )
  
  # Design incomplet (avec NA)
  dt_wide_incomplete <- data.table::data.table(
    patient_id = paste0("P", 1:10),
    temps1 = c(rnorm(9, 50, 10), NA),
    temps2 = c(rnorm(8, 55, 10), NA, NA),
    temps3 = rnorm(10, 60, 10)
  )
  
  dt_complete <- data.table::melt(
    dt_wide_complete, 
    id.vars = "patient_id", 
    variable.name = "varstrat",
    value.name = "var1"
  )
  dt_complete$varstrat <- as.factor(dt_complete$varstrat)
  dt_complete$patient_id <- as.factor(dt_complete$patient_id)
  
  dt_incomplete <- data.table::melt(
    dt_wide_incomplete, 
    id.vars = "patient_id",
    variable.name = "varstrat", 
    value.name = "var1"
  )
  dt_incomplete$varstrat <- as.factor(dt_incomplete$varstrat)
  dt_incomplete$patient_id <- as.factor(dt_incomplete$patient_id)
  
  # Test avec design complet (devrait utiliser ANOVA ou Quade)
  result_complete <- handle_multiple_levels(
    dt_wide = dt_wide_complete,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    do_test = TRUE,
    dt = dt_complete
  )
  
  # Test avec design incomplet (devrait utiliser Skillings-Mack si non paramétrique)
  result_incomplete <- handle_multiple_levels(
    dt_wide = dt_wide_incomplete,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = TRUE,
    global_summary = FALSE,
    do_test = TRUE,
    dt = dt_incomplete
  )
  
  expect_true(result_complete$test_used %in% c(
    "Repeated measures ANOVA: within-Subjects designs", "Quade Test")
  )
  expect_equal(result_incomplete$test_used, "Skillings-Mack test")
})

test_that("handle_multiple_levels respects test_more_2_levels parameter", {
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:10),
    temps1 = rnorm(10, 50, 10),
    temps2 = rnorm(10, 55, 10),
    temps3 = rnorm(10, 60, 10)
  )
  
  dt <- data.table::melt(
    dt_wide, 
    id.vars = "patient_id",
    variable.name = "varstrat",
    value.name = "var1"
  )
  dt$varstrat <- as.factor(dt$varstrat)
  dt$patient_id <- as.factor(dt$patient_id)
  
  # Avec test_more_2_levels = FALSE
  result_no_test <- handle_multiple_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    do_test = FALSE,
    dt = dt
  )
  
  # Avec test_more_2_levels = TRUE
  result_with_test <- handle_multiple_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    do_test = TRUE,
    dt = dt
  )
  
  expect_equal(result_no_test$test_used, "/")
  expect_true(is.na(result_no_test$test_result$p.value))
  expect_true(result_with_test$test_used != "/")
  expect_true(!is.na(result_with_test$test_result$p.value))
})

test_that("handle_multiple_levels returns NULL for Difference_description", {
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:10),
    temps1 = rnorm(10, 50, 10),
    temps2 = rnorm(10, 55, 10),
    temps3 = rnorm(10, 60, 10)
  )
  
  dt <- data.table::melt(
    dt_wide, 
    id.vars = "patient_id",
    variable.name = "varstrat",
    value.name = "var1"
  )
  dt$varstrat <- as.factor(dt$varstrat)
  dt$patient_id <- as.factor(dt$patient_id)
  
  result <- handle_multiple_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    do_test = TRUE,
    dt = dt
  )
  
  # Pour plus de 2 niveaux, Difference_description devrait être NULL
  expect_null(result$Difference_description)
})

test_that("handle_multiple_levels adapts metric based on normality", {
  # Données normales
  set.seed(123)
  dt_wide_normal <- data.table::data.table(
    patient_id = paste0("P", 1:30),
    temps1 = rnorm(30, 50, 5),
    temps2 = rnorm(30, 55, 5),
    temps3 = rnorm(30, 60, 5)
  )
  
  dt_normal <- data.table::melt(
    dt_wide_normal, 
    id.vars = "patient_id",
    variable.name = "varstrat",
    value.name = "var1"
  )
  dt_normal$varstrat <- as.factor(dt_normal$varstrat)
  dt_normal$patient_id <- as.factor(dt_normal$patient_id)
  
  result_normal <- handle_multiple_levels(
    dt_wide = dt_wide_normal,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    digits_central = 2,
    digits_sd = 2,
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    do_test = TRUE,
    dt = dt_normal
  )
  
  # Pour données normales avec auto, devrait choisir mean
  expect_equal(result_normal$metric_to_show, "mean")
})


#### factorial paired tables ####

## Mc Nemar test
macnemar1 <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact1",
  varstrat = "visites_2",
  patient_id = "ID2",
  precision = 1,
  force_generate_1_when_0 = FALSE,
  keep_missing_line = TRUE, 
  do_test = TRUE
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
  expect_equal(ncol(macnemar1$line_res), 11)
  expect_true( # check order
    all(names(macnemar1$line_res)[1:2] %in% c("Variable", "Modalites"))
  )
  ## --here more : test values
  
})

macnemar2_na <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact1_na",
  varstrat = "visites_2",
  precision = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = TRUE,
  do_test = TRUE
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
  expect_equal(ncol(macnemar2_na$line_res), 11)
})

macnemar3_missingline <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact3",
  varstrat = "visites_2",
  precision = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = FALSE, 
  do_test = TRUE
)
# macnemar3_missingline
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
  expect_equal(ncol(macnemar3_missingline$line_res), 11)
})

macnemar4_simplify <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact3",
  varstrat = "visites_2",
  precision = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = FALSE,
  simplify = TRUE, 
  do_test = TRUE
)
# macnemar4_simplify
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
  expect_equal(ncol(macnemar4_simplify$line_res), 11)
})

macnemar5_global <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "fact3",
  varstrat = "visites_2",
  precision = 1,
  patient_id = "ID2",
  force_generate_1_when_0 = FALSE,
  keep_missing_line = FALSE,
  global_summary = TRUE,
  do_test = TRUE
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
  expect_equal(ncol(macnemar5_global$line_res), 12)
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

# =
# Tests pour handle_force_generate()
# =

test_that("handle_force_generate works with 0 level", {
  dt <- data.table::data.table(
    var1 = factor(rep(0, 10)),
    patient_id = paste0("P", 1:10)
  )
  
  result <- handle_force_generate(dt, "var1", force_generate_1_when_0 = TRUE)
  
  expect_true(is.list(result))
  expect_true(all(c("dt", "var_levels", "var_nlevels") %in% names(result)))
  expect_equal(result$var_nlevels, 2)
  expect_true("1" %in% result$var_levels)
  expect_true("0" %in% result$var_levels)
})

test_that("handle_force_generate works with 'non' level", {
  dt <- data.table::data.table(
    var1 = factor(rep("non", 10)),
    patient_id = paste0("P", 1:10)
  )
  
  result <- handle_force_generate(dt, "var1", force_generate_1_when_0 = TRUE)
  
  expect_equal(result$var_nlevels, 2)
  expect_true("Oui" %in% result$var_levels)
  expect_true("non" %in% result$var_levels)
})

test_that("handle_force_generate does nothing when force_generate_1_when_0 is FALSE", {
  dt <- data.table::data.table(
    var1 = factor(rep(0, 10)),
    patient_id = paste0("P", 1:10)
  )
  
  result <- handle_force_generate(dt, "var1", force_generate_1_when_0 = FALSE)
  
  expect_equal(result$var_nlevels, 1)
  expect_equal(result$var_levels, "0")
})

test_that("handle_force_generate does nothing with multiple levels", {
  dt <- data.table::data.table(
    var1 = factor(c(rep(0, 5), rep(1, 5))),
    patient_id = paste0("P", 1:10)
  )
  
  result <- handle_force_generate(dt, "var1", force_generate_1_when_0 = TRUE)
  
  expect_equal(result$var_nlevels, 2)
  expect_equal(sort(result$var_levels), c("0", "1"))
})

test_that("handle_force_generate does nothing with non-0/non levels", {
  dt <- data.table::data.table(
    var1 = factor(rep("A", 10)),
    patient_id = paste0("P", 1:10)
  )
  
  result <- handle_force_generate(dt, "var1", force_generate_1_when_0 = TRUE)
  
  expect_equal(result$var_nlevels, 1)
  expect_equal(result$var_levels, "A")
})

# =
# Tests pour compute_frequency_table()
# =

test_that("compute_frequency_table returns correct structure", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 10), rep("B", 10))),
    varstrat = factor(c(rep("T1", 10), rep("T2", 10)))
  )
  
  result <- compute_frequency_table(
    dt = dt, variable_interest = "var1", varstrat = "varstrat", 
    varstrat_levels =  c("T1", "T2"),
    digits = 2
  )
  
  expect_true(data.table::is.data.table(result))
  expect_true(all(c("var1", "varstrat", "n_group_mod", "Nb_mesures", "N_by_visit", 
                    "p", "cell_content") %in% names(result)))
})

test_that("compute_frequency_table calculates percentages correctly", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 5), rep("B", 5), rep("A", 5), rep("B", 5))),
    varstrat = factor(c(rep("T1", 10), rep("T2", 10))),
    patient_id = paste0("P", 1:20)
  )
  
  result <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 1)
  
  # Vérifier que les pourcentages sont corrects (50% pour A et B dans chaque visite)
  percentages <- result[!is.na(result$var1), ]$p
  expect_true(all(grepl("50", percentages)))
})

test_that("compute_frequency_table handles missing data", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 5), rep(NA, 5), rep("A", 5), rep("B", 5))),
    varstrat = factor(c(rep("T1", 10), rep("T2", 10))),
    patient_id = paste0("P", 1:20)
  )
  
  result <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 2)
  
  # Vérifier qu'il y a des lignes NA
  expect_true(any(is.na(result$var1)))
  
  # Vérifier que les lignes NA ont un effectif
  na_rows <- result[is.na(result$var1), ]
  expect_true(nrow(na_rows) > 0)
  expect_true(any(na_rows$n_group_mod > 0))
})

test_that("compute_frequency_table respects digits parameter", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 3), rep("B", 7), rep("A", 3), rep("B", 7))),
    varstrat = factor(c(rep("T1", 10), rep("T2", 10))),
    patient_id = paste0("P", 1:20)
  )
  
  result_1dig <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 1)

  # A = 30%, B = 70%
  expect_true(any(grepl("30%", result_1dig$p)))
})

# =
# Tests pour format_effectif_table()
# =

test_that("format_effectif_table creates correct structure", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 10), rep("B", 10))),
    varstrat = factor(c(rep("T1", 10), rep("T2", 10))),
    patient_id = paste0("P", 1:20)
  )
  
  sumup <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 2)
  result <- format_effectif_table(sumup, "var1", "varstrat", c("T1", "T2"), keep_missing_line = TRUE)
  
  expect_true(data.table::is.data.table(result))
  expect_true("Modalites" %in% names(result))
  expect_true("Nb_mesures" %in% names(result))
  expect_true(all(c("T1", "T2", "T1_N", "T2_N") %in% names(result)))
})

test_that("format_effectif_table handles keep_missing_line parameter", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 5), rep(NA, 5), rep("A", 5), rep("B", 5))),
    varstrat = factor(c(rep("T1", 10), rep("T2", 10))),
    patient_id = paste0("P", 1:20)
  )
  
  sumup <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 2)
  
  result_with_na <- format_effectif_table(sumup, "var1", "varstrat", c("T1", "T2"), keep_missing_line = TRUE)
  result_without_na <- format_effectif_table(sumup, "var1", "varstrat", c("T1", "T2"), keep_missing_line = FALSE)
  
  expect_true("Missing data" %in% result_with_na$Modalites)
  expect_false("Missing data" %in% result_without_na$Modalites)
  expect_true(nrow(result_with_na) > nrow(result_without_na))
})

test_that("format_effectif_table columns are in correct order", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 10), rep("B", 10))),
    varstrat = factor(c(rep("T1", 10), rep("T2", 10))),
    patient_id = paste0("P", 1:20)
  )
  
  sumup <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 2)
  result <- format_effectif_table(sumup, "var1", "varstrat", c("T1", "T2"), keep_missing_line = TRUE)
  
  # Vérifier l'ordre : Modalites, Nb_mesures, puis alternance T1, T1_N, T2, T2_N
  expected_order <- c("Modalites", "Nb_mesures", "T1", "T1_N", "T2", "T2_N")
  expect_equal(names(result), expected_order)
})

# =
# Tests pour add_global_summary()
# =

test_that("add_global_summary adds Population_total column", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 6), rep("B", 4))),
    varstrat = factor(c(rep("T1", 5), rep("T2", 5))),
    patient_id = paste0("P", 1:10)
  )
  
  sumup <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 1)
  effectif_tab <- format_effectif_table(sumup, "var1", "varstrat", c("T1", "T2"), keep_missing_line = FALSE)
  
  result <- add_global_summary(effectif_tab, dt, "var1", digits = 1)
  
  expect_true("Population_total" %in% names(result))
})

test_that("add_global_summary calculates correct percentages", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 6), rep("B", 4))),
    varstrat = factor(c(rep("T1", 5), rep("T2", 5))),
    patient_id = paste0("P", 1:10)
  )
  
  sumup <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 1)
  effectif_tab <- format_effectif_table(sumup, "var1", "varstrat", c("T1", "T2"), keep_missing_line = FALSE)
  
  result <- add_global_summary(effectif_tab, dt, "var1", digits = 1)
  
  # A = 60%, B = 40%
  pop_total_A <- result[result$Modalites == "A", ]$Population_total
  pop_total_B <- result[result$Modalites == "B", ]$Population_total
  
  expect_true(grepl("6.*60", pop_total_A))
  expect_true(grepl("4.*40", pop_total_B))
})

test_that("add_global_summary places Population_total in correct position", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 6), rep("B", 4))),
    varstrat = factor(c(rep("T1", 5), rep("T2", 5))),
    patient_id = paste0("P", 1:10)
  )
  
  sumup <- compute_frequency_table(dt, "var1", "varstrat", c("T1", "T2"), digits = 1)
  effectif_tab <- format_effectif_table(sumup, "var1", "varstrat", c("T1", "T2"), keep_missing_line = FALSE)
  
  result <- add_global_summary(effectif_tab, dt, "var1", digits = 1)
  
  # Population_total devrait être après Nb_mesures
  col_names <- names(result)
  pop_idx <- which(col_names == "Population_total")
  nb_idx <- which(col_names == "Nb_mesures")
  
  expect_true(pop_idx > nb_idx)
})

# =
# Tests pour perform_mcnemar_test()
# =

test_that("perform_mcnemar_test works with valid 2x2 table", {
  # Créer des données appariées 2x2
  mcnemar_dt <- data.table::data.table(
    patient_id = paste0("P", 1:20),
    T1 = factor(c(rep("A", 10), rep("B", 10))),
    T2 = factor(c(rep("A", 5), rep("B", 5), rep("A", 5), rep("B", 5)))
  )
  
  result <- perform_mcnemar_test(
    mcnemar_dt = mcnemar_dt, varstrat_levels = c("T1", "T2"), variable_interest =  "var1"
  )
  
  expect_true(is.list(result))
  expect_true(all(c("test_result", "test_used", "ongoing_message") %in% names(result)))
  expect_equal(result$test_used, "McNemar's Chi-squared test")
  expect_true(!is.null(result$test_result$p.value))
})

test_that("perform_mcnemar_test returns NULL for non-2x2 table", {
  # Créer des données avec 3 niveaux
  mcnemar_dt <- data.table::data.table(
    patient_id = paste0("P", 1:30),
    T1 = factor(c(rep("A", 10), rep("B", 10), rep("C", 10))),
    T2 = factor(c(rep("A", 10), rep("B", 10), rep("C", 10)))
  )
  
  result <- perform_mcnemar_test(mcnemar_dt, c("T1", "T2"), "var1")
  
  expect_null(result)
})

test_that("perform_mcnemar_test detects low discordant pairs", {
  # Créer des données avec peu de paires discordantes
  mcnemar_dt <- data.table::data.table(
    patient_id = paste0("P", 1:20),
    T1 = factor(c(rep("A", 18), rep("B", 2))),
    T2 = factor(c(rep("A", 17), rep("B", 3)))
  )
  
  result <- perform_mcnemar_test(mcnemar_dt, c("T1", "T2"), "var1")
  
  expect_true(grepl("Less than 10 discordant pairs", result$ongoing_message))
})

# =
# Tests pour perform_marginal_homogeneity_test()
# =

test_that("perform_marginal_homogeneity_test detects unbalanced design", {
  # Design non-balancé (nombre différent de mesures par visite)
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 5), rep("B", 5), rep("A", 3))),
    varstrat = factor(c(rep("T1", 5), rep("T2", 5), rep("T3", 3))),
    patient_id = c(paste0("P", 1:5), paste0("P", 1:5), paste0("P", 1:3))
  )
  
  result <- perform_marginal_homogeneity_test(
    dt = dt, 
    variable_interest = "var1",
    varstrat = "varstrat", 
    patient_id = "patient_id"
  )
  
  expect_equal(result$test_used, "/")
  expect_true(grepl("Not balanced design", result$ongoing_message))
})

test_that("perform_marginal_homogeneity_test respects test_more_2_levels parameter", {
  # Design balancé
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 5), rep("B", 5), rep("A", 5))),
    varstrat = factor(c(rep("T1", 5), rep("T2", 5), rep("T3", 5))),
    patient_id = rep(paste0("P", 1:5), 3)
  )
  
  result_no_test <- perform_marginal_homogeneity_test(
    dt = dt, 
    variable_interest = "var1",
    varstrat =  "varstrat", 
    patient_id = "patient_id"
  )
  result_with_test <- perform_marginal_homogeneity_test(
    dt = dt, 
    variable_interest = "var1",
    varstrat =  "varstrat", 
    patient_id = "patient_id"
  )
  
  expect_equal(result_no_test$test_used, "Marginal Homogeneity Test")
  expect_true(grepl("Marginal Homogeneity Test to valid", result_no_test$ongoing_message))
  
  expect_equal(result_with_test$test_used, "Marginal Homogeneity Test")
  expect_true(!is.null(result_with_test$test_result$p.value))
})

# =
# Tests pour simplify_factorial_table()
# =

test_that("simplify_factorial_table simplifies binary 0/1 variable", {
  effectif_tab <- data.table::data.table(
    Modalites = c("0", "1"),
    T1 = c("5 (50%)", "5 (50%)"),
    T2 = c("6 (60%)", "4 (40%)")
  )
  
  result <- simplify_factorial_table(effectif_tab = effectif_tab)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$Modalites, "1")
})

test_that("simplify_factorial_table simplifies binary oui/non variable", {
  effectif_tab <- data.table::data.table(
    Modalites = c("oui", "non"),
    T1 = c("5 (50%)", "5 (50%)"),
    T2 = c("6 (60%)", "4 (40%)")
  )
  
  result <- simplify_factorial_table(effectif_tab)
  
  expect_equal(nrow(result), 1)
  expect_true(tolower(result$Modalites) == "oui")
})

test_that("simplify_factorial_table ...", {
  effectif_tab <- data.table::data.table(
    Modalites = c("0", "1"),
    T1 = c("5 (50%)", "5 (50%)"),
    T2 = c("6 (60%)", "4 (40%)")
  )
  
  result <- simplify_factorial_table(effectif_tab)
  
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)
})

test_that("simplify_factorial_table does nothing for non-binary variables", {
  effectif_tab <- data.table::data.table(
    Modalites = c("A", "B", "C"),
    T1 = c("3 (30%)", "4 (40%)", "3 (30%)"),
    T2 = c("2 (20%)", "5 (50%)", "3 (30%)")
  )
  
  result <- simplify_factorial_table(effectif_tab)
  
  expect_equal(nrow(result), 3)
})

test_that("simplify_factorial_table handles case insensitivity for oui/non", {
  effectif_tab <- data.table::data.table(
    Modalites = c("OUI", "NON"),
    T1 = c("5 (50%)", "5 (50%)"),
    T2 = c("6 (60%)", "4 (40%)")
  )
  
  result <- simplify_factorial_table(effectif_tab)
  
  expect_equal(nrow(result), 1)
  expect_true(grepl("OUI", result$Modalites, ignore.case = TRUE))
})

# =
# Tests pour perform_paired_factorial_test()
# =

test_that("perform_paired_factorial_test chooses McNemar for 2x2", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 10), rep("B", 10), rep("A", 10), rep("B", 10))),
    varstrat = factor(c(rep("T1", 20), rep("T2", 20))),
    patient_id = rep(paste0("P", 1:20), 2)
  )
  
  result <- perform_paired_factorial_test(
    dt = dt, variable_interest = "var1", varstrat = "varstrat", 
    patient_id = "patient_id", varstrat_levels = c("T1", "T2")
  )
  
  expect_equal(result$test_used, "McNemar's Chi-squared test")
})

test_that("perform_paired_factorial_test detects single visit", {
  dt <- data.table::data.table(
    var1 = factor(c(rep("A", 10), rep("B", 10))),
    varstrat = factor(rep("T1", 20)),
    patient_id = paste0("P", 1:20)
  )
  
  result <- perform_paired_factorial_test(
    dt = dt, variable_interest = "var1", varstrat = "varstrat", 
    patient_id = "patient_id", varstrat_levels =  c("T1")
  )
  
  expect_equal(result$test_used, "/")
  expect_true(grepl("only in 1 visit", result$ongoing_message))
})


#### Traps ####

# desc in paired data but one var not present at the 2nd time 
# (or for the paired group) #

tab7 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "var1",
  varstrat = "group",
  precision = 3,
  signif_digits = 3,
  patient_id = "ID_group",
  global_summary = TRUE,
  force_non_parametric_test = FALSE,
  show_metric = "auto",
  do_test = FALSE
)

tab8 <- compute_paired_factorial_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "var2",
  varstrat = "group",
  precision = 3,
  signif_digits = 3,
  simplify = FALSE,
  patient_id = "ID_group",
  force_generate_1_when_0 = TRUE,
  keep_missing_line = TRUE,
  global_summary = TRUE,
  do_test = FALSE
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
  expect_false(all(grepl("Test", names(tab8$line_res))))

  expect_equal(tab7$line_res$`1`, "1.5 [1;2]")
  expect_equal(tab7$line_res$`2`, "/")
  expect_equal(tab8$line_res$`1`[2], "5 (50%)")
  expect_equal(tab8$line_res$`2`[2], "0 ")

  expect_equal(
    tab7$line_res$message[1],
    "Variable present only in 1 level of varstrat, no paired test applicable")

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

## add + tab7
test_that("handle_two_levels detects variable present in only one level", {
  # Variable présente seulement dans le premier niveau
  dt_wide <- data.table::data.table(
    patient_id = paste0("P", 1:10),
    temps1 = rnorm(10, 50, 10)
    # temps2 est absent (toutes les valeurs sont NA et ont été retirées par na.omit)
  )
  
  dt <- data.table::data.table(
    patient_id = rep(paste0("P", 1:10), 2),
    varstrat = factor(rep(c("temps1", "temps2"), each = 10)),
    var1 = c(rnorm(10, 50, 10), rep(NA, 10))
  )
  
  result <- handle_two_levels(
    dt_wide = dt_wide,
    variable_interest = "var1",
    varstrat = "varstrat",
    patient_id = "patient_id",
    show_metric = "auto",
    force_parametric_test = FALSE,
    force_non_parametric_test = FALSE,
    global_summary = FALSE,
    dt = dt
  )
  
  expect_equal(result$test_used, "/")
  expect_true(is.na(result$test_result$p.value))
  expect_true(grepl("only in 1 level", result$ongoing_message))
  expect_null(result$Difference_description)
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
  expect_equal(
    tab9$line_res$message[1], 
    "Variable present have no data (all NA), no desc, no test applicable"
  )
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
  do_test = TRUE
)
tab12 <- compute_paired_continuous_table_and_test(
  dataframe = modified_sleep,
  variable_interest = "all_zero_values",
  varstrat = "visites_5",
  patient_id = "ID2",
  global_summary = TRUE,
  do_test = TRUE
)

test_that("test all zero vars", {
  expect_true(is.list(tab10))
  expect_true(
    all(names(tab10) %in% c("line_res", "test_result"))
  )
  expect_true(tab10$line_res$Test[1] %in% "/")
  expect_true(tab10$line_res$Variable[1] %in% "all_zero_values")
  # expect_false(tab10$line_res$is_Normal[1]) # no more test... const ! 
  # expect_true(grepl(pattern = "Test non applicable", tab10$line_res$message[1]))
  expect_true(grepl(pattern = "toutes les valeurs de 'x' sont identiques", tab10$line_res$message[1]))
  
  expect_true(is.list(tab11))
  expect_true(
    all(names(tab11) %in% c("line_res", "test_result"))
  )
  # expect_true(tab11$line_res$Test[1] %in% "Skillings-Mack test") # works with all zero
  # but no more applied ! 
  expect_true(tab11$line_res$Variable[1] %in% "all_zero_values")
  # expect_false(tab11$line_res$is_Normal[1]) # not relevant to test ! all values const.
  expect_true(
    grepl(
      pattern = "toutes les valeurs de 'x' sont identiques",
      tab11$line_res$message[1])
  )

  # idem 
  expect_true(is.list(tab12))
  expect_true(
    all(names(tab12) %in% c("line_res", "test_result"))
  )
  # expect_true(tab12$line_res$Test[1] %in% "Skillings-Mack test") # works with all zero
  expect_true(tab12$line_res$Variable[1] %in% "all_zero_values")
  # expect_false(tab12$line_res$is_Normal[1])
  expect_true(grepl(pattern = "toutes les valeurs de 'x' sont identiques", tab12$line_res$message[1]))
})

