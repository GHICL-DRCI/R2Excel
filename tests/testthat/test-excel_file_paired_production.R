dir.create("tmp", showWarnings = FALSE)

path10 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "mesure2", "mesure3"),
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 1,
  signif_digits = 4,
  global_summary = TRUE,
  file = file.path("tmp", "10-desc_paired_data1.xlsx")
)

path11 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "mesure2", "mesure3"),
  varstrat = c("visites_5"),
  patient_id = "ID",
  digits = 2,
  global_summary = TRUE,
  file = file.path("tmp", "11-desc_paired_data2.xlsx")
)

path12 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "fact1", "fact1_na"),
  varstrat = c("visites_2"),
  patient_id = "ID2",
  digits = 2,
  global_summary = TRUE,
  force_generate_1_when_0 = FALSE,  # for fact tab
  keep_missing_line = TRUE, # for fact tab
  file = file.path("tmp", "12-desc_paired_data3.xlsx")
)

path13 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1"),
  varstrat = c("visites_4"),
  patient_id = "ID2",
  digits = 2,
  global_summary = TRUE,
  force_non_parametric_test = TRUE,
  force_generate_1_when_0 = FALSE,
  metric_show = "median",
  keep_missing_line = TRUE, # for fact tab
  file = file.path("tmp", "13-desc_paired_data4.xlsx")
)

tab10 <- readxl::read_excel(
  path10,
  sheet = "quantitative - visites_2"
)
test_that("Test Paired excel", {
  expect_true(file.exists(path10))
  expect_true(file.exists(path11))
  expect_true(file.exists(path12))
  expect_true(file.exists(path13))

  expect_equal(
  as.numeric(unlist(strsplit(x = tab10$Difference_description[1], split = " "))[1]),
  round(mean(modified_sleep$extra[modified_sleep$visites_2 %in% "temps2"] - 
    modified_sleep$extra[modified_sleep$visites_2 %in% "temps1"]), 1)
  )
  expect_true(all(grepl("paired", tab10$Test, ignore.case = TRUE)))
})

#### crossed vars ####
modified_sleep$`var fact spaced` <- as.factor(
  sample(x = c("a", "b"), size = nrow(modified_sleep), replace = TRUE)
)
path14 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "fact1", "fact1_na", "var fact spaced"),
  varstrat = "visites_2*fact3",
  patient_id = "ID2",
  digits = 2,
  global_summary = TRUE,
  force_generate_1_when_0 = FALSE, # for fact tab
  keep_missing_line = FALSE, # for fact tab
  file = file.path("tmp", "14-desc_paired_cross.xlsx")
)

tab_cross_quanti <- readxl::read_excel(
  path14, sheet = "quanti-visites_2-fact3"
)
tab_cross_quali <- readxl::read_excel(
  path14, sheet = "quali-visites_2-fact3"
)
test_that("crossed paired", {
  expect_true(file.exists(path14))
  
  expect_equal(ncol(tab_cross_quanti), 11)
  expect_true(all(unlist(tab_cross_quanti[-1, 5]) %in% "/")) # --insteaded of NA [NA;NA]
  expect_true(all(unlist(tab_cross_quanti[-1, 11]) %in% "/"))
  expect_true(any(grepl("var fact spaced", tab_cross_quali$Variable))) 
  ## --todo, --test eval content of the table ... +++
  expect_true(all( # check order of pop totale
    tab_cross_quanti[1, 2:4] %in% c("Nb_mesures", "Valeurs_manquantes", "fact3=Population_totale")
  ))
})

#### simplify in paired table ####

path15 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("fact1", "fact2", "fact3", "fact1_na"),
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  signif_digits = 4,
  force_generate_1_when_0 = FALSE, # for fact tab
  keep_missing_line = FALSE, # for fact tab
  detail_NB_mesure_sum = TRUE,
  crossed_varstrat_test = TRUE,
  file = file.path("tmp", "15-desc_paired_data6.xlsx")
)
path16 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("fact1", "fact2", "fact3", "fact1_na"),
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  signif_digits = 4,
  simplify = TRUE,
  force_generate_1_when_0 = FALSE, # for fact tab
  keep_missing_line = FALSE, # for fact tab
  detail_NB_mesure_sum = TRUE,
  crossed_varstrat_test = TRUE,
  file = file.path("tmp", "16-desc_paired_data7.xlsx")
)

test_that("simplif paired", {
  expect_true(file.exists(path15))
  expect_true(file.exists(path16))
  tab_nosimp <- readxl::read_excel(path15, sheet = "qualitative - visites_2")
  tab_simp <- readxl::read_excel(path16, sheet = "qualitative - visites_2")
  expect_equal(ncol(tab_nosimp), ncol(tab_simp))
  expect_equal(nrow(tab_simp), 6)
  expect_equal(nrow(tab_nosimp), 8)
  ## --todo, --test eval content of the table ... +++
})

#### desc in paired data but one var not present at the 2nd time (or for the paired group) ####
path17 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("var2", "var1"),
  varstrat = "group",
  patient_id = "ID2",
  digits = 2,
  signif_digits = 4,
  simplify = FALSE,
  file = file.path("tmp", "17-desc_paired_data8.xlsx")
)

path18 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("var2", "var1", "mesure3", "fact1"),
  varstrat = "group",
  patient_id = "ID_group",
  digits = 2,
  signif_digits = 4,
  simplify = FALSE,
  file = file.path("tmp", "18-desc_paired_data9.xlsx")
)
## --todo, --test eval content of the table ... +++

test_that("test var not present", {
  expect_true(file.exists(path17))
  expect_true(file.exists(path18))
})

#### fix global summary ####

path19 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("fact1", "fact2", "fact3", "fact1_na"),
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  signif_digits = 4,
  global_summary = TRUE, # works
  force_generate_1_when_0 = FALSE, # for fact tab
  keep_missing_line = FALSE, # for fact tab
  detail_NB_mesure_sum = TRUE,
  crossed_varstrat_test = TRUE,
  file = file.path("tmp", "19-desc_paired_data10.xlsx")
)
path20 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("fact1", "fact2", "fact3", "fact1_na"),
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 2,
  signif_digits = 4,
  global_summary = TRUE,
  force_generate_1_when_0 = FALSE,
  keep_missing_line = TRUE, # wokrs
  detail_NB_mesure_sum = TRUE,
  crossed_varstrat_test = TRUE,
  file = file.path("tmp", "20-desc_paired_data10a.xlsx")
)
path21 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("var2", "var1", "mesure3", "fact1"),
  varstrat = "group",
  patient_id = "ID_group",
  global_summary = TRUE, # works
  digits = 2,
  signif_digits = 4,
  simplify = FALSE,
  file = file.path("tmp", "21-desc_paired_data11.xlsx")
)

path22 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "fact3", "fact1_na"),
  varstrat = "visites_2*fact1",
  patient_id = "ID2",
  digits = 2,
  signif_digits = 4,
  global_summary = FALSE,
  force_generate_1_when_0 = FALSE, # for fact tab
  keep_missing_line = FALSE, # for fact tab
  detail_NB_mesure_sum = TRUE,
  crossed_varstrat_test = TRUE,
  file = file.path("tmp", "22-desc_paired_data12.xlsx")
)

test_that("test global", {
  expect_true(file.exists(path19))
  expect_true(file.exists(path20))
  expect_true(file.exists(path21))
  expect_true(file.exists(path22))
})


#### fix test_more_2_levels for quali sheet ####

path23 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "fact1", "fact1_na"),
  varstrat = c("visites_5"),
  patient_id = "ID",
  digits = 2,
  global_summary = TRUE,
  force_generate_1_when_0 = FALSE,
  keep_missing_line = TRUE,
  test_more_2_levels = TRUE,
  file = file.path("tmp", "23-desc_paired_data_tested.xlsx")
)

test_that("test_more_2_levels", {
  expect_true(file.exists(path23))
})

#### test padj ####

path24 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "mesure2", "mesure3",  "fact1", "fact1_na",  "fact3"),
  varstrat = "visites_2",
  patient_id = "ID2",
  digits = 1,
  global_summary = TRUE,
  show_p_adj = TRUE,
  file = file.path("tmp", "24-desc_paired_adj.xlsx")
)

path25 <- save_excel_paired_results(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings", "mesure1", "fact3", "fact1_na"),
  varstrat = "visites_2*fact1",
  patient_id = "ID2",
  digits = 2,
  signif_digits = 4,
  global_summary = FALSE,
  force_generate_1_when_0 = FALSE, # for fact tab
  keep_missing_line = FALSE, # for fact tab
  detail_NB_mesure_sum = TRUE,
  crossed_varstrat_test = TRUE,
  show_p_adj = TRUE,
  file = file.path("tmp", "25-desc_paired_data12_adj.xlsx")
)

test_that("Test Padj", {
  expect_true(file.exists(path24))
  expect_true(file.exists(path25))
})

#### test droplevels ####

tmp26 <- modified_sleep[group %in% 1, ] # keep only 1 values over the 2 levels
path26_paired <- save_excel_paired_results(
  dataframe = tmp26,
  file = file.path("tmp", "26-droplevels_paired.xlsx"),
  vars = c("group"),
  varstrat = "visites_2",
  patient_id = "ID2",
  drop_levels = TRUE
)
sheet_26 <- readxl::read_excel(
  file.path("tmp", "26-droplevels_paired.xlsx")
)
path26b_paired <- save_excel_paired_results(
  dataframe = tmp26,
  file = file.path("tmp", "26b-droplevels_paired.xlsx"),
  vars = c("group"),
  varstrat = "visites_2",
  patient_id = "ID2",
  drop_levels = TRUE, keep_missing_line = FALSE
)
sheet_26b <- readxl::read_excel(
  file.path("tmp", "26b-droplevels_paired.xlsx")
)

test_that("test drop_levels", {
  expect_true(nrow(sheet_26)==2)
  expect_true(nrow(sheet_26b)==1)
})


#### Variables_all_na ####
modified_sleep$all_na_var <- NA
path27 <- save_excel_paired_results(
  dataframe = modified_sleep,
  file = file.path("tmp", "27-allna.xlsx"),
  vars = c(
    "all_na_var", "extra"
  ),
  varstrat = "visites_2",
  patient_id = "ID2"
)
modified_sleep$all_na_var <- NULL
sheet_27 <- readxl::read_excel(
  file.path("tmp", "27-allna.xlsx")
)
sheet_27na <- readxl::read_excel(
  file.path("tmp", "27-allna.xlsx"), sheet = "Variables_all_na"
)
sheet_names <- readxl::excel_sheets(file.path("tmp", "27-allna.xlsx"))
test_that("test all na", {
  expect_true(nrow(sheet_27) == 1)
  expect_true(sheet_27$Variable == "extra")
  expect_equal(sheet_names, c("quantitative - visites_2", "Variables_all_na"))
  expect_equal(sheet_27na$Variables_all_na, c("all_na_var"))
})

#### save_excel_paired_results_filtertest ####

save_excel_paired_results_filtertest(
  dataframe = modified_sleep,
  vars = c("extra", "extra_with_missings"),
  varstrat = "visites_2",
  patient_id = "ID2",
  file = file.path("tmp", "28-desc_paired_data_tested.xlsx")
)
sheet_28 <- readxl::read_excel(
  file.path("tmp", "28-desc_paired_data_tested.xlsx")
)

test_that("test save_excel_paired_results_filtertest", {
  expect_true(nrow(sheet_28) == 2)
  expect_equal(sheet_28$Variable, c("extra", "extra_with_missings"))
  expect_equal(sheet_28$N_individuals, sheet_28$temps1_N)
  expect_equal(sheet_28$N_individuals, sheet_28$temps2_N)
})

#### end ####
# clear tmp test folder
unlink("tmp", recursive = TRUE)

