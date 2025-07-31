dt <- data.frame(
  date1 = as.Date(c("2025-07-30", "2025-07-29", "2025-07-28")), 
  date2 = as.Date(c("2025-06-20", "2025-06-10", "2025-06-01")), 
  date3 = as.POSIXct(c("2024-06-20", "2024-06-10", "2024-06-01")), 
  autre = c(1,2,3)
)
dt2 <- data.frame(
  date1 = as.Date(c("2025-07-30", "2025-07-29", "2025-07-28", "2025-07-30", "2025-07-29", "2025-07-28")), 
  date2 = as.POSIXct(c("2024-06-20", "2024-06-10", "2024-06-01", "2025-07-30", "2025-07-29", "2025-07-28")), 
  fact = as.factor(c(rep("A", 3), rep("B", 3)))
)

#### date utils ####

vars_dates <- get_dates(dataframe = dt, vars = names(dt))
vars_dates2 <- get_dates(dataframe = dt2, vars = names(dt2))

test_that("test date detection", {
  expect_equal(length(vars_dates), 3)
  expect_equal(length(vars_dates2), 2)
})

#### date tables ####

tab_date1 <- compute_date_table(
  dataframe = dt,
  vars = names(dt)
)
# tab_date1


tab_date2 <- compute_date_table(
  dataframe = dt2,
  vars = names(dt2), 
  varstrat = "fact"
)
tab_date2

test_that("test date table", {
  expect_true(is.data.frame(tab_date1))
  expect_equal(nrow(tab_date1), length(vars_dates))
  # avec varstrat
  expect_true(is.data.frame(tab_date2))
  expect_equal(nrow(tab_date2), length(vars_dates2) * nlevels(dt2$fact))
})


#### date excel sheet ####
dir.create("tmp", showWarnings = FALSE)
save_excel_results(
  dataframe = dt2,
  vars = names(dt2), 
  varstrat = "fact", 
  file = file.path("tmp", "a-desc_date.xlsx")
)

test_that("test date file", {
  expect_true(file.exists(file.path("tmp", "a-desc_date.xlsx")))
  expect_equal(
    readxl::excel_sheets(file.path("tmp", "a-desc_date.xlsx")),
    "date - fact"
  )
  tab <- data.table::setDT(
    readxl::read_excel(file.path("tmp", "a-desc_date.xlsx"))
  )
  expect_true(all(tab == tab_date2))
})
# clear tmp test folder
unlink("tmp", recursive = TRUE)


# tested
# save_excel_results(
#   dataframe = prechucard_dt,
#   file = "02-desctest.xlsx",
#   vars =  c(
#     "DDN", "Date HJ", ## desc date to dev
#     "Centre", "Genre", "Age", 
#     "Chutes_expliquees", "Une_chute_inexpliquee", "Plus_une_chutes_inexpliquees", 
#     "Porteur_d_un-PM",
#     "Charlson", "Créatinine", "DFG_CKD_epi", "Nbre_médicaments"
#   ),
#   force_generate_1_when_0 = FALSE
# )
# save_excel_results(
#   dataframe = prechucard_dt,
#   file = "02-desctest2.xlsx",
#   vars =  c(
#     "DDN", "Date HJ", ## desc date to dev
#     "Centre", "Genre", "Age", 
#     "Chutes_expliquees", "Une_chute_inexpliquee", 
#     "Porteur_d_un-PM",
#     "Charlson", "Créatinine", "DFG_CKD_epi", "Nbre_médicaments"
#   ),
#   varstrat = "Plus_une_chutes_inexpliquees"
# )
# 
# 
# save_excel_results(
#   dataframe = prechucard_dt,
#   file = "02-desctest3.xlsx",
#   vars =  c(
#     "DDN", "Date HJ", ## desc date to dev
#     "Age", 
#     "Chutes_expliquees", "Une_chute_inexpliquee", 
#     "Porteur_d_un-PM",
#     "Charlson", "Créatinine", "DFG_CKD_epi", "Nbre_médicaments"
#   ),
#   varstrat = "Centre*Genre"
# )
