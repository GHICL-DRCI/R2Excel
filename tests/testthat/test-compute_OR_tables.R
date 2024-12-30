tab_or1 <- get_OR_univar(
  dataframe = modified_state,
  dependent_var = "binary_test",
  explanatory_vars = c(
    "Population", "Income", "Illiteracy", "Life Exp", "Murder",
    "HS Grad", "Frost",
    "state.division", "state.region",
    "var_conti_trap", # trap
    "var_quali_trap", # trap
    "yes_no_french_question",
    "all_count_zero", # trap
    "var_ab_NAN"
  ),
  check_n_levels = FALSE
)
reg <- glm(formula = "binary_test ~ `HS Grad`", family = binomial(), data = modified_state)
or_by_hand <- unname(round(exp(reg$coefficients)[2], 2))
p_by_hand <- round(summary(reg)$coefficients[2, "Pr(>|z|)"], 3)

test_that("OR computation", {
  expect_equal(length(unique(tab_or1$Variable)), 11) # 14 - 3 traps
  expect_equal(ncol(tab_or1), 4)
  # test values
  expect_equal(
    as.double(gsub("(.*) (.*)", "\\1", tab_or1[tab_or1$Variable %in% "HS Grad", .(OR)])),
    or_by_hand
  )
  expect_equal(
    round(as.double(unlist(tab_or1[tab_or1$Variable %in% "HS Grad", .(OR_P_valeur)])), 3),
    p_by_hand
  )

})

tab_or2 <- get_OR_univar(
  dataframe = modified_state,
  dependent_var = "binary_test",
  explanatory_vars = c(
    "var_conti_trap", # trap
    "var_quali_trap", # trap
    "all_count_zero" # trap
  )
)
test_that("OR check traps", {
  expect_true(is.null(tab_or2))
})

tab_or3 <- get_OR_univar(
  dataframe = modified_state,
  dependent_var = "binary_test",
  explanatory_vars = c(
    "Population",
    "zero_levels" # trap : zero levels
  ),
  check_n_levels = TRUE
)
test_that("OR check levels option", {
  expect_equal(nrow(tab_or3), 1)
})


# iris$bin <- as.factor(ifelse(iris$Species == "setosa", 0, 1))
# # summary(glm(bin~Sepal.Width, family = binomial(), data = iris))
# tab_or4 <- get_OR_univar(
#   dataframe = iris,
#   dependent_var = "bin",
#   explanatory_vars = c(
#     "Sepal.Width"
#   ),
#   check_n_levels = TRUE
# )
## ok 10 digits p values

## --here test rules about sample size, IC inf, etc
