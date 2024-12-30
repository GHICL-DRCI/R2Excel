# Deploy test on functions "tests_stats"

#### test cochran ####
test_that("cochran", {

  ## check_cochran
  res_cochran_false <- local({
    effobs <- table(modified_state[, c("state.region", "election")])
    # tableau des effectifs theoriques
    efftheo <- as.table(round(rowSums(effobs) %*% t(colSums(effobs)) / sum(effobs), 2))
    names(dimnames(efftheo)) <- names(dimnames(effobs))
    rownames(efftheo) <- rownames(effobs)
    colnames(efftheo) <- colnames(effobs)
    check_cochran(
      efftheo = efftheo
    )
  })

  res_cochran_true <- local({
    effobs <- table(modified_state[, c("binary_test", "election")])
    # tableau des effectifs theoriques
    efftheo <- as.table(round(rowSums(effobs) %*% t(colSums(effobs)) / sum(effobs), 2))
    names(dimnames(efftheo)) <- names(dimnames(effobs))
    rownames(efftheo) <- rownames(effobs)
    colnames(efftheo) <- colnames(effobs)
    check_cochran(
      efftheo = efftheo
    )
  })

  expect_false(res_cochran_false)
  expect_true(res_cochran_true)
})

#### test fisher ####
test_that("fisher", {
  ## test fisher condition
  tc <- with(modified_state, table(var_ab_NAN, election))
  res_fisher_false <- check_fisher(tc)
  tc2 <- with(modified_state, table(state.region, election))
  res_fisher_true <- check_fisher(tc2)

  expect_false(res_fisher_false)
  expect_true(res_fisher_true)
})

#### test prop ####

test_that("test_proportions", {

  ## test_proportions
  res_prop <- test_proportions(
    dataframe = modified_state[, c("state.division", "state.region", "binary_test", "election")],
    vars = c("state.region", "state.division", "binary_test"),
    varstrat = "election"
  )

  expect_true(is.list(res_prop))
  expect_equal(length(res_prop), 3)

  # results
  expect_true(is.data.frame(res_prop$results))
  expect_equal(
    rownames(res_prop$results), c("state.region", "state.division", "binary_test")
  )
  expect_equal(
    res_prop$results$Test,
    c("Fisher's Exact Test for Count Data", "Fisher's Exact Test for Count Data", "khi2 (Pearson's Chi-squared test)") # because cochran FALSE x 2 + TRUE x 1
  )

  # details
  expect_true(is.list(res_prop$details))
  expect_equal(length(res_prop$details), 3) # 3 variables of interest

  # res_prop$details$state.region$observations # --here to add test
  # res_prop$details$state.region$observed # --here to add test
  # res_prop$details$state.region$theorical # --here to add test

  expect_true(class(res_prop$details$state.region$Test) == "htest")

  # 4 digits signif by default
  expect_equal(res_prop$results$P_valeur[1], signif(res_prop$details$state.region$Test$p.value, 4))
  expect_equal(res_prop$results$P_valeur[2], signif(res_prop$details$state.division$Test$p.value, 4))
  expect_equal(res_prop$results$P_valeur[3], signif(res_prop$details$binary_test$Test$p.value, 4))

  expect_true(grepl(res_prop$results$Test[1], res_prop$details$state.region$Test$method, ignore.case = TRUE))
  expect_true(grepl(res_prop$results$Test[2], res_prop$details$state.region$Test$method, ignore.case = TRUE))
  expect_true(
    grepl(gsub("\\)", "", unlist(strsplit(x = res_prop$results$Test[3], split = "(", fixed = TRUE))[2]), res_prop$details$binary_test$Test$method, ignore.case = TRUE)
  )

  # selection
  expect_equal(res_prop$selection, c("state.region", "state.division")) # binary test is not selected because p>0.05
})

test_that("test_proportions_fisher", {

  res_prop_fisher_hide <- test_proportions(
    dataframe = modified_state[, c("var_ab_NAN", "election")],
    vars = c("var_ab_NAN"),
    varstrat = "election"
  )

  expect_true(is.na(res_prop_fisher_hide$results$P_valeur))
})

#### test continuous ####

## test_means
res_means <- test_means(
  dataframe = modified_state[, c("Population", "Income", "election")],
  vars = c("Population", "Income"),
  varstrat = "election"
)

test_that("test_means", {

  expect_true(is.list(res_means))
  expect_equal(length(res_means), 3)

  # result
  expect_true(is.data.frame(res_means$results))
  expect_equal(
    rownames(res_means$results), c("Population", "Income")
  )
  expect_equal(
    any(
      c(
        shapiro.test(modified_state[["Population"]][modified_state$election %in% "red"])$p.value,
        shapiro.test(modified_state[["Population"]][modified_state$election %in% "blue"])$p.value
      ) < 0.05
    ),
    res_means$results$Test[1] %in% "Wilcoxon rank sum exact test (Mann-Whitney)"
  )
  expect_equal(
    all(
      c(
        shapiro.test(modified_state[["Income"]][modified_state$election %in% "red"])$p.value,
        shapiro.test(modified_state[["Income"]][modified_state$election %in% "blue"])$p.value
      ) > 0.05
    ),
    res_means$results$Test[2] %in% "Student T-test"
  )

  # details
  expect_true(is.list(res_means$details))
  expect_equal(length(res_means$details), 2) # 2 variables of interest
  # res_means$details$Population$statistics
  # res_means$details$Population$observations
  # res_means$details$Population$Test
  expect_true(class(res_means$details$Population$Test) == "htest")

  expect_equal(res_means$results$P_valeur[1], signif(res_means$details$Population$Test$p.value, 4))
  expect_equal(res_means$results$P_valeur[2], signif(res_means$details$Income$Test$p.value, 4), tolerance = 0.0000001)

  expect_true(
    grepl(
      gsub("test ", "test", unlist(strsplit(x = res_means$results$Test[1], split = "(", fixed = TRUE))[1]),
      res_means$details$Population$Test$method,
      ignore.case = TRUE
    )
  )
  expect_true(
    grepl(gsub("Student T-test", "Two Sample t-test", res_means$results$Test[2]), res_means$details$Income$Test$method, ignore.case = TRUE)
  )

  # selection
  expect_equal(res_means$selection, c("Income")) # Population  is not selected because p>0.05

  ## no test possible if less than 4 obs in one group :

  res_means_inf_4 <- test_means( # for the moment error
    dataframe = rbind(
      modified_state[, c("Population", "Income", "election")][modified_state$election %in% "red", ],
      modified_state[, c("Population", "Income", "election")][modified_state$election %in% "blue", ][1:3, ]
    ),
    vars = c("Population", "Income"),
    varstrat = "election"
  )

  expect_true(
    res_means_inf_4$results$message[1] ==
      "Error : L'effectif est < 4 dans au moins l un des groupes ! Pas de test possible"
  )
})

test_that("estimation_diff_means", {
  ## estimate_diff_mean
  estim_diff <- estimate_diff_mean(
    dataframe = modified_state[, c("Population", "Income", "election")],
    vars = c("Population", "Income"),
    varstrat = "election",
    digits = 2
  )

  expect_true(is.data.frame(estim_diff))
  expect_equal(
    estim_diff$variable, c("Population", "Income")
  )
  expect_equal(
    res_means$results$Test %in% "Wilcoxon rank sum exact test (Mann-Whitney)",
    estim_diff$Stat %in% "Mediane"
  )
  expect_equal(
    res_means$results$Test %in% "Student T-test",
    estim_diff$Stat %in% "Moyenne"
  )

  expect_equal(
    estim_diff$Difference[estim_diff$variable %in% "Income"],
    round(
      mean(
        modified_state$Income[modified_state$election %in% "blue"], na.rm = TRUE
      ) - mean(
        modified_state$Income[modified_state$election %in% "red"], na.rm = TRUE
      )
      ,
      digits = 2
    )
  )
  
})


#### test force_non_parametric_test param ####
test_that("test force_non_parametric_test", {
  # force_non_parametric_test
  test_not_forced <- test_means(
    dataframe = modified_state[, c("Population", "Income", "election")],
    vars = c("Population", "Income"),
    varstrat = "election",
    force_non_parametric_test = FALSE
  )
  # test_not_forced$results

  test_forced <- test_means(
    dataframe = modified_state[, c("Population", "Income", "election")],
    vars = c("Population", "Income"),
    varstrat = "election",
    force_non_parametric_test = TRUE
  )
  # test_forced$results
  expect_true(
    test_not_forced$results[2, "Test"] %in% "Student T-test" &
    test_forced$results[2, "Test"] %in%  "Wilcoxon rank sum exact test (Mann-Whitney)"
  )
  expect_true(
    grepl("force_non_parametric_test", test_forced$results[2, "message"])
  )

})

#### test shapi ####
# res_means_error_shapi --here to imagine
