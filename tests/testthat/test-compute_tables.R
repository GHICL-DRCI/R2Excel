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
   digits = 2
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
    c("", paste0(varstrat_wanted, unique(modified_state$election)))
  )
  expect_identical(
    colnames(res_conti_tabs[[1]]),
    c("mean", "sd", "median", "Q1", "Q3", "min", "max", "N", "NA", "Nb_mesures", "is_Normal") # default
  )
})

test_that("test values in conti tab", {
  expect_equal(
    res_conti_tabs[[1]][1, "mean"],
    mean(modified_state[[vars_wanted[1]]])
  )
  expect_false( # round 1 digits
    res_conti_tabs[[1]][1, "mean"] == round(mean(modified_state[[vars_wanted[1]]]), 1)
  )
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
    res_conti_tabs[[1]]["electionred", "NA"],
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
    res_conti_tabs[["Frost"]][1, "NA"],
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
  digits = 2
)

test_that("test dim in conti subset tab", {
  expect_equal(length(res_conti_tabs_subset), 1)
  expect_equal(nrow(res_conti_tabs_subset[[1]]), 3) # pop globla + red + blue
  expect_equal(ncol(res_conti_tabs_subset[[1]]), 3) # "mean", "sd", "N"
  expect_identical(
    rownames(res_conti_tabs_subset[[1]]),
    c("", paste0(varstrat_wanted, unique(modified_state$election)))
  )
  expect_identical(
    colnames(res_conti_tabs_subset[[1]]),
    c("mean", "sd", "N")
  )
})

#### factorial tables ####

vars_wanted <- c("state.division", "state.region", "binary_test")
varstrat_wanted <- "election"
res_fact_tabs <- compute_factorial_table(
  dataframe = modified_state,
  vars = vars_wanted,
  varstrat = varstrat_wanted,
  digits = 2,
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
    c(levels(modified_state[[vars_wanted[1]]]), "NA", "Nb_mesures")
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
  digits = 2,
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
  digits = 2,
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
  digits = 2,
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
  digits = 2,
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
  digits = 2
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

