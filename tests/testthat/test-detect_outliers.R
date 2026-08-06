message("test outliers detection - done v0.2.2") 

# Données exemple
set.seed(1992)
dt <- data.table::data.table(
  PAM     = c(rnorm(43, 85, 10), 145, 150),
  LACTATE = c(rnorm(44, 1.8, 0.5), 8.2, NA),
  FC      = c(rnorm(44, 75, 10), 180, NA),
  BMI     = rnorm(45, 22, 1)
)
# dt

# Tukey - détail (voir Row_index)
dt1 <- detect_outliers(dt = dt, vars = names(dt), method = "Tukey", verbose = FALSE)

# Tukey - résumé
dt2 <- detect_outliers(dt, vars = names(dt), method = "Tukey", summary = TRUE, verbose = FALSE)

# Zscore - détail
dt3 <- detect_outliers(dt, vars = names(dt), method = "Zscore", verbose = FALSE)

# Zscore - résumé
dt4 <- detect_outliers(dt, vars = names(dt), method = "Zscore", summary = TRUE, verbose = FALSE)

# Tukey avec threshold custom (boxplot étendu : 3 au lieu de 1.5)
dt5 <- detect_outliers(dt, vars = names(dt), method = "Tukey", threshold = 3, verbose = FALSE)

# Sur des variables spécifiques seulement
dt6 <- detect_outliers(dt, vars = c("PAM", "FC"), method = "Zscore", verbose = FALSE)

# no outliers
dt7 <- detect_outliers(dt = dt, vars = "BMI", method = "Tukey", verbose = FALSE)

test_that("error outliers", {
  expect_error( 
    detect_outliers(dt = iris, vars = "Species", method = "Zscore")
  )
})

test_that("no outliers", {
  expect_null(dt7)
  expect_equal(
    dt2$N_outliers[dt2$Variable %in% "BMI"], 
    0
  )
})

test_that("find outliers summary", {
  expect_equal(nrow(dt2), length(names(dt)))
  expect_equal(nrow(dt4), length(names(dt)))
  expect_equal(
    dt2$N_outliers[dt2$Variable %in% "FC"],
    nrow(dt1[dt1$Variable %in% "FC", ])
  )
  expect_equal(
    dt4$N_outliers[dt4$Variable %in% "PAM"],
    nrow(dt3[dt3$Variable %in% "PAM", ])
  )
})

test_that("find outliers detail", {
  expect_equal(unique(dt6$Variable), c("PAM", "FC"))
  expect_equal(
    dt$PAM[dt6$Row_index[dt6$Variable %in% "PAM"]], 
    dt6$Value[dt6$Variable %in% "PAM"]
  )
})

test_that("Computation outliers ranges Tukey", {
  expect_equal(
    dt2$Q1[dt2$Variable %in% "PAM"],
    unname(round(quantile(dt$PAM, probs = 0.25), 3))
  )
  expect_equal(
    dt2$Q3[dt2$Variable %in% "PAM"],
    unname(round(quantile(dt$PAM, probs = 0.75), 3))
  )
  expect_equal(
    dt2$Lower_bound[dt2$Variable %in% "PAM"],
    unname(round(
      round(quantile(dt$PAM, probs = 0.25), 3) -
        dt2$Threshold[dt2$Variable %in% "PAM"] * (
          dt2$Q3[dt2$Variable %in% "PAM"] - dt2$Q1[dt2$Variable %in% "PAM"]
        )
      ,3
    ))
  )
})


test_that("Computation outliers ranges Zscore", {
  expect_equal(
    dt4$SD[dt4$Variable %in% "PAM"],
    round(sd(dt$PAM), 3)
  )
  expect_equal(
    dt4$Mean[dt4$Variable %in% "PAM"],
    (round(mean(dt$PAM), 3))
  )
  expect_equal(
    dt4$Upper_bound[dt4$Variable %in% "PAM"],
    round(
      dt4$Mean[dt4$Variable %in% "PAM"] +
        dt4$Threshold[dt4$Variable %in% "PAM"] * dt4$SD[dt4$Variable %in% "PAM"]
        
      ,3
    )
  )
})

