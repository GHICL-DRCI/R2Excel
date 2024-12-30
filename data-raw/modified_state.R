## Code to prepare `modified_state` dataset goes here

### Creation of Toy data used to test descriptive function.
### Contributor : Sahara Graf - 15.06.2021

state.x77 <- as.data.frame(datasets::state.x77)
state.division <- data.frame(state.division = datasets::state.division)
state.region <- data.frame(state.region = datasets::state.region)
state.abb <- data.frame(state.abb = datasets::state.abb)

modified_state <- cbind(state.x77, state.division, state.region, state.abb)

# Add some missing values
modified_state[c(2, 15, 33, 50), "Income"] <- NA
modified_state[c(10, 21, 44, 49), "Murder"] <- NA
modified_state[c(5, 10, 33, 49), "Frost"] <- NA

# Red states (vote)
list1 <- c(
  "AL", "AK", "AR", "FL", "ID", "IN", "IA", "KS", "KY", "LA", "MS", "MO", "MT",
  "NE", "NC", "ND", "OH", "OK", "SC", "SD", "TN", "TX", "UT", "WA", "WV", "WY"
)
# Blue states (vote)
list2 <- setdiff(modified_state$state.abb, list1)
# create variable election as factor
modified_state$election <- factor(NA, levels = c("red", "blue"))
modified_state$election[which(modified_state$state.abb %in% list1)] <- "red"
modified_state$election[which(modified_state$state.abb %in% list2)] <- "blue"

modified_state$binary_test <- factor(
  c(rep(0, times = 10), rep(1, times = 10), rep(0, times = 10), rep(1, times = 15), rep(0, times = 5)),
  levels = 0:1
)
modified_state$yes_no_french_question <- factor(
  c(rep("oui", nrow(modified_state)*0.4), rep("non", nrow(modified_state)*0.6)),
  levels = c("non", "oui"), labels = c("non", "oui")
)
modified_state$all_count_zero <- factor(
  0,
  levels = c(0), labels = c(0)
)

set.seed(42)
modified_state$var_ab_NAN <- sample(x = c("a", "b"), size = nrow(modified_state), replace = TRUE)
modified_state$var_ab_NAN <- ifelse(modified_state$election %in% "red", NA, modified_state$var_ab_NAN)
modified_state$var_ab_NAN <- factor(modified_state$var_ab_NAN, levels = c("a", "b"))
with(modified_state, table(var_ab_NAN, election))

## remove variable state.abb, no more used
modified_state$state.abb <- NULL

str(modified_state) # dataframe readt ti be tested
# clear envir
rm(state.abb, state.division, state.region, state.x77, list1, list2)

## add special cases
set.seed(1992)
modified_state$special_condition <- as.factor(ifelse(
  modified_state$election %in% "red",
  NA,
  sample(c("oui", "non", "peut-etre"), size = length(!modified_state$election %in% "red"),replace = TRUE)
))
modified_state$special_measures <- ifelse(
  modified_state$election %in% "red",
  NA,
  rnorm(n = length(!modified_state$election %in% "red"), mean = 42, sd = 4.2)
)

modified_state$var_conti_trap <- ifelse(modified_state$binary_test == 1, NA, sample(1:25, size = 50, replace = T))
modified_state$var_quali_trap <- as.factor(ifelse(modified_state$binary_test == 1, NA, sample(c(0, 1), size = 50, replace = T)))

# trap
modified_state$zero_levels <- NA
modified_state$zero_levels <- as.factor(modified_state$zero_levels)

# Save object
usethis::use_data(modified_state, overwrite = TRUE)
