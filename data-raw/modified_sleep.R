## Code to prepare `modified_sleep` dataset goes here

### Creation of Toy data used to test descriptive function.

require(stats)
modified_sleep <- rbind(sleep, sleep)

# Add some missing values
modified_sleep$extra[20:30] <- modified_sleep$extra[20:30] * 0.15
modified_sleep$extra[31:40] <- modified_sleep$extra[1:10] * 0.2
modified_sleep$extra_with_missings <- modified_sleep$extra
modified_sleep[c(1, 2, 13), "extra_with_missings"] <- NA

## creation of some numeric vars
set.seed(42)
modified_sleep$mesure1 <- sample(x = modified_state$Income, size = 40, replace = TRUE)
modified_sleep$mesure2 <- sample(x = modified_state$Illiteracy, size = 40, replace = TRUE)
modified_sleep$mesure3 <- sample(x = modified_state$Area, size = 40, replace = TRUE) # non normal

# modified_state$var_conti_trap <- ifelse(modified_state$binary_test ==1, NA, sample(1:25, size = 50, replace = T))

## creation of some factor vars
# table(modified_sleep$group)
modified_sleep$group
modified_sleep$fact1 <- modified_state$election[1:nrow(modified_sleep)]
modified_sleep$fact1_na <- modified_sleep$fact1
modified_sleep[c(10), "fact1_na"] <- NA
modified_sleep$fact2 <- modified_state$yes_no_french_question[1:nrow(modified_sleep)]
modified_sleep$fact3 <- modified_sleep$fact2
modified_sleep[c(1, 2, 13, 14, 15), "fact3"] <- NA

# create variable visites as factor
modified_sleep$visites_2 <- factor( # complete block design
  c(rep(1, times = 20), rep(2, times = 20)),
  levels = 1:2, labels = paste0("temps", 1:2)
)
modified_sleep$ID2 <- factor( # complete block design
  c(letters[1:20], letters[1:20]),
  levels = letters[1:20], labels = letters[1:20]
)
modified_sleep$visites_4 <- factor( # complete block design
  c(rep(1, times = 10), rep(2, times = 10), rep(3, times = 10), rep(4, times = 10)),
  levels = 1:4, labels = paste0("visiteM", 1:4)
)
modified_sleep$visites_5 <- factor( # incomplete block design
  c(rep(1, times = 10), rep(2, times = 10), rep(3, times = 10), rep(4, times = 5), rep(5, times = 5)),
  levels = 1:5, labels = paste0("visiteD", 1:5)
)

## paired data but one var not present at the 2nd time (or for the paired group)
modified_sleep$var1 <- c(rep(1, 5), rep(2, 5), rep(NA, 30))
modified_sleep$var2 <- as.factor(c(rep("A", 5), rep("B", 5), rep(NA, 30)))

table(modified_sleep$var1, modified_sleep$group)
table(modified_sleep$var2, modified_sleep$group) # ID2
table(modified_sleep$var2, modified_sleep$ID2)

modified_sleep$ID_group <- factor(
  c(letters[1:10], letters[1:10], letters[11:20], letters[11:20]),
  levels = letters[1:20], labels = letters[1:20]
)

modified_sleep$all_zero_values <- 0 # will make an error during shapiro test


str(modified_sleep)

# Save object
usethis::use_data(modified_sleep, overwrite = TRUE)
