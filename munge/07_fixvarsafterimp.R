
# Categorize imp vars for forestplot --------------------------------------

tcdataimp <- tcdataimp %>%
  mutate(age_entry_cat = factor(case_when(
    age_entry < 70 ~ 1,
    age_entry >= 70 ~ 2
  ), levels = 1:2, labels = c("<70", ">=70")))

# Create vars for comp risk analysis --------------------------------------

vars <- c("prevhfhosp", "prevhfhosp_cat", "prevhfhosp1yr", modvars)
for (i in seq_along(vars)) {
  tcdataimp <<- create_crvar(tcdataimp, vars[i])
}
