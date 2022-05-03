
# Categorize imp vars for forestplot --------------------------------------

tcdataimp <- tcdataimp %>%
  select(-CKD) %>%
  mutate(
    age_entry_cat = factor(case_when(
      age_entry < 70 ~ 1,
      age_entry >= 70 ~ 2
    ), levels = 1:2, labels = c("<70", ">=70")),
    CKD = ynfac(case_when(
      is.na(gfrckdepi2021) ~ NA_real_,
      gfrckdepi2021 < 60 ~ 1,
      gfrckdepi2021 >= 60 ~ 0
    )) # again to use the imputed values
  )

# Create vars for comp risk analysis --------------------------------------

vars <- c("prevhfhosp", "prevhfhosp_cat", "prevhfhosp1yr", modvars)
for (i in seq_along(vars)) {
  tcdataimp <<- create_crvar(tcdataimp, vars[i])
}