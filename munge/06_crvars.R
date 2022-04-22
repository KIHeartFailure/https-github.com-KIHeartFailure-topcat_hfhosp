# Create vars for comp risk analysis --------------------------------------

vars <- c("prevhfhosp", "prevhfhosp_cat", "prevhfhosp1yr", modvars)
for (i in seq_along(vars)) {
  tcdataimp <<- create_crvar(tcdataimp, vars[i])
}
