
tcdataimp <- tcdata

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

impfunc <- function(var) {
  cl <- class(tcdata %>% pull(!!sym(var)))
  if (cl == "numeric") {
    impval <- median(tcdata %>% pull(!!sym(var)), na.rm = T)
  }
  if (cl %in% c("factor", "character")) {
    if (var == "ntprobnp_bnp_cat") {
      impval <- "Tertile 2"
    } else {
      impval <- Mode(tcdata %>% pull(!!sym(var)))
    }
  }
  tcdataimp <<- tcdataimp %>%
    mutate(!!sym(var) := replace_na(!!sym(var), impval))
}

for (i in seq_along(modvars)) {
  impfunc(modvars[i])
}


#koll <- print(CreateTableOne(
#  vars = modvars,
#  data = tcdataimp,
# ),
# missing = TRUE,
# printToggle = FALSE,
# )
