tabvars <- c(
  "age_entry",
  "GENDER",
  "race",
  "ef_tot",
  "ef_tot_cat",
  "EF",
  "EF_cat",
  "MI",
  "STROKE",
  "CABG",
  "PCI",
  "COPD",
  "HTN",
  "revasc",
  "AFIB",
  "DM",
  "ICD",
  "PACEMAKER",
  "anemia",
  "nyha_class_cat",
  "bmi",
  "HR",
  "SBP",
  "DBP",
  "map",
  "CR_mgdl",
  "gfrckdepi2021",
  "HB_gdL",
  "NA_mmolL", "K_mmolL",
  "ntprobnp",
  "bnp",
  "ntprobnp_bnp_cat",
  "QRS_DUR", "ECG_AFIB",
  "spironolactone",
  "rasi",
  "bbl",
  "cbl",
  "statin",
  "diuretics",
  "loopdiuretics"
)

nomodvars <- c(
  "race", "ef_tot", "ef_tot_cat", "EF",
  "SBP",
  "DBP",
  "CR_mgdl",
  "HB_gdL",
  "NA_mmolL", "K_mmolL",
  "ntprobnp",
  "ntprobnp_cat",
  "bnp",
  "bnp_cat",
  "QRS_DUR", "ECG_AFIB",
  "diuretics"
)

modvars <- tabvars[!tabvars %in% nomodvars]

modvarscox <- modvars
modvarscox[modvarscox == "loopdiuretics"] <- "strata(loopdiuretics)" # strata variables
kontvars <- c("age_entry", "bmi", "HR", "map", "gfrckdepi2021")
modvarscox[modvarscox %in% kontvars] <- paste0("ns(", kontvars, ", df = 5)") # continous variables w splines
