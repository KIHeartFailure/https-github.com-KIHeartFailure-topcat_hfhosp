tabvars <- c(
  "age_entry",
  "GENDER",
  "race",
  #"countryfac",
  "ef_tot",
  "ef_tot_cat",
  "EF",
  "EF_cat",
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
  "MI",
  "STROKE",
  "revasc",
  "COPD",
  "HTN",
  "AFIB",
  "DM",
  "ICD",
  "PACEMAKER",
  "anemia",
  "CKD",
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
  "map",
  "DBP",
  "CR_mgdl",
  "anemia",
  "NA_mmolL", 
  "ntprobnp",
  "ntprobnp_cat",
  "bnp",
  "bnp_cat",
  "QRS_DUR", "ECG_AFIB",
  "diuretics", 
  "CKD"
)

modvars <- tabvars[!tabvars %in% nomodvars]

modvarscox <- modvars
modvarscox[modvarscox == "loopdiuretics"] <- "strata(loopdiuretics)" # strata variables
kontvars <- c("age_entry", "bmi", "HR", "SBP", "gfrckdepi2021", "HB_gdL", "K_mmolL")
modvarscox[modvarscox %in% kontvars] <- paste0("ns(", kontvars, ", df = 4)") # continous variables w splines
