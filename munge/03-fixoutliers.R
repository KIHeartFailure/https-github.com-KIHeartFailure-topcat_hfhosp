fixmis <- function(var){
  var <- ifelse(var < 0, NA, var)
}

tcdata <- tcdata %>%
  mutate(across(c(age_entry, HR, SBP, DBP, CR_mgdl, 
                                      height, weight, 
                                      NA_mmolL, K_mmolL, HB_gdL, QRS_DUR, CR_mgdL), fixmis))