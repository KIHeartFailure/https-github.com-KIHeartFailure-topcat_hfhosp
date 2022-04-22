
ProjectTemplate::reload.project(list(munging = FALSE, data_loading = FALSE))


# Import data from TOPCAT -------------------------------------------------

topcatpath <- "C:/Users/Lina/STATISTIK/Projects/TOPCAT/data/TOPCAT_2019a/data/"

formats <- read_sas("C:/Users/Lina/STATISTIK/Projects/TOPCAT/data/TOPCAT_2019a/topcat_formats.sas7bdat")
  
dataout <- read_sas(paste0(topcatpath, "Outcomes/outcomes.sas7bdat"))

t007bymed <- read_sas(paste0(topcatpath, "sas_data/t007_allvisits_bymed.sas7bdat"))
#t007bysub <- read_sas(paste0(topcatpath, "sas_data/t007_allvisits_bysub.sas7bdat"))

echobase <- read_sas(paste0(topcatpath, "sas_data/echo_baseline.sas7bdat"))

t002 <- read_sas(paste0(topcatpath, "sas_data/t002.sas7bdat"))
t003 <- read_sas(paste0(topcatpath, "sas_data/t003.sas7bdat"))
t004 <- read_sas(paste0(topcatpath, "sas_data/t004.sas7bdat"))
t005 <- read_sas(paste0(topcatpath, "sas_data/t005.sas7bdat"))
t006 <- read_sas(paste0(topcatpath, "sas_data/t006.sas7bdat"))
t008 <- read_sas(paste0(topcatpath, "sas_data/t008.sas7bdat"))
t010 <- read_sas(paste0(topcatpath, "sas_data/t010.sas7bdat"))
t011 <- read_sas(paste0(topcatpath, "sas_data/t011.sas7bdat"))
#t027 <- read_sas(paste0(topcatpath, "sas_data/t027.sas7bdat"))
t030 <- read_sas(paste0(topcatpath, "sas_data/t030.sas7bdat"))
t031 <- read_sas(paste0(topcatpath, "sas_data/t031.sas7bdat"))
t070 <- read_sas(paste0(topcatpath, "sas_data/t070.sas7bdat"))
t079 <- read_sas(paste0(topcatpath, "sas_data/t079.sas7bdat"))

# Store as RData in /data folder ------------------------------------------


save(file = "./data/rawData.RData", list = c("formats", "dataout", "echobase", ls()[str_detect(ls(), "t0")]))
