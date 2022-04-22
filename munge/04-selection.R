
# Inclusion/exclusion criteria --------------------------------------------------------

flow <- c("Number in TOPCAT", nrow(tcdata))

tcdata <- tcdata %>%
  mutate(americas = country %in% c(6, 5, 2, 1)) %>%
  filter(americas)  #& IN_ALL == 1 & EX_ALL == 1) %>%
flow <- rbind(flow, c("TOPCAT AMERICAS (Canada/USA/Argentina/Brazil)", nrow(tcdata)))

tcdata <- tcdata %>%
  filter(!is.na(CHF_HOSP))
flow <- rbind(flow, c("No missing info on previous HF hospitalization", nrow(tcdata)))

#tcdata <- tcdata %>%
#  filter(IN_ALL == 1 & EX_ALL == 1)
#flow <- rbind(flow, c("Fulfilling inclusion and no exclusion criteria", nrow(tcdata)))

flow <- rbind(flow, c("BELOW INCLUDED FOR SPLINE ANALYSIS", NA))

flow <- rbind(flow, c("Prior HFH", nrow(tmp <- tcdata %>% filter(prevhfhosp == "Yes"))))
flow <- rbind(flow, c("HFH <= 1 year prior", nrow(tmp <- tmp %>% filter(chfdc_dt3num <= 365))))
flow <- rbind(flow, c("Previous HFH dates after randomization (incorrect)", nrow(tmp <- tmp %>% filter(chfdc_dt3num >= 0))))