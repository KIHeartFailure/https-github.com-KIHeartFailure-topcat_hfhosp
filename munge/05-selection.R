
# Inclusion/exclusion criteria --------------------------------------------------------

flow <- c("Number in TOPCAT", nrow(tcdata))

tcdata <- tcdata %>%
  mutate(americas = country %in% c(6, 5, 2, 1)) %>%
  filter(americas) # & IN_ALL == 1 & EX_ALL == 1) %>%
flow <- rbind(flow, c("TOPCAT AMERICAS (Canada/USA/Argentina/Brazil)", nrow(tcdata)))

tcdata <- tcdata %>%
  filter(!is.na(CHF_HOSP))
flow <- rbind(flow, c("No missing info on previous HF hospitalization", nrow(tcdata)))

flow <- rbind(flow, c(". Included in spline analysis - Prior HFH", nrow(tmp <- tcdata %>% filter(prevhfhosp == "Yes"))))