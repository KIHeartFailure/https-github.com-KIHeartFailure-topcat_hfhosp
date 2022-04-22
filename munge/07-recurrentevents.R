
# koll <- hfhosp %>% group_by(ID, outtime_hfh) %>% slice(1) %>% ungroup()

hfhosprec <- bind_rows(
  hfhosp %>% select(ID, out_hfh, outtime_hfh),
  tcdata %>% select(ID, out_cvd, out_cvdsens, outtime_cvd)
) %>%
  mutate(
    outtime = coalesce(outtime_hfh, outtime_cvd),
    out_hfh = replace_na(out_hfh, 0),
    out_hfh = ynfac(out_hfh),
    out_cvdhfh = coalesce(out_cvd, out_hfh), 
    out_cvdsenshfh = coalesce(out_cvdsens, out_hfh)
  ) %>%
  arrange(ID, outtime)

hfhosprec <- hfhosprec %>%
  group_by(ID) %>%
  mutate(
    timestop = outtime,
    timestart = lag(timestop)
  ) %>%
  select(ID, outtime, outtime_hfh, out_hfh, out_cvdhfh, out_cvdsenshfh, timestart, timestop) %>%
  ungroup() %>%
  mutate(timestart = replace_na(timestart, 0))

hfhosprec <- hfhosprec %>%
  group_by(ID, timestop) %>%
  arrange(desc(out_cvdhfh)) %>%
  slice(1) %>%
  ungroup()
  
hfhosprec <- hfhosprec %>%
  group_by(ID) %>%
  arrange(outtime) %>%
  mutate(eventno = 1:n(), 
         lastevent = n()) %>%
  ungroup()
  
tcdataimprec <- left_join(
  tcdataimp %>% select(ID, prevhfhosp, prevhfhosp_cat, prevhfhosp1yr, chfdc_dt3num, !!!syms(modvars)), 
  hfhosprec, 
  by = "ID")

# t027u <- t027 %>%
#  filter(HOSP_CV == 4) %>%
#  group_by(ID, hospital_dt3) %>%
# arrange(desc(discharge_dt3)) %>%
#  slice(1) %>%
#  ungroup()

# koll <- left_join(hfhosp %>% select(ID, site_dt3),
#                  t027u %>% select(ID, HOSP_YN, hospital_dt3, discharge_dt3),
#                  by = c("ID", "site_dt3" = "hospital_dt3"))


#
# koll <- tcdata %>% filter(out_cvd != cvd_death) %>%
#   select(term_dt_1, dod1, site_dt3, cec_dt3, DEATH_CAUSE, CV_DEATH, NONCV_DEATH, out_cvd, outtime_cvd, death, cvd_death, time_death, hfhosp, time_hfhosp, primary_ep, time_primary_ep)
#
# koll <- tcdata %>% filter(outtime_cvd != time_death) %>%
#   select(term_dt_1, dod1, site_dt3, cec_dt3, DEATH_CAUSE, CV_DEATH, NONCV_DEATH, out_cvd, outtime_cvd, death, cvd_death, time_death, hfhosp, time_hfhosp, primary_ep, time_primary_ep)
# koll <- tcdata %>% filter(outtime_hfh != time_hfhosp) %>%
#   select(ID, term_dt_1, out_cvd, outtime_cvd, time_death, hfhosp, time_hfhosp, primary_ep, time_primary_ep, out_hfh, outtime_hfh)

# koll2 <- t070 %>% filter(ID == 193981)
#
# tcdata %>% count(hfhosp)
#

#
#
# t030 <- t030 %>% select(ID, term_dt_1)
#
# t031 <- t031 %>% select(ID, dod1)
#
# t079 <- t079 %>% select(ID, site_dt3, cec_dt3, DEATH_CAUSE, CV_DEATH, NONCV_DEATH)
#
