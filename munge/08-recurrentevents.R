
# koll <- hfhosp %>% group_by(ID, outtime_hfh) %>% slice(1) %>% ungroup()

hfhosprec <- bind_rows(
  hfhosp %>% select(ID, out_hfh, outtime_hfh),
  tcdata %>% select(ID, out_cvd, out_cvdsens, outtime_cvd)
) %>%
  mutate(
    timestop = coalesce(outtime_hfh, outtime_cvd),
    out_hfh = replace_na(out_hfh, 0),
    out_hfh = ynfac(out_hfh),
    out_cvdhfh = coalesce(out_cvd, out_hfh),
    out_cvdsenshfh = coalesce(out_cvdsens, out_hfh)
  ) %>%
  arrange(ID, timestop)

hfhosprec <- hfhosprec %>%
  group_by(ID) %>%
  mutate(
    timestart = lag(timestop)
  ) %>%
  select(ID, outtime_hfh, out_hfh, out_cvdhfh, out_cvdsenshfh, timestart, timestop) %>%
  ungroup() %>%
  mutate(timestart = replace_na(timestart, 0))

hfhosprec <- hfhosprec %>%
  group_by(ID, timestop) %>%
  arrange(desc(out_cvdhfh)) %>%
  slice(1) %>%
  ungroup()

hfhosprec <- hfhosprec %>%
  group_by(ID) %>%
  arrange(timestop) %>%
  mutate(
    eventno = 1:n(),
    lastevent = n()
  ) %>%
  ungroup()

tcdataimprec <- left_join(
  tcdataimp %>% select(ID, prevhfhosp, prevhfhosp_cat, prevhfhosp1yr, chfdc_dt3num, age_entry_cat, CKD, !!!syms(modvars)),
  hfhosprec,
  by = "ID"
)