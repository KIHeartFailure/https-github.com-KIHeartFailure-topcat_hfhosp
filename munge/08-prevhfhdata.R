tcdataimphf <- tcdataimp %>%
  filter(
    prevhfhosp1yr == "Yes" &
    chfdc_dt3num <= 365 &
    chfdc_dt3num >= 0
  )


tcdataimprechf <- tcdataimprec %>%
  filter(
    prevhfhosp1yr == "Yes" &
    chfdc_dt3num <= 365 &
    chfdc_dt3num >= 0
  )
