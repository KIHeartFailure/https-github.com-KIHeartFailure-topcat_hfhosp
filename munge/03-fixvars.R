tcdata <- tcdata %>%
  mutate(
    race = factor(case_when(
      RACE_WHITE == 1 ~ 1,
      RACE_BLACK == 1 ~ 2,
      RACE_ASIAN == 1 ~ 3,
      RACE_OTHER == 1 ~ 4
    ),
    levels = 1:4,
    labels = c("White", "Black", "Asian", "Other")
    ),

    GENDER = factor(GENDER, levels = 1:2, labels = c("Male", "Female")),

    revasc = case_when(
      is.na(CABG) | is.na(PCI) ~ NA_real_,
      CABG == 1 | PCI == 1 ~ 1,
      TRUE ~ 0
    ),

    bmi = as.numeric(weight) / (as.numeric(height) / 100)^2,

    CR_mgdl = coalesce(CR_mgdl, CR_mgdL),

    # eGFR according to CKD-EPI 2021 https://www.nejm.org/doi/full/10.1056/NEJMoa2102953
    tmp_k = if_else(GENDER == "Female", 0.7, 0.9),
    tmp_a = if_else(GENDER == "Female", -0.241, -0.302),
    tmp_add = if_else(GENDER == "Female", 1.012, 1),
    gfrckdepi2021 = 142 * pmin(CR_mgdl / tmp_k, 1)^tmp_a * pmax(CR_mgdl / tmp_k, 1)^-1.200 * 0.9938^age_entry * tmp_add,

    anemia = ynfac(case_when(
      is.na(HB_gdL) ~ NA_real_,
      GENDER == "Female" & HB_gdL < 12 | GENDER == "Male" & HB_gdL < 13 ~ 1,
      TRUE ~ 0
    )),

    ef_tot_cat = factor(case_when(
      ef_tot < 50 ~ 2,
      ef_tot >= 50 ~ 1
    ),
    levels = 1:2,
    labels = c(">=50", "40-49")
    ),

    EF_cat = factor(case_when(
      EF < 50 ~ 2,
      EF >= 50 ~ 1
    ),
    levels = 1:2,
    labels = c(">=50", "40-49")
    ),

    ntprobnp = if_else(BNP_TYPE == 2, BNP_VAL, NA_real_),
    bnp = if_else(BNP_TYPE == 1, BNP_VAL, NA_real_),

    chfdc_dt3num = chfdc_dt3 * - 365,
    chfdc_dt3num = if_else(STATUS == 1, 0, chfdc_dt3num),

    prevhfhosp = case_when(
      CHF_HOSP == 1 | STATUS == 1 ~ 1,
      TRUE ~ 0
    ),

    prevhfhosp_cat = factor(case_when(
      prevhfhosp == 0 ~ 0,
      chfdc_dt3num <= 30 ~ 1,
      chfdc_dt3num <= 90 ~ 2,
      chfdc_dt3num <= 30 * 6 ~ 3,
      chfdc_dt3num <= 365 ~ 4,
      chfdc_dt3num > 365 ~ 5
    ),
    levels = 0:5,
    labels = c(
      "No prior HFH",
      "HFH < 30d",
      "HFH 30-90d",
      "HFH 90d-6mo",
      "HFH 6mo-1yr",
      "HFH > 1yr"
    )
    ),

    prevhfhosp1yr = case_when(
      CHF_HOSP == 1 & chfdc_dt3num <= 365 | STATUS == 1 ~ 1,
      TRUE ~ 0
    ),

    ECG_AFIB = if_else(ECG_AFIB == -2, NA_real_, ECG_AFIB),
    nyha_class_cat = factor(nyha_class_cat, levels = 1:2, labels = c("I-II", "III-IV"))
  ) %>%
  mutate(across(c(
    MI,
    STROKE,
    CABG,
    PCI,
    COPD,
    HTN,
    AFIB,
    DM,
    ICD,
    PACEMAKER,
    ECG_AFIB,
    revasc,
    prevhfhosp,
    prevhfhosp1yr
  ), ynfac))


ntq <- tcdata %>%
  summarise(ntq = list(enframe(quantile(ntprobnp,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(ntq)) %>%
  spread(name, value)

bnpq <- tcdata %>%
  summarise(bnpq = list(enframe(quantile(bnp,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(bnpq)) %>%
  spread(name, value)

tcdata <- tcdata %>%
  mutate(
    ntprobnp_cat = case_when(
      ntprobnp < ntq$`33%` ~ 1,
      ntprobnp < ntq$`66%` ~ 2,
      ntprobnp >= ntq$`66%` ~ 3
    ),
    ntprobnp_cat = factor(ntprobnp_cat, labels = c("Tertile 1", "Tertile 2", "Tertile 3")),
    bnp_cat = case_when(
      bnp < bnpq$`33%` ~ 1,
      bnp < bnpq$`66%` ~ 2,
      bnp >= bnpq$`66%` ~ 3
    ),
    bnp_cat = factor(bnp_cat, labels = c("Tertile 1", "Tertile 2", "Tertile 3")),
    ntprobnp_bnp_cat = coalesce(ntprobnp_cat, bnp_cat)
  )


# outcomes

tcdata <- tcdata %>%
  mutate(
    outtime_cvd = coalesce(cec_dt3, site_dt3),
    outtime_cvd = coalesce(outtime_cvd, term_dt_1),
    outtime_cvd = ifelse(outtime_cvd == 0, 1, outtime_cvd),
    out_cvd = ynfac(case_when(
      DEATH_CAUSE %in% c(-8, 1) ~ 1,
      TRUE ~ 0
    )),
    out_cvdsens = ynfac(case_when(
      DEATH_CAUSE %in% c(1) ~ 1,
      TRUE ~ 0
    )),
  )

hfhosp <- t070 %>%
  filter(CRITERIA == 1) %>%
  mutate(
    outtime_hfh = coalesce(site_dt3, cec_dt3),
    out_hfh = 1
  )

tcdata <- left_join(tcdata,
  hfhosp %>%
    group_by(ID) %>%
    arrange(outcome) %>%
    slice(1) %>%
    ungroup() %>%
    select(ID, out_hfh, outtime_hfh),
  by = "ID"
) %>%
  mutate(
    out_hfh = ynfac(replace_na(out_hfh, 0)),
    outtime_hfh = coalesce(outtime_hfh, term_dt_1),
    out_cvdhfh = if_else(out_cvd == "Yes" | out_hfh == "Yes", "Yes", "No"),
    out_cvdsenshfh = if_else(out_cvdsens == "Yes" | out_hfh == "Yes", "Yes", "No"),

    out_hfh_cr = create_crevent(out_hfh, death, eventvalues = c("Yes", 1)),
    out_cvd_cr = create_crevent(out_cvd, death, eventvalues = c("Yes", 1)),
    out_cvdsens_cr = create_crevent(out_cvdsens, death, eventvalues = c("Yes", 1)),
    out_cvdsenshfh_cr = create_crevent(out_cvdsenshfh, death, eventvalues = c("Yes", 1))
  )


hfhospno <- hfhosp %>%
  group_by(ID) %>%
  count() %>%
  ungroup() %>%
  rename(out_hfhno = n)

tcdata <- left_join(tcdata,
  hfhospno,
  by = "ID"
) %>%
  mutate(
    out_hfhno = replace_na(out_hfhno, 0),
    out_cvdhfhno = ifelse(out_cvd == "Yes", out_hfhno + 1, out_hfhno),
    out_cvdsenshfhno = ifelse(out_cvdsens == "Yes", out_hfhno + 1, out_hfhno)
  )


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
