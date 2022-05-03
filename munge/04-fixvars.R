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
    map = (SBP + 2 * DBP) / 3,

    # eGFR according to CKD-EPI 2021 https://www.nejm.org/doi/full/10.1056/NEJMoa2102953
    tmp_k = if_else(GENDER == "Female", 0.7, 0.9),
    tmp_a = if_else(GENDER == "Female", -0.241, -0.302),
    tmp_add = if_else(GENDER == "Female", 1.012, 1),
    gfrckdepi2021 = 142 * pmin(CR_mgdl / tmp_k, 1)^tmp_a * pmax(CR_mgdl / tmp_k, 1)^-1.200 * 0.9938^age_entry * tmp_add,
    CKD = case_when(
      is.na(gfrckdepi2021) ~ NA_real_,
      gfrckdepi2021 < 60 ~ 1,
      gfrckdepi2021 >= 60 ~ 0
    ),
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
    labels = c("HFpEF", "HFmrEF")
    ),
    EF_cat = factor(case_when(
      EF < 50 ~ 2,
      EF >= 50 ~ 1
    ),
    levels = 1:2,
    labels = c("HFpEF", "HFmrEF")
    ),
    ntprobnp = if_else(BNP_TYPE == 2, BNP_VAL, NA_real_),
    bnp = if_else(BNP_TYPE == 1, BNP_VAL, NA_real_),
    chfdc_dt3num = chfdc_dt3 * -365,
    chfdc_dt3num = if_else(chfdc_dt3num < 0, 0, chfdc_dt3num),
    chfdc_dt3num = if_else(STATUS == 1, 0, chfdc_dt3num),
    prevhfhosp = case_when(
      CHF_HOSP == 1 | STATUS == 1 ~ 1,
      TRUE ~ 0
    ),
    prevhfhosp_cat = factor(case_when(
      prevhfhosp == 0 ~ 0,
      chfdc_dt3num == 0 ~ 6,
      chfdc_dt3num <= 30 ~ 5,
      chfdc_dt3num <= 90 ~ 4,
      chfdc_dt3num <= 180 ~ 3,
      chfdc_dt3num <= 365 ~ 2,
      chfdc_dt3num > 365 ~ 1
    ),
    levels = 0:6,
    labels = c(
      "No prior HFH",
      "HFH >365 d",
      "HFH 181-365 d",
      "HFH 91-180 d",
      "HFH 31-90 d",
      "HFH 1-30 d",
      "In-hospital"
    )
    ),
    prevhfhosp1yr = case_when(
      CHF_HOSP == 1 & chfdc_dt3num <= 365 | STATUS == 1 ~ 1,
      TRUE ~ 0
    ),
    spironolactone = if_else(drug == 2, 0, 1),
    ECG_AFIB = if_else(ECG_AFIB == -2, NA_real_, ECG_AFIB),
    nyha_class_cat = factor(nyha_class_cat, levels = 1:2, labels = c("I-II", "III-IV")),
    countryfac = factor(country, levels = c(6, 5, 2, 1), labels = c("Argentina", "Brazil", "Canada", "US"))
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
    prevhfhosp1yr,
    spironolactone,
    CKD
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
    bnp_cat = case_when(
      bnp < bnpq$`33%` ~ 1,
      bnp < bnpq$`66%` ~ 2,
      bnp >= bnpq$`66%` ~ 3
    ),
    ntprobnp_bnp_cat = coalesce(ntprobnp_cat, bnp_cat),
    ntprobnp_bnp_cat = replace_na(ntprobnp_bnp_cat, 4),
    ntprobnp_bnp_cat = factor(ntprobnp_bnp_cat, labels = c("Tertile 1", "Tertile 2", "Tertile 3", "Missing")),
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
    out_cvdhfh_cr = create_crevent(out_cvdhfh, death, eventvalues = c("Yes", 1)),
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
