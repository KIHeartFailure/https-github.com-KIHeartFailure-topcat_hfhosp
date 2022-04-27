
ProjectTemplate::reload.project()

# check assumptions for cox models primary outcome ---------------------------------

mod <- coxph(formula(paste0(
  "Surv(timestart, timestop, out_cvdhfh == 'Yes') ~ prevhfhosp1yr + ",
  paste(modvars, collapse = " + "),
  " + cluster(ID) + strata(eventno)"
)),
data = tcdataimprec
)


testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.1, ])

x11()
plot(testpat[1], resid = F, ylim = c(-4, 4))
plot(testpat[2], resid = F, ylim = c(-4, 4))
plot(testpat[5], resid = F, ylim = c(-4, 4))
plot(testpat[8], resid = F, ylim = c(-4, 4))
plot(testpat[11], resid = F, ylim = c(-4, 4))
plot(testpat[12], resid = F, ylim = c(-4, 4))
plot(testpat[13], resid = F, ylim = c(-4, 4))
plot(testpat[17], resid = F, ylim = c(-4, 4))
plot(testpat[18], resid = F, ylim = c(-4, 4))
plot(testpat[19], resid = F, ylim = c(-4, 4))
plot(testpat[28], resid = F, ylim = c(-4, 4))

# loop as strata

ggcoxzph(testpat, var = "loopdiuretics")


# outliers

x11()
ggcoxdiagnostics(mod, type = "dfbeta", linear.predictions = TRUE)


# linearity

modvarskont <- tcdataimp %>%
  select(!!!syms(modvars)) %>%
  select(where(is.numeric))



modvarstmp <- c("ns(age_entry, 5) + GENDER + EF_cat + MI + STROKE + CABG + PCI + COPD + HTN + revasc + AFIB + DM + ICD + PACEMAKER + anemia + 
nyha_class_cat + ns(bmi, 5) + ns(HR, 5) + ns(map, 5) + ns(gfrckdepi2021, 5) + ntprobnp_bnp_cat + spironolactone + rasi + bbl + cbl + statin + strata(loopdiuretics)")


mod <- coxph(formula(paste0(
  "Surv(timestart, timestop, out_cvdhfh == 'Yes') ~ prevhfhosp1yr + ",
  modvarstmp,
  " + cluster(ID) + strata(eventno)"
)),
data = tcdataimprec
)

x11()
ggcoxfunctional(Surv(timestart, timestop, out_cvdhfh == 'Yes') ~ age_entry, data = tcdataimprec)
termplot(mod, term = 2, rug = T)

x11()
ggcoxfunctional(Surv(timestart, timestop, out_cvdhfh == 'Yes') ~ bmi, data = tcdataimprec) # nej
termplot(mod, term = 18, rug = T)

x11()
ggcoxfunctional(Surv(timestart, timestop, out_cvdhfh == 'Yes') ~ HR, data = tcdataimprec) # OK
termplot(mod, term = 19, rug = T)

x11()
ggcoxfunctional(Surv(timestart, timestop, out_cvdhfh == 'Yes') ~ map, data = tcdataimprec) #Nja
termplot(mod, term = 20, rug = T)

x11()
ggcoxfunctional(Surv(timestart, timestop, out_cvdhfh == 'Yes') ~ gfrckdepi2021, data = tcdataimprec) # OK
termplot(mod, term = 21, rug = T)