library(tidyverse)

molwt = 325.78

dose = c("0.04", "0.18", "0.18", "0.18", "0.18")
ages = c("Neonates", "Infant", "Child", "School_Age", "Adolescent")

expname = c("fbw_HF20_results", 
            # "fbw_HF20_results", 
            "fbw_HF20_results", 
            "fbw_M60_results", 
            "fbw_M100_results", 
            "fbw_ST150_results")

nages <- length(ages)

crrt_df = nocrrt_df = grf_df = NULL
for (i in 1:nages) {
  print(paste("Reading", ages[i], "dose", dose[i]))
  print("---- CRRT ----")
  tmpfile = paste0("./Midazolam/CRRT/Midazolam_", ages[i], "_", dose[i], "_", expname[i],".csv")
  print(tmpfile)
  tmp = read.csv(tmpfile)
  names(tmp) <- c("id", "time", "conc")
  tmp <- tmp %>%
    filter(time >= 3600 & time <= 4200) %>%
    mutate(conc = conc * molwt,
           age = ages[i])
  crrt_df = rbind(crrt_df, tmp)

}


plot_crrt <- crrt_df %>%
  group_by(age) %>%
  summarize(y0 = min(conc),
            y05 = quantile(conc, 0.05),
            y25 = quantile(conc, 0.25),
            y50 = median(conc),
            y75 = quantile(conc, 0.75),
            y95 = quantile(conc, 0.95),
            y100 = max(conc)) %>%
  mutate(age = factor(age, 
                      levels = c("Neonates", "Infant", "Child", "School_Age", "Adolescent"),
                      labels = c("Neonatal\n(0.04 mcg/hr/kg)", 
                                 "Infant\n(0.18 mcg/hr/kg)", 
                                 "Child\n(0.18 mcg/hr/kg)", 
                                 "School Age\n(0.18 mcg/hr/kg)", 
                                 "Adolescent\n(0.18 mcg/hr/kg)")),
         exp = "CRRT")

write.csv(plot_crrt, "Midazolam/midazolam_stats.csv", row.names = FALSE)
