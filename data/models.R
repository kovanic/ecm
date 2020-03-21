source("import.r")
library(plm)
reg1 = plm(opioid_death_rate ~ med + personal_income_per_cap + property_crime_on100k + percent_black + divorce_rate,
           data = panel_data,
           index = c("state", "year"),
           effect = "time",
           model = "random")
summary(reg1)