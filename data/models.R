# clean_leg = leg[rowSums(is.na(leg[c(2,3,4,5)])) != 4, ]
# clean_leg[is.na(clean_leg)] = "-"
# time <- stargazer(clean_leg%>%select(c(1,2,4)), summary = FALSE, title = "Год вступления в силу закона о полной/частичной легализации", header = FALSE)
# 
# 
library(estimatr)
library(lmtest)
#модели метода dd для зависимой all_drug_d_hm, all_opioid_d и регрессора med
diff <- data.frame()
diff <- data.frame(state = tolower(a[[1]]))


states_after_2010 <- tolower(c((leg%>%subset(medical>=2010)%>%select(state))[[1]]))   
m = trunc(mean(c((leg%>%subset(medical>=2010)%>%select(medical))[[1]])))
for(i in 1:51){
  diff$status[i] <- if_else(diff$state[i] %in% selected_states[[1]],1,0)
  if(diff$status[i] == 1){
    b <- na.omit(panel_data_clear%>%subset(med == 0 & state == diff$state[i])%>%select(all_drug_d_hm, all_opioid_d))
    c <- na.omit(panel_data_clear%>%subset(med == 1 & state == diff$state[i])%>%select(all_drug_d_hm, all_opioid_d))
  }else{
    b <- na.omit(panel_data_clear%>%subset(year <= m & state == diff$state[i])%>%select(all_drug_d_hm, all_opioid_d))
    c <- na.omit(panel_data_clear%>%subset(year >= m & state == diff$state[i])%>%select(all_drug_d_hm, all_opioid_d))
  }
  diff$mean_mort_0[i] <- mean(b$all_drug_d_hm)
  diff$mean_mort_1[i] <- mean(c$all_drug_d_hm)
  diff$mean_opioid_mort_0[i] <- mean(b$all_opioid_d)
  diff$mean_opioid_mort_1[i] <- mean(c$all_opioid_d)
}

unselected_states <- data_for_plot %>%
    group_by(state) %>% summarise(val = sum(status)) %>%
    filter(val == 0) %>% select(1)
all <- c(states_after_2010,c(unselected_states[[1]]))

diff <- diff%>%subset(state%in% c(unselected_states[[1]], states_after_2010))
diff <- diff%>%mutate(delta_mort = mean_mort_1 - mean_mort_0, delta_opioid_mort =
                mean_opioid_mort_1 - mean_opioid_mort_0 )

dd1 <- lm(delta_mort ~ status, diff)

dd2 <- lm(delta_opioid_mort ~ status, diff)


#модели метода dd для зависимой all_drug_d_hm, all_opioid_d и регрессора rec
diff1 <- data.frame()
diff1 <- data.frame(state = tolower(a[[1]]))


states_rec <- tolower(c((leg%>%subset(!is.na(recreational))%>%select(state))[[1]]))   
states_control = tolower(c((leg%>%subset((medical<=2010 & is.na(recreational))|state =="Delaware"))[[1]]))
m1 <- mean(c((leg%>%subset(!is.na(recreational))%>%select(recreational))[[1]]))

for(i in 1:51){
  diff1$status[i] <- if_else(diff1$state[i] %in% states_control, 0, 1)
  if(diff1$status[i] == 1){
    b <- na.omit(panel_data_clear%>%subset(rec == 0 & state == diff1$state[i]& year>=2010)%>%select(all_drug_d_hm, all_opioid_d))
    c <- na.omit(panel_data_clear%>%subset(rec == 1 & state == diff1$state[i])%>%select(all_drug_d_hm, all_opioid_d))
  }else{
    b <- na.omit(panel_data_clear%>%subset(year <= m1 & year>=2010 & state == diff1$state[i])%>%select(all_drug_d_hm, all_opioid_d))
    c <- na.omit(panel_data_clear%>%subset(year >= m1 & state == diff1$state[i])%>%select(all_drug_d_hm, all_opioid_d))
  }
  diff1$mean_mort_0[i] <- mean(b$all_drug_d_hm)
  diff1$mean_mort_1[i] <- mean(c$all_drug_d_hm)
  diff1$mean_opioid_mort_0[i] <- mean(b$all_opioid_d)
  diff1$mean_opioid_mort_1[i] <- mean(c$all_opioid_d)
}


diff1 <- diff1%>%subset(state %in% c(states_rec,states_control))%>%
  mutate(delta_mort = mean_mort_1 - mean_mort_0, delta_opioid_mort =
                        mean_opioid_mort_1 - mean_opioid_mort_0 )

dd3 <- lm(delta_mort ~ status, diff1)

dd4 <- lm(delta_opioid_mort ~ status+mean_opioid_mort_0, diff1)




