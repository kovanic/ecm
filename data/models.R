library(plm)
clse = function(reg) {
  # index(reg, "id") returns the id or entity variable vector
  G = length(unique(index(reg, "id")))
  N = length(index(reg, "id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method = "arellano", type = "HC1", cluster = "group")))
  return(rob)
}


#FE, FE+time, RE, pool для "all_drug_d_hm"  

 #  "med"                   "rec"                  
 #  "pdmp"                  "total_crime_on100k"   "gdp_per_cap"           "income_per_cap"        "alco_consumption"    
 #  "unemployment"          "age_0_14"             "age_15_24"             "age_25_44"             "age_45_59"            
 #  "age_60_more"           "northeast"            "midwest"               "south"                 "west"                  
 #  "poverty"               "percent_male"         "percent_black"         "percent_hisp_origin"        
 #  "all_drug_d_hm"         "all_opioid_d"         "prescription_opioid_d" "synthetic_opioid_d"          
 #  "prescr_rate"          
 #  "med_neighbors"         "rec_neighbors" 
 

#######################################################################################
#Зависимая - смертность от передозов всеми наркотиками
#######################################################################################
 reg1 <- plm(all_drug_d_hm ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, model = "pooling")
 summary(reg1)


 reg2 <- plm(all_drug_d_hm ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
               data = panel_data_clear, index = c("state", "year"),
               model = "within", effect = "individual")
 summary(reg2)

 reg3 <- plm(all_drug_d_hm ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
               data = panel_data_clear, index = c("state", "year"),
               model = "within", effect = "twoways")
 summary(reg3)

 reg4 <- plm(all_drug_d_hm ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
               data = panel_data_clear, index = c("state", "year"),
               model = "random", effect = "individual")
 summary(reg4)


  # stargazer(reg1, reg2, reg3, reg4, reg5,
  #          se = list(clse(reg1), clse(reg2), clse(reg3), clse(reg4)),
  #          title = "Базовая модель", type="text",
  #          column.labels = c("Pooled","FE", "FE+time", "RE"),
  #          df = FALSE, digits = 2, out = "lala1.txt")


##FE, FE+time, RE, pool для "all_drug_d_hm"  +prescr
 reg21 <- plm(all_drug_d_hm ~ med + rec + pdmp + prescr_rate+
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
               data = panel_data_clear, model = "pooling")
 summary(reg21)
 
 
 reg22 <- plm(all_drug_d_hm ~ med + rec + pdmp + prescr_rate+
               total_crime_on100k + income_per_cap + 
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
               data = panel_data_clear, index = c("state", "year"),
               model = "within", effect = "individual")
 summary(reg22)
 
 reg23 <- plm(all_drug_d_hm ~ med + rec + pdmp + prescr_rate+
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
               data = panel_data_clear, index = c("state", "year"),
               model = "within", effect = "twoways")
 summary(reg23)
 
 reg24 <- plm(all_drug_d_hm ~ med + rec + pdmp + prescr_rate+
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
               data = panel_data_clear, index = c("state", "year"),
               model = "random", effect = "individual")
 summary(reg24)
 
 # stargazer(reg21, reg22, reg23, reg24,
 #           se = list(clse(reg21), clse(reg22), clse(reg23), clse(reg24)),
 #           title = "Влияние количества выписанных рецептов", type="text",
 #           column.labels = c("Pooled","FE", "FE+time", "RE"),
 #           df = FALSE, digits = 2, out = "lala2.txt")
 # 
 
 ##FE, FE+time, RE, pool для "all_drug_d_hm"  +prescr
 reg31 <- plm(all_drug_d_hm ~ med + rec + pdmp + med_neighbors + rec_neighbors+ 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, model = "pooling")
 summary(reg31)
 
 
 reg32 <- plm(all_drug_d_hm ~ med + rec + pdmp +  med_neighbors + rec_neighbors+ 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, index = c("state", "year"),
             model = "within", effect = "individual")
 summary(reg32)
 
 reg33 <- plm(all_drug_d_hm ~ med + rec + pdmp +  med_neighbors + rec_neighbors+ 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, index = c("state", "year"),
             model = "within", effect = "twoways")
 summary(reg33)
 
 reg34 <- plm(all_drug_d_hm ~ med + rec + pdmp +  med_neighbors + rec_neighbors+ 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, index = c("state", "year"),
             model = "random", effect = "individual")
 summary(reg34)
 
 # stargazer(reg31, reg32, reg33, reg34,
 #           se = list(clse(reg31), clse(reg32), clse(reg33), clse(reg34)),
 #           title = "Влияние пространственных эффектов", type="text",
 #           column.labels = c("Pooled","FE", "FE+time", "RE"),
 #           df = FALSE, digits = 2, out = "lala3.txt")
 # 
 
#######################################################################################
#Зависимая - смертность от передозов опиатами
#######################################################################################
 reg_1 <- plm(all_opioid_d ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, model = "pooling")
 summary(reg_1)
 
 
 reg_2 <- plm(all_opioid_d ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, index = c("state", "year"),
             model = "within", effect = "individual")
 summary(reg_2)
 
 reg_3 <- plm(all_opioid_d ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, index = c("state", "year"),
             model = "within", effect = "twoways")
 summary(reg_3)
 
 reg_4 <- plm(all_opioid_d ~ med + rec + pdmp + 
               total_crime_on100k + income_per_cap+
               alco_consumption + unemployment +
               age_0_14 + age_15_24 + age_45_59+age_25_44+
               northeast+midwest+south+
               percent_male+percent_black+percent_hisp_origin,
             data = panel_data_clear, index = c("state", "year"),
             model = "random", effect = "individual")
 summary(reg_4)
 
 # stargazer(reg_1, reg_2, reg_3, reg_4,
 #           se = list(clse(reg_1), clse(reg_2), clse(reg_3), clse(reg_4)),
 #           title = "Базовая модель", type="text",
 #           column.labels = c("Pooled","FE", "FE+time", "RE"),
 #           df = FALSE, digits = 2, out = "lala_1.txt")
 # 
 
 ##FE, FE+time, RE, pool для "all_drug_d_hm"  +prescr
 reg_21 <- plm(all_opioid_d ~ med + rec + pdmp + prescr_rate+
                total_crime_on100k + income_per_cap+
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, model = "pooling")
 summary(reg_21)
 
 
 reg_22 <- plm(all_opioid_d ~ med + rec + pdmp + prescr_rate+
                total_crime_on100k + income_per_cap + 
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, index = c("state", "year"),
              model = "within", effect = "individual")
 summary(reg_22)
 
 reg_23 <- plm(all_opioid_d ~ med + rec + pdmp + prescr_rate+
                total_crime_on100k + income_per_cap+
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, index = c("state", "year"),
              model = "within", effect = "twoways")
 summary(reg_23)
 
 reg_24 <- plm(all_opioid_d ~ med + rec + pdmp + prescr_rate+
                total_crime_on100k + income_per_cap+
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, index = c("state", "year"),
              model = "random", effect = "individual")
 summary(reg_24)
 
 # stargazer(reg_21, reg_22, reg_23, reg_24,
 #           se = list(clse(reg_21), clse(reg_22), clse(reg_23), clse(reg_24)),
 #           title = "Влияние количества выписанных рецептов", type="text",
 #           column.labels = c("Pooled","FE", "FE+time", "RE"),
 #           df = FALSE, digits = 2, out = "lala_2.txt")
 # 
 
 ##FE, FE+time, RE, pool для "all_drug_d_hm"  +prescr
 reg_31 <- plm(all_opioid_d ~ med + rec + pdmp + med_neighbors + rec_neighbors+ 
                total_crime_on100k + income_per_cap+
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, model = "pooling")
 summary(reg_31)
 
 
 reg_32 <- plm(all_opioid_d ~ med + rec + pdmp +  med_neighbors + rec_neighbors+ 
                total_crime_on100k + income_per_cap+
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, index = c("state", "year"),
              model = "within", effect = "individual")
 summary(reg_32)
 
 reg_33 <- plm(all_opioid_d ~ med + rec + pdmp +  med_neighbors + rec_neighbors+ 
                total_crime_on100k + income_per_cap+
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, index = c("state", "year"),
              model = "within", effect = "twoways")
 summary(reg_33)
 
 reg_34 <- plm(all_opioid_d ~ med + rec + pdmp +  med_neighbors + rec_neighbors+ 
                total_crime_on100k + income_per_cap+
                alco_consumption + unemployment +
                age_0_14 + age_15_24 + age_45_59+age_25_44+
                northeast+midwest+south+
                percent_male+percent_black+percent_hisp_origin,
              data = panel_data_clear, index = c("state", "year"),
              model = "random", effect = "individual")
 summary(reg_34)
 # 
 # stargazer(reg_31, reg_32, reg_33, reg_34,
 #           se = list(clse(reg_31), clse(reg_32), clse(reg_33), clse(reg_34)),
 #           title = "Влияние пространственных эффектов", type="text",
 #           column.labels = c("Pooled","FE", "FE+time", "RE"),
 #           df = FALSE, digits = 2, out = "lala_3.txt")
 # 
 # 




##############################################################################
# diff-in-diff
##############################################################################

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




