p1 <- panel_data_clear %>% gather(crime, rate, violent_crime_on100k:property_crime_on100k) %>%
  group_by(year, crime) %>% summarise(avg_rate = mean(rate, na.rm = TRUE)) %>% 
  ggplot(aes(x=year, y=avg_rate, col=crime)) + geom_line() +
  ggtitle("Нисходящая динамика уровня преступности") +
  labs(x = "Годы", y = "Среднее количество преступлений на 100.000 человек") +
  scale_color_manual(name = "Виды преступлений", 
                     labels = c("Мелкий разбой", "С применением насилия"), 
                     values = c("purple", "blue")) + 
  theme(axis.text.x = element_text(color = "grey20", size = 10), 
        axis.text.y = element_text(color = "grey20", size = 10), 
        text = element_text(size = 10))

p2 <- panel_data_clear %>% 
  ggplot(aes(x=factor(med), y=alldrugs_death_rate)) + geom_boxplot() +
  ggtitle("Различный разброс данных по передозировкам") +
  labs(x = "Статус медицинской легализации", y = "Количество передозировок на 100.000 человек")

p3 <- panel_data_clear %>% 
  ggplot(aes(x=factor(full), y=alldrugs_death_rate)) + geom_boxplot() +
  ggtitle("Различный разброс данных по передозировкам") +
  labs(x = "Статус рекреационной (полной) легализации", y = "Количество передозировок на 100.000 человек")

p4 <- ggcorrplot(cor((panel_data_clear %>%
                  select(-c("state", "population", "year", "total_crime_on100k", "full",
                            "percent_male")) %>%
                  drop_na())), 
           lab = TRUE, lab_size = 5, digits = 1) + 
  theme(axis.text.x = element_text(color = "grey20", size = 10), 
        axis.text.y = element_text(color = "grey20", size = 10), 
        text = element_text(size = 10))

# prepare mini-data to visualise
legal <- legal %>%
  mutate(med = replace_na(med, 0),
         full = replace_na(full, 0),
         status = med + full,
         state = tolower(state))

data_for_plot <- inner_join(legal, overdose, by = c('year', 'state'))

# make selection of states
selected_states <- data_for_plot %>%
  group_by(state) %>% summarise(val = sum(status)) %>% 
  filter(val>0) %>% select(1)

# plot5
p5 <- ggplot(subset(data_for_plot, state %in% c(selected_states[[1]])), 
                aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
  geom_line(aes(group = state), size = 2) + facet_wrap(. ~ state) +
  ggtitle("Граф.1 Количество смертей от передозировок опиоидами (на 100 000 населения штата)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom", legend.direction = "horizontal")+
        scale_color_manual(name="Cтепень легализации",
                           labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                           values=c("green", "purple", "orange"))+
        scale_x_discrete(breaks = c(seq(1999,2018,3)))+
  labs(x = "Годы", y = "Количество передозировок на 100.000 человек")



# plot6
p6 <- data_for_plot %>% filter(year > 1998) %>%
  ggplot(aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.2 Динамика количества передозировок опиоидами") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ",
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))

# plot7
p7 <- data_for_plot %>% filter(year > 1998) %>%
  ggplot(aes(x = factor(year), y = alldrugs_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.3 Динамика количества передозировок всеми типами наркотиков") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ", 
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))


# # summary statistics
# #install.packages("psych")
# # make summary
# data <- panel_data_clear %>% select(-c(1, 3:4)) %>% group_by(year)
# summary_statistics_by_year <- data.frame(describeBy(data, group="year", mat=TRUE, type=0, digits=2)) %>% select(2, 4:7, 10:11)
# # delete unnecessary summary for year
# summary_statistics_by_year <- summary_statistics_by_year[-c(1:21),] %>% drop_na()
# # edit name of the column
# names(summary)[1] <- "year"
# # add column of variables and change order of the columns
# summary_statistics_by_year$variable <- row.names(summary_statistics_by_year)
# summary_statistics_by_year <- summary_statistics_by_year[c(8, 1:7)]
# 
# # s <- summary_statistics_by_year %>% filter(year %in% c(2000, 2005, 2010, 2015))


Northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
               "Rhode Island", "Vermont","New Jersey", "New York","Pennsylvania")

Midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin","Iowa",
             "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota",
             "South Dakota")
South <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina",
           "South Carolina", "Virginia", "District of Columbia","West Virginia","
           Alabama", "Kentucky", "Mississippi","Tennessee", "Arkansas", "Louisiana"
           ,"Oklahoma","Texas")
West <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico"
          , "Utah", "Wyoming","Alaska", "California", "Hawaii", "Oregon","Washington")




# Northeast
p8 <- data_for_plot %>% filter(year > 1998, state %in% tolower(Northeast)) %>%
  ggplot(aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.2 Динамика количества передозировок опиоидами") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ",
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))

p9 <- data_for_plot %>% filter(year > 1998,state %in% tolower(Northeast)) %>%
  ggplot(aes(x = factor(year), y = alldrugs_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.3 Динамика количества передозировок всеми типами наркотиков") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ", 
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))

#Midwest
p10 <- data_for_plot %>% filter(year > 1998, state %in% tolower(Midwest)) %>%
  ggplot(aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.2 Динамика количества передозировок опиоидами") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ",
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))

p11 <- data_for_plot %>% filter(year > 1998,state %in% tolower(Midwest)) %>%
  ggplot(aes(x = factor(year), y = alldrugs_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.3 Динамика количества передозировок всеми типами наркотиков") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ", 
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))

#South
p12 <- data_for_plot %>% filter(year > 1998, state %in% tolower(South)) %>%
  ggplot(aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.2 Динамика количества передозировок опиоидами") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ",
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))


p13 <- data_for_plot %>% filter(year > 1998,state %in% tolower(South)) %>%
  ggplot(aes(x = factor(year), y = alldrugs_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.3 Динамика количества передозировок всеми типами наркотиков") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ", 
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))


#West
p14 <- data_for_plot %>% filter(year > 1998, state %in% tolower(West)) %>%
  ggplot(aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.2 Динамика количества передозировок опиоидами") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ",
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))

# plot7
p15 <- data_for_plot %>% filter(year > 1998,state %in% tolower(West)) %>%
  ggplot(aes(x = factor(year), y = alldrugs_death_rate, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.3 Динамика количества передозировок всеми типами наркотиков") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name="Cтепень легализации: ", 
                     labels = c("Полный запрет", "Для медицинских целей","Для рекреацонных целей"),
                     values=c("green", "purple", "orange"))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 70, hjust = 1))



