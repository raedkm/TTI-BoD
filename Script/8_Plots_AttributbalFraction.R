#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Pollutant estimate
#Part    : (08) Plotting burden estimates 
#Purpose : Plot Attributable fractions
#Created by Raed Alotaibi
#Date Created: 19-July-2019
#Last Updated: 13-Aug-2019
#---------------------------------------------#


# Preventing scientific notations
options(scipen=10000)


# Sampling ----------------------------------------------------------------
# set.seed(1)
# index <- sample(1:nrow(burden), 10000)
# burden_s <- burden[index, ]  %>%
#   mutate(INCOME = recode(burden[index, ]$INCOME,
#                          "<20,000" = "<$20,000" ,
#                          "20,000 to <35,000" = "$20,000 to <$35,000",
#                          "35,000 to <50,000" = "$35,000 to <$50,000",
#                          "50,000 to <75,000" = "$50,000 to <$75,000" ,
#                          ">=75,000" = ">=$75,000"),
#           POLLUT = recode(burden[index, ]$POLLUT,
#                 "pm10" = "PM10",
#                 "pm25" = "PM2.5"))
# # levels(burden_s$INCOME)


burden_s <- burden  %>%
  mutate(INCOME = recode(burden$INCOME,
                         "<20,000" = "<$20,000" ,
                         "20,000 to <35,000" = "$20,000 to <$35,000",
                         "35,000 to <50,000" = "$35,000 to <$50,000",
                         "50,000 to <75,000" = "$50,000 to <$75,000" ,
                         ">=75,000" = ">=$75,000"),
          POLLUT = recode(burden$POLLUT,
                          "pm10" = "PM10",
                          "pm25" = "PM2.5"))

# Assiging theme options ---------------------------------------------------------

theme_text <-  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=)

theme_text2 <- theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     strip.text = element_text(size = 9, 
                                               margin = margin(0.07,0,0.07,0,"cm")))




breaks_m <- seq(0,max(burden$PAF, na.rm = T),0.1)
scale_y_bod <-   scale_y_continuous(breaks  = breaks_m,  labels = percent_format(accuracy = 1))


mean_dot <- stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") 




# AF ----------------------------------------------------------------------

# #	POLLUT AF 
# burden_s %>%
#   select(YEAR, POLLUT, PAF) %>% 
#   #filter(POLLUT == 'NO2') %>% 
#   ggplot(aes(x = YEAR, y = PAF,  col = YEAR)) +
#   facet_wrap( ~ POLLUT, nrow =  3 ) +
#   geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
#   theme_bw() +
#   theme_text2 +
#   scale_y_bod +
#   scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
#   scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
#   ggsave("All_AF_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)

#	POLLUT AF by state 
burden_s %>%
  select(STATE, YEAR, POLLUT, PAF) %>% 
  #filter(POLLUT == 'NO2') %>% 
  ggplot(aes(x = POLLUT, y = PAF,  col = YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("ALL_AF_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


# AF by Living location -------------------------------------------------------------

#	POLLUT AF by living location
burden_s %>%
  select(YEAR, URBAN, POLLUT, PAF) %>% 
  #filter(POLLUT == 'NO2') %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = YEAR)) +
  facet_wrap( ~ POLLUT, nrow =  3 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("All_AF_Urban_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	NO2 AF by living location
burden_s %>%
  select(YEAR, URBAN, POLLUT, PAF) %>% 
  filter(POLLUT == 'NO2') %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = YEAR)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("NO2_AF_Urban_year.png" ,path = "Results/Plots", width = 9.5, height = 6,  dpi = 360, pointsize=3)


#	PM10 AF by living location
burden_s %>%
  select(YEAR, URBAN, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm10') %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = YEAR)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM10_AF_Urban_year.png" ,path = "Results/Plots", width = 9.5, height = 6,  dpi = 360, pointsize=3)


#	PM2.5 AF by living location
burden_s %>%
  select(YEAR, URBAN, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm25') %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = YEAR)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM25_AF_Urban_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


# AF by Living location and state --------------------------------------------------


#	NO2 AF by state and living location
burden_s %>%
  select(STATE, YEAR, URBAN, POLLUT, PAF) %>% 
  filter(POLLUT == 'NO2') %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("NO2_AF_Urban_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	PM10 AF by state and living location
burden_s %>%
  select(STATE, YEAR, URBAN, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm10') %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM10_AF_Urban_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	PM2.5 AF by state and living location
burden_s %>%
  select(STATE, YEAR, URBAN, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm25') %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM25_AF_Urban_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)



# AF by Income  --------------------------------------------------


#	POLLUT AF by income 
burden_s %>%
  select(YEAR, INCOME, POLLUT, PAF) %>% 
  #filter(POLLUT == 'NO2') %>% 
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = YEAR)) +
  facet_wrap( ~ POLLUT, nrow =  3 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("All_AF_Income_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	NO2 AF by income
burden_s %>%
  select(YEAR, INCOME, POLLUT, PAF) %>% 
  filter(POLLUT == 'NO2') %>%
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = YEAR)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  #mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("NO2_AF_Income_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	PM10 AF by income
burden_s %>%
  select(YEAR, INCOME, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm10') %>% 
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = YEAR)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  #mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM10_AF_Income_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	PM2.5 AF by income
burden_s %>%
  select(YEAR, INCOME, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm25') %>% 
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = YEAR)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  #mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM25_AF_Income_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


# AF by Income and state --------------------------------------------------


#	NO2 AF by state and income
burden_s %>%
  select(STATE, YEAR, INCOME, POLLUT, PAF) %>% 
  filter(POLLUT == 'NO2') %>%
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  #mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("NO2_AF_Income_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	PM10 AF by state and income
burden_s %>%
  select(STATE, YEAR, INCOME, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm10') %>% 
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  #mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM10_AF_Income_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	PM2.5 AF by state and income
burden_s %>%
  select(STATE, YEAR, INCOME, POLLUT, PAF) %>% 
  filter(POLLUT == 'pm25') %>% 
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  #mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("PM25_AF_Income_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


