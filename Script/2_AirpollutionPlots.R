#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Pollutant estimate
#Part    : (02) Plotting air pollution concentrations 
#Purpose : Plot air pollution concnetrations
#Created by Raed Alotaibi
#Date Created: 26-June-2019
#Last Updated: 13-Aug-2019
#---------------------------------------------#


# Preventing scientific notations
options(scipen=10000)



# Assiging theme options ---------------------------------------------------------

theme_text2 <- theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     strip.text = element_text(size = 5, 
                                               margin = margin(0.05,0,0.05,0,"cm")))


breaks_m_y <- seq(0,max(burden$CONC, na.rm = T),10)
scale_y_bod <-   scale_y_continuous(breaks  = breaks_m_y)


breaks_m_x <- seq(0,0.5,0.025)
scale_y_bod <-   scale_y_continuous(breaks  = breaks_m_x,  labels = percent_format(accuracy = 1))


mean_dot <- stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") 




#	NO2 concentration by state  
burden %>%
  select(STATE, YEAR, POLLUT, CONC) %>% 
  filter(POLLUT == 'NO2') %>% 
  ggplot(aes(x = YEAR, y = CONC, fill= YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  mean_dot +
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_fill_manual(values=c("#004e82", "#a5dbff"))+
  ggsave("NO2_state_year.png" ,path = "Output/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)



#	PM2.5 concentration by state  
burden %>%
  select(STATE, YEAR, POLLUT, CONC) %>% 
  filter(POLLUT == 'pm25') %>% 
  ggplot(aes(x = YEAR, y = CONC, fill= YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  mean_dot +
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_fill_manual(values=c("#e5d600", "#f9f6cf"))+
  ggsave("PM2.5_state_year.png" ,path = "Output/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)




#	PM10 concentration by state  
burden %>%
  select(STATE, YEAR, POLLUT, CONC) %>% 
  filter(POLLUT == 'pm10') %>% 
  ggplot(aes(x = YEAR, y = CONC, fill= YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  mean_dot +
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_fill_manual(values=c("#600000", "#ffaaaa"))+
  ggsave("PM10_state_year.png" ,path = "Output/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)



