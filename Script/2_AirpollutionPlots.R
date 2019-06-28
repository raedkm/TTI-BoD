#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Pollutant estimate
#Part    : (02) Plotting air pollution concentrations 
#Purpose : Plot air pollution concnetrations
#Created by Raed Alotaibi
#Date Created: June-26-2019
#---------------------------------------------#

library(ggplot2)
library(scales)
library(forcats)


# Preventing scientific notations
options(scipen=10000)


# Sampling ----------------------------------------------------------------
set.seed(1)
index <- sample(1:nrow(census_2), 10000)
census_2_s <- census_2[index, ]  


# Assiging theme options ---------------------------------------------------------

theme_text <-  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=element_blank())

theme_text2 <- theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     strip.text = element_text(size = 5, 
                                               margin = margin(0.05,0,0.05,0,"cm")))


breaks_m <- seq(0,max(census_2$NO2),10)
scale_y_no2 <-   scale_y_continuous(breaks  = breaks_m)

breaks_m <- seq(0,max(census_2$PM2.5, na.rm = T),10)
scale_y_pm2.5 <-   scale_y_continuous(breaks  = breaks_m)

breaks_m <- seq(0,max(census_2$PM10, na.rm = T),10)
scale_y_pm10 <-   scale_y_continuous(breaks  = breaks_m)


breaks_m <- seq(0,0.5,0.025)
scale_y_bod <-   scale_y_continuous(breaks  = breaks_m,  labels = percent_format(accuracy = 1))


breaks_m <- seq(0,0.5,0.05)
scale_y_bod2 <-   scale_y_continuous(breaks  = breaks_m,  labels = percent_format(accuracy = 1))


mean_dot <- stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") 




#	NO2 concentration by state and living location
census_2_s %>%
  ggplot(aes(x = fct_rev(YEAR), y = NO2, fill= YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  mean_dot +
  theme_bw() +
  theme_text2 +
  scale_y_no2 +
  coord_flip()+
  scale_fill_manual(values=c("#004e82", "#a5dbff"))+
  ggsave("NO2_state_year.png" ,path = "Results/Plots", width = 9.5, height = 6,  dpi = 360, pointsize=3)



#	PM2.5 concentration by state and living location
census_2_s %>%
  ggplot(aes(x = fct_rev(YEAR), y = PM2.5, fill= YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  mean_dot +
  theme_bw() +
  theme_text2 +
  scale_y_no2 +
  coord_flip()+
  scale_fill_manual(values=c("#e5d600", "#f9f6cf"))+
  ggsave("PM2.5_state_year.png" ,path = "Results/Plots", width = 9.5, height = 6,  dpi = 360, pointsize=3)




#	PM10 concentration by state and living location
census_2_s %>%
  ggplot(aes(x = fct_rev(YEAR), y = PM10, fill= YEAR)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  mean_dot +
  theme_bw() +
  theme_text2 +
  scale_y_no2 +
  coord_flip()+
  scale_fill_manual(values=c("#600000", "#ffaaaa"))+
  ggsave("PM10_state_year.png" ,path = "Results/Plots", width = 9.5, height = 6,  dpi = 360, pointsize=3)
