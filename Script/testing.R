

theme_text2 <- theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     strip.text = element_text(size = 8, 
                                               margin = margin(0.1,0,0.1,0,"cm")))


mean_dot <- stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") 




# #	NO2 concentration by state and living location
# census_2_s %>%
#   ggplot(aes(x = fct_rev(YEAR), y = NO2, fill= YEAR)) +
#   facet_wrap( ~ STATE, nrow =  7 ) +
#   geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
#   mean_dot +
#   theme_bw() +
#   theme_text2 +
#   scale_y_no2 +
#   #coord_flip()+
#   scale_fill_manual(values=c("#a30000", "#ffdddd"))+
#   ggsave("NO2_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


# 
# #NO2
# scale_fill_manual(values=c("#004e82", "#a5dbff"))+
# 
# #PM2.5
# scale_fill_manual(values=c("#e5d600", "#f9f6cf"))+
#   
# #PM10  
# scale_fill_manual(values=c("#600000", "#ffaaaa"))+
# 
#   #All   
# scale_fill_manual(values=c("#004e82", "#a5dbff", 
#                            "#e5d600", "#f9f6cf",
#                            "#600000", "#ffaaaa"))+
#   
  



# Gathering pollutant into one column -------------------------------------


census_3_s <- census_2_s %>% 
  gather("POLLUT" , "CONC", -GISJOIN, -YEAR, -FIPS, -PlaceFIPS, -PLACEA, -STATE, -URBAN, -TOTAL, -CHILDREN)


# # Testing all plot --------------------------------------------------------
# 
# burden_All_join_2 %>%
#   na.omit() %>%
#   ggplot(aes(x = Pollutant, y = conc,  col = year, fill = Pollutant), alpha = 0.9) +
#   geom_boxplot() +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 0,vjust=0.2), legend.position="right") +
#   scale_fill_discrete(name = "Pollutant") +
#   scale_fill_manual(values=c("#0094ff", "#ffd000", "#ff1d00")) +
#   scale_color_manual(values = c("#000000","#8c8c8c"))+
#   labs(y = "ug/m3", x = "") 
# 


#	ALL concentration by state and living location
census_3_s %>%
  na.omit() %>% 
  ggplot(aes(x = POLLUT, y = CONC,  col = YEAR, fill = POLLUT), alpha = 0.5) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = T, outlier.size = 0.1, lwd =0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_no2 +
  scale_fill_discrete(name = "POLLUT") +
  scale_fill_manual(values=c("#0094ff", "#ffd000", "#ff1d00")) +
  scale_color_manual(values = c("#cccccc", "#000000"))+
  labs(y = "ug/m3", x = "") +
  #coord_flip()+
  ggsave("ALL_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


#	ALL concentration by state and living location
census_3_s %>%
  filter(YEAR == 2010) %>% 
  na.omit() %>% 
  ggplot(aes(x = STATE, y = CONC, fill = POLLUT), alpha = 0.5) +
  facet_wrap( ~ POLLUT, nrow =  7 ) +
  geom_boxplot(show.legend = T, outlier.size = 0.1, lwd =0.1) + 
  theme_classic() +
  theme_text2 +
  scale_y_no2 +
  scale_fill_discrete(name = "POLLUT") +
  scale_fill_manual(values=c("#0094ff", "#ffd000", "#ff1d00")) +
  scale_color_manual(values = c("#cccccc", "#000000"))+
  labs(y = "ug/m3", x = "") +
  #coord_flip()+
  ggsave("ALL_state_year.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)



census_2 <- census_2 %>% 
  select(-M_INCOME)
