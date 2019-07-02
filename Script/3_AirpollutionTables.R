#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Pollutant estimate
#Part    : (03)  Air pollution concentration tables
#Purpose : Create air pollution concentration tables
#Created by Raed Alotaibi
#Date Created: 28-June-2019
#Last Updated: 28-June-2019
#---------------------------------------------#



## Note : need the burden data frame (run scipt 1 first)


# Table for pollutant concentration summary by year -----------------------

Table_pollut <- burden %>% 
  group_by(POLLUT, YEAR) %>% 
  select(CONC) %>%
  #na.omit() %>% 
  summarise( Mean = mean(CONC, na.rm = T),
             Min = min(CONC, na.rm = T), 
             first = quantile(CONC, .25, na.rm = T),
             Median = median(CONC, na.rm = T),
             third = quantile(CONC, .75, na.rm = T),
             Max = max(CONC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  t() %>% 
  as.data.frame()




# Table for pollutant concentration mean by year and state ----------------

Table_pollut_state <-  burden %>% 
  group_by(POLLUT, YEAR, STATE) %>% 
  select(CONC) %>%
  #na.omit() %>% 
  summarise( MEAN = mean(CONC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  spread(STATE, MEAN) %>% 
  as.data.frame()


table_names <- c('NO2 2000', 'NO2 2010', 'PM10 2000', 'PM10 2010', 'PM2.5 2000', 'PM2.5 2010')

Table_pollut_state_t <- t(Table_pollut_state[,c(-1,-2)]) 

colnames(Table_pollut_state_t) <- table_names






  