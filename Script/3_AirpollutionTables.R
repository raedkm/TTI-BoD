#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Pollutant estimate
#Part    : (03)  Air pollution concentration tables
#Purpose : Create air pollution concentration tables
#Created by Raed Alotaibi
#Date Created: 28-June-2019
#Last Updated: 13-Aug-2019
#---------------------------------------------#


## Note : need the burden data frame (run script "1_DataSets.R" first)


# Table for pollutant concentration summary by year -----------------------

Table_pollut_summary <- burden %>% 
  group_by(POLLUT, YEAR) %>% 
  summarise( Mean = mean(CONC, na.rm = T),
             Min = min(CONC, na.rm = T), 
             first = quantile(CONC, .25, na.rm = T),
             Median = median(CONC, na.rm = T),
             third = quantile(CONC, .75, na.rm = T),
             Max = max(CONC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  as.data.frame() %>% 
  select(-POLLUT, -YEAR) %>% 
  t() %>%  
  as.data.frame()

colnames(Table_pollut_summary) <- c("NO2_2000", "NO2_2010", "PM10_2000", "PM10_2010", "PM25_2000", "PM25_2010") 


write_csv(Table_pollut_summary, path = "Output/Tables/Pollutant_Summary.csv", col_names = T, )



# Table for pollutant concentration mean by year  ----------------

Table_names_pollut <- c("Level","NO2_2000", "NO2_2010", "PM10_2000", "PM10_2010", "PM25_2000", "PM25_2010")

# By year

Table_pollut_All <- burden %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise( MEAN = mean(CONC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  spread(YEAR, MEAN) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(Table_pollut_All) <- Table_names_pollut


# By living location

Table_pollut_urban <-  burden %>% 
  group_by(POLLUT, YEAR, URBAN) %>% 
  summarise( MEAN = mean(CONC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  spread(YEAR, MEAN) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(Table_pollut_urban) <- Table_names_pollut


# By income

Table_pollut_income <-  burden %>% 
  group_by(POLLUT, YEAR, INCOME) %>% 
  summarise( MEAN = mean(CONC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  spread(YEAR, MEAN) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(Table_pollut_income) <- Table_names_pollut


# Joining all tables

(Table_pollut <- rbind(Table_pollut_All, Table_pollut_urban,  Table_pollut_income)) 


write_csv(Table_pollut, path = "Output/Tables/Pollutant.csv")




# Table for pollutant concentration mean by year and state ----------------

Table_pollut_state <-  burden %>% 
  group_by(POLLUT, YEAR, STATE) %>% 
  summarise( MEAN = mean(CONC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  spread(YEAR, MEAN) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))


write_csv(Table_pollut_state, path = "Output/Tables/Pollutant_State.csv")



  