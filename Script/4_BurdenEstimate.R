#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Burden estimate
#Part    : (04)  Burden tables
#Purpose : Create burden tables
#Created by Raed Alotaibi
#Date Created: 28-June-2019
#Last Updated: 1-July-2019
#---------------------------------------------#

library(scales)    


## Note : need the burden data frame (run script "1_DataSets.R" first)


# Table of Incident cases by year -----------------------------------------


# Total
Table_incidentCases <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter only one pollutant to prevent repeated count
  group_by(YEAR) %>%  
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) 


# Living location
Table_incidentCases_urban <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, URBAN) %>%  
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, IncidentCases) 


# Median income
Table_incidentCases_income <-
  burden %>%
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, INCOME) %>%
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = -2)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, IncidentCases)


# Total by state
Table_incidentCases_state <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(STATE, YEAR) %>%  
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, IncidentCases)



# Table of AC by year -----------------------


# Total
Table_AttributableCases_urban <- 
  burden %>% 
  group_by(POLLUT, YEAR, URBAN) %>%  
  #filter(POLLUT == 'NO2') %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases)


# Living location
Table_AttributableCases_urban <- 
  burden %>% 
  group_by(POLLUT, YEAR, URBAN) %>%  
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases)


# Median income
Table_AttributableCases_income <-
  burden %>%
  group_by(POLLUT, YEAR, INCOME) %>%
  summarise(AttributableCases = sum(AC, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = -2)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, AttributableCases)


# Total by state
# Median income
Table_AttributableCases_state <-
  burden %>%
  group_by(STATE, POLLUT, YEAR) %>%
  summarise(AttributableCases = sum(AC, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = -2)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, AttributableCases)


Table_incidentCases_state <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(STATE, YEAR) %>%  
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, IncidentCases)




