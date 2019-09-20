#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Burden estimate
#Part    : (04)  Burden tables
#Purpose : Create burden tables
#Created by Raed Alotaibi
#Date Created: 28-June-2019
#Last Updated: 13-Aug-2019
#---------------------------------------------#



## Note : need the burden data frame (run script "1_DataSets.R" first)



# Table of Incident cases by year -----------------------------------------
Table_names <- c("Level","2000", "2010")

# Total
Table_incidentCases_ALL <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter only one pollutant to prevent repeated count
  mutate(ALL = 'Total') %>% 
  group_by(YEAR, ALL) %>% 
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, IncidentCases) 

colnames(Table_incidentCases_ALL) <- Table_names

# Living location
Table_incidentCases_urban <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, URBAN) %>%  
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, IncidentCases) 

colnames(Table_incidentCases_urban) <- Table_names


# Median income
Table_incidentCases_income <-
  burden %>%
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, INCOME) %>%
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, IncidentCases)

colnames(Table_incidentCases_income) <- Table_names



# Joining the tables
(Table_incidentCases <- rbind(
  Table_incidentCases_ALL, 
  Table_incidentCases_urban, 
  Table_incidentCases_income)) 

write_csv(Table_incidentCases, path = "Output/Tables/IncidentCases.csv")



# Table of incident cases by year and state ----------------------------------------

Table_incidentCases_state <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(STATE, YEAR) %>%  
  summarise(IncidentCases = sum(CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, IncidentCases)



colnames(Table_incidentCases_state) <- Table_names

write_csv(Table_incidentCases_state, path = "Output/Tables/IncidentCases_State.csv")



# Table of AC by year -----------------------

Table_names <- c("Level","NO2_2000", "NO2_2010", "PM10_2000", "PM10_2010", "PM25_2000", "PM25_2010")

# Total
Table_AttributableCases_ALL <- 
  burden %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(Table_AttributableCases_ALL) <- Table_names


# Living location
Table_AttributableCases_urban <- 
  burden %>% 
  group_by(POLLUT, YEAR, URBAN) %>%  
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(Table_AttributableCases_urban) <- Table_names



# Median income
Table_AttributableCases_income <-
  burden %>%
  group_by(POLLUT, YEAR, INCOME) %>%
  summarise(AttributableCases = sum(AC, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(Table_AttributableCases_income) <- Table_names



# Joining AC tables
(Table_AttributableCases <- rbind(
  Table_AttributableCases_ALL, 
  Table_AttributableCases_urban, 
  Table_AttributableCases_income)) 


write_csv(Table_AttributableCases, path = "Output/Tables/AC.csv")


# Table of AC by year and state ----------------------------------------------------

Table_AttributableCases_state <-
  burden %>%
  group_by(STATE, POLLUT, YEAR) %>%
  summarise(AttributableCases = sum(AC, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))


colnames(Table_AttributableCases_state) <- Table_names


write_csv(Table_AttributableCases_state, path = "Output/Tables/AC_State.csv")



# Table of AF by year ------------------------------------------

Table_incidentCases
Table_AttributableCases

Table_names_AF <- c("Level", 
                    "IC_2000", "IC_2010",
                    "AC_NO2_2000", "AC_NO2_2010",
                    "AC_PM10_2000", "AC_PM10_2010",
                    "AC_PM25_2000", "AC_PM25_2010")

round_AF <- c("AF_NO2_2000", "AF_NO2_2010", "AF_PM10_2000", "AF_PM10_2010", "AF_PM25_2000", "AF_PM25_2010")  
  
Table_incidentCases_join <- Table_incidentCases %>% 
  left_join(Table_AttributableCases, by = "Level")

colnames(Table_incidentCases_join) <- Table_names_AF


Table_AF <- Table_incidentCases_join %>% 
  mutate_at(vars(-Level), funs(gsub(",", "", .))) %>% 
  mutate_at(vars(-Level), funs(as.numeric(.))) %>% 
  mutate(AF_NO2_2000 = AC_NO2_2000/IC_2000,
         AF_NO2_2010 = AC_NO2_2010/IC_2010,
         AF_PM10_2000 = AC_PM10_2000/IC_2000,
         AF_PM10_2010 = AC_PM10_2010/IC_2010,
         AF_PM25_2000 = AC_PM25_2000/IC_2000,
         AF_PM25_2010 = AC_PM25_2010/IC_2010) %>% 
  mutate_at(round_AF, percent, accuracy = 1) %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) 
  
  

write_csv(Table_AF, path = "Output/Tables/AF.csv")


# Table of AF by year and state ------------------------------------------

Table_incidentCases_state
Table_AttributableCases_state

Table_incidentCases_state_join <- Table_incidentCases_state %>% 
  left_join(Table_AttributableCases_state, by = "Level") %>% 
  as_tibble()

colnames(Table_incidentCases_state_join) <- Table_names_AF



Table_AF_state <- Table_incidentCases_state_join %>% 
  mutate_at(vars(-Level), funs(gsub(",", "", .))) %>% 
  mutate_at(vars(-Level), funs(as.numeric(.))) %>% 
  mutate(AF_NO2_2000 = AC_NO2_2000/IC_2000,
         AF_NO2_2010 = AC_NO2_2010/IC_2010,
         AF_PM10_2000 = AC_PM10_2000/IC_2000,
         AF_PM10_2010 = AC_PM10_2010/IC_2010,
         AF_PM25_2000 = AC_PM25_2000/IC_2000,
         AF_PM25_2010 = AC_PM25_2010/IC_2010) %>% 
  mutate_at(round_AF, percent, accuracy = 1)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) 



write_csv(Table_AF_state, path = "Output/Tables/AF_state.csv")



