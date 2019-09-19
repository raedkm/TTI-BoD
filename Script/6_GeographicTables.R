#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Geographics
#Part    : (06)  Geographic tables
#Purpose : Create geographic tables
#Created by Raed Alotaibi
#Date Created: 3-July-2019
#Last Updated: 13-Aug-2019
#---------------------------------------------#





## Note : need the burden data frame (run script "1_DataSets.R" first)





# Table of N by year -----------------------------------------
Table_names <- c("Level","2000", "2010")

# Total
Table_n_ALL <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter only one pollutant to prevent repeated count
  mutate(ALL = 'Total',
         N = 1) %>% 
  group_by(YEAR, ALL) %>% 
  summarise(n = sum(N, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, n) 

colnames(Table_n_ALL) <- Table_names



# Living location
Table_n_urban <- 
  burden %>% 
  filter(POLLUT == 'NO2',) %>%  #We need to filter onl one pollutant to prevent repeated count
  mutate(N = 1) %>%  
  group_by(YEAR, URBAN) %>%  
  summarise(n = sum(N, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, n) 

colnames(Table_n_urban) <- Table_names


# Median income
Table_n_income <-
  burden %>%
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  mutate(N = 1) %>%  
  group_by(YEAR, INCOME) %>%
  summarise(n = sum(N, na.rm = T)) %>%
  #mutate_if(is.numeric, round, digits = -2)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, n)

colnames(Table_n_income) <- Table_names



# Joining the tables
(Table_n <- rbind(
  Table_n_ALL, 
  Table_n_urban, 
  Table_n_income)) 

write_csv(Table_n, path = "Results//Tables//n.csv")


# Table of N year and state ----------------------------------------

Table_n_state <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  mutate(N = 1) %>%  
  group_by(STATE, YEAR) %>%  
  summarise(n = sum(N, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, n) %>% 
  mutate_at(vars(-STATE), funs(gsub(",", "", .))) %>% 
  mutate_at(vars(-STATE), funs(as.numeric(.))) %>% 
  mutate(Change = percent((`2010`- `2000`)/`2000`)) %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) 



write_csv(Table_n_state, path = "Results//Tables//n_State.csv")