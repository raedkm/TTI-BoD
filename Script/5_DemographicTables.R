#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Demographics
#Part    : (0)  Demographic tables
#Purpose : Create demographic tables
#Created by Raed Alotaibi
#Date Created: 3-July-2019
#Last Updated: 3-July-2019
#---------------------------------------------#

library(scales)    


## Note : need the burden data frame (run script "1_DataSets.R" first)


# Creating a uniquea spread function for multiple value columns -----------

myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

#Example:: t2 <- df %>% myspread(student, c(A, B))


# Table of Children by year -----------------------------------------
Table_names <- c("Level","2000", "2010")

# Total
Table_childTotal_ALL <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter only one pollutant to prevent repeated count
  mutate(ALL = 'Total') %>% 
  group_by(YEAR, ALL) %>% 
  summarise(childTotal = sum(CHILDREN, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, childTotal) 

colnames(Table_childTotal_ALL) <- Table_names

# Living location
Table_childTotal_urban <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, URBAN) %>%  
  summarise(childTotal = sum(CHILDREN, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, childTotal) 

colnames(Table_childTotal_urban) <- Table_names


# Median income
Table_childTotal_income <-
  burden %>%
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, INCOME) %>%
  summarise(childTotal = sum(CHILDREN, na.rm = T)) %>%
  #mutate_if(is.numeric, round, digits = -2)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, childTotal)

colnames(Table_childTotal_income) <- Table_names



# Joining the tables
(Table_childTotal <- rbind(
  Table_childTotal_ALL, 
  Table_childTotal_urban, 
  Table_childTotal_income)) 

write_csv(Table_childTotal, path = "Results//Tables//childTotal.csv")


# Table of children year and state ----------------------------------------

Table_childTotal_state <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(STATE, YEAR) %>%  
  summarise(childTotal = sum(CHILDREN, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, childTotal) %>% 
  mutate_at(vars(-STATE), funs(gsub(",", "", .))) %>% 
  mutate_at(vars(-STATE), funs(as.numeric(.))) %>% 
  mutate(Change = percent((`2010`- `2000`)/`2000`)) %>% 
mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) 



write_csv(Table_childTotal_state, path = "Results//Tables//childTotal_State.csv")



# Table of population by year -----------------------------------------

# Total
Table_popTotal_ALL <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter only one pollutant to prevent repeated count
  mutate(ALL = 'Total') %>% 
  group_by(YEAR, ALL) %>% 
  summarise(popTotal = sum(TOTAL, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, popTotal) 

colnames(Table_popTotal_ALL) <- Table_names

# Living location
Table_popTotal_urban <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, URBAN) %>%  
  summarise(popTotal = sum(TOTAL, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, popTotal) 

colnames(Table_popTotal_urban) <- Table_names


# Median income
Table_popTotal_income <-
  burden %>%
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(YEAR, INCOME) %>%
  summarise(popTotal = sum(TOTAL, na.rm = T)) %>%
  #mutate_if(is.numeric, round, digits = -2)  %>%
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>%
  spread(YEAR, popTotal)

colnames(Table_popTotal_income) <- Table_names



# Joining the tables
(Table_popTotal <- rbind(
  Table_popTotal_ALL, 
  Table_popTotal_urban, 
  Table_popTotal_income)) 

write_csv(Table_popTotal, path = "Results//Tables//popTotal.csv")


# Table of incident cases by year and state ----------------------------------------

Table_popTotal_state <- 
  burden %>% 
  filter(POLLUT == 'NO2') %>%  #We need to filter onl one pollutant to prevent repeated count
  group_by(STATE, YEAR) %>%  
  summarise(popTotal = sum(TOTAL, na.rm = T)) %>% 
  #mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, popTotal) %>% 
  mutate_at(vars(-STATE), funs(gsub(",", "", .))) %>% 
  mutate_at(vars(-STATE), funs(as.numeric(.))) %>% 
  mutate(Change = percent((`2010`- `2000`)/`2000`)) %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) 



write_csv(Table_popTotal_state, path = "Results//Tables//popTotal_State.csv")





