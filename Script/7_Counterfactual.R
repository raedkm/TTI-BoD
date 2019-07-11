#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Demographics
#Part    : (0)  Counterfactual scenarios
#Purpose : Create counterfactual scenarios and tables
#Created by Raed Alotaibi
#Date Created: 10-July-2019
#Last Updated: 10-July-2019
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


# Counterfactual Scenrio --------------------------------------------------
# Note: To run the counterfactual scenarios, uncomment the desired scenario (only one scenario at a time) 
# and continue running the code.
# Warning: The saved tables and plots in the following scripts will be saved using the counterfactual scenario

#Scenario 1 - WHO guidlines
burden_sc1 <- burden %>%
  select(YEAR, POLLUT, CONC, CASES, CRF, UNIT) %>% 
  mutate(CONC = ifelse(POLLUT == 'NO2' & CONC > 40, 40, CONC),
         CONC = ifelse(POLLUT == 'pm10'& CONC > 20, 20, CONC),
         CONC = ifelse(POLLUT == 'pm25'& CONC > 10, 10, CONC),
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES) %>% 
  select(-CRF, -UNIT, -RRnew, -UNIT) %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) 

write_csv(burden_sc1, path = "Results//Tables//CounterScenario1.csv")



# #Scenario 2 - Lowest deteceted levels
Min_NO2  <- burden %>% filter(POLLUT == 'NO2')  %>% select(CONC) %>% as.data.frame() %>% min(na.rm = T)
Min_pm10 <- burden %>% filter(POLLUT == 'pm10') %>% select(CONC) %>% as.data.frame() %>% min(na.rm = T)
Min_pm25 <- burden %>% filter(POLLUT == 'pm25') %>% select(CONC) %>% as.data.frame() %>% min(na.rm = T)

burden_sc2 <- burden %>%
  select(YEAR, POLLUT, CONC, CASES, CRF, UNIT) %>% 
  mutate(CONC = ifelse(POLLUT == 'NO2' & CONC > Min_NO2, Min_NO2, CONC),
         CONC = ifelse(POLLUT == 'pm10'& CONC > Min_pm10, Min_pm10, CONC),
         CONC = ifelse(POLLUT == 'pm25'& CONC > Min_pm25, Min_pm25, CONC),
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES) %>% 
  select(-CRF, -UNIT, -RRnew, -UNIT) %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = -2)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) 

write_csv(burden_sc2, path = "Results//Tables//CounterScenario2.csv")



range(burden_sc$CONC, na.rm = T)
