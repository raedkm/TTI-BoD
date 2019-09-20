#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Demographics
#Part    : (09)  Sensitivity Analysis
#Purpose : Create Sensitivity Analysis  tables
#Created by Raed Alotaibi
#Date Created: 10-July-2019
#Last Updated: 13-Aug-2019
#---------------------------------------------#



## Note : need the burden data frame (run script "1_DataSets.R" first)




# Assigning the upper and lower limits of IR and CRF for each pollutant --------



IR_up  <- 0.144
IR_low <- 0.105


# CRF for NO2

CRF_NO2_up   <- 1.07
CRF_NO2_low  <- 1.02


# CRF for PM10

CRF_PM10_up  <- 1.08
CRF_PM10_low <- 1.02


# CRF for PM2.5

CRF_PM25_up  <- 1.05
CRF_PM25_low <- 1.01


Table_names <- c("Level","NO2_2000", "NO2_2010", "PM10_2000", "PM10_2010", "PM25_2000", "PM25_2010")




# Producing the estimates of the AC  --------------------------------------



# (1) Upper limit of IR and CRF
burden_Up_IR_CRF <- burden %>% 
  mutate(IR = IR_up, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_up, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_up, CRF_PM25_up)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES) %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(burden_Up_IR_CRF) <- Table_names


# (2) Lower limit of IR and CRf
burden_Low_IR_CRF <- burden %>% 
  mutate(IR = IR_low, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_low, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_low, CRF_PM25_low)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES) %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(burden_Low_IR_CRF) <- Table_names



# (3) Upper limit of IR and Lower limit of CRF
burden_Up_IR_Low_CRF <- burden %>% 
  mutate(IR = IR_up, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_low, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_low, CRF_PM25_low)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES)  %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(burden_Up_IR_Low_CRF) <- Table_names




# (4) Lower limit of IR and Upper limit of CRF
burden_Low_IR_Up_CRF <- burden %>% 
  mutate(IR = IR_low, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_up, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_up, CRF_PM25_up)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES) %>% 
  mutate(ALL = 'Total') %>% 
  group_by(POLLUT, YEAR, ALL) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)  %>% 
  mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
  spread(YEAR, AttributableCases) %>% 
  myspread(POLLUT, c(`2000`,`2010` ))

colnames(burden_Low_IR_Up_CRF) <- Table_names



# Producing the sensitivity table -----------------------------------------


Sensitivity <- bind_rows(
  burden_Up_IR_CRF,
  burden_Low_IR_Up_CRF,
  burden_Up_IR_Low_CRF,
  burden_Low_IR_CRF) 

Sensitivity$Level <- c("Up_CRF_Up_IR", 
                       "Up_CRF_Low_IR",
                       "Low_CRF_Up_IR",
                       "Low_CRF_Low_IR")


write_csv(Sensitivity, path = "Output/Tables/Sensitivity_AC.csv")

