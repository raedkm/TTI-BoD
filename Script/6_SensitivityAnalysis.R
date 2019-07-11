#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Demographics
#Part    : (0)  Sensitivity Analysis
#Purpose : Create Sensitivity Analysis  tables
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



# Sensitivity analysis ----------------------------------------------------
# Note: To run the Sensitivity analysis, uncomment the desired scenario (only one scenario at a time) 
# and continue running the code.
# Warning: The saved tables and plots in the following scripts will be saved using the sensitity analysis results

IR_up  <- 0.144
IR_low <- 0.105

CRF_NO2_up   <- 1.07
CRF_NO2_low  <- 1.02

CRF_PM10_up  <- 1.08
CRF_PM10_low <- 1.02

CRF_PM25_up  <- 1.05
CRF_PM25_low <- 1.01




# (1) Upper limit of IR and CRF
burden <- burden %>% 
  mutate(IR = IR_up, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_up, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_up, CRF_PM25_up)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES)


# (2) Lower limit of IR and CRf
burden <- census_3 %>% 
  mutate(IR = IR_low, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_low, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_low, CRF_PM25_low)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES)


# (3) Upper limit of IR and Lower limit of CRF
burden <- census_3 %>% 
  mutate(IR = IR_up, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_low, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_low, CRF_PM25_low)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES)


# (4) Lower limit of IR and Upper limit of CRF
burden <- census_3 %>% 
  mutate(IR = IR_low, 
         CRF   = ifelse(POLLUT == 'NO2', CRF_NO2_up, 
                        ifelse(POLLUT == 'PM10', CRF_PM10_up, CRF_PM25_up)),
         CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
         RRnew = exp((log(CRF)/UNIT)*CONC),
         PAF   = (RRnew - 1)/(RRnew), 
         AC    = PAF*CASES)



