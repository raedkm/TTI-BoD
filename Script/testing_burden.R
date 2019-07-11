
# Storing Input variables (IR = incidence rate, PRV = prevalence rate, CRF = concentration-responce function, UNIT = CRF units)
IR  <- 0.0125

PRV_2000 <- 0.124
PRV_2010 <- 0.137


CRF_NO2  <- 1.05
CRF_pm10 <- 1.05
CRF_pm25 <- 1.03


UNIT_NO2  <- 4
UNIT_pm10 <- 2
UNIT_pm25 <- 1

# Runing the burden estimate
burden_t  <- census_3 %>% 
  mutate(
     PRV   = ifelse(census_3$YEAR == 2000, PRV_2000, PRV_2010),
     CRF   = ifelse(census_3$POLLUT == 'NO2', CRF_NO2, 
                    ifelse(census_3$POLLUT == 'pm10', CRF_pm10, 
                           CRF_pm25)),
     UNIT  = ifelse(census_3$POLLUT == 'NO2', UNIT_NO2, 
                    ifelse(census_3$POLLUT == 'pm10', UNIT_pm10, 
                           UNIT_pm25)),
     CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
     RRnew = exp((log(CRF)/UNIT)*CONC),
     PAF   = (RRnew - 1)/(RRnew), 
     AC    = PAF*CASES)    
