#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Pollutant estimate
#Part    : (01) Preparing data sets 
#Purpose : Read in census data, income data, pollutant conc, incidence rate (National), and prevelance rate (National)
#         Followed by joining the data sets 
#         Followed by estimating the burden
#Created by Raed Alotaibi
#Date Created: 26-June-2019
#Last updated: 28-June-2019
#---------------------------------------------#


# Stop clock --------------------------------------------------------------
ptm <- proc.time()

# Loading packagesa -------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(stringr)
library(stringi)
library(DT)
library(htmltools)





# Loading Census 2000 -----------------------------------------------------
path_census2000 <- "Data\\Census\\nhgis0042_ds147_2000_block.csv"


var_census2000 <- c("GISJOIN","YEAR", "STATE", "STATEA", "FXS001", 
                    "FXT002", "FXT003", "FXU001", "FXU002",       
                    "FYM001", "FYM002", "FYM003", "FYM004", 
                    "FYM024", "FYM025", "FYM026", "FYM027", 
                    "PLACEA" )


# Making new variable of total children count "CHILDREN" and renaming variables
census2000 <- fread(file = path_census2000,data.table = F, stringsAsFactors = F, verbose = T, select = var_census2000,
                    colClasses = list(factor = c("STATE"))) %>% 
  filter(FXS001 > 0) %>% 
  mutate(CHILDREN = FYM001 + FYM002 + FYM003 + FYM004 +
                    FYM024 + FYM025 + FYM026 + FYM027) %>% 
  mutate(FIPS = stri_sub(GISJOIN, 2,3)) %>%
  mutate(PLACEA = str_pad(PLACEA, 5, pad = "0")) %>%
  mutate(PlaceFIPS = paste0(FIPS, PLACEA)) %>% 
  mutate(URBAN = case_when(
    FXU001 > 0 ~ "Urbanized area", 
    FXU002 > 0 ~ "Urban cluster", 
    FXT002 > 0 ~ "Rural", 
    FXT003 > 0 ~ "Not defined",
    TRUE ~ "Not defined" 
  )) %>% 
  rename(TOTAL = FXS001) %>% 
  select(GISJOIN, YEAR, FIPS,  PlaceFIPS, PLACEA, STATE, URBAN, TOTAL,  CHILDREN) %>% 
  as_tibble()



# Loading Census 2010 -----------------------------------------------------
path_census2010 <- "Data\\Census\\nhgis0043_ds172_2010_block.csv"


var_census2010 <- c("GISJOIN", "YEAR", "STATE", "STATEA", "H7V001", 
                "H7W003", "H7W004", "H7W005", "H7W006",      
                "H76003", "H76004", "H76005", "H76006", 
                "H76027", "H76028", "H76029", "H76030", 
                "PLACEA" )


# Making new variable of total children count "CHILDREN" and renaming variables
census2010 <- fread(file = path_census2010,data.table = F, stringsAsFactors = F, verbose = T, select = var_census2010,
                colClasses = list(factor = c("STATE"))) %>% 
  filter(H7V001 > 0) %>% 
  mutate(CHILDREN = H76003 + H76004 + H76005 +H76006 +
                    H76027 + H76028 + H76029 + H76030) %>% 
  mutate(FIPS = stri_sub(GISJOIN, 2,3)) %>%
  mutate(PLACEA = str_pad(PLACEA, 5, pad = "0")) %>%
  mutate(PlaceFIPS = paste0(FIPS, PLACEA)) %>% 
  mutate(URBAN = case_when(
    H7W003 > 0 ~ "Urbanized area", 
    H7W004 > 0 ~ "Urban cluster", 
    H7W005 > 0 ~ "Rural", 
    H7W006 > 0 ~ "Not defined",
    TRUE ~ "Not defined" 
  )) %>% 
  rename(TOTAL = H7V001) %>% 
  select(GISJOIN, YEAR, FIPS, PlaceFIPS, PLACEA, STATE, URBAN, TOTAL,  CHILDREN) %>% 
  as_tibble()



# Joining Census data by row  ---------------------------------------------------------
census <- bind_rows(census2000, census2010)

rm(census2000)
rm(census2010)


# Loading NO2  --------------------------------------------------------

#Year 2000 data
var_NO2_2000 <- c("GISJOIN", "Y2000")

NO2_2000 <- fread("Data\\Pollutant\\NO2_2000.csv", data.table = F, stringsAsFactors = F,  verbose = T, select = var_NO2_2000) %>% 
  mutate(YEAR = 2000) %>% 
  mutate(NO2 = Y2000*1.88) %>% 
  select(GISJOIN,YEAR, NO2) %>% 
  as_tibble()



#Year 2010 data
var_NO2_2010 <- c("GISJOIN", "Y2010")

NO2_2010 <- fread("Data\\Pollutant\\NO2_2010.csv", data.table = F, stringsAsFactors = F,  verbose = T, select = var_NO2_2010) %>% 
  mutate(YEAR = 2010) %>% 
  mutate(NO2 = Y2010*1.88) %>% 
  select(GISJOIN,YEAR, NO2) %>% 
  as_tibble()


#Binding data
NO2 <- bind_rows(NO2_2000, NO2_2010)

rm(NO2_2000)
rm(NO2_2010)



# Loading PM  --------------------------------------------------------

#Year 2000 data
load("Data\\Pollutant\\CACES_P3v1_2000blocks.RData")

PM_2000 <-preds.2000.00cw %>% 
  select(GJOIN2000, pm25.wght, pm10.wght) %>% 
  mutate(YEAR = 2000) %>% 
  rename(PM2.5 = pm25.wght,
         PM10 = pm10.wght,
         GISJOIN = GJOIN2000)

rm(preds.2000.00cw)  


#Year 2010 data
load("Data\\Pollutant\\CACES_P3v1_2010blocks.RData")

PM_2010 <- preds.2010 %>% 
  select(GISJOIN, pm25, pm10) %>% 
  mutate(YEAR = 2010) %>% 
  rename(PM2.5 = pm25,
         PM10 = pm10) %>% 
  as_tibble()

rm(preds.2010)  


#Binding data
PM <- bind_rows(PM_2000, PM_2010)

rm(PM_2000)
rm(PM_2010)


# Joining Pollutant  to census data ----------------------------------------------

census_2 <- census %>% 
  left_join(NO2, by = c("GISJOIN", "YEAR")) %>% 
  left_join(PM, by = c("GISJOIN", "YEAR")) %>% 
  mutate_at("YEAR", factor)

rm(NO2)
rm(PM)
rm(census)




# Converting wide format to long format -----------------------------------
census_3 <- census_2 %>% 
  gather("POLLUT" , "CONC", -GISJOIN, -YEAR, -FIPS, -PlaceFIPS, -PLACEA, -STATE, -URBAN, -TOTAL, -CHILDREN)




# Estimating the burden ---------------------------------------------------

    burden <- census_3 %>% 
    mutate(IR = 0.0125, 
           PRV   = ifelse(YEAR == 2000, 0.124, 0.137),
           CRF   = ifelse(POLLUT == 'NO2', 1.05, ifelse(POLLUT == 'PM10', 1.05, 1.03)),
           UNIT  = ifelse(POLLUT == 'NO2', 4, ifelse(POLLUT == 'PM10', 2, 1)),
           CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
           RRnew = exp((log(CRF)/UNIT)*CONC),
           AF    = (RRnew - 1)/(RRnew), 
           AC    = AF*CASES 
    )
  

# Examinig sample of the data frame
s <- sample(1:34389278, 25, replace=FALSE)

burden[s,] %>% 
  as.data.frame()




# Stop clock --------------------------------------------------------------
proc.time() - ptm


