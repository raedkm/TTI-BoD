#---------------------------------------------#
#Project : Final report TRAP project - 2019
#Sub     : Pollutant estimate
#Part    : (01) Preparing data sets 
#Purpose : Read in census data, income data, pollutant conc, incidence rate (National), and prevelance rate (National)
#         Followed by joining the data sets 
#         Followed by estimating the burden
#Created by Raed Alotaibi
#Date Created: 26-June-2019
#Last Updated: 2-July-2019
#---------------------------------------------#

# Increasing memory allocation to run the analysis
memory.limit(size=50000)

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
library(forcats)


# Start clock --------------------------------------------------------------
ptm <- proc.time()



# Loading Census -----------------------------------------------------
#Year 2000 data
census2000_path <- "Data\\Census\\nhgis0042_ds147_2000_block.csv"


census2000_var <- c("GISJOIN","YEAR", "STATE", "STATEA", "FXS001", 
                    "FXT002", "FXT003", "FXU001", "FXU002",       
                    "FYM001", "FYM002", "FYM003", "FYM004", 
                    "FYM024", "FYM025", "FYM026", "FYM027", 
                    "PLACEA" )


# Making new variable of total children count "CHILDREN" and renaming variables
census2000 <- fread(file = census2000_path,data.table = F, stringsAsFactors = F, verbose = F, select = census2000_var) %>% 
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



#Year 2010 data

census2010_path <- "Data\\Census\\nhgis0043_ds172_2010_block.csv"

census2010_var <- c("GISJOIN", "YEAR", "STATE", "STATEA", "H7V001", 
                "H7W003", "H7W004", "H7W005", "H7W006",      
                "H76003", "H76004", "H76005", "H76006", 
                "H76027", "H76028", "H76029", "H76030", 
                "PLACEA" )


# Making new variable of total children count "CHILDREN" and renaming variables
census2010 <- fread(file = census2010_path,data.table = F, stringsAsFactors = F, verbose = F, select = census2010_var) %>% 
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



# Joining Census data by row  

census <- bind_rows(census2000, census2010)

rm(census2000)
rm(census2010)




# Loading Median Income ---------------------------------------------------

#Year 2000 data

income_2000_path <- "Data\\Census\\nhgis0044_ds152_2000_blck_grp.csv"

income_2000_var <- c("GISJOIN",  "HF6001")

income_2000 <- fread(file = income_2000_path, data.table = F, stringsAsFactors = F, verbose = F,  select = income_2000_var) %>% 
  rename(GISJOIN_i = GISJOIN,
         M_INCOME = HF6001) %>%
  mutate(YEAR = 2000,
         INCOME = case_when(
           M_INCOME < 20000 ~ "<$20,000", 
           between(M_INCOME, 20000, 34999) ~ "$20,000 to <$35,000", 
           between(M_INCOME, 35000, 49999) ~ "$35,000 to <$50,000", 
           between(M_INCOME, 50000, 74999) ~ "$50,000 to <$75,000", 
           M_INCOME >= 75000  ~ ">=$75,000", 
           TRUE ~ "Not defined")) %>% 
  as_tibble()  


#Year 2010 data

income_2010_path <- "Data\\Census\\nhgis0043_ds176_20105_2010_blck_grp.csv"

income_2010_var <- c("GISJOIN", "JOIE001")

income_2010 <- fread(file = income_2010_path, data.table = F, stringsAsFactors = F, verbose = F,  select = income_2010_var) %>% 
  rename(GISJOIN_i = GISJOIN,
         M_INCOME = JOIE001) %>%
  mutate(YEAR = 2010,
         INCOME = case_when(
           M_INCOME < 20000 ~ "<$20,000", 
           between(M_INCOME, 20000, 34999) ~ "$20,000 to <$35,000", 
           between(M_INCOME, 35000, 49999) ~ "$35,000 to <$50,000", 
           between(M_INCOME, 50000, 74999) ~ "$50,000 to <$75,000", 
           M_INCOME >= 75000  ~ ">=$75,000", 
           TRUE ~ "Not defined")) %>% 
  as_tibble()  





#Binding data
income <- bind_rows(income_2000, income_2010)

rm(income_2000)
rm(income_2010)




# Loading NO2  --------------------------------------------------------

#Year 2000 data
NO2_2000_path <-"Data\\Pollutant\\NO2_2000.csv"
NO2_2000_var <- c("GISJOIN", "Y2000")

NO2_2000 <- fread(NO2_2000_path, data.table = F, stringsAsFactors = F,  verbose = F, select = NO2_2000_var) %>% 
  mutate(YEAR = 2000) %>% 
  mutate(NO2 = Y2000*1.88) %>% 
  select(GISJOIN,YEAR, NO2) %>% 
  as_tibble()



#Year 2010 data
NO2_2010_path <-"Data\\Pollutant\\NO2_2010.csv"
NO2_2010_var <- c("GISJOIN", "Y2010")

NO2_2010 <- fread(NO2_2010_path, data.table = F, stringsAsFactors = F,  verbose = F, select = NO2_2010_var) %>% 
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
PM_2000_path <-"Data\\Pollutant\\CACES_2000.csv"
PM_2000_var <- c("GISJOIN", "pm25", "pm10")

PM_2000 <- fread(PM_2000_path, data.table = F, stringsAsFactors = F,  verbose = F, select = PM_2000_var) %>% 
  mutate(YEAR = 2000) %>% 
  as_tibble()



#Year 2010 data
PM_2010_path <-"Data\\Pollutant\\CACES_2010.csv"
PM_2010_var <- c("GISJOIN", "pm25", "pm10")

PM_2010 <- fread(PM_2010_path, data.table = F, stringsAsFactors = F,  verbose = F, select = PM_2010_var) %>% 
  mutate(YEAR = 2010) %>% 
  as_tibble() 


#Binding data
PM <- bind_rows(PM_2000, PM_2010)

rm(PM_2000)
rm(PM_2010)




# Joining  Census,  Meidian Income, and Pollutant Data----------------------------------------------

census_2 <- census %>% 
  mutate(GISJOIN_i = substr(GISJOIN, 1, 15)) %>% 
  left_join(income, by = c("GISJOIN_i", "YEAR")) %>% 
  left_join(NO2,    by = c("GISJOIN", "YEAR")) %>% 
  left_join(PM,     by = c("GISJOIN", "YEAR")) %>% 
  select(-GISJOIN_i, -M_INCOME)



rm(census)
rm(income)
rm(NO2)
rm(PM)


# Converting wide format to long format  -----------------------------------

census_3 <- census_2 %>% 
  gather("POLLUT" , "CONC", -GISJOIN, -YEAR, 
         -FIPS, -PlaceFIPS, -PLACEA, -STATE, 
         -URBAN, -TOTAL, -CHILDREN, -INCOME) 
 

rm(census_2)

# Estimating the burden ---------------------------------------------------

convert_var <- c("YEAR", "URBAN", "INCOME", "POLLUT" ) #converting variables to factors to reduce file size

IR  <- 0.0125

burden <- census_3 %>% 
    mutate(
           PRV   = ifelse(YEAR == 2000, 0.124, 0.137),
           CRF   = ifelse(POLLUT == 'NO2', 1.05, 
                   ifelse(POLLUT == 'pm10', 1.05, 1.03)),
           UNIT  = ifelse(POLLUT == 'NO2', 4, 
                   ifelse(POLLUT == 'pm10', 2, 1)),
           CASES = (CHILDREN - (CHILDREN * PRV)) * IR,
           RRnew = exp((log(CRF)/UNIT)*CONC),
           PAF   = (RRnew - 1)/(RRnew), 
           AC    = PAF*CASES) %>%
  mutate_at(convert_var, factor) %>% #converting select variables to factors
  mutate(INCOME = fct_relevel(INCOME, "Not defined", 
                                      "<$20,000", 
                                      "$20,000 to <$35,000",
                                      "$35,000 to <$50,000", 
                                      "$50,000 to <$75,000", 
                                      ">=$75,000"),
         URBAN  = fct_relevel(URBAN,  "Urbanized area", 
                                      "Urban cluster", 
                                      "Rural", 
                                      "Not defined"),
         POLLUT = fct_relevel(POLLUT, "NO2", "pm25", "pm10"))   #Reordering the levels of factors for plots and table


rm(census_3)



# Examinig sample of the burden data frame
s <- sample(1:34389278, 25, replace=FALSE)

burden[s,] %>% 
  as.data.frame()



# Stop clock --------------------------------------------------------------
proc.time() - ptm


