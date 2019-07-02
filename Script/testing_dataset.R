

# Census ------------------------------------------------------------------


census2010_path <- "Data\\Census\\nhgis0043_ds172_2010_block.csv"


census2010_var <- c("GISJOIN", "YEAR", "STATE", "STATEA", "H7V001", 
                    "H7W003", "H7W004", "H7W005", "H7W006",      
                    "H76003", "H76004", "H76005", "H76006", 
                    "H76027", "H76028", "H76029", "H76030", 
                    "PLACEA" )


# Making new variable of total children count "CHILDREN" and renaming variables
census2010 <- fread(file = census2010_path,data.table = F, stringsAsFactors = F, verbose = T, select = census2010_var,
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





# Loading Median Income ---------------------------------------------------

#Year 2000 data

income_2000_path <- "Data\\Census\\nhgis0044_ds152_2000_blck_grp.csv"

income_2000_var <- c("GISJOIN",  "HF6001")

income_2000 <- fread(file = income_2000_path, data.table = F, stringsAsFactors = F, verbose = T,  select = income_2000_var) %>% 
  rename(GISJOIN_i = GISJOIN,
         M_INCOME = HF6001) %>%
  mutate(YEAR = 2000,
         INCOME = as.factor(case_when(
           M_INCOME < 20000 ~ "<$20,000", 
           between(M_INCOME, 20000, 34999) ~ "$20,000 to <$35,000", 
           between(M_INCOME, 35000, 49999) ~ "$35,000 to <$50,000", 
           between(M_INCOME, 50000, 74999) ~ "$50,000 to <$75,000", 
           M_INCOME >= 75000  ~ ">=$75,000", 
           TRUE ~ "Not defined"))) %>% 
  mutate(INCOME = forcats::fct_relevel(INCOME, "<$20,000", "$20,000 to <$35,000", "$35,000 to <$50,000", 
                                       "$50,000 to <$75,000", ">=$75,000")) %>% 
   
  as_tibble()  


#Year 2010 data

income_2010_path <- "Data\\Census\\nhgis0043_ds176_20105_2010_blck_grp.csv"

income_2010_var <- c("GISJOIN", "JOIE001")

income_2010 <- fread(file = income_2010_path, data.table = F, stringsAsFactors = F, verbose = T,  select = income_2010_var) %>% 
  rename(GISJOIN_i = GISJOIN,
         M_INCOME = JOIE001) %>%
  mutate(YEAR = 2010,
         INCOME = as.factor(case_when(
           M_INCOME < 20000 ~ "<$20,000", 
           between(M_INCOME, 20000, 34999) ~ "$20,000 to <$35,000", 
           between(M_INCOME, 35000, 49999) ~ "$35,000 to <$50,000", 
           between(M_INCOME, 50000, 74999) ~ "$50,000 to <$75,000", 
           M_INCOME >= 75000  ~ ">=$75,000", 
           TRUE ~ "Not defined"))) %>% 
  mutate(INCOME = forcats::fct_relevel(INCOME, "<$20,000", "$20,000 to <$35,000", 
                  "$35,000 to <$50,000", "$50,000 to <$75,000", ">=$75,000")) %>% 
  as_tibble()  





#Binding data
income <- bind_rows(income_2000, income_2010)

rm(income_2000)
rm(income_2010)


# Loading Pollutant data --------------------------------------------------
load("CACES_P3v1_2000blocks")

var_names <- names(pred_2010)

pred_2000 <- preds.2000.00cw 
colnames(pred_2000) <- var_names 


pred_2010 <- preds.2010 %>%
  as_tibble() %>% 
  select(-block_fip)


write_csv(pred_2000, path = "Data\\Pollutant\\CACES_2000.csv")
write_csv(pred_2010, path = "Data\\Pollutant\\CACES_2010.csv")



# Joining  Census,  Meidna Income, and Pollutant Data----------------------------------------------

census_2 <- census %>% 
  mutate(GISJOIN_i = substr(GISJOIN, 1, 15)) %>% 
  left_join(income, by = "GISJOIN_i") %>% 
  left_join(NO2, by = c("GISJOIN", "YEAR")) %>% 
  left_join(PM, by = c("GISJOIN", "YEAR")) %>% 
  mutate_at("YEAR", factor)

rm(NO2)
rm(PM)
rm(census)


# Memoray checking --------------------------------------------------------

##To know the current storage capacity
memory.limit()

## To increase the storage capacity
memory.limit(size=56000)

## I did this to increase my storage capacity to 7GB


census_2 <- census_2 %>% 
 select( -M_INCOME)



# Compressing the data set ------------------------------------------------

burden_d <- burden[index, ] 

convert_var <- c("GISJOIN", "FIPS", "PlaceFIPS", "YEAR", "FIPS", "PlaceFIPS", "PLACEA", "STATE", "URBAN", "INCOME", "POLLUT", "IR", "PRV", "CRF", "UNIT")


burden_d2 <- burden_d %>% 
  mutate_at(convert_var, factor)

# 5.3 gb >> 4 gb >> 

burden <- burden %>% 
  mutate_at("GISJOIN", factor)
