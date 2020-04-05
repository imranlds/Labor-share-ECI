library(tidyverse)
library(dplyr)
library(countrycode)
library(foreign)
library(readxl)
library(RcppRoll) # To calculate rolling mean
library(haven)
################################################################
setwd("C:/Users/Imran/Google Drive/WIP/Growth inequality and economic complexity of nations/Data")
################################################################

QOG <- read.csv(choose.files()) # QOG time series data from data store

QOG <- QOG %>%
  rename(ocode = ccode)  


################################################################
# Income inequality data

allginis_2013 <- read_excel("allginis_2013.xls", 
                            sheet = "data") %>%
  mutate(ocode = countrycode(contcod, "iso3c", "iso3n")) %>%
  select(ocode, year, contains("gini"))

################################################################
# Economic complexity index

eci <- read.csv("eci_1964-2017.csv") %>% 
  mutate(ocode = countrycode(country, "country.name", "iso3n"),
         eci = (9/6)*(ECI - 3) + 10, # Normalize from 1-10
         eci_plus = (9/6)*(ECI_plus - 4) + 13) %>% # Normalize from 1-10
  select(-c(2:3))

################################################################
# Polity IV index

p4v2018 <- read_excel("C:/Users/Imran/Google Drive/Reserach Work/Data store/p4v2018.xls") %>% 
  mutate(ocode = countrycode(country, "country.name", "iso3n")) %>%
  select(ocode, year, polity2) %>%
  filter(year > 1969) %>%
  filter(!is.na(ocode), !is.na(year)) %>%
  arrange(ocode, year)

################################################################
###### Corruption Data ###############
# ICRG data
ICRG_corr <- read_excel("3BResearchersDataset2018.xls", 
                        sheet = "F-Corruption", skip = 7) %>%
  filter(Country != "USSR",
         Country != "Serbia & Montenegro *",
         Country != "West Germany") %>% # Problematic codes
  mutate(ocode = countrycode(Country, "country.name", "iso3n")) %>%
  select(-Country) %>%
  gather(year, icrg_corr, -ocode) %>%
  filter(!is.na(ocode), !is.na(year)) %>%
  mutate(year = as.integer(year))

ti_cpi <- read.csv("C:/Users/Imran/Google Drive/Reserach Work/Data store/ti_cpi.csv") %>%
  filter(Country != "Serbia and Montenegro") %>%
  filter(Country != "Yugoslavia") %>%
  mutate(ocode = countrycode(Country, "country.name", "iso3n")) %>%
  rename(ti_corruption = ti_cpi) %>%
  select(ocode, year, ti_corruption) 

################################################################
###### EODB Data ###############

eodb <- read_excel("C:/Users/Imran/Google Drive/Reserach Work/Data store/EODB_Historical-data_COMPLETE-dataset-with-scores.xlsx", 
                        skip = 3) 

eodb <- eodb %>%
  select(1, 3:5, 
         "Procedures - Men (number)",
         "Time - Men (days)",
         "Cost - Men (% of income per capita)",
         "Procedures - Women (number)",
         "Time - Women (days)",
         "Cost - Women (% of income per capita)",
         "Paid-in Minimum capital (% of income per capita)"
         ) %>%
  mutate(procedures = (`Procedures - Men (number)` + `Procedures - Women (number)`)/2,
         time = (`Time - Men (days)` + `Time - Women (days)`)/2,
         cost = (`Cost - Men (% of income per capita)` + `Cost - Women (% of income per capita)`)/2,
         min_capital = `Paid-in Minimum capital (% of income per capita)`
         ) %>%
  rename(year = `DB Year`,
         Income_group = "Income group") %>%
  mutate(ocode = countrycode(`Country code`, "iso3c", "iso3n")) %>%
  filter(!is.na(ocode), !is.na(year)) %>%
  select(16, 4, 12:15)

colnames(eodb)[3:6] <- paste("eodb", colnames(eodb[,c(3:6)]), sep = "_")


################################################################
###### REER Data from CEPII ###############

# reer <- read.csv("REER_Weights_bar.csv", skip = 1) %>%
#   gather(Country, cepii_eq_reer, -year) %>%
#   mutate(ocode = countrycode(Country, "country.name", "iso3n")) %>%
#   filter(!is.na(ocode), !is.na(year)) %>%
#   select(-Country) 

################################################################
###### PWT ###############
library(pwt9)

data("pwt9.1")


pwt9.1 <-  force(pwt9.1)  %>%
  mutate(ocode = countrycode(country, "country.name", "iso3n")) %>%
  filter(!is.na(ocode), !is.na(year)) %>%
  select(-c(1:2)) %>%
  select( 51, 1:50)

colnames(pwt9.1)[4:51] <- paste("pwt91", colnames(pwt9.1[,c(4:51)]), sep = "_")

###### WDI ###############
# library(WDI)
# 
# wdi <- as.data.frame(WDI_data[["country"]])  %>%
#   filter(country != "Korea, Dem. Rep.") %>%
#   mutate(ocode = countrycode(country, "country.name", "iso3n")) %>%
#   filter(!is.na(ocode)) 

#write.csv(wdi, "wdi.csv", row.names = F) 

# String split in excel

pwt9.1 <- read.csv("wdi.csv") %>%
  mutate(OECD_nonOECD = trimws(OECD_nonOECD),
         income_group = trimws(income_group)) %>%
select(3:6) %>%
  full_join(pwt9.1)

################################################################
# Data join

Extended_QOG <- QOG %>%
  left_join(allginis_2013) %>%
  left_join(eci) %>%
  left_join(p4v2018) %>%
  left_join(ICRG_corr)  %>%
  left_join(eodb) %>%
  left_join(pwt9.1) %>%
#  left_join(wdi) %>%
#  filter(year %in% c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)) %>%
  filter(!is.na(ocode), !is.na(year)) %>%
  arrange(ocode, year)

write.dta(Extended_QOG, "Extended_QOG.dta")

################################################################

library(haven)
extd_QOG <- read_dta("Extended_QOG.dta")

attributes(extd_QOG$ocode) <- NULL # Remove attributes
attributes(extd_QOG$year) <- NULL # Remove attributes


################################################################

# Statistics on Public Expenditures for Economic Development (SPEED)
# International Food Policy Research Institute (IFPRI), 2015, 
#"Statistics on Public Expenditures for Economic Development (SPEED)", https://doi.org/10.7910/DVN/INZ3QK, Harvard Dataverse, V3 

var_list <- c("gdpeducation_ppp", "gdphealth_ppp", "gdpdefense_ppp", "gdpcom_ppp", 
           "gdptransp_ppp", "gdptc_ppp", "gdpsp_ppp", "gdpmining_ppp", "gdpfuel_ppp", "gdpother_ppp", "gdptotal_ppp")

ifpri <-  read_excel("C:/Users/Imran/Google Drive/Reserach Work/Data store/IFPRI/001_Main_SPEED_Dataset_2015.xls", 
                     sheet = "gdpag_ppp", n_max = 147) %>%
  mutate(ocode = countrycode(country, "country.name", "iso3n")) %>%
  select(5:38) %>%
  gather(year, gdpag_ppp, -ocode) %>%
  mutate(year = as.numeric(year))

for (var in var_list) {
  dat <- read_excel("C:/Users/Imran/Google Drive/Reserach Work/Data store/IFPRI/001_Main_SPEED_Dataset_2015.xls", 
                                                         sheet = var, n_max = 147) %>%
    mutate(ocode = countrycode(country, "country.name", "iso3n")) %>%
    select(5:38) %>%
    gather(year, value, -ocode) %>%
    mutate(year = as.numeric(year))
  
  names(dat)[3] <- c(var)
  
  ifpri <- ifpri %>%
    left_join(dat)
}

colnames(ifpri)[3:14] <- paste("ifpri", colnames(ifpri[,c(3:14)]), sep = "_")
################################################################

library(haven)
extd_QOG <- read_dta("Extended_QOG.dta")

extd_QOG <- extd_QOG %>%
  mutate(ocode = as.numeric(ocode),
       year = as.numeric(year))

extd_QOG <- extd_QOG %>%
  left_join(ifpri)

#write.dta(extd_QOG, "Extended_QOG_03172020.dta")

################################################################

# Income inequality data from http://utip.lbj.utexas.edu/data.html

#extd_QOG <- read_dta("Extended_QOG_03172020.dta")

ehii_gini <- read_excel("C:/Users/Imran/Google Drive/Reserach Work/Data store/Income inequality - EHII/UtipUnidoEhiiV2017_v.1.xlsx", 
                   sheet = "gini") %>%
  select(1, 4:56) %>%
  gather(year, ehii_gini, -country) %>%
  mutate(year = as.numeric(year)) %>%
  rename(ocode = country)

extd_QOG <- extd_QOG %>%
  full_join(ehii_gini)

#write.dta(extd_QOG, "Extended_QOG_03192020.dta")


################################################################
# ILO labor union data

# extd_QOG <- read_dta("Extended_QOG_03192020.dta")
# 
# extd_QOG <- extd_QOG %>%
#   mutate(ocode = as.numeric(ocode),
#          year = as.numeric(year))

cbc <- read.csv("ILO data/Collective bargaining coverage rate.csv") %>%
  mutate(ocode = countrycode(country, "country.name", "iso3n")) %>%
  select(2:4) %>%
  rename(ilo_cbc = "ilo_collective_bargaining_coverage_rate_percent")
  

tud <- read.csv("ILO data/Trade union density.csv")  %>%
  mutate(ocode = countrycode(country, "country.name", "iso3n")) %>%
  select(2:4) %>%
  rename(ilo_trade = "ilo_trade_union_density_rate_percent")

extd_QOG <- extd_QOG %>%
  full_join(cbc) %>%
  full_join(tud)

write_dta(extd_QOG, "Extended_QOG_03252020.dta")  

################################################################
# Global Burden of Disease Study 2017 (GBD 2017) Data Resources
# Source: http://ghdx.healthdata.org/gbd-results-tool

ihme_gbd <- read_csv("IHME-GBD_2017_DATA-03d7b044-1/IHME-GBD_2017_DATA-03d7b044-1.csv") 

gbd <- ihme_gbd %>%
  mutate(ocode = countrycode(location, "country.name", "iso3n")) %>%
  filter(measure != "Deaths", 
         metric == "Percent") %>%
  select(c(5, 7:8, 11)) %>%
  spread(cause, val)

# Apply labels

library(expss)
gbd <- gbd %>%
  rename(gbd_all_pr = "All causes",
         gbd_comm_pr = "Communicable, maternal, neonatal, and nutritional diseases",
         gbd_ent_pr = "Enteric infections",                                         
         gbd_hiv_pr = "HIV/AIDS and sexually transmitted infections",              
         gbd_mat_pr = "Maternal and neonatal disorders",                            
         gbd_ntrop_pr = "Neglected tropical diseases and malaria",                   
         gbd_nutr_pr = "Nutritional deficiencies",                                   
         gbd_other_pr = "Other infectious diseases",                                 
         gbd_res_pr = "Respiratory infections and tuberculosis")                   

gbd <- apply_labels(gbd,
                    gbd_all_pr = "All causes (%)",
                    gbd_comm_pr = "Communicable, maternal, neonatal, and nutritional diseases (%)",
                    gbd_ent_pr = "Enteric infections (%)",                                         
                    gbd_hiv_pr = "HIV/AIDS and sexually transmitted infections (%)",              
                    gbd_mat_pr = "Maternal and neonatal disorders (%)",                            
                    gbd_ntrop_pr = "Neglected tropical diseases and malaria (%)",                   
                    gbd_nutr_pr = "Nutritional deficiencies (%)",                                   
                    gbd_other_pr = "Other infectious diseases (%)",                                 
                    gbd_res_pr = "Respiratory infections and tuberculosis (%)")
                


gbd1 <- ihme_gbd %>%
  mutate(ocode = countrycode(location, "country.name", "iso3n")) %>%
  filter(measure != "Deaths", 
         metric != "Percent") %>%
  select(c(5, 7:8, 11)) %>%
  spread(cause, val)

# Apply labels


gbd1 <- gbd1 %>%
  rename(gbd_all_nr = "All causes",
         gbd_comm_nr = "Communicable, maternal, neonatal, and nutritional diseases",
         gbd_ent_nr = "Enteric infections",                                         
         gbd_hiv_nr = "HIV/AIDS and sexually transmitted infections",              
         gbd_mat_nr = "Maternal and neonatal disorders",                            
         gbd_ntrop_nr = "Neglected tropical diseases and malaria",                   
         gbd_nutr_nr = "Nutritional deficiencies",                                   
         gbd_other_nr = "Other infectious diseases",                                 
         gbd_res_nr = "Respiratory infections and tuberculosis")                   

gbd1 <- apply_labels(gbd1,
                     gbd_all_nr = "All causes (number)",
                     gbd_comm_nr = "Communicable, maternal, neonatal, and nutritional diseases (number)",
                     gbd_ent_nr = "Enteric infections (number)",                                       
                     gbd_hiv_nr = "HIV/AIDS and sexually transmitted infections (number)",             
                     gbd_mat_nr = "Maternal and neonatal disorders (number)",                            
                     gbd_ntrop_nr = "Neglected tropical diseases and malaria (number)",                  
                     gbd_nutr_nr = "Nutritional deficiencies (number)",                                 
                     gbd_other_nr = "Other infectious diseases (number)",                                 
                     gbd_res_nr = "Respiratory infections and tuberculosis (number)"
                     )


  
extd_QOG <- extd_QOG %>%
  full_join(gbd) %>%
  full_join(gbd1)

write_dta(extd_QOG, "Extended_QOG_03262020.dta")  

  
