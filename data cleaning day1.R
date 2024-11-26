install.packages("tidyverse")
library(tidyverse)
library("janitor")
library(lubridate)
#uploading the csv file
#my_mal_data <- read.csv("data/malaria data.csv")
#RDT_data <- read.csv("data/RDT data.csv") 

# changing to RDS
#saveRDS(my_mal_data, file = "data/malaria data.RDS")
my_mal_data <- readRDS("data/malaria data.RDS") %>%
  janitor::clean_names() %>% 
  rename(data_element= data)


#summary stat
names(my_mal_data)

str(my_mal_data)

#Data cleanning
table(my_mal_data$data_element)

#trimming the data name from my_mal_data
my_mal_data <- my_mal_data %>%
  mutate(data_element_new = gsub(".*?- ", "", data_element)) 

table(my_mal_data$data_element_new)

#standardize region names
my_mal_data <- my_mal_data %>%
  mutate(region= str_to_title(organisation_unit))

#find names to modify
my_mal_data %>% 
  #filter(grepl("Region", region)) %>%
  distinct(region)

#removing region from the regional name
my_mal_data <- my_mal_data %>% 
  mutate(region= gsub(" Region", "", region))

#removing Ethiopia
my_mal_data <- my_mal_data %>%
  mutate(region= gsub(" Ethiopian", "", region)) %>%
  mutate(region= gsub(" Ethiopia", "", region)) 

# remove city administration from Addis and Diredawa
my_mal_data <- my_mal_data %>%
  mutate(region= gsub(" City Administration", "", region))

#trimming the data name from RDT_data
RDT_data$Data <- gsub(".*?or ", "", RDT_data$Data)


#renaming malaria diagnosis under data to RDT performed
RDT_data <- RDT_data %>%
  mutate(Data = recode(Data, 
                       `malaria diagnosis` = "RDT performed",
                       `RDT Positive` = "RDT positive"))

#replacing long data element names into shorter ones
my_mal_data <- my_mal_data %>%
  mutate(Data = recode(Data,
                       " Malaria due to Plasmodium falciparum associated with Malaria due to Plasmodium Vivax (Mixed Malaria)" = "mixed malaria-PF/PV",
                       "Malaria due to Plasmodium vivax" = "PV malaria",
                       "Malaria due to Plasmodium falciparum" = "PF malaria",
                       "Malaria without parasitological confirmation" = "mal-no parastological confirmation"),
         "Malaria due to Plasmodium ovale" = "PO malaria",
         "Other parasitologically confirmed malaria" = "other parastologically confirmed",
         "Malaria due to Plasmodium malariae"= "PM malaria")


#renaming age column in short in my_mal_data
my_mal_data <- my_mal_data %>%
  rename(age_range= `age_in_years_1_1_4_5_14_15_29_30_64_65`)

#renaming age column in short in RDT_data
RDT_data <- RDT_data %>%
  rename(age= Age.in.years...1..1.4..5.14..15.29..30.64..65..)

#replacing "to" as indicator of age range to a hyphen in my_mal_data
my_mal_data <- my_mal_data %>%
  mutate(age_range= gsub("to", "-", age_range))


#replacing "to" as indicator of age range to a hyphen in RDT data
RDT_data$age <- gsub("to", "-", RDT_data$age)

#renaming OPD IPD in short in my_mal_data
my_mal_data <- my_mal_data %>%
  rename(dept = Departments.OPD.IPD.Categories)

#renaming outcome in short in my_mal_data
my_mal_data <- my_mal_data %>%
  rename(outcome = Outcome..Morbidity..Mortality.)
unique(my_mal_data$period)

# Separate the 'period' column into 'month' and 'year' in my_mal_data
my_mal_data <- my_mal_data %>%
  mutate(Period = as.character(period)) %>%  # Ensure 'period' is character type
  separate(Period, into = c("month", "year"), sep = " ")

#ethiopian months assignment
ethiopian_months <- data.frame(mon= 1:12, eth_mon= c("Meskerem", "Tikemet", "Hidar", "Tahisas", "Tir", "Yekatit", "Megabit", "Miyazia", "Ginbot", "Sene", "Hamle", "Nehase")) 

#creating number for months
my_mal_data <- my_mal_data %>%
  left_join(ethiopian_months, by = c("month" = "eth_mon"))
  

# Separate the 'period' column into 'month' and 'year' in RDT_data
RDT_data <- RDT_data %>%
  mutate(Period = as.character(Period)) %>%  # Ensure 'Period' is character type
  separate(Period, into = c("month", "year"), sep = " ")

#renaming organizational data with region
RDT_data <- RDT_data %>%
  rename(region=Organisation.unit)

#removing the word "region" from the values under region from my_mal_data
my_mal_data <- my_mal_data %>%
  mutate(region = gsub(" region$", "", region, ignore.case = TRUE)) 

#removing the word "region" from the values under region from RDT data
RDT_data <- RDT_data %>%
  mutate(region = gsub(" region$", "", region, ignore.case = TRUE)) 


#removing unwated columns- i don't know how they showed up
my_mal_data <-
  subset(my_mal_data, select = -10) #-10 is the column number i deleted

#created a data frame for only RDT positive cases
confirmed_cases <- RDT_data %>%
  filter(Data== "RDT positive")

#created a data frame for morbidity cases
morbidity_data <- my_mal_data %>%
  filter(outcome == "Morbidity")

#created a data frame for mortality cases
mortality_data <- my_mal_data %>%
  filter(outcome == "Mortality")
#created a data frame for clinically diagnosed malaria cases
clinical_dx_mal <- my_mal_data %>%
  filter(Data=="mal-no parastological confirmation")

#summarising the total cases in each region
malaria_case_summary <- my_mal_data %>%
  group_by(region) %>%
  summarise(total_cases = sum(Value), .groups = 'drop')


#creating confirmed and presumed summary table
conf_pres_summary <- my_mal_data %>%
  select(Data, region, Value) %>%
  group_by(region,Data) %>%
  summarise(sum(Value)) %>%
  pivot_wider(names_from = Data, values_from = `sum(Value)`) %>%

#creating presumed and confirmed columns
conf_pres_summary$confirmed <- conf_pres_summary$`Malaria due to Plasmodium malariae`+ conf_pres_summary$`Malaria due to Plasmodium ovale`+ conf_pres_summary$`Other parasitologically confirmed malaria`+ conf_pres_summary$`PF malaria`+ conf_pres_summary$`PV malaria`+ conf_pres_summary$`mixed malaria-PF/PV`
conf_pres_summary$presumed <- conf_pres_summary$ 'mal-no parastological confirmation'

#keeping region, confirmed and presumed cases column only
conf_pres_summary <- conf_pres_summary %>%
  select(region, presumed, confirmed)

#changing the presumed and confirmed summary to longer data
conf_pres_summary <- conf_pres_summary %>%
  pivot_longer(cols = c("presumed", "confirmed"), names_to = "case type", 
               values_to = "count")



  