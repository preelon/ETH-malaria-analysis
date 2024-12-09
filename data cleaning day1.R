install.packages("tidyverse")
library(tidyverse)
library("janitor")
library(lubridate)

browseURL("https://github.com/asiraj-nd/Migration_model/blob/master/data_cleaning_flow_chart.pdf")

# Provided functions
month_to_cmc <- function(date) {
  year = year(date)
  month = month(date)
  
  cmc = (year - 1900) * 12 + month
  return(cmc)
}

cmc_to_month <- function(cmc) {
  proc_cmc <- data.frame(year = floor(cmc / 12) + 1900) %>%
    mutate(month = cmc - (year - 1900) * 12) %>%
    mutate(ym_text = case_when(month == 0 ~ paste0(year - 1, "-", 12),
                               TRUE ~ paste0(year, "-", month)))
  
  return(ym(proc_cmc$ym_text))  
}

ethiopian_to_greg <- function(cmc = NULL){
  
  ey = trunc((cmc-1)/12)+1900
  em = cmc-(ey-1900)*12
  ed = 1
  
  joffset = 1723856
  n = 30*(em -1)  + ed - 1 # ed - 1 if actually 0
  jd = joffset + 365 + 365*(ey-1) + trunc(ey/4) + n
  
  z = jd + 0.5
  w = trunc((z-1867216.25)/36524.25)
  x = trunc(w/4)
  a = z+1+w-x
  b = a+1524
  c = trunc((b-122.1)/365.25)
  d = trunc(365.25*c)
  e = trunc((b-d)/30.6001)
  f = trunc(30.6001*e)
  day = b-d-f
  
  month = e-1
  
  month.low  <-(month<=12)*(e-1)
  month.high <-(month>12)*(e-13)
  month = month.low+month.high
  
  year.low <- (month < 3)*(c-4715)
  year.high <-(month>=3)*(c-4716)
  year = year.low + year.high
  
  outcmc = 12*(year - 1900)+month
  return(outcmc)
} 

# Ensure year and mon are numeric before conversion
my_mal_data <- my_mal_data %>%
  mutate(
    year = as.numeric(year),
    mon = as.numeric(mon)
  )

# Check for any NA values after conversion
if (any(is.na(my_mal_data$year)) || any(is.na(my_mal_data$mon))) {
  stop("There are non-numeric values in the year or mon columns.")
}

#uploading the csv file
#my_mal_data <- read.csv("data/malaria data.csv")
RDT_data <- read.csv("data/RDT data.csv") 

#loading user defined functions
#Function to check if an Ethiopian year is a leap year

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
my_mal_data_updated <- my_mal_data %>%
  mutate(data_element_new = gsub(".*?- ", "", data_element)) 

table(my_mal_data$data_element_new)

#standardize region names
my_mal_data_updated <- my_mal_data %>%
  mutate(region= str_to_title(organisation_unit))

#find names to modify
my_mal_data %>% 
 # filter(grepl("Region", region, ignore.case = FALSE, 
              # perl = FALSE, fixed = FALSE, useBytes = FALSE)) %>%
  distinct(region)

#removing region from the regional name
my_mal_data_updated <- my_mal_data %>% 
  mutate(region= gsub(" Region", "", organisation_unit))

#removing Ethiopia
my_mal_data_updated <- my_mal_data_updated %>%
  mutate(region= gsub(" Ethiopian", "", region)) %>%
  mutate(region= gsub(" Ethiopia", "", region)) 

# remove city administration from Addis and Diredawa
my_mal_data_updated <- my_mal_data_updated %>%
  mutate(region= gsub(" City Administration", "", region))

#changing to title case

#trimming the data name from RDT_data
RDT_data$Data <- gsub(".*?or ", "", RDT_data$Data)


#renaming malaria diagnosis under data to RDT performed
RDT_data <- RDT_data %>%
  mutate(Data = recode(Data, 
                       `malaria diagnosis` = "RDT performed",
                       `RDT Positive` = "RDT positive"))

#replacing long data element names into shorter ones
my_mal_data_updated <- my_mal_data_updated %>%
  mutate(data_element_new = recode(data_element,
                       " Malaria due to Plasmodium falciparum associated with Malaria due to Plasmodium Vivax (Mixed Malaria)" = "mixed malaria-PF/PV",
                       "Malaria due to Plasmodium vivax" = "PV malaria",
                       "Malaria due to Plasmodium falciparum" = "PF malaria",
                       "Malaria without parasitological confirmation" = "mal-no parastological confirmation"),
         "Malaria due to Plasmodium ovale" = "PO malaria",
         "Other parasitologically confirmed malaria" = "other parastologically confirmed",
         "Malaria due to Plasmodium malariae"= "PM malaria")


#renaming age column in short in my_mal_data
my_mal_data_updated <- my_mal_data_updated %>%
  rename(age_range= `age_in_years_1_1_4_5_14_15_29_30_64_65`)

#renaming age column in short in RDT_data
RDT_data <- RDT_data %>%
  rename(age= age)

#replacing "to" as indicator of age range to a hyphen in my_mal_data
my_mal_data_updated <- my_mal_data_updated %>%
  mutate(age_range= gsub("to", "-", age_range))


#replacing "to" as indicator of age range to a hyphen in RDT data
RDT_data$age <- gsub("to", "-", RDT_data$age)

#renaming OPD IPD in short in my_mal_data
my_mal_data_updated <- my_mal_data_updated %>%
 rename(dept =departments_opd_ipd_categories)

#renaming outcome in short in my_mal_data
my_mal_data_updated <- my_mal_data_updated %>%
  rename(outcome = outcome_morbidity_mortality)

unique(my_mal_data$period)

# Separate the 'period' column into 'month' and 'year' in my_mal_data
my_mal_data_updated <- my_mal_data_updated %>%
  mutate(Period = as.character(period)) %>%  # Ensure 'period' is character type
  separate(Period, into = c("month", "year"), sep = " ")

#ethiopian months assignment
ethiopian_months <- data.frame(mon= 1:12, eth_mon= c("Meskerem", "Tikemet", "Hidar", "Tahisas", "Tir", "Yekatit", "Megabit", "Miyazia", "Ginbot", "Sene", "Hamle", "Nehase")) 




#Removing unnecessary columns (col 11,12&13)
#my_mal_data_updated <- my_mal_data [, -c(11, 12, 13)]

# Convert Ethiopian dates to CMC and then to Gregorian date
#creating number for months
my_mal_data_updated <- my_mal_data_updated %>%
  #left_join(ethiopian_months, by = c("month" = "eth_mon"))%>%
  mutate(year= as.numeric(year)) %>%
  mutate(cmc = ((year-1900) *12 + mon)) %>% # Convert Ethiopian Year and Month to CMC
        mutate(gregorian_date_cmc = ethiopian_to_greg(cmc)) %>% #converting eth_cmc to greg_cmc
  mutate(greg_period = cmc_to_month(gregorian_date_cmc)) # Convert greg CMC to Gregorian Date directly


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

library(usethis)
library(gh)

gh::gh_whoami()

  