library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)

#loading the data for list of organization for 3 regions
structural_units <- read_csv ("data/three_region_district_list.csv") %>%
  janitor::clean_names()

#loading phem data
phem_dat <- read_csv("data/phem_three_region.csv")

#creating a data frame of unique region names from phem data
region_name_practice= data.frame(region= unique(phem_dat$Region))

#filtering out the region names that are found in region_name_practice but not in structural_unit                        
region_name_practice %>%
  filter(!region %in% structural_units$region)%>% #this keeps rows which are found in region_name_practice
  arrange(region)                                 # but not in structual_unit data

unique(phem_dat$Region)
unique(structural_units$region)

#removing white space from both side of Region in phem_dat
phem_dat <- phem_dat %>%
  mutate(Region= str_trim(Region, side="both"))

#replacing Benishangul-Gumuz and Benishangul Gumuz-Gumu in phem data with Benishangul_Gumuz
phem_dat <- phem_dat %>%
  mutate(region_new= case_when(Region %in% c("Benishangul-Gumuz", "Benishangul Gumuz-Gumuz") ~ "Benishangul_Gumuz", TRUE ~ Region))

unique(phem_dat$region_new)


#Double checking the joined dataset to see if there are any non-matching names left
phem_dat %>%
  filter(!region_new %in% structural_units$region)%>% #this keeps rows which are found in region_name_practice
  arrange(region_new)
