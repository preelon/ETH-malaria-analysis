install.packages("tidyverse")
library(tidyverse)
#uploading the csv file
my_mal_data <- read.csv("data/malaria data.csv")
RDT_data <- read.csv("data/RDT data.csv")
# 
#trimming the data name
my_mal_data$Data <- gsub(".*?- ", "", my_mal_data$Data)

unique(my_mal_data$Data)

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


#renaming age column in short
my_mal_data <- my_mal_data %>%
  rename(age= Age.in.years...1..1.4..5.14..15.29..30.64..65..)

#replacing "to" as indicator of age range to a hyphen
my_mal_data$age <- gsub("to", "-", my_mal_data$age)

#renaming OPD IPD in short
my_mal_data <- my_mal_data %>%
  rename(dept = Departments.OPD.IPD.Categories)

#renaming outcome in short
my_mal_data <- my_mal_data %>%
  rename(outcome = Outcome..Morbidity..Mortality.)

# Separate the 'period' column into 'month' and 'year'
my_mal_data <- my_mal_data %>%
  mutate(Period = as.character(Period)) %>%  # Ensure 'Period' is character type
  separate(Period, into = c("month", "year"), sep = " ")

#renaming organizational data with region
my_mal_data <- my_mal_data %>%
  rename(region=Organisation.unit)

#removing unwated columns- i don't know how they showed up
my_mal_data <-
  subset(my_mal_data, select = -10) #-10 is the column number i deleted
