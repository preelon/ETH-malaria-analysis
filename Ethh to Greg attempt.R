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

# Ensure year and month_number are numeric before conversion
my_mal_data <- my_mal_data %>%
  mutate(
    year = as.numeric(year),
    mon = as.numeric(mon)
  )

# Check for any NA values after conversion
if (any(is.na(my_mal_data$year)) || any(is.na(my_mal_data$mon))) {
  stop("There are non-numeric values in the year or mon columns.")
}

# Convert Ethiopian dates to CMC and then to Gregorian date
my_mal_data <- my_mal_data %>%
  mutate(cmc = (year *12 + mon), # Convert Ethiopian Year and Month to CMC
         gregorian_date_cmc = ethiopian_to_greg(cmc), # Convert CMC to Gregorian Date
         gregorian_date = as.Date(sprintf("%d-%02d-%02d", 
                                          year(gregorian_date_cmc), 
                                          month(gregorian_date_cmc), 
                                          day(gregorian_date_cmc)))) # Format as Date

# Display the updated data frame with the new column
print(my_mal_data)



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

ethiopian_to_greg <- function(cmc = NULL) {
  ey = trunc((cmc - 1) / 12) + 1900
  em = cmc - (ey - 1900) * 12
  ed = 1
  
  joffset = 1723856
  n = 30 * (em - 1) + ed - 1 # ed - 1 if actually 0
  jd = joffset + 365 + 365 * (ey - 1) + trunc(ey / 4) + n
  
  z = jd + 0.5
  w = trunc((z - 1867216.25) / 36524.25)
  
  x = trunc(w / 4)
  
  a = z + 1 + w - x
  b = a + 1524
  c = trunc((b - 122.1) / 365.25)
  
  d = trunc(365.25 * c)
  
  e = trunc((b - d) / 30.6001)
  
  day = b - d - trunc(30.6001 * e)
  
  month = e - (ifelse(e >13,13,0)) # Adjust month if it's greater than December
  
  # Correctly assign year based on month calculation
  year <- ifelse(month <3, c-4715, c-4716)
  
  # Return a Date object instead of CMC
  return(as.Date(sprintf("%d-%02d-%02d",as.integer(year),as.integer(month), as.integer(day))))
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

# Convert Ethiopian dates to CMC and then to Gregorian date
my_mal_data <- my_mal_data %>%
  mutate(cmc = (year *12 + mon), # Convert Ethiopian Year and Month to CMC
         gregorian_date_cmc = ethiopian_to_greg(cmc)) # Convert CMC to Gregorian Date directly

# Check intermediate results for debugging
print(my_mal_data %>% select(cmc, gregorian_date_cmc))

# Display the updated data frame with the new column
print(my_mal_data)


