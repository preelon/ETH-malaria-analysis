#Assignments given by Amir
#step 1: Convert Ethiopian Sen 2016 to "20161001" 
#step 2: Convert "20161001" to date format of YMD

#my answers
#defining the year anbd the month
year <- 2016
month <- 10

#create the string in the desired format
ethiopian_date_string <- paste0(year, sprintf("%02d", month), "01")
print(ethiopian_date_string)

#converting the string to date format
date_object <- ymd(ethiopian_date_string)
print(date_object)

#Alternatively we can use the numeric method as follows
output1 <- year*10000
output2 <- month*100
final_output= output1 + output2 +01