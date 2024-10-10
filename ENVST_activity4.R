#load packages dplyr and lubridate
library(dplyr)
library(lubridate)
library(ggplot2)

#read in data
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                     na.strings = "#N/A")

#parse the date data, call "dateF" for date formatted
weather$dateF <- mdy_hm(weather$Date)

#before setting up a function, test out the operation to make sure it does what you want
interval <- weather$dateF[2] %--% weather$dateF[1]
interval

#create for entire vector
#note: can't construct a time interval for the first observation and the last observation
interval_whole <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval_whole

#set up a function to repeatedly check intervals in the data 
timeInterval <- function(x){
  x[-length(x)] %--% x[-1]
}

#new concept: for loop
#allows us to iteratively complete a task, lowkey computationally inefficient though since R is vector based 
#(for loops will take a long time for lots and lots of data)
#curly brackets indicate we will put code that will do a series of operations that will be referenced later on (either in a function or through iterative process)
for(i in 1:6){
  print(paste("example", i))
}

seq_ex <- c(1, 4, 6)

for(i in seq_ex){
  print(paste("example", i))
}

#turn sequence into character string, need to create an empty vector
char_ex <- character()
for(i in 1:6){
  char_ex[i] <- paste("example", i)
}
#this puts character data into the vector 

#do it for numeric
num_ex <- numeric()
for(i in 1:6){
  num_ex[i] <- 6*i
}
#using i indicates that the operation will be applied to and saved within each slot in the vector

#can use any string of numbers you want, but you have to be mindful of your existing vector length
num_ex2 <- numeric()
for(i in 2:6){
  num_ex2[i] <- 6*i
}
#if the sequence of numbers doesn't match up with vector length, R will automatically populate the vector slot with "NA"


### in-class prompts
#Prompt 1: Calculate a rolling average of air temperatures over eight 15 min measurements (2 hours) for January of 2022 using a for loop. 
#Make a plot of the 15 minute air temperature and the rolling average.
#create a month column
weather$month <- month(weather$dateF)
#create a year column 
weather$year <- year(weather$dateF)

#filter to isolate Jan 2022
Jan22 <- weather %>%
  filter(month == 1 & year == 2022)

#check mean of first 8 observations
mean(Jan22$AirTemp[1:8])

#set up empty numeric vector
roll_avg_temp <- numeric()

#create the function
for(i in 8:nrow(Jan22)){
  roll_avg_temp[i] <- mean(Jan22$AirTemp[(i-7):i])
}

#create column in Jan22
Jan22$roll_avg_temp <- roll_avg_temp


#Prompt 2: see if the solar radiation measurements experienced any issues with 
#build up or accumulation on the sensor in May and June of 2021.

#need to filter to isolate May and June 2021
MayJune21 <- weather %>%
  filter(year == 2021 & month %in% c(5, 6))
#it does not appear as though there were any issues with buildup or accumulation on the sensor

#Prompt 3: Check for any date time issues using the function created in the tutorial. 
#Investigate instances of date time issues. What happens around daylight savings? 
#Are there issues with the time zone assumption?
# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row

# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]
#around Daylight Savings in March, the interval is much longer than 900 seconds, since the time jumps ahead an hour
#since we haven't specified the timezone, it appears all the data has defaulted to the UTC timezone

### Homework Prompts
#Q1: As the weather station data manager, you been asked to share precipitation data with 
#the village of Clinton. You want to ensure that there are no issues with the bird excrement 
#or frozen precipitation. You want to exclude any precipitation that occurs when the air 
#temperature is below zero. You also want to check that no precipitation measurements are 
#used if the X and Y level observations are more than 2 degrees.

weather$precip.QC <- ifelse(is.na(weather$Precip) | weather$AirTemp < 0 | 
                              weather$XLevel > 2 | weather$YLevel > 2, 
                            NA, # set NA for missing, air temp below 0, or X or Y level above 2
                            weather$Precip) # keep valid values

#find the number of missing values
sum(is.na(weather$precip.QC))
#which is 14094

#Q2: Create a data flag that warns a user if the battery voltage falls below 8.5 Volts. 
#Explain how you set up the flag.
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
#battery volts values are in mV, so 8.5 volts is 8500 mV

weather$BatteryFlag <- ifelse(weather$BatVolt < 8500, # check if at or below 8500 mV
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

#Q3: You should also create a function that checks for observations that are in unrealistic 
#data ranges in air temperature and solar radiation. Explain how your function works.
realistic_weather <- function(x) {
  clean_x <- x[x$AirTemp >= -15 & x$AirTemp <= 29 & 
               x$SolRad >= 0 & x$SolRad <= 1000, ]
}

#Q4: Make a plot of winter air temperatures in Jan - Mar of 2021. Check for persistence issues that 
#might indicate snow accumulation on the sensor. Describe whether you think this might be an issue.

#step 1 need to filter for a Jan-Mar 2021 data set
JanMar21 <- weather %>%
  filter(year == 2021 & month %in% c(1, 2, 3))

#check for accumulation or persistence issues
ggplot(JanMar21,
       aes(x = dateF, y = AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()
#graphs looks good to me

#want to get a daily avg so it's easier to see when you plot
JanMar21_daily <- JanMar21 %>%
  group_by(doy) %>%
  summarize(AirTemp_avg = mean(AirTemp, na.rm = TRUE))

#now plot as a line graph
ggplot(JanMar21_daily,
       aes(x = doy, y = AirTemp_avg)) + 
  geom_line(color = "steelblue") + 
  labs(title = "Daily Average Winter Air Temperature (Jan-March 2021)", 
       x = "Day of Year", 
       y = "Air Temperature (°C)") + 
  geom_hline(yintercept = 0, color = "red")


#Q5: You are asked for total daily precipitation in March and April of 2021. Use a for loop to 
#exclude (convert to NA) any days that include temperatures less than 35 degrees F on that day 
#or the day prior to ensure that measurements are not likely to be affected by snow accumulation 
#on the sensor. How many daily observations have precipitation observations (not a NA) in your final data table?

#first need to convert air temp to F (from C)
weather$AirTempF<- weather$AirTemp * (9/5) + 32

#next need to filter for March and April 2021
MarApr21 <- weather %>%
  filter(year == 2021 & month %in% c(3, 4)) %>%
  group_by(doy) %>%
  summarize(daily_precip = sum(Precip, na.rm = TRUE),  # Total daily precipitation
            min_temp = min(AirTempF, na.rm = TRUE))  # Minimum temp within the day

#create the for loop over each row and check temperature conditions
for (i in 2:nrow(MarApr21)) {  # Start from 2 because we compare with the previous day
  
  # Get the current and previous day's minimum temperatures in Fahrenheit
  current_temp <- MarApr21$min_temp[i]
  previous_temp <- MarApr21$min_temp[i - 1]
  
  # If the current or previous day's min temp is below 35°F, set daily_precip to NA
  if (current_temp < 35 | previous_temp < 35) {
    MarApr21$daily_precip[i] <- NA
  }
}

#lastly need the sum of total daily precipitation for March and April, excluding NA values
total_precip <- sum(MarApr21$daily_precip, na.rm = TRUE)
#the total precip for March and April 2021 is 48.282 mm

#find the number of rows for which there is an observation 
61 - sum(is.na(MarApr21$daily_precip))
#there are 19 rows with an observation






  
