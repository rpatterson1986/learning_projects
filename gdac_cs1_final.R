#Cyclistic bike-share analysis case study; Differences between Casual vs. Member riders
#Author: Robert C. Patterson Jr. / Kaggle user name: DontquestionIt
#5/14/2022
################################################################################################



#getwd()
#setwd()
install.packages("tidyverse") #Essential data analysis package(s)
install.packages("dplyr") #More data wrangling assistance
install.packages("lubridate") #To handle date times better
install.packages("janitor") #Easier data cleaning and to use compare_df_cols()
install.packages("stringr") #To capitalize values
install.packages("skimr") #to use skim without charts()
install.packages("hms") #For converting values into h:m:s
install.packages("ggplot2") #To create plots with our clean data
library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(stringr)
library(skimr)
library(hms)
library(ggplot2)

#Loading initial data sets to assign them as variables/DFs
apr2021 <- read.csv("202104-divvy-tripdata.csv")
may2021 <- read.csv("202105-divvy-tripdata.csv")
jun2021 <- read.csv("202106-divvy-tripdata.csv")
jul2021 <- read.csv("202107-divvy-tripdata.csv")
aug2021 <- read.csv("202108-divvy-tripdata.csv")
sep2021 <- read.csv("202109-divvy-tripdata.csv")
oct2021 <- read.csv("202110-divvy-tripdata.csv")
nov2021 <- read.csv("202111-divvy-tripdata.csv")
dec2021 <- read.csv("202112-divvy-tripdata.csv")
jan2022 <- read.csv("202201-divvy-tripdata.csv")
feb2022 <- read.csv("202202-divvy-tripdata.csv")
mar2022 <- read.csv("202203-divvy-tripdata.csv")

#Inspecting sample size of the data sets before merging all of it into one data set.

#Comparing columns.
colnames(apr2021)
colnames(oct2021)
colnames(feb2022)

##
#as.tibble space for markdown/report***Replace as needed/wanted when creating r Markdown.
as_tibble(colnames(apr2021))
as_tibble(colnames(oct2021))
as_tibble(colnames(feb2022))

#Utilizing compare_df_cols() with the janitor package.
as_tibble(compare_df_cols(apr2021, oct2021, feb2022))

#Utilizing glimpse() from the dplyr package instead of str().
glimpse(apr2021)
glimpse(oct2021)#Discovered NAs or "" / blank values - here. **Remember 
#start_station_name for later.
glimpse(feb2022)

as_tibble(apr2021)
as_tibble(oct2021) 
as_tibble(feb2022)

#Compare data types for columns of all data sets to ensure they're consistent
compare_df_cols(apr2021, may2021, jun2021, jul2021, aug2021,
                sep2021, oct2021, nov2021, dec2021, jan2022,
                feb2022, mar2022)

#bind_rows() from dplyr package to combine data frames by rows.
test_df <- bind_rows(apr2021, may2021, jun2021, jul2021, aug2021,
                     sep2021, oct2021, nov2021, dec2021, jan2022,
                     feb2022, mar2022)

#Utilizing the dplyr package for renaming the column "member_casual" to 
#"user_type".
test_df <- test_df %>%
  rename(user_type = member_casual)

#Capitalizing user types; Need to fix for vizes.
test_df$user_type = str_to_title(test_df$user_type)

#Verifying set values for user_type and rideable_type.
table(test_df$user_type)
table(test_df$rideable_type)

#Various inspection views of the new data frame here.
colnames(test_df)
as_tibble(test_df)
summary(test_df)
glimpse(test_df) 

#Skim_without_charts() from skimr package to obtain a more detailed overview.
#Also to get a better idea of what's going on with our NAs and "" values.
#Notice how start_station_name does not come as "NA" but our lat/lng end 
#values are accounted for in "n_missing". "" == "empty"; NA == "n_missing".
skim_without_charts(test_df)

#Remove NA values
test_df <- na.omit(test_df)

################################################################################
################################################################################
################################################################################

#For our case study purposes, we're not going to consider the blanks or "" in the station
#id and names as bad data in regards to our time stamps for rides. 
#Without further investigation into the matter, I feel removing these records 
#will reflect a more biased outcome vs not removing them.

#If we're to dive even further to see how station/location played a
#role, we'll have to validate these records. Yes, we already can with the provided
#data but validating learning case studies via deductive reasoning. But I feel,
#that's beyond this project's scope at this time.

################################################################################
################################################################################
################################################################################



#Will now explore station names to verify no test/bad data.
################################################################################
#Inspiration/solution for cleaning station names found in:
#Narro (2022, April 30). Eric Narro's blog: Cyclistic - Comparing members vs casual users. 
#Retrieved from https://enarroied.github.io/posts/cyclistic/
#seriously - Eric's case study is the best. Learned a lot while reviewing his work/findings.
################################################################################
#
#Create dfs just our start/end  and lat/lng columns
station1 <- test_df[c("start_station_name", "start_station_id", "start_lat", "start_lng")]
station2 <- test_df[c("end_station_name", "end_station_id", "end_lat", "end_lng")]

#Rename columns for our purposes.
station1 <- rename(station1, station_id ="start_station_id", name = "start_station_name",
                   lat = "start_lat", lng = "start_lng")

station2 <- rename(station2, station_id ="end_station_id", name = "end_station_name",
                   lat = "end_lat", lng = "end_lng")

#Merge our two data frames into a combined new one.
stations <- rbind(station1, station2)

#To keep unique combinations of extracted data..
stations <- unique(stations[c("station_id", "name", "lat", "lng")])

#order by station id
stations <- stations[order(stations$station_id),]

#We still have station names being repeated due to various lat/lng variations.
#Organizing by setting the lat/lng values as mean to narrow down station names via aggregate()
stations_lat <- aggregate(stations$lat, list(stations$station_id, stations$name), FUN=mean )
stations_lng <- aggregate(stations$lng, list(stations$station_id, stations$name), FUN=mean )

#Create data frame to store newly aggregated data
stations2 <- data.frame(station_id = stations_lat$Group.1,
                        name = stations_lat$Group.2, lat = stations_lat$x,
                        lng = stations_lng$x)

#The "$Group.1", "$Group.2" came from the newly created stations_lat data frame

#To remove additional empty row
stations2 <- stations2[-c(1),]

#Remove duplicates
stations2 <- stations2 %>%
  distinct(station_id, .keep_all = TRUE)

#write file for in-depth investigation in excel
write.csv(stations2, "stations.csv", row.names = FALSE)

#Results found of "bad data". See "1st_clean.R" for details.
###351 *will investigate id further with this one.
bad_df <- test_df[c("start_station_name", "start_station_id", "user_type",
                    "date", "end_station_name",
                    "end_station_id", "duration_sec", "duration_min", "rideable_type")]
bad_df <- bad_df %>%
  filter(start_station_id == "351") #checked start/end station ids here

#Only two records contain start id and start station name as "351"
#A lot of empties but this seems to be a legit station for customers. Will leave 351 alone
#as it seems to be a flake.
#Worth noting that this station started to appear in data since August...
#^Would require further confirmation from proper dept heads/SMEs.

#Final list for easy ref:

#Bissell St & Armitage Ave - Charging
#DIVVY CASSETTE REPAIR MOBILE STATION
#Hastings WH 2
#Lincoln Ave & Roscoe St - Charging
#Pawel Bialowas - Test- PBSC charging station
#Throop/Hastings Mobile Station
#Wilton Ave & Diversey Pkwy - Charging
#WEST CHI-WATSON
#Base - 2132 W Hubbard Warehouse

#IDs:
#ID - DIVVY 001 *only tied to WEST CHI-WATSON.

#Removing bad data here:
#should have 5723532 objects
test_df <- test_df %>%
  filter(start_station_name != "Bissell St & Armitage Ave - Charging") %>%
  filter(start_station_name != "DIVVY CASSETTE REPAIR MOBILE STATION") %>%
  filter(start_station_name != "Hastings WH 2") %>%
  filter(start_station_name != "Lincoln Ave & Roscoe St - Charging") %>%
  filter(start_station_name != "Pawel Bialowas - Test- PBSC charging station") %>%
  filter(start_station_name != "Throop/Hastings Mobile Station") %>%
  filter(start_station_name != "Wilton Ave & Diversey Pkwy - Charging") %>%
  filter(start_station_name != "WEST CHI-WATSON") %>%
  filter(start_station_name != "Base - 2132 W Hubbard Warehouse") %>%
  filter(end_station_name != "Bissell St & Armitage Ave - Charging") %>%
  filter(end_station_name != "DIVVY CASSETTE REPAIR MOBILE STATION") %>%
  filter(end_station_name != "Hastings WH 2") %>%
  filter(end_station_name != "Lincoln Ave & Roscoe St - Charging") %>%
  filter(end_station_name != "Pawel Bialowas - Test- PBSC charging station") %>%
  filter(end_station_name != "Throop/Hastings Mobile Station") %>%
  filter(end_station_name != "Wilton Ave & Diversey Pkwy - Charging") %>%
  filter(end_station_name != "WEST CHI-WATSON") %>%
  filter(end_station_name != "Base - 2132 W Hubbard Warehouse")
#5722311 objects remaining

#Leaving entries with "(Temp)" alone; Would require further confirmation 
#from proper dept heads/SMEs.

#California Ave & Francis Pl (Temp)
#Franklin St & Adams St (Temp)
#Pulaski Rd & Eddy St (Temp)
#Wentworth Ave & 24th St (Temp)
#Wood St & Taylor St (Temp)
################################################################################



##Breakdown thought process for handling date/time data types and concerns in R Markdown
################################################################################
#Will now create additional columns for day, day of week, month and year from 
#started_at date for further in-depth analysis.

test_df$date <- as.Date(test_df$started_at) #Default format is yyyy-mm-dd
test_df$month <- format(as.Date(test_df$date), "%b_%y")#Example - "Apr_21"
test_df$day <- format(as.Date(test_df$date), "%d")
test_df$year <- format(as.Date(test_df$date), "%Y")
test_df$day_of_week <- format(as.Date(test_df$date), "%a")

#Testing/validating to ensure no NA here to surprise us; Extensive validating due to date/time data type.
table(test_df$month, useNA = c("ifany"))
table(test_df$day_of_week, useNA = c("ifany"))
table(test_df$day, useNA = c("ifany"))
table(test_df$year, useNA = c("ifany"))

##Will address the data type concerns regarding date/time here.

#Change started/ended at from character to data type; POSIXct
test_df$started_at <- as.POSIXct(test_df$started_at, 
                                 format ="%Y-%m-%d %H:%M:%S")
test_df$ended_at <- as.POSIXct(test_df$ended_at, 
                               format ="%Y-%m-%d %H:%M:%S")

##Breakdown thought process for handling date/time data types and concerns along the way*
################################################################################

#Extract HH:MM from started/ended_at to hold as separate variable.
test_df$stime_hm <- format(test_df$started_at, format = "%H:%M")
test_df$etime_hm <- format(test_df$ended_at, format = "%H:%M")

#Need to ensure our hm values are in POSIXct for vizes.
test_df$stime_hm <- as.POSIXct(test_df$stime_hm, format = "%H:%M")
test_df$etime_hm <- as.POSIXct(test_df$etime_hm, format = "%H:%M")

##Breakdown thought process for handling date/time data types and concerns along the way*
################################################################################

#Convert POSIXct data type from started/ended_at into an integer; 
#Time value into seconds from 01/01/1970 ; Computer Output != Perception of time
test_df$stime <- as.integer(test_df$started_at)
test_df$etime <- as.integer(test_df$ended_at)

#Calculating rider duration; displaying results in seconds, then minutes
#and then hours.
test_df$duration_sec <- test_df$etime - test_df$stime
test_df$duration_min <- as.integer(test_df$duration_sec / 60) #Removing decimal; Output != Perception of time
test_df$duration_hr <- as.integer(test_df$duration_sec / 3600) #Removing decimal; Output != Perception of time

##Using the as_hms() from the hms r package in regards to "Output != Perception of time"
##Ref - https://www.rdocumentation.org/packages/hms/versions/0.4.1/topics/hms

test_df$duration_hms <- as_hms(test_df$duration_sec)

#Setting order for the day_of_week and month columns for organized analysis.
test_df$day_of_week <- ordered(test_df$day_of_week,
                               levels=c("Sun", "Mon", "Tue", "Wed", "Thu",
                                        "Fri", "Sat"))

test_df$month <- ordered(test_df$month,
                         levels=c("Apr_21", "May_21", "Jun_21", "Jul_21", 
                                  "Aug_21", "Sep_21", "Oct_21", "Nov_21",
                                  "Dec_21", "Jan_22", "Feb_22", "Mar_22"))



#Will now save a .csv file of the combined data set for Tableau, SQL, etc. use; If needed/wanted.

write.csv(test_df,'total_data.csv', row.names = FALSE)

###############################################################################
###############################################################################

#Will remove more "bad data" meeting the following criteria:
#*Casual users that are logged for over 24 hours; Full day pass is not clearly 
#defined in case study..
####^Decided to remove all records over 24 hours.
#*Any negative value in duration sec; Upon further thought, we'll remove any 
#time logged under 2 minutes. min() should not be 0.

#^
#Title: How Long Does It Take To Bike A Mile?
#Author:Mick White
#Link: https://bicycleuniverse.com/how-long-to-bike-mile/#Flat_Road

###############################################################################

#Creating new data frame without records of 86400+ seconds(secs in a day) 
#and two minutes or less.

all_trips <- test_df %>%
  filter(duration_min >= 2)%>%
  filter(duration_sec <= 86400)

#Review the cleaned and filtered data frame
skim_without_charts(all_trips)

#Descriptive analysis on duration in seconds and minutes.
summary(all_trips$duration_sec, mean, median, max, min)
summary(all_trips$duration_min, mean, median, max, min)

#Compare causal and member users in seconds
setNames(aggregate(all_trips$duration_sec ~ all_trips$user_type, FUN = mean), 
         c("Customer Type", "Average Seconds"))
setNames(aggregate(all_trips$duration_sec ~ all_trips$user_type, FUN = median),
         c("Customer Type", "Median (sec)"))
setNames(aggregate(all_trips$duration_sec ~ all_trips$user_type, FUN = max),
         c("Customer Type", "Max (sec)"))
setNames(aggregate(all_trips$duration_sec ~ all_trips$user_type, FUN = min), 
         c("Customer Type", "Min (sec)")) #120 seconds or above; need further verification over ride times under 2 mins

#Compare causal and member users in minutes
setNames(aggregate(all_trips$duration_min ~ all_trips$user_type, FUN = mean), 
         c("Customer Type", "Average Minutes"))
setNames(aggregate(all_trips$duration_min ~ all_trips$user_type, FUN = median),
         c("Customer Type", "Median (min)"))
setNames(aggregate(all_trips$duration_min ~ all_trips$user_type, FUN = max),
         c("Customer Type", "Max (min)"))
setNames(aggregate(all_trips$duration_min ~ all_trips$user_type, FUN = min), 
         c("Customer Type", "Min (min)")) #2 minutes or above; need further verification over ride times under 2 mins

#Average ride time by each day for casual vs. member users. Both in seconds and minutes.
setNames(aggregate(all_trips$duration_sec ~ all_trips$user_type + 
                   all_trips$day_of_week, FUN = mean),
         c("Customer Type", "Weekday", "Average (sec)"))

setNames(aggregate(all_trips$duration_min ~ all_trips$user_type + 
                   all_trips$day_of_week, FUN = mean),
         c("Customer Type", "Weekday", "Average (min)"))

#Alternative view of the average ride time by each day for casual vs. member,
#by weekday per user type. Both sec & min.
setNames(aggregate(all_trips$duration_sec ~ all_trips$day_of_week + 
                   all_trips$user_type, FUN = mean),
         c("Weekday", "Customer Type", "Average (sec)"))

setNames(aggregate(all_trips$duration_min ~ all_trips$day_of_week +
                   all_trips$user_type, FUN = mean),
         c( "Weekday", "Customer Type", "Average (min)"))

#Analyze ridership data with amount of rides by user type per weekday.
all_trips %>% 
  group_by(user_type, day_of_week) %>%  #groups by user_type and day_of_week
  summarise(number_of_rides = n(),	#calculates the number of rides and average duration 
            average_sec = mean(duration_sec),
            average_min = mean(duration_min)) %>% #calculates the average duration
  arrange(user_type, day_of_week)	#sorts

#Visualize the number of rides by user type per weekday.
all_trips %>% 
  group_by(user_type, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = user_type)) +
  geom_col(width= 0.75, position = position_dodge(width = 0.75)) +
  labs(x = "Weekday", y = "Number of Rides", 
       title = "Number of Rides for Each Weekday Per Type of Customer", 
       fill = "Customer Type",
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_fill_manual(values=c("#1e90ff", "#dc582a" )) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
#Label setting converts output to actual number instead of showing the 
#amount of digits as exponents.

#Horizontal alignment per weekday if desired; *DO NOT USE - LEAVING HERE FOR FINAL REPORT.
all_trips %>% 
  group_by(user_type, day_of_week) %>%
  summarise(number_of_rides = n()) %>% 
  arrange(user_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = user_type)) +
  geom_col(width= 0.75, position = position_dodge(width = 0.75)) +
  labs(x = "Weekday", y = "Number of Rides",
       title = "Number of Rides for Each Weekday Per Type of Customer",
       fill = "Customer Type",
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_fill_manual(values=c("#1e90ff", "#dc582a" )) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  coord_flip()

#^Horizontal alignment fixed/completed;
all_trips %>% 
  group_by(user_type, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user_type, day_of_week)  %>% 
  mutate(day_of_week = factor(day_of_week, levels=c( "Sat", "Fri", "Thu", "Wed",
                                                     "Tue", "Mon", "Sun"))) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = factor(user_type,
             levels = c("Member", "Casual")))) +
  geom_col(width= 0.75, position = position_dodge(width = 0.75)) +
  labs(x = "Weekday", y = "Number of Rides", 
       title = "Number of Rides for Each Weekday Per Type of Customer",
       fill = "Customer Type",
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_fill_manual(values=c( "#dc582a", "#1e90ff")) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  guides(fill = guide_legend(reverse=TRUE)) +
  coord_flip()

#Total number of rides per weekday
all_trips %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides)) +
  geom_col(width= 0.75, position = position_dodge(width = 0.75)) +
  geom_bar(stat = "identity", fill = "#007a78") +
  labs(x = "Weekday", y = "Number of Rides", 
       title = "Number of Rides Per Weekday", 
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

#Visualization for average duration per weekday
all_trips %>% 
  group_by(user_type, day_of_week) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>% 
  arrange(user_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(x = "Weekday", y = "Average Time (Mins)", 
       title = "Average Ride Time in Minutes per Weekday", fill = "Customer Type", 
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_fill_manual(values=c( "#1e90ff", "#dc582a"))

#Number_of_rides per ride (bike) type
all_trips%>%
  group_by(user_type, rideable_type)%>%
  summarize(number_of_rides = n(),
            average_duration = mean(duration_min))%>%
  arrange(user_type, rideable_type)%>%
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = user_type)) +
  geom_col(width= 0.25, position = position_dodge(width = 0.25)) +
  labs(title="Number of Rides for Each Type of Bike", 
       x = "Type of Bike", y = "Number of Rides", fill = "Customer Type", 
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_x_discrete(limit=c("classic_bike", "docked_bike", "electric_bike"),
                   labels=c("Classic Bike", "Docked Bike", "Electric Bike")) +
  scale_fill_manual(values=c( "#1e90ff", "#dc582a")) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

#Number of rides per user type
all_trips%>%
  group_by(user_type)%>%
  summarize(number_of_rides = n())%>%
  arrange(user_type)%>%
  ggplot(aes(x = user_type, y = number_of_rides, fill = user_type)) +
  geom_col(width= 0.5, position = position_dodge(width = 0.5)) +
  labs(x = "Type of Customer", y = "Number of Rides", fill = "Customer Type",
       title="Number of Rides Per Type of Customer",
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_fill_manual(values=c( "#1e90ff", "#dc582a")) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

#Demand in a day by hour per customer type; plot graph.
all_trips %>%
  group_by(user_type, stime_hm) %>%
  summarise(number_of_trips= n())%>%
  ggplot(aes(x = stime_hm, y = number_of_trips, color = user_type))+
  geom_point() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Time in Hours", y= "Number of Rides", color = "Customer Type", 
       title ="24 Hour Ride Distribution Per Type of Customer",
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022") +
  scale_color_manual(values=c( "#1e90ff", "#dc582a"))

###
#Just in case - Convert hm values into POSIXct date time
all_trips$stime_hm <- as.POSIXct(all_trips$stime_hm, format = "%H:%M")

all_trips$etime_hm <- as.POSIXct(all_trips$etime_hm, format = "%H:%M")
###

#Average ride duration in minutes by Customer type per Month.
all_trips %>%
  group_by(user_type, month)%>%
  summarise(avg_time= mean(duration_min)) %>%
  arrange(month)%>%
  #mutate(month = factor(month, levels=c("Jan_22", "Feb_22", "Mar_22", "Apr_21",
  #                                      "May_21", "Jun_21", "Jul_21", "Aug_21",
  #                                      "Sep_21", "Oct_21", "Nov_21", "Dec_21"))) %>% #to view months as jan-dec.
  ggplot(aes(x=month, y=avg_time, fill= user_type))+ 
  geom_col(width=0.5, position= position_dodge(width=0.5)) +
  theme(axis.text.x= element_text(angle = 30)) +
  labs(x="Month", y="Average Ride Time in Minutes", fill = "Customer Type",
       title="Monthly Average of Ride Duration in Minutes", 
       caption="Data provided by Motivate International Inc.,
       Data range:  April 2021 - March 2022")+
  scale_x_discrete(limit=c("Apr_21", "May_21", "Jun_21", "Jul_21", 
                           "Aug_21", "Sep_21", "Oct_21", "Nov_21",
                           "Dec_21", "Jan_22", "Feb_22", "Mar_22"),
                   labels=c("Apr 21", "May 21", "Jun 21", "Jul 21", 
                            "Aug 21", "Sep 21", "Oct 21", "Nov 21",
                            "Dec 21", "Jan 22", "Feb 22", "Mar 22")) +
  #  scale_x_discrete(limit=c("Jan_22", "Feb_22", "Mar_22", "Apr_21", 
  #                           "May_21", "Jun_21", "Jul_21", "Aug_21",
  #                           "Sep_21", "Oct_21", "Nov_21", "Dec_21"),
  #                   labels=c("Jan 22", "Feb 22", "Mar 22", "Apr 21", 
  #                            "May 21", "Jun 21", "Jul 21", "Aug 21",
  #                            "Sep 21", "Oct 21", "Nov 21", "Dec 21")) +
  scale_fill_manual(values=c("#1e90ff", "#dc582a"))

