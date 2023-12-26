# Import Dataset csv Files
q1_2023_01 <- read_csv("2023-cyclist-bike-share-data/202301-divvy-tripdata.csv")
q1_2023_02 <- read_csv("2023-cyclist-bike-share-data/202302-divvy-tripdata.csv")
q1_2023_03 <- read_csv("2023-cyclist-bike-share-data/202303-divvy-tripdata.csv")
q2_2023_04 <- read_csv("2023-cyclist-bike-share-data/202304-divvy-tripdata.csv")
q2_2023_05 <- read_csv("2023-cyclist-bike-share-data/202305-divvy-tripdata.csv")
q2_2023_06 <- read_csv("2023-cyclist-bike-share-data/202306-divvy-tripdata.csv")
q3_2023_07 <- read_csv("2023-cyclist-bike-share-data/202307-divvy-tripdata.csv")
q3_2023_08 <- read_csv("2023-cyclist-bike-share-data/202308-divvy-tripdata.csv")
q3_2023_09 <- read_csv("2023-cyclist-bike-share-data/202309-divvy-tripdata.csv")
q4_2023_10 <- read_csv("2023-cyclist-bike-share-data/202310-divvy-tripdata.csv")
q4_2023_11 <- read_csv("2023-cyclist-bike-share-data/202311-divvy-tripdata.csv")

# Check column names for each of the files for any inconsistencies that need to be solved (renaming the columns to be consistent)
colnames(q1_2023_01)
colnames(q1_2023_02)
colnames(q1_2023_03)
colnames(q2_2023_04)
colnames(q2_2023_05)
colnames(q2_2023_06)
colnames(q3_2023_07)
colnames(q3_2023_08)
colnames(q3_2023_09)
colnames(q4_2023_10)
colnames(q4_2023_11)

# To view the structure of the data and Check for column names and the datatypes of the columns
str(q1_2023_01)
str(q1_2023_02)
str(q1_2023_03)
str(q2_2023_04)
str(q2_2023_05)
str(q2_2023_06)
str(q3_2023_07)
str(q3_2023_08)
str(q3_2023_09)
str(q4_2023_10)
str(q4_2023_11)

# combine the data into single data frame all_trips
all_trips <- bind_rows(q1_2023_01, q1_2023_02, q1_2023_03, q2_2023_04, q2_2023_05, q2_2023_06, q3_2023_07, q3_2023_08, q3_2023_09, q4_2023_10, q4_2023_11)

# CLEANING DATA FOR ANALYSIS

# Remove columns not necessary for analysis
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_name, start_station_id, end_station_name, end_station_id))

# EDA and Find Basic information about the data 

# Columns present in the data
colnames(all_trips)

# Number of Rows present in the data
nrow(all_trips)

# Dimensions of the Data Frame (rows x columns)
dim(all_trips)

# Display first 5 observations
head(all_trips)

# Display last 5 observations 
tail(all_trips)

# To view the structure of the data
str(all_trips)

# To view basic statistical summary of the data
summary(all_trips)  

# To find the various unique elements present in the 'members_casual' column
unique(all_trips[,"member_casual"])

# To find the count of each of the unique values present in the 'members_casual' column
table(all_trips$member_casual)

# To View the structure of the data (similar to str() function)  
glimpse(all_trips)

# Add 'date' column representing the start date of the ride
all_trips$date <- as.Date(all_trips$started_at)

# Add 'month' column representing the month the ride started
all_trips$month <- format(as.Date(all_trips$date), "%m")

# Add 'day' column representing the day the ride started
all_trips$day <- format(as.Date(all_trips$date), "%d")

# Add 'year' column representing the year the ride started
all_trips$year <- format(as.Date(all_trips$date), "%Y")

# Add 'day_of_week' column representing the day of the week the ride started
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add 'ride_length' column representing time take for the ride (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Convert to month names
all_trips$month_names <- format(all_trips$started_at, "%B")

# Optionally, create a new column with abbreviated month names
all_trips$month_abbrev <- format(all_trips$started_at, "%b")

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Once again view the structure of the data 
glimpse(all_trips)

# Check number of null values in each column 
colSums(is.na(all_trips))

# CREATE NEW DATA FRAME AS MODIFING THE DATA, HENCE USE DIFFERENT DATA FRAME 

# Remove duplicates
all_trips_v2 <- distinct(all_trips)
nrow(all_trips)
nrow(all_trips_v2)
colSums(is.na(all_trips))
colSums(is.na(all_trips_v2))

all_trips_v2 <- all_trips_v2[!(all_trips_v2$ride_length <= 0),]
# Number of rows removed = 1214

head(all_trips_v2)

# Sub-setting data based on casual riders and member riders
casual_riders <- all_trips_v2[all_trips_v2$member_casual == "casual",] 
nrow(casual_riders)
colSums(is.na(casual_riders))

member_riders <- all_trips_v2[all_trips_v2$member_casual == "member",]
nrow(member_riders)
colSums(is.na(member_riders))

# Descriptive analysis on ride_length_mins
summary(all_trips_v2$ride_length)

# Average ride length by casual and member riders
mean(casual_riders$ride_length)
mean(member_riders$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts


# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides by Day of Week and Member Type",
       x = "Day of Week",
       y = "Number of Rides",
       fill = "Member Type") + scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average ride length by Day of Week and Member Type",
       x = "Day of Week",
       y = "Average Ride Length",
       fill = "Member Type")

# Preferred Bike type in general and based on casual and member riders
table(all_trips_v2$rideable_type)
table(casual_riders$rideable_type)
table(member_riders$rideable_type)

# Preferred Bike type in general and based on casual and member riders
table(all_trips_v2$day_of_week)
table(casual_riders$day_of_week)
table(member_riders$day_of_week)

# Create subsets for each quarter using the "month" column
q1 <- all_trips_v2[all_trips_v2$month %in% c("01", "02", "03"), ]
q2 <- all_trips_v2[all_trips_v2$month %in% c("04", "05", "06"), ]
q3 <- all_trips_v2[all_trips_v2$month %in% c("07", "08", "09"), ]
q4 <- all_trips_v2[all_trips_v2$month %in% c("10", "11", "12"), ]

# Display the number of rides in each quarter
cat("Number of rides in Q1:", nrow(q1), "\n")
cat("Number of rides in Q2:", nrow(q2), "\n")
cat("Number of rides in Q3:", nrow(q3), "\n")
cat("Number of rides in Q4:", nrow(q4), "\n")

# Create a data frame for ggplot2
rides_data <- data.frame(Quarter = c("Q1", "Q2", "Q3", "Q4"), Rides = c(nrow(q1), nrow(q2), nrow(q3), nrow(q4)))

# Calculate quarter for each ride
all_trips_v2$Quarter <- cut(all_trips_v2$started_at, breaks = "quarters", labels = c("Q1", "Q2", "Q3", "Q4"))

# Group by member_casual and Quarter
rides_data_quarters <- all_trips_v2 %>%
  group_by(member_casual, Quarter) %>%
  summarise(number_of_rides = n())

# a1. Quarterly Analysis
quarterly_trend_plot_stacked <- ggplot(rides_data_quarters, aes(x = Quarter, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = number_of_rides), position = position_stack(vjust = 0.5), size = 3, color="white") +
  labs(title = "Number of Rides Across Quarters by Member Type",x = "Quarter",y = "Number of Rides",fill = "Member Type") +
  scale_fill_manual(values = c("casual" = "orange", "member" = "blue"))

print(quarterly_trend_plot_stacked)

# a2. Quarterly Analysis
quarterly_trend_plot <- ggplot(all_trips_v2, aes(x = Quarter, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Quarterly Analysis",
       x = "Quarter",
       y = "Number of Rides",
       fill = "Member Type") +
  scale_fill_manual(values = c("casual" = "skyblue", "member" = "darkorange")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

print(quarterly_trend_plot)

# Group by member_casual and Quarter
rides_data_month <- all_trips_v2 %>%
  group_by(member_casual, month_abbrev) %>%
  summarise(number_of_rides = n())

# Set the order of months
rides_data_month$month_abbrev <- factor(rides_data_month$month_abbrev, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# b1. Monthly Ride Distribution stacked
monthly_ride_distribution <- ggplot(rides_data_month, aes(x = month_abbrev , y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = number_of_rides), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  labs(title = "Number of Rides Across Months by Member Type", x = "Months", y = "Number of Rides", fill = "Member Type") +
  scale_fill_manual(values = c("casual" = "orange", "member" = "blue")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

print(monthly_ride_distribution)

# Set the order of months
all_trips_v2$month_abbrev <- factor(all_trips_v2$month_abbrev, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# b2. Monthly Ride Distribution
monthly_dist_plot <- ggplot(all_trips_v2, aes(x = month_abbrev, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Monthly Ride Distribution",
       x = "Month",
       y = "Number of Rides",
       fill = "Member Type") +
  scale_fill_manual(values = c("casual" = "skyblue", "member" = "darkorange")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

print(monthly_dist_plot)

# c1. Ride Distribution by Days of Week stacked
day_of_week_plot_stacked <- ggplot(all_trips_v2, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = "stack", stat = "count") +  # Change position to "stack"
  labs(title = "Ride Distribution by Days of Week",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "Member Type") +
  scale_fill_manual(values = c("casual" = "skyblue", "member" = "darkorange")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

print(day_of_week_plot_stacked)

# c2. Ride Distribution by Days of Week
day_of_week_plot <- ggplot(all_trips_v2, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Ride Distribution by Days of Week",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "Member Type") +
  scale_fill_manual(values = c("casual" = "skyblue", "member" = "darkorange")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

print(day_of_week_plot)

# d. Rideable Type Distribution
rideable_type_dist_plot <- ggplot(all_trips_v2, aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Rideable Type Distribution",
       x = "Rideable Type",
       y = "Number of Rides",
       fill = "Member Type") +
  scale_fill_manual(values = c("casual" = "skyblue", "member" = "darkorange"))

print(rideable_type_dist_plot)


# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv', row.names = FALSE)