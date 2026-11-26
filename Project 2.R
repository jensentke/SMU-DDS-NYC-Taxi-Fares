#Project starts here#

#Download packages#
install.packages("arrow")
library(arrow)
library(dplyr)
library(lubridate)
library(ggplot2)

#Bring in data#
taxi_5 <- read_parquet(file.choose(), header = TRUE)
taxi_6 <- read_parquet(file.choose(), header = TRUE)
taxi_7 <- read_parquet(file.choose(), header = TRUE)


taxi_summer2019 <- bind_rows(taxi_5,taxi_6,taxi_7)
head(taxi_summer2019)
names(taxi_summer2019)
glimpse(taxi_summer2019)

#20,880,424 rows of data or trips provided in summer 2019.#

#Makes time stamped data meaningful and easier to read.#
taxi_summer2019 <- taxi_summer2019 %>%
  mutate(
    tpep_pickup_datetime = as.POSIXct(tpep_pickup_datetime),
    tpep_dropoff_datetime = as.POSIXct(tpep_dropoff_datetime)
  )

#Cleaning data for negatives/zero fares, etc#
taxi_clean <- taxi_summer2019 %>%
  filter(
    !is.na(tpep_pickup_datetime),
    fare_amount > 0,
    total_amount > 0,
    trip_distance >= 0,
    passenger_count >= 0,
    !PULocationID %in% c(264, 265),
    !DOLocationID %in% c(264, 265)
  )

#New Columns for additional analysis#
taxi_clean <- taxi_clean %>%
  mutate(
    tipped = as.integer(tip_amount > 0), ### Yes(1)/No(0) to tipping
    pickup_dow = wday(tpep_pickup_datetime, week_start = 1, label = FALSE), # 1 = Mon (numeric day)
    pickup_dow_f = wday(tpep_pickup_datetime, week_start = 1, label = TRUE), # "Mon" (day name)
    pickup_month_name = month(tpep_pickup_datetime, label = TRUE, abbr = TRUE), # "Jan" (month name)
    pickup_year = year(tpep_pickup_datetime), # year - all 2019
    pickup_ym = format(tpep_pickup_datetime, "%Y-%m"), # "2019-01"
    pickup_hour = hour(tpep_pickup_datetime), # 0-23
    fare_pretip = total_amount - tip_amount, ### fare amount before tip
    tip_pct = if_else(fare_pretip > 0, tip_amount / fare_pretip * 100, NA_real_), ### tip % on base fare
    tip_share_of_total = if_else(total_amount > 0, tip_amount / total_amount * 100, NA_real_), ### tip% on total
    airport_trip = if_else(airport_fee >0, 1,0), ### was it an airport trip
    tip_pct_airport = if_else(airport_trip == 1, tip_pct, NA_real_), ### Percent tip for airport trip
    tip_per_passenger = if_else(passenger_count > 0, #### Tip % by passenger
                                tip_amount / passenger_count, NA_real_),
    dist_bin = cut(trip_distance, ###distance of trips in mile bins
                   breaks = c(seq(0, 9, by = 1), Inf), right = FALSE, include.lowest = TRUE,
                   labels = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9+")),
    paid_by_card = as.integer(payment_type == 1) ### tips by payment type (cash not recorded)
  )

glimpse(taxi_clean)

#Format pickup and dropoff locations as factor#
taxi_clean$PULocationID=as.factor(taxi_clean$PULocationID)
taxi_clean$DOLocationID=as.factor(taxi_clean$DOLocationID)

#View data#
glimpse(taxi_clean)

#Summarize total trips, average tip, and total tips by pickup location, ranked by highest average tip.#
tips_pickup <- taxi_clean %>% group_by(PULocationID) %>%
  summarise(total_trips = n(), avg_tip = round(mean(tip_amount, na.rm = TRUE), 2), 
            total_tips = round(sum(tip_amount, na.rm = TRUE), 2), .groups = "drop") %>% arrange(desc(avg_tip)) 

#View summary#
View(tips_pickup)

#Summarize total trips, average tip, and total tips by dropoff location, ranked by highest average tip.#
tips_dropoff <- taxi_clean %>% group_by(DOLocationID) %>%
  summarise(total_trips = n(), avg_tip = round(mean(tip_amount, na.rm = TRUE), 2),
    total_tips = round(sum(tip_amount, na.rm = TRUE), 2), .groups = "drop") %>% arrange(desc(avg_tip))

#View summary#
View(tips_dropoff)

#Filter out locations with less than 1000 trips#
tips_pickup %>% filter(total_trips >= 1000) %>% arrange(desc(avg_tip)) %>% head(20)
tips_dropoff %>% filter(total_trips >= 1000) %>% arrange(desc(avg_tip)) %>% head(20)

#Plot top 20 pickup locations by average tip#
tips_pickup %>% slice_max(avg_tip, n = 20) %>%
  ggplot(aes(x = reorder(PULocationID, avg_tip), y = avg_tip)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0("$", avg_tip)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 20 NYC Taxi Pickup Locations by Average Tip",
       x = "Pickup Location ID", y = "Average Tip Amount ($)")

#Plot top 20 dropoff locations by average tip#
tips_dropoff %>% slice_max(avg_tip, n = 20) %>%
  ggplot(aes(x = reorder(DOLocationID, avg_tip), y = avg_tip)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = paste0("$", avg_tip)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 20 NYC Taxi Drop-off Locations by Average Tip",
       x = "Drop-off Location ID", y = "Average Tip Amount ($)")

#Comparing tips, fares and distances by dropoff location#
dropoff_analysis <- taxi_clean %>% group_by(DOLocationID) %>%
  summarise(total_trips = n(), avg_tip = round(mean(tip_amount, na.rm = TRUE), 2),
    avg_fare = round(mean(fare_amount, na.rm = TRUE), 2),
    avg_distance = round(mean(trip_distance, na.rm = TRUE), 2),
    .groups = "drop") %>% filter(total_trips >= 1000) %>% arrange(desc(avg_tip))

#Plot Average Fare vs Average Tip by dropoff location#
ggplot(dropoff_analysis, aes(x = avg_fare, y = avg_tip)) +
  geom_point(color = "navy") +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5,
           label = paste0("r = ", round(cor(dropoff_analysis$avg_fare,dropoff_analysis$avg_tip), 3)))+ 
  labs(title = "Average Fare vs. Average Tip by Drop-off Location", x = "Average Fare ($)", y = "Average Tip ($)")

#Plot Trip Distance vs Average Tip by dropoff location#
ggplot(dropoff_analysis, aes(x = avg_distance, y = avg_tip)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE) + annotate("text", x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5,
           label = paste0("r = ", round(cor(dropoff_analysis$avg_distance, dropoff_analysis$avg_tip), 3)))
+  labs(title = "Trip Distance vs. Average Tip by Drop-off Location", x = "Average Distance (Miles)", y = "Average Tip ($)")

#Correlation table#
cor(dropoff_analysis %>% select(avg_tip, avg_fare, avg_distance))

#Noticed an outlier in both graphs. Want to remove the outlier and see how this affects correlation#
dropoff_no_outlier <- dropoff_analysis %>%
  filter(avg_tip < max(avg_tip))

cor(dropoff_no_outlier$avg_fare, dropoff_no_outlier$avg_tip)
cor(dropoff_no_outlier$avg_distance, dropoff_no_outlier$avg_tip)

#Determine outlier from data#
dropoff_analysis %>% slice_max(avg_tip, n = 1)

#Outlier is Newark Airport#

#Compare all 3 major airports. Newark (1) JFK(132), LaGuardia(138)#
taxi_clean_airport = taxi_clean %>% filter(as.character(DOLocationID) %in% c("1", "132", "138")) %>%
  group_by(DOLocationID) %>%
  summarise(trips = n(), avg_tip = round(mean(tip_amount, na.rm = TRUE), 2),
    avg_fare = round(mean(fare_amount, na.rm = TRUE), 2),
    avg_distance = round(mean(trip_distance, na.rm = TRUE), 2))

#Compare tipping percent by airport#
taxi_clean %>%
  filter(DOLocationID %in% c(1, 132, 138), payment_type == 1,
         fare_pretip > 0, tip_amount >= 0) %>%
  group_by(DOLocationID) %>%
  summarise(trips = n(), avg_tip = round(mean(tip_amount), 2),
            avg_tip_pct = round(mean(tip_pct, na.rm = TRUE), 2),
            median_tip_pct = round(median(tip_pct, na.rm = TRUE), 2))