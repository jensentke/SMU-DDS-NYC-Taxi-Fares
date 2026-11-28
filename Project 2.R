#Project starts here#

#Download packages#
install.packages("arrow")
library(arrow)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)

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

#Run ANOVA to compare the average tip amount by pickup and dropoff location#
set.seed(1)
taxi_sample <- taxi_clean %>% slice_sample(n = 100000)
summary(aov(tip_amount ~ PULocationID, data = taxi_sample))
summary(aov(tip_amount ~ DOLocationID, data = taxi_sample))

#Combined pickup day and pickup hour to pickup and dropoff location and ran ANOVA#
fit_combined <- aov(tip_amount ~ PULocationID + DOLocationID + pickup_dow_f + factor(pickup_hour), data = taxi_sample)
summary(fit_combined)

#Add fare amount and trip distance to the combined ANOVA#
fit_combined2 <- aov(tip_amount ~ PULocationID + DOLocationID +pickup_dow_f + factor(pickup_hour) 
        + fare_amount + trip_distance, data = taxi_sample)
summary(fit_combined2)

#Test if adding fare and distance improves model#
anova(fit_combined, fit_combined2)

#Compare the percentage of tip variation explained by each model#
summary.lm(fit_combined)$r.squared
summary.lm(fit_combined2)$r.squared

#Calculate RMSE and MAE to evaluate model prediction errors on training data. RMSE 2.15, MAE 1.46#
sqrt(mean(residuals(fit_combined2)^2))
mean(abs(residuals(fit_combined2)))

#Check the distribution of prediction errors#
summary(residuals(fit_combined2))
quantile(abs(residuals(fit_combined2)), c(.90, .95, .99, 1))

#Check residual patterns and potential outliers#
plot(fitted(fit_combined2), residuals(fit_combined2),pch = 16, cex = 0.3, col = "steelblue",
     xlab = "Predicted Tip ($)", ylab = "Residual ($)") 
abline(h = 0, col = "red")

#Identify the 10 trips with the largest prediction errors#
taxi_outliers <- taxi_sample %>%
  mutate(predicted_tip = fitted(fit_combined2),
         residual = residuals(fit_combined2)) %>%
  arrange(desc(abs(residual))) %>%
  select(tip_amount, predicted_tip, residual, fare_amount,
         trip_distance, payment_type, PULocationID, DOLocationID)

head(taxi_outliers, 10)

#Count extreme tips and trips with zero recorded distance#
taxi_sample %>%
  summarise(total_trips = n(),
            tips_over_30 = sum(tip_amount > 30, na.rm = TRUE),
            zero_distance = sum(trip_distance == 0, na.rm = TRUE),
            non_card = sum(payment_type != 1, na.rm = TRUE))

#Add payment type to the existing model#
fit_combined3 <- update(fit_combined2, . ~ . + paid_by_card)
summary(fit_combined3)

#Compare model improvement and prediction errors RMSE 1.71, MAE .97#
anova(fit_combined2, fit_combined3)
summary.lm(fit_combined3)$r.squared
sqrt(mean(residuals(fit_combined3)^2))
mean(abs(residuals(fit_combined3)))


#Group uncommon pickup and drop-off locations into Other#
taxi_sample <- taxi_sample %>% mutate(PULocationID = fct_lump_min(PULocationID, min = 100),
         DOLocationID = fct_lump_min(DOLocationID, min = 100))

#Split data into 80% training and 20% testing#
set.seed(23)
train_id <- sample(nrow(taxi_sample), 80000)
taxi_train <- taxi_sample[train_id, ]
taxi_test <- taxi_sample[-train_id, ]

#Train regression model with all seven predictors#
fit_train <- lm(tip_amount ~ PULocationID + DOLocationID + pickup_dow_f + factor(pickup_hour) 
                + fare_amount + trip_distance + paid_by_card, data = taxi_train)

#Calculate RMSE and MAE using unseen trips. RMSE 1.82, MAE .99#
pred <- predict(fit_train, newdata = taxi_test)
valid <- !is.na(pred)
rmse <- sqrt(mean((taxi_test$tip_amount[valid] - pred[valid])^2))
mae <- mean(abs(taxi_test$tip_amount[valid] - pred[valid]))
round(c(RMSE = rmse, MAE = mae), 3)

#Check linear regression assumptions. Residuals not randomly scattered around 0. QQ plot not linear and has heavy tails. 
#Likely due to heavy tipping patterns#
par(mfrow = c(2, 2))
plot(fit_train)
par(mfrow = c(1, 1))

#Identify the 10 observations with the highest Cook's distance#
cooks <- cooks.distance(fit_train)
head(sort(cooks, decreasing = TRUE), 10)

#Review the five trips with the highest Cook's distance#
top_cooks <- as.integer(names(head(sort(cooks, decreasing = TRUE), 5)))

taxi_train[top_cooks, ] %>% select(tip_amount, fare_amount, trip_distance, payment_type,PULocationID, DOLocationID)

#Exclude zero-distance trips and disputed payments#
taxi_train_clean <- taxi_train %>% filter(trip_distance > 0, payment_type != 4)

#Refit regression using cleaned training data#
fit_clean <- update(fit_train, data = taxi_train_clean)

#Evaluate cleaned model's training performance#
sqrt(mean(residuals(fit_clean)^2))
mean(abs(residuals(fit_clean)))

#Apply the same cleaning rules to the test data#
taxi_test_clean <- taxi_test %>% filter(trip_distance > 0, payment_type != 4)

#Predict tips using the cleaned model#
pred_clean <- predict(fit_clean, newdata = taxi_test_clean)

#Calculate test RMSE and MAE. 1.75 and .97#
rmse_clean <- sqrt(mean((taxi_test_clean$tip_amount - pred_clean)^2))
mae_clean <- mean(abs(taxi_test_clean$tip_amount - pred_clean))
round(c(RMSE = rmse_clean, MAE = mae_clean), 3)

#Compare original and cleaned models on identical test records. Slight improvement but not significant 1.746 and .968#
pred_original <- predict(fit_train, newdata = taxi_test_clean)

compare <- data.frame(
  Model = c("Original", "Cleaned"),
  RMSE = c(sqrt(mean((taxi_test_clean$tip_amount - pred_original)^2)),
           rmse_clean),
  MAE = c(mean(abs(taxi_test_clean$tip_amount - pred_original)),
          mae_clean))

round_results <- compare
round_results[, 2:3] <- round(round_results[, 2:3], 3)
round_results

#Recreate the original sample and 80/20 split#
set.seed(123)
taxi_sample2 <- taxi_clean %>% slice_sample(n = 100000)

set.seed(123)
train_id <- sample(nrow(taxi_sample2), 80000)
taxi_train2 <- taxi_sample2[train_id, ]
taxi_test2 <- taxi_sample2[-train_id, ]

#Exclude zero-distance trips and disputed payments#
taxi_train2 <- taxi_train2 %>%  filter(trip_distance > 0, payment_type != 4)
taxi_test2 <- taxi_test2 %>% filter(trip_distance > 0, payment_type != 4)

#Identify common locations using training data#
pu_keep <- names(which(table(taxi_train2$PULocationID) >= 100))
do_keep <- names(which(table(taxi_train2$DOLocationID) >= 100))

#Apply the same grouping to training and testing data#
group_loc <- function(x, keep) {factor(ifelse(as.character(x) %in% keep, as.character(x), "Other"))}

taxi_train2$PULocationID <- group_loc(taxi_train2$PULocationID, pu_keep)
taxi_test2$PULocationID <- group_loc(taxi_test2$PULocationID, pu_keep)

taxi_train2$DOLocationID <- group_loc(taxi_train2$DOLocationID, do_keep)
taxi_test2$DOLocationID <- group_loc(taxi_test2$DOLocationID, do_keep)

#Fit the final regression model#
fit_final <- lm(tip_amount ~ PULocationID + DOLocationID +pickup_dow_f + factor(pickup_hour) +
                  fare_amount + trip_distance + paid_by_card,data = taxi_train2)

#Calculate RMSE and MAE on unseen trips#
pred_final <- predict(fit_final, newdata = taxi_test2)

rmse_final <- sqrt(mean((taxi_test2$tip_amount - pred_final)^2))
mae_final <- mean(abs(taxi_test2$tip_amount - pred_final))

round(c(RMSE = rmse_final, MAE = mae_final), 3)

#Save final regression model and location mappings#
saveRDS(list(model = fit_final, pu_keep = pu_keep,do_keep = do_keep), "taxi_final_model.rds")
