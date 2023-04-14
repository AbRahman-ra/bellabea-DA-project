#========== DATA ANALYSIS ==========#

# Set up
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("ggsci")
install.packages("tinytex")
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggsci)
tinytex::install_tinytex(force = TRUE)

activity <- read.csv("A:/My_Career/DataAnalytics/CaseStudy2/AfterAnalysis/workfiles/activity.csv")
activityH <- read.csv("A:/My_Career/DataAnalytics/CaseStudy2/AfterAnalysis/workfiles/activityH.csv")
sleep <- read.csv("A:/My_Career/DataAnalytics/CaseStudy2/AfterAnalysis/workfiles/sleep.csv")
weight <- read.csv("A:/My_Career/DataAnalytics/CaseStudy2/AfterAnalysis/workfiles/weight.csv")
uSteps <- read.csv("A:/My_Career/DataAnalytics/CaseStudy2/AfterAnalysis/workfiles/Supplement/uSteps.csv")
uSleep <- read.csv("A:/My_Career/DataAnalytics/CaseStudy2/AfterAnalysis/workfiles/Supplement/uSleep.csv")

#==========

# Count people in 3 categories
num_activ <- n_distinct(activity$id) #33
num_sleep <- n_distinct(sleep$id) #24
num_wei <- n_distinct(weight$id) #8

# Count people who recorded their steps & weight
steps_wei <- inner_join(activity, weight, by = "id")
num_activ_wei <- n_distinct(steps_wei$id) #8

# Count people who recorded their steps & sleep
steps_sleep <- inner_join(activity, sleep, by = "id")
num_activ_sleep <- n_distinct(steps_sleep$id) #24

# Count people who recorded their weight & sleep
wei_sleep <- inner_join(weight, sleep, by = "id")
num_wei_sleep <- n_distinct(wei_sleep$id) #6

# Count people who recorded their steps, sleep & weight
activ_wei_sleep <- inner_join(activity,weight,sleep, by = "id")
num_activ_wei_sleep <- n_distinct(activ_wei_sleep$id) #8

# Count all participants
all_ppl <- merge(merge(weight, sleep, by = "id", all = TRUE), activity, by = "id", all = TRUE)
num_all_ppl <- n_distinct(all_ppl$id) #33

#==========

# Calculate the total and average weight loss for each user
# Convert the date column to a date format
weight$date <- as.Date(weight$dateTime, format = "%m/%d/%Y")

# Group the weights table by id and get the minimum and maximum dates for each id
id_dates <- weight %>%
  group_by(id) %>%
  summarize(first_date = min(date), last_date = max(date))

# Join the weights table with the id_dates table to get the weight at the first and last dates for each id
weight_first_last <- weight %>%
  inner_join(id_dates, by = "id") %>%
  filter(date %in% c(first_date, last_date))

# Calculate the total and average weight loss for each id
weight_pivot <- weight_first_last %>%
  group_by(id) %>%
  summarize(num_days = as.numeric(last_date - first_date + 1),
            tot_weight_lossKg = first(weightKg) - last(weightKg),
            avg_weight_lossKg = (first(weightKg) - last(weightKg)) / num_days,
            bmi_first = first(bmi),
            bmi_last = last(bmi))

# Remove duplicate rows from pivot table
weight_pivot <- unique(weight_pivot)

#==========

# Calculate weight recording time frequency
freqtable <- weight %>%
  group_by(time) %>%
  summarize(frequency = n())

#==========

# Calculate average distance walked by user
# Reconfirming date format
activity$date2 <- as.Date(activity$date, format = "%d/%m/%Y")

distance_pivot <- activity %>%
  group_by(id) %>%
  summarize(
    tot_distance = sum(totalDistance),
    activDays = n_distinct(activityDate),
    avg_distance = mean(totalDistance),
    distanceSD = sd(totalDistance),
    normDistanceSD = distanceSD/avg_distance)

#==========

# Calculate total distance walked each hour
activH_pivot <- activityH %>%
  group_by(activityHour) %>%
  summarize(tot_stepH = sum(stepTotal))

#==========

# Calculate the weekly average distance in Km for user
uSteps$date = as.Date(uSteps$date, format = "%d/%m/%Y")

uSteps_pivot <- uSteps %>%
  mutate(week = floor_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(avg_weekDistKm = mean(distanceKm),
            weekDistKmSD = sd(distanceKm),
            normWeekDistSD = weekDistKmSD/avg_weekDistKm)

#==========

# Calculate average time slept by users
sleep_pivot <- sleep %>%
  group_by(id) %>%
  summarize(avg_sleep = mean(totalMinutesAsleep),
            avg_inBed = mean(totalTimeInBed),
            sleepSD = sd(totalMinutesAsleep),
            norm_sleepSD = sleepSD/avg_sleep,
            avg_sleepQ = avg_sleep/avg_inBed)

#==========

# Calculate average sleeping duration for user
uSleep$start <- as.POSIXct(uSleep$start, format = "%Y-%m-%d %H:%M:%S")
uSleep$stop = as.POSIXct(uSleep$stop, format = "%Y-%m-%d %H:%M:%S")
uSleep$recordedSleepTimeU = difftime(uSleep$stop, uSleep$start, units = "mins")

uSleep_pivot <- uSleep %>%
  mutate(week = floor_date(start, unit = "week")) %>%
  group_by(week) %>%
  summarize(avgWeeklySleep = mean(deepSleepTime+shallowSleepTime),
            avgInBed = mean(deepSleepTime+shallowSleepTime+wakeTime),
            uSleepSD = sd(deepSleepTime+shallowSleepTime),
            unorm_sleepSD = uSleepSD/avgWeeklySleep,
            avg_uSleepAcc = avgWeeklySleep/mean(as.numeric(recordedSleepTimeU)),
            avg_uSleepQ = avgWeeklySleep/avgInBed)

#========== END OF ANALYSIS ==========#

#========== DATA VISUALIZATION ==========#

# First, weights data visualizations
# When do people log their weights?
wei_freq <- ggplot(freqtable, aes(x = time, y = frequency)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "When do people log their weights?", x = "Time", y = "Frequency")
wei_freq

# Weight loss for users
ggplot(data = weight_pivot, aes(x = as.character(id), y = tot_weight_lossKg, fill = num_days)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "cyan", high = "green") +
  scale_y_continuous(limits = c(-0.7, 1.9), breaks = seq(-0.7, 1.9, by = 0.2)) +
  labs(title = "Weight Loss vs. Workout Days by User ID",
       x = "User ID", y = "Total Weight Loss (Kg)") +
  geom_text(aes(label = num_days, y = tot_weight_lossKg + 0.1), position = position_dodge(width = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Total steps walked every hour
ggplot(activH_pivot, aes(x = activityHour, y = tot_stepH, group = 1)) + 
  geom_smooth(stat = "identity", color = "turquoise", n = 20) +
  geom_point(color = "purple") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(title = "Activity Hour vs Total Steps",
       x = "Activity Hour",
       y = "Total Steps")

# Consistency of users (Total distance - active days - Coefficient of Variation)
ggplot(data = distance_pivot, aes(x = as.character(id), y = avg_distance, fill = normDistanceSD)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 13.4), breaks = seq(0, 13.4, by = 2)) +
  scale_fill_gradient(low = "turquoise", high = "darkblue") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        legend.position = "right",
        legend.background = element_rect(fill = "white", colour = "black", size = 0.5, linetype = "solid"),
        legend.title = element_text(size = 8, face = "bold", hjust = 0),
        legend.text = element_text(size = 7, face = "bold")) +
  labs(title = "Average distance covered by users",
       x = "ID",
       y = "Average Distance in km",
       fill = "inconsistency (%)")

ggplot(data = distance_pivot)+
  geom_smooth(color = "maroon", fill = NA, aes(x = tot_distance, y = normDistanceSD*100))+
  geom_point(aes(x = tot_distance, y = normDistanceSD*100), color = "darkgreen")+
  scale_x_continuous(limits = c(0, 425), breaks = seq(0, 425, by = 50))+
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25))+
  labs(title = "The power of walking habit",
       x = "Total Distance (km)",
       y = "Inconsistency \"CoV\" (%)")
cPowerofHabit <- cor(distance_pivot$normDistanceSD, distance_pivot$tot_distance , use = "complete.obs")

ggplot(data = distance_pivot)+
  geom_smooth(color = "darkblue", fill = NA, aes(y = activDays, x = normDistanceSD*100))+
  geom_point(aes(y = activDays, x = normDistanceSD*100), color = "darkorange")+
  scale_y_continuous(limits = c(0, 31), breaks = seq(0, 31, by = 2))+
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25))+
  labs(title = "The power of walking habit - 2",
       y = "Nymber of Active Days",
       x = "Inconsistency \"CoV\" (%)")
cPowerofHabit2 <- cor(distance_pivot$normDistanceSD, distance_pivot$activDays , use = "complete.obs")


# Sleep Data
sleep_walk <- inner_join(sleep_pivot, distance_pivot, by = "id") %>%
  select(norm_sleepSD, normDistanceSD, id)

# Create the scatter plot
ggplot(sleep_walk, aes(x = norm_sleepSD*100, y = normDistanceSD*100)) +
  geom_point(color = "maroon") +
  geom_smooth(color = "darkblue", fill = NA)+
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 25)) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  labs(title = "Sleep Quality and Walking Habits",
       x = "Inconsistency in Sleep Duration (%)",
       y = "Inconsistency in Walking Distance (%)")
cSleepWalk <- cor(sleep_walk$norm_sleepSD, sleep_walk$normDistanceSD, use = "complete.obs")

# User Data
ggplot(data = uSteps_pivot, aes(x = week, y = avg_weekDistKm)) +
  geom_point(color = "maroon")+
  geom_smooth(color = "navy", size = 1.5, n = 300)+
  scale_x_date(limits = c(as.Date("2016-04-17"), as.Date("2023-01-16")),
               date_breaks = "6 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(title = "Average Walking Pattern for User",
       x = "Month",
       y = "Average Walking Distance (Km)")


ggplot(data = uSteps_pivot, aes(x = week, y = normWeekDistSD*100)) +
  geom_point(color = "darkgreen")+
  geom_smooth(color = "gold", size = 1.5, n = 300)+
  scale_x_date(limits = c(as.Date("2016-04-17"), as.Date("2023-01-16")),
               date_breaks = "6 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 25)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(title = "How inconsistency changes by time?",
       x = "Month",
       y = "Walking Inconsistency (%)")


uSleep_pivot$week <- as.Date(uSleep_pivot$week) # convert to Date format
ggplot(data = uSleep_pivot, aes(x = week, y = unorm_sleepSD*100)) +
  geom_point(color = "darkblue")+
  geom_smooth(color = "magenta", size = 1.5, n = 300)+
  scale_x_date(limits = c(as.Date("2016-04-17"), as.Date("2023-01-16")),
               date_breaks = "6 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 25)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(title = "How inconsistency changes by time?",
       x = "Month",
       y = "Sleeping Inconsistency (%)")