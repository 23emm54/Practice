# this is a practice script for git

# packages

library(readr)
library(car)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(forcats)

# practice dataset upload

mlo_monthly_co2 <- read_csv("~/GitHub/Practice/monthly_in_situ_co2_mlo.csv")
View(mlo_monthly_co2)
summary(mlo_monthly_co2)
str(mlo_monthly_co2)

# Data cleaning for base CO2

yr_av <- data.frame(mlo_monthly_co2$year, mlo_monthly_co2$month, mlo_monthly_co2$CO2, mlo_monthly_co2$sta)

yr_av <- rename(yr_av, "year" = "mlo_monthly_co2.year", "month" = "mlo_monthly_co2.month", "co2" = "mlo_monthly_co2.CO2", "station" = "mlo_monthly_co2.sta")
View(yr_av)
as.factor(yr_av$month)
str(yr_av)

yr_av%>%
  mutate(month = as.factor(month)%>%
           fct_recode("Jan" = "1", "Feb" = "2", "Mar" = "3", "Apr" = "4", "May" = "5", "Jun" = "6", 
                      "Jul" = "7", "Aug" = "8", "Sep" = "9", "Oct" = "10", "Nov" = "11", "Dec" = "12"))

clean_yr_av <- subset(yr_av, co2 > 0)

co2_year <- clean_yr_av %>%
  group_by(year) %>%
  summarise(avg_co2 = mean(co2))

View(co2_year)

# Graph of average CO2 per year with non-adjusted values

ggplot(co2_year, aes(x = year, y = avg_co2)) +
  geom_line() +
  labs(title = "Average CO2 Levels per Year",
       x = "Year",
       y = "Average CO2 concentration") +
  theme_minimal()

# Cleaning adjusted CO2 values

adj_yr <- data.frame(mlo_monthly_co2$year, mlo_monthly_co2$month, mlo_monthly_co2$seas_adjust_filled, mlo_monthly_co2$sta)

adj_yr <- rename(adj_yr, "year" = "mlo_monthly_co2.year", "month" = "mlo_monthly_co2.month", "adjusted.co2" = "mlo_monthly_co2.seas_adjust_filled", "station" = "mlo_monthly_co2.sta")
View(adj_yr)
as.factor(adj_yr$month)
str(yr_av)

year_to_remove <- 1958
Year_to_remove2 <- 2024

adj_yr <- adj_yr %>%
  filter(year != year_to_remove)

adj_yr <- adj_yr %>%
  filter(year != Year_to_remove2)

View(adj_yr)

adj_yr%>%
  mutate(month = as.factor(month)%>%
           fct_recode("Jan" = "1", "Feb" = "2", "Mar" = "3", "Apr" = "4", "May" = "5", "Jun" = "6", 
                      "Jul" = "7", "Aug" = "8", "Sep" = "9", "Oct" = "10", "Nov" = "11", "Dec" = "12"))
adj_yr$month <- month.name[adj_yr$month]

# finding the 400 ppm threshold

index <- which(adj_yr$adjusted.co2 > 400)[1]
threshold_year <- adj_yr$year[index]
threshold_month <- adj_yr$month[index]

print(paste(threshold_month, threshold_year))

#create a new date column
adj_yr$month <- match(adj_yr$month, month.name)
adj_yr$date <- as.Date(paste(adj_yr$year, adj_yr$month, "01", sep="-"))

# graph of CO2 levels per year with adjusted values

ggplot(adj_yr, aes(x = date, y = adjusted.co2)) +
  geom_line() +
  geom_point() +
  xlab("Date") +
  ylab("CO2 Levels (ppm)") +
  ggtitle("CO2 Levels Over Time") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  
  # Add a vertical line when CO2 crossed 400 ppm
  geom_vline(xintercept = as.numeric(as.Date(paste(threshold_year, "01", "01", sep="-"))), color = "red", linetype = "dashed") +
  annotate("text", x = as.Date(paste(threshold_year, "01", "01", sep="-")), y = 400, label = "CO2 crossed 400 ppm", vjust = -1, color = "red")

