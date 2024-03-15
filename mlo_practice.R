#this is a practice script for git

#packages

library(readr)
library(car)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(forcats)

#practice dataset upload

mlo_monthly_co2 <- read_csv("~/GitHub/Practice/monthly_in_situ_co2_mlo.csv")
View(mlo_monthly_co2)
summary(mlo_monthly_co2)
str(mlo_monthly_co2)

#Data cleaning

yr_av <- data.frame(mlo_monthly_co2$year, mlo_monthly_co2$month, mlo_monthly_co2$CO2, mlo_monthly_co2$sta)

yr_av <- rename(yr_av, "year" = "mlo_monthly_co2.year", "month" = "mlo_monthly_co2.month", "co2" = "mlo_monthly_co2.co2", "station" = "mlo_monthly_co2.sta")
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

ggplot(co2_year, aes(x = year, y = avg_co2)) +
  geom_line() +
  labs(title = "Average CO2 Levels per Year",
       x = "Year",
       y = "Average CO2 concentration") +
  theme_minimal()






