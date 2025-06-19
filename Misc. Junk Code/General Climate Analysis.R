## Linear and Transforming the Climate data for J17 Well
## Group 4 (Da Dream Team!!!)
## libraries:

#install.packages("corrplot")
library("astsa")
library("ggplot2")
library("dplyr")
library("corrplot")


## Getting the data
climate.data <- read.csv("Original Data Sets/Climate Data/San Antonio Airport (SAT) climate data (1948 to 2025).csv")
well.data <- read.csv("Original Data Sets/Well Depth Data/j17waterlevels.csv", skip = 6)
lake.data <- read.csv("Original Data Sets/Surface water level data/CanyonLakeRev.txt")


## Extracting the data from 1991 to 2020
climate <- climate.data %>%
  filter(DATE >= as.Date("1991-1-1") & DATE <= as.Date("2020-12-31")) %>%
  select(DATE, TAVG, TMAX, TMIN, TSUN,  ACSH, AWND, PGTM, PRCP) %>%
  mutate(DATE = as.Date(DATE))

    ## Filtering in TAVG if it is not present, would probably be better as an apply statement.

      for(i in 1:length(climate$TAVG)){
        if(is.na(climate$TAVG[i]) == TRUE){
          climate$TAVG[i] = (climate$TMAX[i] + climate$TMIN[i])/2
        }
      }


well <- well.data %>%
  mutate(DATE = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(DATE >= as.Date("1991-1-1") & DATE <= as.Date("2020-12-31")) %>%
  select(DATE, WaterLevel, Change, WaterElevation)


lake <- lake.data %>%
  mutate(DATE = as.Date(date)) %>%
  filter(date >= as.Date("1991-1-1") & date <= as.Date("2020-12-31")) %>%
  select(DATE, water_level, percent_full)

# Renaming some fo the columns to avoid naming issues with the well data.
colnames(lake) <- c("DATE","waterlevel_lake","percentfull_lake")

# Merging the Data
water <- merge(climate, lake, by.climate = "DATE", by.lake = "DATE", all = TRUE) %>%
         merge(well, by.well = "DATE", all = TRUE)


##--## Trying to figure out why the DATE section is not working.
water[which(is.na(water$percentfull_lake) & is.na(water$waterlevel_lake)),]
    ## 26 missing data sets


View((water[which(is.na(water$WaterLevel) & is.na(water$Change) & is.na(water$WaterElevation)),]))
    ## 10975 - 2009 = 8966 missing data


## Possible Solution is to remove the data...
water <- merge(climate, lake, by.climate = "DATE", by.lake = "DATE", all = FALSE) %>%
  merge(well, by.well = "DATE", all = FALSE)


## Building a correlation matrix
correlation = cor(water[,-c(1,13)],use = "complete.obs") ## note: this removes all missing values
View(correlation)

corrplot(correlation, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)


## Simple Regression
names(water)
fit <- lm(waterlevel_lake ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit) ##TMAX fails as it is a transformation of TAVG and TMIN
plot(fit)

fit <- lm(percetnfull_lake  ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit)

fit <- lm(WaterLevel  ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit)

fit <- lm(Change  ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit)

fit <- lm(WaterElevation  ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit)




