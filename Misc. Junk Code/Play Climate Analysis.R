## Linear and Transforming the Climate data for J17 Well
## Group 4 (Da Dream Team!!!)
## libraries:


#install.packages("corrplot")
#install.packages("aTSA")
#install.packages("EnvStats")
library("astsa")
library("ggplot2")
library("dplyr")
library("corrplot")
library("lubridate")
library("tidyverse")
library("zoo")
library("aTSA")
library("EnvStats")
library("APStatTools")

###--------------------------------------------------------------------------###
## Getting the data

climate.data <- read.csv("Original Data Sets/Climate Data/San Antonio Airport (SAT) climate data (1948 to 2025).csv")
well.data <- read.csv("Original Data Sets/Well Depth Data/j17waterlevels.csv", skip = 6)
lake.data <- read.csv("Original Data Sets/Surface water level data/CanyonLakeRev.txt")
pop.data <- read.csv("Original Data Sets/MonthlyPop.csv")[,-c(1)]
# I used this to make sure I correctly matched what Helene did
helene <- read.csv("Original Data Sets/MonthlyData.csv")

## Extracting the data from 1991 to 2020
climate <- climate.data %>%
  filter(DATE >= as.Date("1991-1-1") & DATE <= as.Date("2020-12-31")) %>%
  select(DATE, TAVG, TMAX, TMIN, TSUN, ACSH, AWND, PGTM, PRCP) %>%
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




###--------------------------------------------------------------------------###
##  Handling Missing Data

#missing information for population
missingYear = c(rep(2002,2), rep(2005,3), rep(2006,3))
missingMonth = c(10,11,9,11,12,5,7,9)
missingPop = c(rep(1414000,2),rep(1538000,3),rep(1582000,3))
missingDate = as.yearmon(paste(missingYear,missingMonth), "%Y %m")

missingPopDF = data.frame(missingYear,missingMonth,missingPop,missingDate)
names(missingPopDF) = c("year", "month", "population", "Date")


#Merge original population DF and missing info DF
pop_averageDF = rbind(pop.data,missingPopDF)
pop <- pop_averageDF %>%
  arrange(year, month)

# Merging the Data
water <- merge(climate, lake, by.climate = "DATE", by.lake = "DATE", all = TRUE) %>%
         merge(well, by.well = "DATE", all = TRUE)


### Attempting to coerce the data into a more usable format, by averaging over months.

names(water)
monthly_averages <- water %>%
  select(DATE, TAVG, PRCP, percentfull_lake, WaterElevation) %>%
  group_by(year = year(DATE), month = month(DATE)) %>%
  summarise(TAVG = mean(TAVG, na.rm = TRUE),
            PRCP = sum(PRCP),
            percentfull_lake = mean(percentfull_lake, na.rm = TRUE),
            WaterElevation = mean(WaterElevation, na.rm = TRUE))
            #TSUN = mean(TSUN, na.rm = TRUE), #TSUN has been removed due to excessive missing data



water <- cbind(monthly_averages,population = pop$population)

#Getting rid of separate year and month columns
water$Date <- as.yearmon(paste(water$year, water$month), "%Y %m")

drops <- c("year","month")
water <- water[ , !(names(water) %in% drops)]

#View(water)

#Handling Missing WaterElevation data
water[which(is.na(water$WaterElevation)),]

missing <- which(is.na(water$WaterElevation))
for(i in missing){
  water$WaterElevation[i] <- mean(water$WaterElevation[c(i-2,i+2)],na.rm = TRUE)
}

#water[missing,]
#View(water)


## Possible Alternative for handling population data
## It will run through the dates an "smooth" the data to a consistent increase
## between each month, as opposed to keeping the same value for each month. I do not
## Think it helped though. I think it actually made it much worse.
#for(i in seq(12,360,by=12)){
#          if(!is.na(water$population[i + 1])){
#            step <- (water$population[(i) + 1] - water$population[(i)])/12
#          }else{
#            step <- (2400000 - water$population[(i)])/12 #imputing upper bound from https://fred.stlouisfed.org/series/SATPOP
#          }
#          stepup <- cumsum(rep(step,11))
#          water$population[(i - 10):(i)] <-  water$population[(i - 10):(i)] + stepup
#}


##--## Trying to figure out why the DATE section is not working.
#water[which(is.na(water$percentfull_lake) & is.na(water$waterlevel_lake)),]
    ## 26 missing data sets


#View((water[which(is.na(water$WaterLevel) & is.na(water$Change) & is.na(water$WaterElevation)),]))
    ## 10975 - 2009 = 8966 missing data


## Possible Solution is to remove the data...
#water <- merge(climate, lake, by.climate = "DATE", by.lake = "DATE", all = FALSE) %>%
#  merge(well, by.well = "DATE", all = FALSE)




###--------------------------------------------------------------------------###
## Plotting the data sets ##

##------------ Transformed Data ------------- ##
## Building a correlation matrices to see if any significance can be found.
names(water)
correlation = cor(water[,-c(6)],use = "complete.obs") ## note: this removes all missing values
correlation

corrplot(correlation, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)

panel.cor <-function(x,y,...){
  par(usr=c(0,1,0,1))
  r <- round(cor(x,y, use = "complete.obs"),2)
  text(0.5,0.5,r,cex=1.25)
}

pairs(water, lower.panel= panel.cor, col = "steelblue")

## lowess fitting for the data This fails for TSUN
par(mfrow = c(3,2))
names <- names(water)
for(i in colnames(water)){
  ts.plot(water[[i]], col = "steelblue2", lwd = 1.5, main = as.character(i))
  lines(lowess(water[[i]], f = .4), col = "firebrick", lwd = 1.5)
}

## attempting some lag to see if there is anything interesting.

as.ts(water)

names <- names(water)
for(i in colnames(water)){
  lag2.plot(water$percentfull_lake, water[[i]] , 5, col = "steelblue")
}


lag1.plot(water$percentfull_lake, 12, col = "steelblue")
lag2.plot(water$percentfull_lake, water$TAVG , 12, col = "steelblue")
#lag2.plot(water$percentfull_lake, water$TSUN , 5, col = "steelblue")
lag2.plot(water$percentfull_lake, water$PRCP , 12, col = "steelblue")
lag2.plot(water$percentfull_lake, water$population , 5, col = "steelblue")
lag2.plot(water$percentfull_lake, water$Date , 5, col = "steelblue")


as.ts(water)
lag1.plot(water$WaterElevation, 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$TAVG , 12, col = "steelblue")
#lag2.plot(water$WaterElevation, water$TSUN , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$PRCP , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$population , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$Date , 12, col = "steelblue")



###--------------------------------------------------------------------------###
##  Transforming the Data to stationary

### Initial ACF plots
par(mfrow = c(3,2))
acf(water$population, main = "Population", lwd = 1.5, col = "steelblue")
acf(water$percentfull_lake, main = "Percent full", lwd = 1.5, col = "steelblue")
acf(water$WaterElevation, main = "Water Elevation", col = "steelblue", lwd = 1.5)
acf(water$PRCP, main = "Preciptation", lwd = 1.5, col = "steelblue")
acf(water$TAVG, main = "Average Temp.", lwd = 1.5, col = "steelblue")

### Population data
par(mfrow = c(2,1))
tsplot(water$population, main = "Population of the San Antonio Region",
     ylab = "Population", col = "steelblue", lwd = 3)
acf(water$population, main = "Population", lwd = 1.5, col = "steelblue")



### working on transforming lake data if needed.
### This seemed worked really well.
par(mfrow = c(2,1))
stationary.test(diff(water$percentfull_lake))

#Checking the original data to compare with the differenced data
tsplot((water$percentfull_lake),
       main = "Original Lake Percentage", xlab = "Percent Full",
       col = "steelblue")
acf(water$percentfull_lake, main = "Percent full", lwd = 1.5, col = "steelblue")

#Plotting the differenced data
tsplot(diff(water$percentfull_lake, lag = 1),
       main = "Differenced Lake Percentage", ylab = "Percent Full",
       col = "steelblue")
acf(diff(water$percentfull_lake, lag = 1),lwd = 1.5, col = "steelblue")



### working on transforming well data
par(mfrow = c(2,1))
stationary.test(water$WaterElevation)

#Checking the original data to compare with the differenced data
tsplot(water$WaterElevation,
       main = "Original WaterElevation", xlab = "WaterElevation",
       col = "steelblue")
acf(water$WaterElevation, main = "Water Elevation", col = "steelblue", lwd = 1.5)

#Plotting the differenced data
tsplot(diff(water$WaterElevation),
       main = "Differenced WaterElevation", xlab = "WaterElevation",
       col = "steelblue")
acf(diff(water$WaterElevation, lag = 1), lwd = 1.5)


### working on precipitation data
par(mfrow = c(3,2))
stationary.test(water$PRCP)

#Checking the original data to compare with the differenced data
tsplot(water$PRCP,
       main = "Original Precipitation", ylab = "Precipitation",
       col = "steelblue")
acf(water$PRCP, main = "Preciptation", lwd = 1.5, col = "steelblue")

#Plotting the differenced data
tsplot(diff(water$PRCP),
       main = "Differenced Precipitation", ylab = "Precipitation",
       col = "steelblue")
acf(diff(water$PRCP))

#Trying to see if I can remove some of the inconsistent peaks in that data.
tsplot(diff(water$PRCP ^ (1/2)),
       main = "Differenced and Transformed", ylab = "Precipitation",
       col = "steelblue")
acf(diff(water$PRCP ^ (1/2)))
qqnorm(diff(water$PRCP ^ (1/2)))


### Working with the Temperature data. (I stole this straigh off of John's branch)















## Simple Regression
names(water)
fit <- lm(waterlevel_lake ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit) ##TMAX fails as it is a transformation of TAVG and TMIN
plot(fit)

fit <- lm(percentfull_lake  ~ TAVG + PRCP + population, data = water)
summary(fit)

fit <- lm(WaterLevel  ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit)

fit <- lm(Change  ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit)

fit <- lm(WaterElevation  ~ TAVG + TMAX + TSUN + ACSH + AWND + PGTM + PRCP, data = water)
summary(fit)




