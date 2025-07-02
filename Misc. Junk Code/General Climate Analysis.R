## Linear and Transforming the Climate data for J17 Well
## Group 4 (Da Dream Team!!!)
## libraries:

## Data that the group is interested in..
##----  Waterelevation, Average temp, precipitation, population

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

###--------------------------------###
## Getting the data

climate.data <- read.csv("Original Data Sets/Climate Data/San Antonio Airport (SAT) climate data (1948 to 2025).csv")
well.data <- read.csv("Original Data Sets/Well Depth Data/j17waterlevels.csv", skip = 6)
lake.data <- read.csv("Original Data Sets/Surface water level data/CanyonLakeRev.txt")
pop.data <- read.csv("Original Data Sets/MonthlyPop.csv")[,-c(1)]
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


###---------------------------------------###
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

#Get rid of separate year and month columns
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

water[missing,]
View(water)


## Possible Alternative for handling population data
for(i in seq(12,360,by=12)){
          if(!is.na(water$population[i + 1])){
            step <- (water$population[(i) + 1] - water$population[(i)])/12
          }else{
            step <- (2400000 - water$population[(i)])/12 #imputing upper bound from https://fred.stlouisfed.org/series/SATPOP
          }
          stepup <- cumsum(rep(step,11))
          water$population[(i - 10):(i)] <-  water$population[(i - 10):(i)] + stepup
}


##--## Trying to figure out why the DATE section is not working.
#water[which(is.na(water$percentfull_lake) & is.na(water$waterlevel_lake)),]
    ## 26 missing data sets


#View((water[which(is.na(water$WaterLevel) & is.na(water$Change) & is.na(water$WaterElevation)),]))
    ## 10975 - 2009 = 8966 missing data


## Possible Solution is to remove the data...
#water <- merge(climate, lake, by.climate = "DATE", by.lake = "DATE", all = FALSE) %>%
#  merge(well, by.well = "DATE", all = FALSE)


###-------------------------------------###
## Plotting the data sets ##

##--- Untransformed Data --- ##
## Building a correlation matrices to see if any significance can be found.
names(water)
correlation = cor(water[,-c(7)],use = "complete.obs") ## note: this removes all missing values

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
  ts.plot(water[[i]], col = "steelblue2", lwd = 2, main = as.character(i))
  lines(lowess(water[[i]], f = .4), col = "red3", lwd = 2)
}

## attempting some lags

## lake data
as.ts(water)

## This short version does not quite work
names <- names(water)
for(i in colnames(water)){
  lag2.plot(water$percentfull_lake, water[[i]] , 5, col = "steelblue")
}


lag1.plot(water$percentfull_lake, 12, col = "steelblue")
lag2.plot(water$percentfull_lake, water$TAVG , 12, col = "steelblue")
lag2.plot(water$percentfull_lake, water$TSUN , 5, col = "steelblue")
lag2.plot(water$percentfull_lake, water$PRCP , 12, col = "steelblue")
lag2.plot(water$percentfull_lake, water$population , 5, col = "steelblue")
lag2.plot(water$percentfull_lake, water$Date , 5, col = "steelblue")


as.ts(water)
lag1.plot(water$WaterElevation, 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$TAVG , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$TSUN , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$PRCP , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$population , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$Date , 12, col = "steelblue")



###--- Transformed data












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




