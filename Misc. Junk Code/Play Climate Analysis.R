## Linear and Transforming the Climate data for J17 Well
## Group 4 (Da Dream Team!!!)
##
## Color themes:
##
##
##
##
## Naming Schemes:
##
##
##
##
##
##----------------------------------------------------------------------------##

## libraries:
#install.packages("corrplot")
#install.packages("aTSA")
#install.packages("EnvStats")

#install.packages("tseries")
#install.packages("GGally")
#install.packages("forecast")
#install.packages("fGarch")
#install.packages("FinTS")
library("astsa") #General Times Series Plot
library("ggplot2") #Better Graphics
library("GGally") #extends ggplot2
library("dplyr") #filter(), select(), mutate(), group_by(), summarize(), arrange()
library("readr") #for reading in csv's
library("tseries") #Augmented Dickey-Fuller (ADF) test for stationary
library("corrplot") #Allows for building pretty Correlation plots
library("lubridate") #date and time handling
library("tidyverse")
library("zoo") #For clean time series models.
library("aTSA")
library("EnvStats")
library("forecast")
library("fGarch")
library("FinTS")

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
######### From Helene handling the population data #######
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


#HELENE: remove create well dataframe with everything in water except for the lake data
df <- subset(df, select = -c(a, c))
well <- water[,-3]



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

##HELENE: plotting well dataframe correlations##
names(well)
correlationWell = cor(well[,-c(5)],use = "complete.obs") ## note: this removes the date column and all missing values
correlationWell

corrplot(correlationWell, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)

panel.cor <-function(x,y,...){
  par(usr=c(0,1,0,1))
  r <- round(cor(x,y, use = "complete.obs"),2)
  text(0.5,0.5,r,cex=1.25)
}

pairs(well, lower.panel= panel.cor, col = "steelblue")

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

par(mar=c(3,3,3,3))
lag1.plot(water$WaterElevation, 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$TAVG , 12, col = "steelblue")
#lag2.plot(water$WaterElevation, water$TSUN , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$PRCP , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$population , 12, col = "steelblue")
lag2.plot(water$WaterElevation, water$Date , 12, col = "steelblue")


##HELENE: repeat lag plots for well data

as.ts(well)

names <- names(well)
for(i in colnames(well)){
  lag2.plot(well$WaterElevation, water[[i]] , 5, col = "steelblue")
}

###--------------------------------------------------------------------------###
##  Transforming the Data to stationary

### Initial ACF plots
par(mfrow = c(3,2))
acf(water$population, main = "Population", lwd = 1.5, col = "steelblue")
acf(water$percentfull_lake, main = "Percent full", lwd = 1.5, col = "steelblue")
acf(water$WaterElevation, main = "Water Elevation", col = "steelblue", lwd = 1.5)
acf(water$PRCP, main = "Preciptation", lwd = 1.5, col = "steelblue")
acf(water$TAVG, main = "Average Temp.", lwd= 1.5, col = "steelblue")


##HELENE: Initial PACF plots
par(mfrow = c(3,2), mar=c(1.2,3,2.75,2) + 0.1)
pacf(water$population, main = "Population", lwd = 1.5, col = "steelblue")
pacf(water$WaterElevation, main = "Water Elevation", col = "steelblue", lwd = 1.5)
pacf(water$PRCP, main = "Preciptation", lwd = 1.5, col = "steelblue")
pacf(water$TAVG, main = "Average Temp.", lwd = 1.5, col = "steelblue")


### Population data
par(mfrow = c(3,1))
tsplot(water$population, main = "Population of the San Antonio Region",
       ylab = "Population", col = "steelblue", lwd = 3)
acf(water$population, main = "ACF Population", lwd = 1.5, col = "steelblue")
pacf(water$population, main = "PACF Population")
adf.test(diff(water$population))

##HELENE: Water Elevation

par(mfrow = c(3,1))
tsplot(water$WaterElevation, main = "Water Elevation of J17 Well",
       ylab = "Population", col = "steelblue", lwd = 3)
acf(water$WaterElevation, main = "Water Elevation ACF")
pacf(water$WaterElevation, main = "Water Elevation PACF")

adf.test(water$WaterElevation)
adf.test(diff(water$WaterElevation))

##HELENE: Average Temperature
par(mfrow = c(3,1))
tsplot(water$TAVG, main = "Average Temperature",
       ylab = "Population", col = "steelblue", lwd = 3)
acf(water$TAVG, main = "Average Temperature ACF")
pacf(water$TAVG, main = "Average Temperature PACF")

adf.test(water$TAVG)
adf.test(diff(water$TAVG))

##HELENE: Precipitation
par(mfrow = c(3,1))
tsplot(water$PRCP, main = "Precipitation",
       ylab = "Population", col = "steelblue", lwd = 3)
acf(water$PRCP, main = "Precipitation ACF")
pacf(water$PRCP, main = "Precipitation PACF")

adf.test(water$PRCP)
adf.test(diff(water$PRCP))
=======
acf(water$TAVG, main = "Average Temp.", lwd = 1.5, col = "steelblue")

### Population data
par(mfrow = c(2,1))
tsplot(water$population, main = "Population of the San Antonio Region",
       ylab = "Population", col = "steelblue", lwd = 3)
acf(water$population, main = "Population", lwd = 1.5, col = "steelblue")

### working on transforming lake data if needed.
### This seemed worked really well.
par(mfrow = c(2,1))
adf.test((water$percentfull_lake),alternative = "stationary")

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
adf.test(diff(water$percentfull_lake),alternative = "stationary")


### working on transforming well data
par(mfrow = c(2,1))
adf.test(water$WaterElevation, alternative = "stationary")

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

adf.test(diff(water$WaterElevation), alternative = "stationary")


### working on precipitation data
par(mfrow = c(3,2))
adf.test(water$PRCP, alternative = "stationary")

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
adf.test(diff(water$PRCP), alternative = "stationary")

#Trying to see if I can remove some of the inconsistent peaks in that data.
tsplot(diff(water$PRCP ^ (1/2)),
       main = "Differenced and Transformed", ylab = "Precipitation",
       col = "steelblue")
acf(diff(water$PRCP ^ (1/2)))
qqnorm(diff(water$PRCP ^ (1/2)))
adf.test(diff(water$PRCP ^ (1/2)), alternative = "stationary")



### Working with the Temperature data.
##### (I stole this straight off of John's branch) #####

#Non-detrended Temp lag 12
names(water)
df <- water %>%
  arrange(Date) %>% #get dates in numerical order
  mutate(Temp_seasonal = TAVG - lag(TAVG, 12))

#Augmented Dickey-Fuller Test for stationary of variable (Temp_seasonal)
#filtered out first 12 values, which are NA due to 12-month lag
df_clean <- df %>% filter(!is.na(Temp_seasonal))
adf.test(df_clean$Temp_seasonal, alternative = "stationary")

#Regression temp lag 12, which is supposedly now stationary
fit_temp_seasonal <- lm(WaterElevation ~ Temp_seasonal + PRCP + population, data = df)
summary(fit_temp_seasonal)

#scatterplot for Temp_seasonal
ggplot(df, aes(x = Date, y = Temp_seasonal)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Temperature Lag 12 with Trend Line",
    x = "Date", y = "Temp (Lag 12)"
  ) +
  theme_minimal()

# Fit linear model
model <- lm(Temp_seasonal ~ as.numeric(Date), data = df)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Print regression equation for Temp_Seasonal plot
cat("Regression equation:\n")
cat("y =", round(slope, 6), "* time +", round(intercept, 3), "\n")




### Working with Population data (Stolen Straight from John)
#########From Helene, population segmentation#######

## Figure out how to do a dummy variable to replace splitting data into two sets##
df <- water
before2000 = df$population[1:120] # Jan 1991 to Dec 2000
timeB = 1:length(before2000)
timeB2 = timeB^2
after2000 = df$population[121:360] # Jan 2001 to Dec 2020
timeA = 1:length(after2000)
timeA2 = timeA^2

## I tried an exponential model first:
popB2000 <- lm(log(before2000)~before2000)

summary(popB2000)

popA2000 <- lm(log(after2000)~after2000)

summary(popA2000)

## I tried a quadratic model next
popFitQuadB2000 <- lm(before2000~timeB2+timeB, na.action=NULL)

summary(popFitQuadB2000)

popFitQuadA2000 <- lm(after2000~timeA2+timeA, na.action=NULL)

summary(popFitQuadA2000)

## Both models fit. I think the quadratic may be easier to use to detrend, but I'm not sure.

# Combine into original dataframe
df$time_before  <- c(timeB, rep(0, length(after2000)))
# - `timeB`: sequence from 1 to 120 (for the first 120 months: Jan 1991 to Dec 2000)
# - `rep(0, length(after2000))`: fills the remaining 240 months (2001–2020) with 0s
# - So:
#   - `time_before = 1, 2, ..., 120, 0, 0, ..., 0`
#
# This vector **tracks time within the "before 2000" group** and is zero elsewhere.
df$time_before_sq   <- c(timeB2, rep(0, length(after2000)))

df$time_after       <- c(rep(0, length(before2000)), timeA)
# - `rep(0, length(before2000))`: 120 zeros for pre-2001
# - `timeA`: 1 to 240 for Jan 2001 to Dec 2020
# - So:
#   - `time_after = 0, 0, ..., 0, 1, 2, ..., 240`
#
# This activates the time trend **only after 2000**.
df$time_after_sq  <- c(rep(0, length(before2000)), timeA2)

#Regression of all dummys against data
popDummy <- lm(population ~ time_before + time_before_sq + time_after + time_after_sq, data = df)
summary(popDummy)

#Regression of all dummys against data WITHOUT square after 2000
popDummyLinA2000 <- lm(population ~ time_before + time_before_sq + time_after, data = df)
summary(popDummyLinA2000)

#Check for stationarity of popDummyLinA2000
#Extract residuals
resids <- resid(popDummyLinA2000)
#ADF test
adf.test(resids, alternative = "stationary")

#Check for stationarity of pop Dummy (with square)
#Extract residuals
residsSq <- resid(popDummyLinA2000)
adf.test(residsSq, alternative = "stationary")
tsplot(residsSq)
acf2(residsSq)




## Alternative for population stationary.
## This produces a much smaller p-value, but it does not look better. I looks like a mess.
# Dummy non adjusted fit
dummy <- ifelse(format(water$Date, "%Y") <= 2000 , 0, 1)

fit.dummy.noadjust  <- lm(log((population)) ~ dummy*(time(population)) + dummy*time(population)^2,
                          data = water)
summary(fit.dummy.noadjust)
plot(fit.dummy.noadjust)

#*********************## Use this one????
# Dummy adjusted for co-variate correlation

dummy <- ifelse(format(water$Date, "%Y") <= 2000 , 0, 1)
time2 <- (time(water$population) - mean(time(water$population)))^2
fit.dummy  <- lm(log(population) ~ dummy*(time(population)) + dummy*(time2),
                 data = water)
tsplot(resid(fit.dummy))

acf2(resid(fit.dummy))
adf.test(resid(fit.dummy), alternative = "stationary")

## HELENE: CCF plots

#water elevation (Y) v population (X)

par(mfrow = c(3,1), mar=c(3,3,3,2))
ccf(diff(water_stationary$population), diff(well$WaterElevation), lag.max=30, main = "CCF Differenced Population and Differenced Water Elevation")

#water elevation (Y) v temperature average
par(mar=c(3,3,3,2))
ccf(diff(well$TAVG), diff(well$WaterElevation), lag.max=30, main = "CCF Temperature and Differenced Water Elevation")


#water elevation (Y) v precipitation

par(mar=c(3,3,3,2))
ccf(well$PRCP, diff(well$WaterElevation), lag.max=30, main = "CCF Precipitation and Differenced Water Elevation")

#########From Helene, population segmentation#######

## Figure out how to do a dummy variable to replace splitting data into two sets##
before2000 = df$Population[1:120] # Jan 1991 to Dec 2000
timeB = 1:length(before2000)
timeB2 = timeB^2
after2000 = df$Population[121:360] # Jan 2001 to Dec 2020
timeA = 1:length(after2000)
timeA2 = timeA^2

## I tried an exponential model first:
popB2000 <- lm(log(before2000)~before2000)

summary(popB2000)

popA2000 <- lm(log(after2000)~after2000)

summary(popA2000)

## I tried a quadratic model next
popFitQuadB2000 <- lm(before2000~timeB2+timeB, na.action=NULL)

summary(popFitQuadB2000)

popFitQuadA2000 <- lm(after2000~timeA2+timeA, na.action=NULL)

summary(popFitQuadA2000)

## Both models fit. I think the quadratic may be easier to use to detrend, but I'm not sure.

# Combine into original dataframe
df$time_before      <- c(timeB, rep(0, length(after2000)))
# - `timeB`: sequence from 1 to 120 (for the first 120 months: Jan 1991 to Dec 2000)
# - `rep(0, length(after2000))`: fills the remaining 240 months (2001–2020) with 0s
# - So:
#   - `time_before = 1, 2, ..., 120, 0, 0, ..., 0`
#
# This vector **tracks time within the "before 2000" group** and is zero elsewhere.
df$time_before_sq   <- c(timeB2, rep(0, length(after2000)))

df$time_after       <- c(rep(0, length(before2000)), timeA)
# - `rep(0, length(before2000))`: 120 zeros for pre-2001
# - `timeA`: 1 to 240 for Jan 2001 to Dec 2020
# - So:
#   - `time_after = 0, 0, ..., 0, 1, 2, ..., 240`
#
# This activates the time trend **only after 2000**.
df$time_after_sq    <- c(rep(0, length(before2000)), timeA2)

#Regression of all dummys against data
popDummy <- lm(Population ~ time_before + time_before_sq + time_after + time_after_sq, data = df)
summary(popDummy)

#Regression of all dummys against data WITHOUT square after 2000
popDummyLinA2000 <- lm(Population ~ time_before + time_before_sq + time_after, data = df)
summary(popDummyLinA2000)

#Check for stationarity of popDummyLinA2000
#Extract residuals
resids <- resid(popDummyLinA2000)
#ADF test
adf.test(resids, alternative = "stationary")

#Check for stationarity of pop Dummy (with square)
#Extract residuals
residsSq <- resid(popDummyLinA2000)
adf.test(residsSq, alternative = "stationary")



### ---- ###
## We probably need to take all of the stationary data and create a new data set based on that?

##HELENE: added everything but temperature.
water_stationary <- data.frame(precipitation = diff(water$PRCP), well = diff(water$WaterElevation), population = water$population[-360], temperature = diff(water$TAVG))

#all three predictors
fit <- lm(well ~ precipitation + population + temperature, water_stationary)
summary(fit)
AIC(fit)
BIC(fit)
summary(fit)$r.squared

#PRCP and TEMP only
fit2 <- lm(well ~ precipitation + temperature, water_stationary)
summary(fit2)
AIC(fit2)
BIC(fit2)
summary(fit2)$r.squared

##population^2?
population2 = water_stationary$population^2

fitPopSq <- lm(well ~ precipitation + population2 + temperature, water_stationary)
summary(fitPopSq)
AIC(fitPopSq)
BIC(fitPopSq)
summary(fitPopSq)$r.squared



###--------------------------------------------------------------------------###
##  Selecting Models for AR(p), MA(q), and ARMA(p,q)

###--------------------------------------------------------------------------###
##  Selecting Models for AR(p), MA(q), and ARMA(p,q)
## Changing TAVG to Temp and Removing Percent full
water <- water %>%
  select(-percentfull_lake) %>%
  rename(Temp = TAVG)
names(water)


##-- Temp model --## John's

plot(water$Temp)
acf(water$Temp)
pacf(water$Temp)

par(mfrow = c(3,1))
tsplot(water$Temp, main = "Average Temperature Raw Data",
       ylab = "Population", col = "steelblue", lwd = 3)
acf(water$Temp, main = "Average Temperature ACF")
pacf(water$Temp, main = "Average Temperature PACF")

# library(urca)
#
# # 12-month differenced series
# diff_temp <- diff(water$Temp, lag = 12)
#
# # Run ADF test with drift and 0 lag
# adf_model <- ur.df(diff_temp, type = "drift", lags = 0)
#
# # View the full regression summary
# summary(adf_model)

adf.test(water$Temp)
adf.test(diff(water$Temp, lag = 12))

library(urca)

# Define lags and types
lags <- 0:5
types <- c("none", "drift", "trend")

# Function to run ADF and extract test statistic + p-value
run_adf <- function(data, type) {
  sapply(lags, function(lag) {
    test <- ur.df(data, type = type, lags = lag)
    stat <- test@teststat[1]  # first row is the tau statistic
    pval <- if (abs(stat) > abs(test@cval[1, "1pct"])) {
      0.01
    } else if (abs(stat) > abs(test@cval[1, "5pct"])) {
      0.05
    } else if (abs(stat) > abs(test@cval[1, "10pct"])) {
      0.10
    } else {
      1.00
    }
    c(stat = round(stat, 2), p.value = pval)
  })
}

# Run all three types
for (type in types) {
  result <- run_adf(water$Temp, type)
  cat("\nType:", match(type, types), ":",
      if (type == "none") "no drift no trend" else if (type == "drift") "with drift no trend" else "with drift and trend", "\n")
  print(t(result))
}


plot(water$Temp, col = "gray", main = "Lowess Trend on Temperature")
lines(lowess(water$Temp), col = "steelblue", lwd = 2)

temp_diff <- (diff(water$Temp, lag = 12))
plot(temp_diff, main="Differenced Series", ylab="Diff Temp")
acf(temp_diff, main="ACF of Differenced Series")
pacf(temp_diff, main="PACF of Differenced Series")

fit <- auto.arima(temp_diff, d = 0, D = 0, seasonal = FALSE)
summary(fit)

sarima(water$Temp, p=1, d=1, q=0)
sarima(water$Temp, p=1, d=1, q=1)
sarima(water$Temp, p=1, d=1, q=1, P=1, D=1, Q=1, S=12)
sarima(water$Temp, p=1, d=1, q=1, P=1, D=0, Q=1, S=12)
sarima(water$Temp, p=1, d=0, q=1, P=1, D=0, Q=1, S=12) #best
model <- sarima(water$Temp, p=1, d=0, q=1, P=1, D=0, Q=1, S=12) #best
res <- residuals(model$fit)

#Test for ARCH/GARCH
acf2(res^2, 20)
# Add a main title across both plots
mtext("ACF and PACF of Squared Residuals for Temperature SARIMA(1,0,1) × (1,0,1)[12]",
      outer = TRUE, cex = 1.2, line = 1, font = 2)
library(FinTS)
ArchTest(res, lags = 12) #ARCH not present

sarima(water$Temp, p=1, d=1, q=0, P=1, D=1, Q=0, S=12)
sarima(water$Temp, p=1, d=0, q=0, P=1, D=0, Q=0, S=12)
sarima(water$Temp, p=1, d=1, q=0, P=1, D=0, Q=0, S=12)

temp_ts <- ts(water$Temp, start = c(1991, 1), frequency = 12)

sarima.for(temp_ts, n.ahead = 24, p=1, d=1, q=1, P=1, D=1, Q=1, S=12)

# Fit linear trend to full series
trend_model <- lm(as.numeric(temp_ts) ~ time_vals)

# Create extended time axis
future_times <- seq(from = as.numeric(tail(time_vals, 1)) + 1/12,
                    by = 1/12, length.out = 24)
full_times <- c(time_vals, future_times)

# Predict trend across historical + forecasted time
trend_line <- predict(trend_model, newdata = data.frame(time_vals = full_times))

# Re-run forecast plot
sarima.for(temp_ts, n.ahead = 24, p=1, d=1, q=1, P=1, D=1, Q=1, S=12)

# Overlay trend line
lines(full_times, trend_line, col = "red", lwd = 2)


# temp_ts <- ts(water$Temp, start = c(1991, 1), frequency = 12)
# time_values <- time(temp_ts)
#
# trend_model <- lm(water$Temp ~ time_values)
# summary(trend_model)



###-- Precipitation model --## Darres's
par(mfrow = c(4,1))
tsplot(water$PRCP, main = "Precipitation",
       ylab = "Population", col = "steelblue")

tsplot(log(water$PRCP), main = "Precipitation",
       ylab = "Population", col = "steelblue")

tsplot(diff(water$PRCP), main = "Precipitation",
       ylab = "Population", col = "steelblue")

tsplot(diff(log(water$PRCP)), main = "Precipitation",
       ylab = "Population", col = "steelblue") #this looks the most like noise to me, but has missing values

adf.test(water$PRCP, alternative = "stationary")
adf.test(diff(water$PRCP))

## ACF AND PACF

par(mfrow=c(2,1))
acf(diff(water$PRCP), main = "Precipitation ACF") #lag 1
pacf(diff(water$PRCP), main = "Precipitation PACF") #decays
#Conclusion: MA(1) with differencing
acf2(diff(water$PRCP))
## MODEL ESTIMATION

prcp011 = sarima(water$PRCP, 1,0,0)
prcp011$ttable #coefficients
prcp011$ICs #BIC,AIC

## FORECASTING
par(mfrow = c(1,1))
sarima.for(as.ts(water$PRCP), n.ahead = 12, 1,0,0,
           main = "Monthly Precipitation Acculmulation")





##-- Population General Regression Model --## Who knows now

y <- predict(fit.dummy, type = "response")
y <- exp(y)
head(y)

tsplot(water$population, main = "Population", ylab = "Population",
       col = "steelblue", lwd = 1.5)
lines(x = 1:length(y), y = y, type = "l",
      col = "firebrick", lwd = 1.5)
legend("topleft",legend = c("Original","Model"),
       col = c("steelblue","firebrick"),
       cex = .8, lwd = 2, lty = c(1,1))




##-- Water Elevation Model --## Dustin
fitwell <- sarima((water$WaterElevation), p = 3, d = 0, q = 0)

sarima((water$WaterElevation), p = 3, d = 1, q = 0) #6.42
sarima((water$WaterElevation), p = 2, d = 1, q = 0) #6.43
sarima((water$WaterElevation), p = 1, d = 1, q = 0) #6.44
sarima((water$WaterElevation), p = 3, d = 0, q = 0) #6.38
sarima((water$WaterElevation), p = 2, d = 0, q = 0) #6.4
sarima((water$WaterElevation), p = 1, d = 0, q = 0) #6.42


model <- (fitwell$ttable[1]*(lag(water$WaterElevation,1) - fitwell$ttable[4]) +
            #fitwell$ttable[2]*(lag(water$WaterElevation,2) - fitwell$ttable[4]) +
            fitwell$ttable[3]*(lag(water$WaterElevation,3) - fitwell$ttable[4]) +
            fitwell$ttable[4])

tsplot(water$WaterElevation[260:361], ylab = "Water Elevation", lwd = 1.5)
lines(x = c(260:361) - 260, y = model[260:361],
      col = "purple", lwd = 1.5, lty = "dashed")
legend("topleft",legend = c("Original Data", "Model"),
       col = c("black","purple"),
       cex = .8, lwd = 2, lty = c(1,2))

water$Date[260:360]

fit <- sarima.for(as.ts(water$WaterElevation),
                  n.ahead = 12, p = 3, d = 0, q = 0,)
lines(x = c(361:372), y = water.test$WaterElevation[1:12],
      col = "purple", lwd = 1.5)
legend("topleft",legend = c("Original Data","forecast","2021 observed data"),
       col = c("black","firebrick","purple"),
       cex = .8, lwd = 2, lty = c(1,1))


names(fit)
str(fit$pred)

error <- ((water.test$WaterElevation - fit$pred[1:12])^2)
mse <- 1/length(error) * sum(error)
mse
sqrt(mse)

## Attempting GARCH with returns  ## Dustin Again
par(mfrow = c(1,1))
ret.well <- (diff(log(water$WaterElevation)))

# Checking the tsplot and for centeral means
par(mfrow = c(2,1))
tsplot(water$WaterElevation, ylab = "Elevation", main = "Well Elevation")

tsplot(ret.well, ylab = "Returns", main = "Returns on Well Variance")
mean(ret.well) ## Very close to zero

# Checking ACF and PACF
acf2(ret.well, main = "Well Returns", col = "steelblue", lwd = 1.5)
fit <- sarima(diff(log(water$WaterElevation)), p = 3, d = 0, q = 0)

resids <- resid(fit$fit)^2
acf2(resids)

# Trying AR(1)-ARCH(1)
fitg1 <- garchFit(~arma(1,0) + garch(1,0), data = ret.well, include.mean = FALSE)
summary(fitg1)
fitg2 <- garchFit(~arma(3,0) + garch(1,0), data = ret.well, include.mean = FALSE)
summary(fitg2)

model1 <- (.2118*lag(ret.well,1))
model2 <- (-.2586*lag(ret.well, 1) + -.04887*lag(ret.well, 2) + -.08054*lag(ret.well, 3))

sd1 <- volatility(fitg1)
sd2 <- volatility(fitg2)

tsplot(ret.well, ylab = "Returns", main = "Returns on Well data mean estimates", lwd = 1.25)
lines(model1, col = "steelblue", lwd = 1.5)
lines(model2, col = "firebrick", lwd = 1.5)
legend("topleft",legend = c("AR(3)-ARCH(1)", "AR(1)-ARCH(1)"),
       col = c("firebrick","steelblue"),
       cex = .5, lwd = 2, lty = c(1,1))

tsplot(ret.well, ylab = "Returns",
       main = "Returns on Well Variance estimates", lwd = 1.25)
lines(model1 + (sd1) - .001, col = "steelblue", lty = "dashed", lwd = 1.5)
lines(model2 - (sd2) + .001, col = "firebrick", lty = "dashed", lwd = 1.5)
legend("topleft",legend = c("AR(3)-ARCH(1)", "AR(1)-ARCH(1)"),
       col = c("firebrick","steelblue"),
       cex = .5, lwd = 2, lty = c(2,2))

plot(sd1, type = 'l', ylab = "Variance Return", xlab = "time",
     main = "Variation AR(1)-ARCH(1)", col = "steelblue", lwd = 1.5)
plot(sd2, type = 'l', ylab = "Variance Return", xlab = "time",
     main = "Variation AR(3)-ARCH(1)", col = "firebrick")


## Back Transforming the model
par(mfrow = c(1,1))
backmodel1 <- c(0)
backmodel2 <- c(0)
for(i in 1:length(model2)){
  backmodel1[i] <- exp(model1[i] + c(1,log(water$WaterElevation))[i])
  backmodel2[i] <- exp(model2[i] + c(1,log(water$WaterElevation))[i])
}

par(mfrow = c(1,1))
tsplot(c(water$WaterElevation), ylab = "Elevation", lwd = 1.5,
       main = "Well Elevation with AR-ARCH mean estimates")
lines(backmodel1, col = "steelblue", lwd = 1.5, lty = 2)
#lines(backmodel2, col = "firebrick", lwd = 1.5)
legend("topright",legend = c("Original", "AR(1)-ARCH(1)"),
       col = c("black","steelblue"),
       cex = .8, lwd = 2, lty = c(1,2))

## Fitting new forecasts
forecast_mod1 <- predict(fitg1, n.ahead = 12)
forecast_mod2 <- predict(fitg1, n.ahead = 12)
names(forecast_mod1)
forbackmodel1 <- exp(forecast_mod1$meanForecast[1] + log(water$WaterElevation)[360])
forbackmodel2 <- exp(forecast_mod2$meanForecast[1] + log(water$WaterElevation)[360])
#This bit is necessary as the first forecast begins with the last known value
for(i in 2:12){
  forbackmodel1[i] <- exp(forecast_mod1$meanForecast[i] + log(forbackmodel1)[i - 1])
  forbackmodel2[i] <- exp(forecast_mod2$meanForecast[i] + log(forbackmodel2)[i - 1])
}
# Indexing by 2 to avoid the pre-forecaster value, then the summed previouse value gets i - 1
# to begin at 1 again.

# genreating confidence intervals
par(mfrow = c(1,2))
forecast_mod1 <- predict(fitg1, n.ahead = 12)
forecast_mod2 <- predict(fitg1, n.ahead = 12)
interval1 <- cbind(forecast_mod1$meanForecast + 1*1.96*forecast_mod1$meanError,
                   forecast_mod1$meanForecast - 1*1.96*forecast_mod1$meanError)
interval2 <- cbind(forecast_mod2$meanForecast + 1*1.96*forecast_mod2$meanError,
                   forecast_mod2$meanForecast - 1*1.96*forecast_mod2$meanError)


tsplot(c(water$WaterElevation[200:360],water.test$WaterElevation[1:12]),
       main = "Forecast AR(1)-ARCH(1) Water Elevation Data", ylab = "Elevation")
lines(x = c(162:173),water.test$WaterElevation[1:12], col = "purple", lwd = 1.5)
lines(x = c(161:172), y = forbackmodel1, col = "steelblue", lwd = 1.5)
#lines(x = c(161:172), y = forbackmodel1, col = "firebrick", lwd = 1.5)
polygon(c(161:172,rev(161:172)), c(intforbackmodel1[,1],intforbackmodel1[,2]),
        col = "steelblue", density = 100)
#polygon(c(161:172,rev(161:172)), c(intforbackmodel2[,1],intforbackmodel2[,2]),
#        col = "firebrick", density = 100)
legend("topright",legend = c("Original", "Observed Future", "Predicted Future"),
       col = c("black","purple","steelblue"),
       cex = .5, lwd = 2, lty = c(1))


## forecasting on the Variance
forecast_mod1$standardDeviation
#forecaste_mod2
ret.wellfut <- diff(log(water.test$WaterElevation))

# Checking the tsplot and for centeral means
tsplot(c(ret.well[200:359], ret.wellfut[1:12]), type = 'l', ylab = "Variance on Return", xlab = "time",
       main = "Variation Forecast for AR(1)-ARCH(1)", col = "black", lwd = 1.5)
lines(sd1[200:361], lwd = 1.5, col = "steelblue")
lines(x = c(161:172), y = ret.wellfut[1:12], col = "purple", lwd = 1.5)
lines(x = c(161:172), forecast_mod1$standardDeviation, lwd = 1.5,
      col = "firebrick")
#lines(x = c(161:172), -1*forecast_mod1$standardDeviation, lwd = 1.5,
#col = "firebrick")
#polygon(c(161:172,rev(161:172)),
#        c(forecast_mod1$standardDeviation,rev(-1*forecast_mod1$standardDeviation)),
#        col = "firebrick", density = 100)
legend("topleft",legend = c("Original", "Observed Future", "Predicted Future", "Model Estimate"),
       col = c("black","purple", "firebrick","steelblue"),
       cex = .5, lwd = 2, lty = c(1))















###--------------------------------------------------------------------------###
##### HELENE MULTIVARIATE

#all three predictors CHECK FOR AUTOCORRELATED ERRORS
well <- as.ts(well)
fit <- lm(well ~ precipitation + population + temperature, water_stationary)
summary(fit)
AIC(fit)
BIC(fit)
summary(fit)$r.squared
resid <- fit$residuals
resid2 <- resid^2
tsplot(resid2)
acf(resid2, 360)
pacf(resid2, 360)
sarima(resid2,1,0,1)

#all three predictors with interactions CHECK FOR AUTOCORRELATED ERRORS
temp <- water_stationary$temperature - mean(water_stationary$temperature)
well <- as.ts(well)
fitInt <- lm(well ~ precipitation * population * temperature, water_stationary)
summary(fit)
AIC(fit)
BIC(fit)
summary(fit)$r.squared
resid <- fit$residuals
resid2 <- resid^2
tsplot(resid2)
acf(resid2, 360)
pacf(resid2, 360)
sarima(resid2,1,0,1)


#####PRCP and TEMP only BEST MODEL
fit2 <- lm(well ~ precipitation + temperature, water_stationary)
summary(fit2)
AIC(fit2)
BIC(fit2)
summary(fit2)$r.squared
fit2resid2 <- fit2$residuals^2

par(mfrow=c(1,1))
tsplot(fit2resid2, main="Linear Regression Residuals")
acf2(fit2resid2, main="P/ACF of Linear Regression Residuals") #Possible AR2 Model


sarima(fit2resid2,1,0,0)
sarima(fit2resid2,2,0,0)

##### Autocorrelated error linear regression:
sarima(water_stationary$well, 2,0,0, xreg=cbind(water_stationary$precipitation, water_stationary$temperature))


#PRCP and TEMP with interaction
fit3 <- lm(well ~ precipitation * temperature, water_stationary)
summary(fit3)
AIC(fit3)
BIC(fit3)
summary(fit3)$r.squared
fit3resid2 <- fit3$residuals^2

ccf(water$TAVG, water$WaterElevation, na.action=na.pass, main="Cross Correlation Function of Well Water Elevation and Average Temperature", type="correlation") #lag 1
ccf(water$PRCP, water$WaterElevation, na.action=na.pass, main="Cross Correlation Function of Well Water Elevation and Cumulative Precipitation", type="correlation") #lag1

fit4 <- dynlm(well ~ L(temperature,-1) + L(precipitation,-1), data=water_stationary)


##### Multivariate time series using VARS

multi = cbind(well=water_stationary$well, temp=water_stationary$temperature, prcp=water_stationary$precipitation)
multiFit = VAR(multi, p=12, type="both")
summary(multiFit)

VARselect(multi, lag.max=20, type="both")

tsplot(resid(multiFit))
acf(resid(multiFit))

## prediction
fit.pr = predict(multiFit, n.ahead = 24, ci = 0.95) # 4 weeks ahead
fanchart(fit.pr) # plot prediction + error


