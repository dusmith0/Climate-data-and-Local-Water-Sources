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


#HELENE: create well dataframe with everything in water except for the lake data
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

acf(water$TAVG, main = "Average Temp.", lwd = 1.5, col = "steelblue")

### Population data
par(mfrow = c(3,1))
tsplot(diff(water$population), main = "Population of the San Antonio Region",
       ylab = "Population", col = "steelblue")
acf(diff(water$population), 360,main = "Population",col = "steelblue")
pacf(diff(water$population), main = "PACF", 360)

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
ccf(water_stationary$population, diff(well$WaterElevation), lag.max=30, main = "CCF Population and Differenced Water Elevation")

#water elevation (Y) v temperature average
par(mar=c(3,3,3,2))
ccf(well$TAVG, diff(well$WaterElevation), lag.max=30, main = "CCF Temperature and Differenced Water Elevation")


#water elevation (Y) v precipitation

par(mar=c(3,3,3,2))
ccf(well$PRCP, diff(well$WaterElevation), lag.max=30, main = "CCF Precipitation and Differenced Water Elevation")

#########From Helene, population segmentation#######

## Figure out how to do a dummy variable to replace splitting data into two sets##
before2000 = water$population[1:120] # Jan 1991 to Dec 2000
timeB = 1:length(before2000)
timeB2 = timeB^2
after2000 = water$population[121:360] # Jan 2001 to Dec 2020
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
popDummy <- lm(population ~ time_before + time_before_sq + time_after + time_after_sq, data = df)
summary(popDummy)

popInt = popDummy$coefficients[1]
popThetaTB = popDummy$coefficients[2]
popThetaTB2 = popDummy$coefficients[3]
popThetaTA = popDummy$coefficients[4]
popThetaTA2 = popDummy$coefficients[5]

## HELENE: Detrend population data based on popDummy Linear Regression

popDetrend = water$population - popInt - popThetaTB*df$time_before-popThetaTB2*df$time_before_sq-popThetaTA*df$time_after-popThetaTA2*df$time_after_sq

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
adf.test(residsSq)


### ---- ###
## We probably need to take all of the stationary data and create a new data set based on that?

##HELENE: added everything but temperature.
water_stationary <- data.frame(precipitation = diff(water$PRCP), well = diff(log(water$WaterElevation)), population = water$population[-360], popDetrend= popDetrend[-360], temperature = diff(water$TAVG))

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

##HELENE: Model Estimation
#AR model estimation for individual predictors. Need to work on population

popAR = sarima(diff(water$population), p=1, d=0, q=0, no.constant=F)

wellAR = sarima(water_stationary$well, p=1, d=0, q=0, no.constant=T)

precipAR = sarima(water_stationary$precipitation, p=1, d=0, q=0, no.constant=T)

tempARMA = sarima(water_stationary$temperature, p=1, d=0, q=1, no.constant=T)
tempAR = sarima(water_stationary$temperature, p=1, d=0, q=0, no.constant=T)## lower AIC,BIC

##Lagged predictor variables
laggedPrecip <- lag(water_stationary$precipitation)
laggedTemp <- lag(water_stationary$temperature)
laggedWell<- lag(water_stationary$well)

phiPrecip <- as.numeric(precipAR$fit[1])
phiTemp<- as.numeric(tempAR$fit[1])
phiWell<- as.numeric(wellAR$fit[1])

phiXlaggedPrecip <- phiPrecip*laggedPrecip
phiXlaggedTemp <- phiTemp*laggedTemp
phiXlaggedWell<- phiWell*laggedWell

fit <- lm(well ~ temperature + laggedPrecip + population + popDetrend, water_stationary)
fit2<- lm(well ~ temperature + laggedPrecip + popDetrend, water_stationary)
fit3<- lm(well ~ temperature + laggedPrecip + population, water_stationary)
fit4 <- lm(well ~ temperature + laggedPrecip, water_stationary)

fitLagged <- lm(water_stationary$well ~ phiXlaggedPrecip + phiXlaggedTemp + popDetrend[-360])
plot(resid(fitLagged))

plot(resid(fit))
plot(resid(fit2))
plot(resid(fit3))
plot(resid(fit4))
plot(resid(fitLagged))


###--------------------------------------------------------------------------###
##  Selecting Models for AR(p), MA(q), and ARMA(p,q)

##HELENE: Water elevation model; Data: water$WaterElevation


tsplot(water$WaterElevation)
tsplot(log(water$WaterElevation)) #varianc stable; log transformation not needed
tsplot(diff(water$WaterElevation)) #improved stationarity; USE DIFFERENCE

par(mfrow=c(2,1))
acf(diff(water$WaterElevation)) #quickly decays
pacf(diff(water$WaterElevation)) #truncates after lag 3

waterElevation310 = sarima(water$WaterElevation, 3,1,0)##SUGGESTED MODEL: Arima(3,1,0)
waterElevation310$ttable #coefficients
waterElevation310$ICs #BIC,AIC

##HELENE: Population model; Data: popDummyLinA2000 ?????
par(mfrow=c(3,1))
tsplot(pop.data$population)
acf(pop.data$population, 360)
pacf(pop.data$population, 360)
adf.test(pop.data$population) #NOT stationary

par(mfrow=c(3,1))
tsplot(diff(pop.data$population))
acf(diff(pop.data$population), 360)
pacf(diff(pop.data$population), 360)
adf.test(diff(pop.data$population)) #NOT stationary

par(mfrow=c(3,1))
tsplot(log(pop.data$population))
acf(log(pop.data$population, 360))
pacf(log(pop.data$population, 360))
adf.test(log(pop.data$population, 360)) #NOT stationary

par(mfrow=c(3,1))
tsplot(diff(log(pop.data$population)))
acf(diff(log(pop.data$population, 360)))
pacf(diff(log(pop.data$population, 360)))
adf.test(diff(log(pop.data$population, 360)))#Stationary

par(mfrow=c(3,1))
tsplot(diff(popDetrend))
acf(diff(popDetrend), 360)
pacf(diff(popDetrend), 360)
adf.test(diff(popDetrend)) #stationary

pop111 = sarima(popDetrend, 1,1,1) ## BEST MODEL???
logPop111 = sarima(log(pop.data$population),1,1,1) ##???

##HELENE: Temperature model; Data: df_clean$Temp_seasonal

tsplot(df_clean$Temp_seasonal)
tsplot(log(df_clean$Temp_seasonal)) #variance stable; log transformation not needed
tsplot(diff(df_clean$Temp_seasonal)) #improved stationarity; USE DIFFERENCE

par(mfrow=c(2,1))
acf(diff(df_clean$Temp_seasonal)) #truncates after lag 1
pacf(diff(df_clean$Temp_seasonal)) #truncates after lag 3

tempSeasonal311 = sarima(df_clean$Temp_seasonal, 3,1,1)##AR 2 and 3 coeff not signif
tempSeasonal111 = sarima(df_clean$Temp_seasonal, 1,1,1)## SUGGESTED MODEL Arima(1,1,1)
tempSeasonal111$ttable #coefficients
tempSeasonal111$ICs #BIC,AIC

##HELENE: Precipitation: data: water$PRCP

tsplot(water$PRCP)

par(mfrow=c(2,1))
acf(water$PRCP)
pacf(water$PRCP) #truncates after lag 1

prcp100 = sarima(water$PRCP, 1,0,0) ## SUGGSETED MODEL AR1

prcp100$ttable #coefficients
prcp100$ICs #BIC,AIC



###--------------------------------------------------------------------------###
## Forcasting and Model Evaluation




