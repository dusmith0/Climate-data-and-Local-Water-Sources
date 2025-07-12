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
install.packages("tseries")
install.packages("GGally")
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


### ---- ###
## We probably need to take all of the stationary data and create a new data set based on that?
water_stationary <- df()





###--------------------------------------------------------------------------###
##  Selecting Models for AR(p), MA(q), and ARMA(p,q)

















###--------------------------------------------------------------------------###
## Forcasting and Model Evaluation




