###
#-- Time frame   1991  to  2020
#-- Body of Water -- Canyon Lake (Percent_full) Helene
#-- J17 Well -- (Water Elevation) John
#-- Climate Choice -- 1948 to 2025 SAT  (Precipitation and Average Temp). For missing data we can average the TMAX and TMIN  Dustin
#--

## libraries:

library("astsa")
library("ggplot2")
library("dplyr")


## Getting the data
  path <- file.path("C:", "Users", "dustin.smith", "OneDrive - Lubbock Independent School District","Pictures",
                  "Documents","Codes","R","Climate data and Water Levels","Original Data Sets",fsep = "\\")
  type <- file.path("Climate Data",fsep = "\\")
  data <- read.csv(paste(path,type,"San Antonio Airport (SAT) climate data (1948 to 2025).csv",sep = "\\"))

## Extracting the data from 1991 to 2020
data <- as.data.frame(data)

new.data <- data %>%
            select(DATE, TAVG, PRCP,TMAX,TMIN) %>%
            filter(DATE >= as.Date("1991-1-1") & DATE <= as.Date("2020-12-31"))


## Filtering in TAVG if it is not present

for(i in 1:length(new.data$TAVG)){
  if(is.na(new.data$TAVG[i]) == TRUE){
    new.data$TAVG[i] = (new.data$TMAX[i] + new.data$TMIN[i])/2
  }
}


## Plotting the time series data
par(mfrow = c(2,1), cex.lab = .7)
Tavg <- ts(new.data$TAVG,frequency = 365, start = 1991, end = 2020)
tsplot(Tavg,
       xlab = "Time 1991 to 2020",
       ylab = "Average Temp.",
       main = "Daily Average Tempurature",
       col = "blue",
       cex = .5, cex.main = .8)

## Moving over years to try to remove the seasonal cycles.
filter_data <- stats::filter(Tavg, filter = rep(1/365,365), sides = 1)
tsplot(filter_data,
       xlab = "Time 1991 to 2020",
       ylab = "Average Temp.",
       main = "Yearly Moving Average Tempurature",
       col = "blue",
       cex = .5, cex.main = .8)

## plotting the precipitation data
par(mfrow = c(3,1))
prcp <- ts(new.data$PRCP,frequency = 365, start = 1991, end = 2020)
tsplot(prcp,
       xlab = "Time 1991 to 2020",
       ylab = "Daily Precipitation",
       main = "Daily Precipitation",
       col = "blue",
       cex = .5, cex.main = .8)

## Moving over a month
moving_prcp <- stats::filter(prcp, filter = rep(1/30,30), sides = 1)
tsplot(moving_prcp,
       xlab = "Time 1991 to 2020",
       ylab = "Daily Precipitation",
       main = "Moving (Monthly) Daily Precipitation",
       col = "blue",
       cex = .5, cex.main = .8)

## Moving over a year
moving_prcp <- stats::filter(prcp, filter = rep(1/365,365), sides = 1)
tsplot(moving_prcp,
       xlab = "Time 1991 to 2020",
       ylab = "Average Temp.",
       main = "Moving (Yeary) Daily Precipitation",
       col = "blue",
       cex = .5, cex.main = .8)

write.csv(new.data,file = paste(path,type,"San Antonio Airport (SAT) climate data (1948 to 2025)rev.csv"))
