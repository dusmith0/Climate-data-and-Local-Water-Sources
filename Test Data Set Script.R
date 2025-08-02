## Building a simple test data se
library("tidyverse")



well.data <- read.csv("Original Data Sets/Well Depth Data/j17waterlevels.csv", skip = 6)

well <- well.data %>%
  mutate(DATE = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(DATE, WaterLevel, Change, WaterElevation)

#View(well)
#names(well)
monthly_averages <- well %>%
  select(DATE, WaterElevation) %>%
  group_by(year = year(DATE), month = month(DATE)) %>%
  summarise(WaterElevation = mean(WaterElevation, na.rm = TRUE))
#TSUN = mean(TSUN, na.rm = TRUE), #TSUN has been removed due to excessive missing data

well <- cbind(monthly_averages)


#Handling Missing WaterElevation data
well[which(is.na(well$WaterElevation)),]

missing <- which(is.na(well$WaterElevation))
for(i in missing){
  well$WaterElevation[i] <- mean(well$WaterElevation[c(i-2,i+2)],na.rm = TRUE)
}


sum(is.na(well$WaterElevation))
head(well$WaterElevation)
tail(well$WaterElevation)


#View(well)
well <- well[-1090,]


#### Fitting the complete data set
length(well$WaterElevation)
par(mfrow = c(1,1))
fitwell <- sarima((well$WaterElevation), p = 3, d = 0, q = 0)

sarima((well$WaterElevation), p = 3, d = 1, q = 0) #6.42
sarima((well$WaterElevation), p = 2, d = 1, q = 0) #6.43
sarima((well$WaterElevation), p = 1, d = 1, q = 0) #6.44
sarima((well$WaterElevation), p = 3, d = 0, q = 0) #6.38
sarima((well$WaterElevation), p = 2, d = 0, q = 0) #6.4
sarima((well$WaterElevation), p = 1, d = 0, q = 0) #6.42


model <- (fitwell$ttable[1]*(lag(well$WaterElevation,1) - fitwell$ttable[4]) +
            #fitwell$ttable[2]*(lag(well$WaterElevation,2) - fitwell$ttable[4]) +
            fitwell$ttable[3]*(lag(well$WaterElevation,3) - fitwell$ttable[4]) +
            fitwell$ttable[4])

tsplot(well$WaterElevation, ylab = "Water Elevation", lwd = 1.5)
lines(x = c(1:length(model)), y = model,
      col = "purple", lwd = 1.5, lty = "dashed")
legend("topleft",legend = c("Original Data", "Model"),
       col = c("black","purple"),
       cex = .8, lwd = 2, lty = c(1,2))

fit <- sarima.for(as.ts(well$WaterElevation),
                  n.ahead = 12, p = 3, d = 0, q = 0,)
legend("topright",legend = c("Original Data","forecast","2021 observed data"),
       col = c("black","firebrick","purple"),
       cex = .8, lwd = 2, lty = c(1,1))

## Attempting GARCH with returns  ## Dustin Again
par(mfrow = c(1,1))
ret.well <- (diff(log(well$WaterElevation)))

# Checking the tsplot and for centeral means
par(mfrow = c(2,1))
tsplot(well$WaterElevation, ylab = "Elevation", main = "Well Elevation")

tsplot(ret.well, ylab = "Returns", main = "Returns on Well Variance")
mean(ret.well) ## Very close to zero

# Checking ACF and PACF
acf2(ret.well, main = "Well Returns", col = "steelblue", lwd = 1.5)
fit <- sarima(diff(log(well$WaterElevation)), p = 3, d = 0, q = 0)

resids <- resid(fit$fit)^2
acf2(resids)

# Trying AR(1)-ARCH(1)
fitg1 <- garchFit(~arma(1,0) + garch(1,1), data = ret.well, include.mean = FALSE)
summary(fitg1)
fitg2 <- garchFit(~arma(3,0) + garch(1,0), data = ret.well, include.mean = FALSE)
summary(fitg2)

model1 <- (.2696*lag(ret.well,1))
model2 <- (.3036*lag(ret.well, 1) + -.7926*lag(ret.well, 2) + -.5166*lag(ret.well, 3))

sd1 <- volatility(fitg1)
sd2 <- volatility(fitg2)

tsplot(ret.well, ylab = "Returns", main = "Returns on Well data mean estimates", lwd = 1.25)
lines(model1, col = "steelblue", lwd = 1.5)
#lines(model2, col = "firebrick", lwd = 1.5)
legend("topleft",legend = c("AR(1)-ARCH(1)"),
       col = c("steelblue"),
       cex = .5, lwd = 2, lty = c(1,1))

tsplot(ret.well, ylab = "Returns",
       main = "Returns on Well Variance estimates", lwd = 1.25)
lines(model1 + (sd1) - .001, col = "steelblue", lty = "dashed", lwd = 1.5)
#lines(model2 - (sd2) + .001, col = "firebrick", lty = "dashed", lwd = 1.5)
legend("topleft",legend = c( "AR(1)-ARCH(1)"),
       col = c("steelblue"),
       cex = .5, lwd = 2, lty = c(2,2))

plot(sd1, type = 'l', ylab = "Variance Return", xlab = "time",
     main = "Variation AR(1)-ARCH(1)", col = "steelblue", lwd = 1.5)
plot(sd2, type = 'l', ylab = "Variance Return", xlab = "time",
     main = "Variation AR(3)-ARCH(1)", col = "firebrick")


## Back Transforming the model
par(mfrow = c(1,1))
backmodel1 <- c(0)
backmodel2 <- c(0)
for(i in 1:length(model1)){
  backmodel1[i] <- exp(model1[i] + c(1,log(well$WaterElevation))[i])
  #backmodel2[i] <- exp(model2[i] + c(1,log(well$WaterElevation))[i])
}


par(mfrow = c(1,1))
tsplot(c(well$WaterElevation), ylab = "Elevation", lwd = 1.5,
       main = "Well Elevation with AR-ARCH mean estimates")
lines(backmodel1, col = "steelblue", lwd = 1.5, lty = 2)
#lines(backmodel2, col = "firebrick", lwd = 1.5)
legend("topright",legend = c("Original", "AR(1)-ARCH(1)"),
       col = c("black","steelblue"),
       cex = .8, lwd = 2, lty = c(1,1))

## Fitting new forecasts
forecast_mod1 <- predict(fitg1, n.ahead = 24)
forecast_mod2 <- predict(fitg1, n.ahead = 24)
names(forecast_mod1)
forbackmodel1 <- exp(forecast_mod1$meanForecast[1] + log(well$WaterElevation)[1089])
forbackmodel2 <- exp(forecast_mod2$meanForecast[1] + log(well$WaterElevation)[1089])
#This bit is necessary as the first forecast begins with the last known value
for(i in 2:24){
  forbackmodel1[i] <- exp(forecast_mod1$meanForecast[i] + log(forbackmodel1)[i - 1])
  forbackmodel2[i] <- exp(forecast_mod2$meanForecast[i] + log(forbackmodel2)[i - 1])
}
# Indexing by 2 to avoid the pre-forecaster value, then the summed previouse value gets i - 1
# to begin at 1 again.

# genreating confidence intervals
par(mfrow = c(1,1))
forecast_mod1 <- predict(fitg1, n.ahead = 24)
forecast_mod2 <- predict(fitg1, n.ahead = 24)
interval1 <- cbind(forecast_mod1$meanForecast + 1*1.96*forecast_mod1$meanError,
                   forecast_mod1$meanForecast - 1*1.96*forecast_mod1$meanError)

intforbackmodel1 <- matrix(nrow = 24, ncol = 2)
intforbackmodel1[1,] <- exp(interval1[1,] + log(well$WaterElevation[1089]))
#This bit is necessary as the first forecast begins with the last known value
for(i in 2:24){
  intforbackmodel1[i,1] <- exp(interval1[i,1] + log(forbackmodel1[i - 1]))
  intforbackmodel1[i,2] <- exp(interval1[i,2] + log(forbackmodel1[i - 1]))
}


tsplot(well$WaterElevation[(1089 - 60):(1089 + 24)], main = "Forecast AR(1)-ARCH(1) Water Elevation Data", ylab = "Elevation", lwd = 1.5)
lines(x = c(61:(60+24)), y = forbackmodel1, col = "steelblue", lwd = 1.5)
lines(backmodel1[(1089 - 60):(1089 + 24)], col = "steelblue", lwd = 1.5, lty = 2)
#lines(x = c(161:172), y = forbackmodel1, col = "firebrick", lwd = 1.5)
polygon(c(c(61:(60+24)),rev(c(61:(60+24)))), c(intforbackmodel1[,1],rev(intforbackmodel1[,2])),
        col = "steelblue", density = 50)
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
       col = "black", lwd = 1.5)
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


