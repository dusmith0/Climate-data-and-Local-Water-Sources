## Transforming the data to stationary
##
##
## Data of interest:  Water: TAVG, PRCP, Percentfull-lake, waterElevation, population

### Working with population data
names(water)

tsplot(water$population)
par(mfrow = c(2,2))


# Dummy non adjusted fit
dummy <- ifelse(format(water$Date, "%Y") <= 2000 , 0, 1)

fit.dummy.noadjust  <- lm(log((population)) ~ dummy*(time(population)) + dummy*time(population)^2,
                          data = water)
summary(fit.dummy.noadjust)
plot(fit.dummy.noadjust)

#*********************## Use this one?
# Dummy adjusted for coevaraite correlation

dummy <- ifelse(format(water$Date, "%Y") <= 2000 , 0, 1)
time2 <- (time(water$population) - mean(time(water$population)))^2
fit.dummy  <- lm(log(population) ~ dummy*(time(population)) + dummy*(time2),
                 data = water)
summary(fit.dummy)
plot(fit.dummy)

acf(resid(fit))


## Attempting Detreading of the data
detrend.pop <- resid(fit.dummy)
plot(detrend.pop)
lines(lowess(detrend.pop))
acf1(resid(fit.dummy))
adf.test(resid(fit.dummy))



#### working on transforming lake data if needed.
###*** This worked really well.
names(water)
stationary.test(diff(water$percentfull_lake))

tsplot((water$percentfull_lake), )
acf(water$percentfull_lake)

tsplot(diff(water$percentfull_lake, lag = 1))
acf(diff(water$percentfull_lake, lag = 1))



#### working on transforming well data
par(mfrow = c(2,1))
stationary.test(water$WaterElevation)
tsplot(water$WaterElevation)
acf(water$WaterElevation)

tsplot(diff(water$WaterElevation))
acf(diff(water$WaterElevation, lag = 1))


#### working on precipitation data
stationary.test(water$PRCP)
tsplot(water$PRCP)
acf(water$PRCP)

tsplot(diff(water$PRCP))
acf(diff(water$PRCP))

tsplot(diff(water$PRCP ^ (1/2)))
acf(diff(water$PRCP ^ (1/2)))


### Working with TAVG data
par(mfrow = c(2,3))

tsplot(water$percentfull_lake, main = "lake Data")
tsplot(water$WaterElevation, main = "Well Data")
tsplot(water$PRCP, main = "Percipitation")

tsplot(diff(water$percentfull_lake), main = "lake Data Differenced")
tsplot(diff(water$WaterElevation), main = "Well Data Differenced")
tsplot(diff(water$PRCP), main = "Percipitation Differenced")

adf.test(water$percentfull_lake)
acf((water$percentfull_lake))
acf((water$WaterElevation))
acf((water$PRCP))
acf(diff(water$percentfull_lake))
acf(diff(water$WaterElevation))
acf(diff(water$PRCP))
