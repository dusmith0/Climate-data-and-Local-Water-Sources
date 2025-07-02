## Transforming the data to stationary
##
##
## Transformations on the Population data
tsplot(water2$population)
par(mfrow = c(2,2))


# Dummy non adjusted fit
dummy <- ifelse(format(water$Date, "%Y") <= 2000 , 0, 1)
fit.dummy.noadjust  <- lm((population) ~ dummy*(time(population)) + dummy*time(population)^2,
                          data = water)
summary(fit.dummy.noadjust)
plot(fit.dummy.noadjust)

#*********************## Use this one?
# Dummy adjusted for coevaraite correlation

dummy <- ifelse(format(water$Date, "%Y") <= 2000 , 0, 1)
time2 <- (time(water$population) - mean(time(water$population)))^2
fit.dummy  <- lm((population) ~ dummy*(time(population)) + dummy*time2,
                 data = water)
summary(fit.dummy)
plot(fit.dummy)



## Attempting Detreading of the data
detrend.pop <- resid(fit.dummy)
plot(detrend.pop)
lines(lowess(detrend.pop))
acf1(resid(fit.dummy))
adf.test(resid(fit.dummy))


###----- Reattempting with new water$population transformation

## Possible Alternative for handling population data
for(i in seq(12,360,by=12)){
  if(!is.na(water$population[i + 1])){
    step <- (water$population[(i) + 1] - water$population[(i)])/12
  }else{
    step <- (2400000 - water$population[(i)])/12 #imputing upper bound from https://fred.stlouisfed.org/series/SATPOP
  }
  stepup <- cumsum(rep(step,11))
  water2$population[(i - 10):(i)] <-  water$population[(i - 10):(i)] + stepup
}

## Transformations on the Population data
tsplot(water2$population)

#Trying to fit a dummy variable for it
fit <- lm(population ~ time(population),
          data = water2)
summary(fit)
plot(fit)


#### working on transforming lake data if needed.
###*** This worked really well.
names(water)
stationary.test(diff(water$percentfull_lake))
fit <- lm(percentfull_lake ~ time(percentfull_lake),
          data = water)

plot(diff(water$percentfull_lake))
acf(diff(water$percentfull_lake))

percentfull_lake_detrend <- diff()


#### working on transforming well data
stationary.test(water$WaterElevation)
tsplot(water$WaterElevation)

tsplot(diff(water$WaterElevation))


#### working on precipitation data
stationary.test(water$PRCP)
tsplot(water$PRCP)
tsplot(diff(water$PRCP))


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
