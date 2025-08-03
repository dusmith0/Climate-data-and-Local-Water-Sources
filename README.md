# **Impact of Climate Change on Local Water Sources in San Antonio, TX**

##### **Author Names:** John Reid, Dustin Smith, Hélène LaVire, Derris Dabbs, Asia Cavazos


## **Goal** 
#### The goal of the current investigation is to study the potential association between climate change parameters, population, and Edwards Aquifer water elevation level in San Antonio, TX over the time period of 1991-2020. Global weather patterns and precipitation events appear to be changing in the area, both in frequency and intensity, as temperature measurements increase in range and volatility. This change in patterns could be associated with changes in the Edwards Aquifer, which provides almost 50% of the region’s municipal water supplies. Our aim is to attempt to model and forecast the changes in the J17 well. 

## **Methods**
#### We primaraly worked with regression and time series methods to analize the data. That is, we worked with simple linear regression, AR(p), MA(q), ARIMA(p,d,q), SARIMA(p,d,q)x(P,D,Q), GARCH(p), and multivariate VAR(p) models.


## **Conclusions**
#### A preliminary investigation of the data indicated trends in aquifer water elevation, increased range and variability in climate measurements, and a steady increase in the area’s population over the time period observed. AR(1), AR(3), SARIMA, and simple linear regression models were fit to the precipitation, water elevation, temperature, and population datasets (respectively) after examination of ACF and PACF plots. Maximum likelihood estimation of model parameters was utilized to develop final models for each time series variable. Customary diagnostics including investigation of residuals, Q-Q plots of residuals, and Ljung-Box (Q) statistics, and the comparison of forecasted values to known values for 2021 verified the validity of each model. Individual time series were analyzed for heteroscedasticity and appropriate GARCH models were fit.  While neither temperature nor precipitation series exhibited varying variances that would benefit from the inclusion of an ARCH model, the well water elevation fit well with an AR(1)-ARCH(1) model.  Both simple linear regression with autocorrelated errors and multivariate time series models were investigated as overall prediction models. Ultimately, a model incorporating the response time series of well water elevation with temperature and precipitation predictive variables in a multivariate time series resulted in the most appropriate predictive model. 



