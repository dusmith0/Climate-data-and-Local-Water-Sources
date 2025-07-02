library(GGally) #extends ggplot2
library(dplyr) #filter(), select(), mutate(), group_by(), summarize(), arrange()
library(readr) #for reading in csv's
library(lubridate) #date and time handling
library(tseries) #Augmented Dickey-Fuller (ADF) test for stationarity
library(ggplot2) #data visualization

# Load the CSV
df <- read_csv("MonthlyData.csv") 

# For MM/DD/YYYY, to convert to YYYY-MM-DD
df <- df %>%
  mutate(Date = parse_date(as.character(Date), format = "%m/%d/%Y"))

#initial regression
fit_monthly <- lm(WaterElevation ~ Temp + Precipitation + Population, data = df)
summary(fit_monthly)

# Handling Missing WaterElevation data (from Dustin)
df[which(is.na(df$WaterElevation)), ] #isolate cells in the df that are na in WaterElevation column
missing <- which(is.na(df$WaterElevation)) #apply these cells to variable called missing
for (i in missing) { 
  df$WaterElevation[i] <- mean(df$WaterElevation[c(i - 2, i + 2)], na.rm = TRUE)
} 
#Loops over each missing index `i`.
#Replaces the missing value at row `i` with the **mean of two surrounding values**:
#One from 2 rows before: `i - 2`
#One from 2 rows after: `i + 2`
#`na.rm = TRUE` ensures the mean is computed even if one of those neighbors is also missing.
df[missing, ]
#Displays the rows that were previously missing, now after being filled in


# Custom panel function to display correlation
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1.2, ...) {
  #Defines a custom function called `panel.cor` to be used as a panel in `pairs()`.
  #**Arguments**:
  #`x`, `y`: the variables for that panel
  #`digits`: number of digits to display
  #`prefix`: optional string before the correlation (e.g., `"r = "`)
  #`cex.cor`: text size scaling
  #`...`: extra arguments (not used here but accepted)
  r <- cor(x, y, use = "complete.obs") #Pearson correlation, complete.obs skips missing values
  txt <- formatC(r, format = "f", digits = digits)
  txt <- paste0(prefix, txt) #formatting commands
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.5, txt, cex = cex.cor)
}

# Subset your data
df_subset <- df %>%
  select(WaterElevation, Temp, Precipitation, Population)

# Scatterplot matrix with correlation coefficients
pairs(df_subset,
      upper.panel = panel.cor, #panel correlations we just defined
      main = "Scatterplot Matrix with Correlation Coefficients")

#Mutate date to correct form YYYY-MM-DD
df <- df %>%
  mutate(Date = mdy(Date)) 

#bug check for date column
names(df)                # Should show both "Temp" and "Date"
summary(df$Temp)         # Should show Min/Mean/etc.
summary(df$Date)         # Should be a proper Date object, not all NA

#Since you know the data is monthly and there are 360 rows, you can reconstruct the date column using seq.Date():
#Used cause I had problems getting Date column to work
df <- df %>%
  mutate(Date = seq.Date(from = as.Date("1991-01-01"), by = "month", length.out = n()))


#If needed, drop rows with missing Temp or Date:
df_clean <- df %>%
  filter(!is.na(Temp), !is.na(Date))



#Non-detrended Temp lag 12
df <- df %>%
  arrange(Date) %>% #get dates in numerical order
  mutate(Temp_seasonal = Temp - lag(Temp, 12))

#Augmented Dickey-Fuller Test for stationary of variable (Temp_seasonal)
#filtered out first 12 values, which are NA due to 12-month lag
df_clean <- df %>% filter(!is.na(Temp_seasonal))
adf.test(df_clean$Temp_seasonal, alternative = "stationary")

#Regression temp lag 12, which is supposedly now stationary
fit_temp_seasonal <- lm(WaterElevation ~ Temp_seasonal + Precipitation + Population, data = df)
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

# Define custom correlation panel for correl matrix with lag 12 temp
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1.2, ...) {
  r <- cor(x, y, use = "complete.obs")
  txt <- formatC(r, format = "f", digits = digits)
  txt <- paste0(prefix, txt)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.5, txt, cex = cex.cor)
}

# Build subset for the matrix
df_subset <- df %>%
  select(WaterElevation, Temp_seasonal, Precipitation, Population) %>%
  filter(complete.cases(.))  # remove NAs for clean plotting

# Create the scatterplot matrix
pairs(df_subset,
      upper.panel = panel.cor,
      main = "Scatterplot Matrix (with Stationary Temperature)")

#More Dickey-Fuller tests for stationarity
# Precipitation (raw or transformed)
adf.test(na.omit(df$Precipitation), alternative = "stationary")

# Population
adf.test(na.omit(df$Population), alternative = "stationary")

# Water Elevation 
adf.test(na.omit(df$WaterElevation), alternative = "stationary")


#initial scatterplot for precip variance
plot(df$Date, df$Precipitation, type = "p",
     main = "Precipitation Over Time",
     xlab = "Date", ylab = "Precipitation")

#initial precip variance = 8.40653
var(df$Precipitation, na.rm = TRUE)

#attempting model with and without logged precip to calculate AIC
#Remember, Temp_seasonal is lag 12
fit1 <- lm(WaterElevation ~ Precipitation + Temp_seasonal + Population, data = df)
fit2 <- lm(WaterElevation ~ log1p(Precipitation) + Temp_seasonal + Population, data = df)

#AICs for both experimental fits
AIC(fit1, fit2)
summary(fit1)
summary(fit2)

#no need to transform precip, similar figures
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





###Everything below is for temperature both de-trended and lagged 12 months. 
###I chose not to detrend since we want to see the trend, right?

#-----------------------------------------------------------------------------
# Detrend temperature using linear regression
#model_trend <- lm(Temp ~ as.numeric(Date), data = df)
#df$temp_detrended <- resid(model_trend)
#
# df <- df %>%
#   arrange(Date) %>%
#   mutate(temp_detrended_seasonal = temp_detrended - lag(temp_detrended, 12))
# 
# summary(model_trend)
# 
# #Augmented Dickey-Fuller Test for stationary of variable (temp in this case)
# #filtered out first 12 values, which are NA due to 12-month lag
# df_clean <- df %>% filter(!is.na(temp_detrended_seasonal))
# adf.test(df_clean$temp_detrended_seasonal, alternative = "stationary")
# 
# fit_temp_detrended <- lm(WaterElevation ~ temp_detrended_seasonal + Precipitation + Population, data = df)
# summary(fit_temp_detrended)
# 
# 
# #scatterplot for temp_detrended_seasonal
# ggplot(df, aes(x = Date, y = temp_detrended_seasonal)) +
#   geom_point(color = "steelblue") +
#   geom_smooth(method = "lm", color = "red", se = FALSE) +
#   labs(
#     title = "Seasonally Detrended Temperature with Trend Line",
#     x = "Date", y = "Temp (Detrended & Deseasonalized)"
#   ) +
#   theme_minimal()
# 
# # Fit linear model
# model <- lm(temp_detrended_seasonal ~ as.numeric(Date), data = df)
# 
# # Extract coefficients
# intercept <- coef(model)[1]
# slope <- coef(model)[2]
# 
# # Print regression equation
# cat("Regression equation:\n")
# cat("y =", round(slope, 6), "* time +", round(intercept, 3), "\n")
# 
# 
# 
# 
# # Define custom correlation panel for correl matrix with detrended temp
# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1.2, ...) {
#   r <- cor(x, y, use = "complete.obs")
#   txt <- formatC(r, format = "f", digits = digits)
#   txt <- paste0(prefix, txt)
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   text(0.5, 0.5, txt, cex = cex.cor)
# }
# 
# # Build subset for the matrix
# df_subset <- df %>%
#   select(WaterElevation, temp_detrended_seasonal, Precipitation, Population) %>%
#   filter(complete.cases(.))  # remove NAs for clean plotting
# 
# # Create the scatterplot matrix
# pairs(df_subset,
#       upper.panel = panel.cor,
#       main = "Scatterplot Matrix (with Stationary Temperature)")


