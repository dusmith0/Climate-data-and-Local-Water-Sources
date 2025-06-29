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
summary(fit_temp_detrended)

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



