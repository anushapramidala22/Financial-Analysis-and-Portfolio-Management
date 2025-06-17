library(wooldridge)
library(xts)
library(dplyr)
library(quantmod)
library(sandwich)
library(lmtest)
library(car)
library(parallel)
library(tidyverse)
library(rugarch)
library(here)
library(plotly)
library(readxl)
library(tseries)
library(forecast)
library(vars)
library(MTS)
library(fUnitRoots)
library(zoo)
library(lattice)
library(grid)

data("intdef")
index <- zoo::as.yearmon(intdef$year + 11/12)
intdef.xts <- xts(intdef[ , -1], order.by = index)
intdef.xts <- intdef.xts[, c("i3", "inf", "def")]
colnames(intdef.xts) <- c("Tbill3mo", "inflation", "deficit")
plot(intdef.xts$inflation, 
     main = "Inflation, Deficits, and Interest Rates",
     legend.loc = "topleft")

##############################################################################################################
# 1. Create a model3 and a model4 using three and six lags for inflation. 
#    Test for autocorrelation of order 1 and order 12 using the LM test. Do you find autocorrelation? Explain. 
##############################################################################################################

intdef.xts$inflationlag1 <- dplyr::lag(intdef.xts$inflation, n=1)
intdef.xts$inflationlag2 <- dplyr::lag(intdef.xts$inflation, n=2)
intdef.xts$inflationlag3 <- dplyr::lag(intdef.xts$inflation, n=3)
intdef.xts$inflationlag4 <- dplyr::lag(intdef.xts$inflation, n=4)
intdef.xts$inflationlag5 <- dplyr::lag(intdef.xts$inflation, n=5)
intdef.xts$inflationlag6 <- dplyr::lag(intdef.xts$inflation, n=6)

# Run a Linear regression model with 3 lags
model3 <- lm(inflation ~ inflationlag1 + inflationlag2 + inflationlag3, data = intdef.xts)


# Check for autocorrelation in model3
resid_model3 <- resid(model3)
plot(resid_model3)
tsdisplay(resid_model3)
hist(resid_model3)
bgtest(model3, order = 1)
bgtest(model3, order = 12)

# Run a Linear regression model with 6 lags
model4 <- lm(inflation ~ inflationlag1 + inflationlag2 + inflationlag3 + 
             inflationlag4 + inflationlag5 + inflationlag6, 
             data = intdef.xts)


# Check for autocorrelation in model4
resid_model4 <- resid(model4)
plot(resid_model4)
tsdisplay(resid_model4)
hist(resid_model4)
bgtest(model4, order = 1)
bgtest(model4, order = 12)

##############################################################################################################
# 2. Test for Heteroskedasticity with the BP test and, if necessary, estimate the model using 
#    HAC standard errors. Did the errors change?
##############################################################################################################

# Check for heteroskedasticity (Breusch-Pagan test)
bptest(model3)
bptest(model4)

# Below 5%, then it is heteroskedastic
# Maybe we can use HAC errors to correct our Heteroskedasticity
coeftest(model3, df = Inf, vcov = NeweyWest)
coeftest(model4, df = Inf, vcov = NeweyWest)

# Compare standard errors before and after HAC correction
se_before_model3 <- sqrt(diag(vcov(model3)))
se_before_model4 <- sqrt(diag(vcov(model4)))
se_after_model3 <- sqrt(diag(NeweyWest(model3)))
se_after_model4 <- sqrt(diag(NeweyWest(model4)))

se_comparison_model3 <- data.frame(
  Coefficients = names(coef(model3)),
  SE_Before = se_before_model3,
  SE_After_HAC = se_after_model3
)

se_comparison_model4 <- data.frame(
  Coefficients = names(coef(model4)),
  SE_Before = se_before_model4,
  SE_After_HAC = se_after_model4
)

print(se_comparison_model3)
print(se_comparison_model4)

##############################################################################################################
# 3. Plot the residuals for model3 and model4, and their ACF and PACF. 
#    Do you find any significant lags? What does that mean? 
##############################################################################################################

# Plotting the residuals for model3
resid_model3 <- resid(model3)
plot(resid_model3, main = "Residuals of Model 3")
tsdisplay(resid_model3)

# Plotting the residuals for model4
resid_model4 <- resid(model4)
plot(resid_model4, main = "Residuals of Model 4")
tsdisplay(resid_model4)

##############################################################################################################
# 4. Using the EUStockMarkets dataset, remove seasonality from each of the series. 
#    Use the method you judge appropriate and justify your choice. 
##############################################################################################################

class(EuStockMarkets)
head(EuStockMarkets)
plot.ts(EuStockMarkets)
frequency(EuStockMarkets)

# Check for stationarity for each of the markets
apply(EuStockMarkets, 2, adf.test)


# Extract the time series for each of the markets into seperate variables
DAX_1 = EuStockMarkets[,1]
SMI_1 = EuStockMarkets[,2]
CAC_1 = EuStockMarkets[,3]
FTSE_1 = EuStockMarkets[,4]


# Using multiplicative decompostion method to remove seasonality
dax_mul = decompose(DAX_1, type = "multiplicative")
smi_mul = decompose(SMI_1, type = "multiplicative")
cac_mul = decompose(CAC_1, type = "multiplicative")
ftse_mul = decompose(FTSE_1, type = "multiplicative")


# Calculate variances for multiplicative models
var_dax_mul <- var(dax_mul$random, na.rm = TRUE)
var_smi_mul <- var(smi_mul$random, na.rm = TRUE)
var_cac_mul <- var(cac_mul$random, na.rm = TRUE)
var_ftse_mul <- var(ftse_mul$random, na.rm = TRUE)

# Deseasonalise each of the individual time series
DAX_seasonal_adj = DAX_1 / dax_mul$seasonal
SMI_seasonal_adj = SMI_1 / smi_mul$seasonal
CAC_seasonal_adj = CAC_1 / cac_mul$seasonal 
FTSE_seasonal_adj = FTSE_1 / ftse_mul$seasonal

# Create a new multivariate time series by combining the deasonalised univariate time series of each market
combined_deasonalised_mts = cbind(DAX_seasonal_adj, SMI_seasonal_adj, CAC_seasonal_adj, FTSE_seasonal_adj) 

# Rename the columns
colnames(combined_deasonalised_mts) <- c("DAX", "SMI", "CAC", "FTSE")

# Plot the deseasonalised MTS and the original MTS
plot(combined_deasonalised_mts)
plot(EuStockMarkets)

##############################################################################################################
# 5. Estimate an auto.arima model for each series, report and explain briefly the results. 
#    Are the series stationary? 
##############################################################################################################

DAXarima <- auto.arima(DAX_seasonal_adj)
SMIarima <- auto.arima(SMI_seasonal_adj)
CACarima <- auto.arima(CAC_seasonal_adj)
FTSEarima <- auto.arima(FTSE_seasonal_adj)

summary(DAXarima)
summary(SMIarima)
summary(CACarima)
summary(FTSEarima)

##############################################################################################################
# 6. Build a new dataset (EuStockMarketsDIFF), which contains the series adjusted for seasonality. 
#    Then run the ADF test to check for stationarity. Do the results coincide with the auto.arima analysis? 
#    In case they are not stationary, difference them. 
##############################################################################################################

# Difference the dataset to make it stationary (this will remove the trend component in each of the series)
EuStockMarketsDIFF = diffM(combined_deasonalised_mts)

# Check the stationarity of each column after deseasonalizing and differencing
apply(EuStockMarketsDIFF, 2, adf.test)

# Selects the optimal number of lags for the VAR model using information criteria 
# (AIC, HQ, SC, FPE) on the differenced EuStockMarkets dataset, without including a constant term.
VARselect(EuStockMarketsDIFF, type = "none", lag.max = 10)

##############################################################################################################
# 7. Estimate the VAR model selecting the lags with an information criterion of your preference. 
#    Interpret the coefficients. 
##############################################################################################################

# Estimating the model using 9 lags, as suggested by Akaike Information Criteria
var_model_lag9 <- vars::VAR(EuStockMarketsDIFF, p = 9, type = "const")
summary(var_model_lag9)

##############################################################################################################
# 8. Apply the Granger causality test to all your variables and report whether they should be kept or not. 
#    If you decide to remove a variable from the model, re-estimate the VAR without it.
##############################################################################################################

causality(var_model_lag9, cause = c("DAX"))
causality(var_model_lag9, cause = c("SMI"))
causality(var_model_lag9, cause = c("CAC"))
causality(var_model_lag9, cause = c("FTSE"))

##############################################################################################################
# 9. Report a forecast for the next 25 months of each of the variables in the VAR you identified.
##############################################################################################################

# Generate a forecast using the VAR model (with lag = 9) for the next 25 time steps
forecast_var_model = predict(var_model_lag9, n.ahead = 25)

predict(var_model_lag9, n.ahead = 25)

# Forecasted DAX values from VAR model
DAX_forecast = forecast_var_model$fcst$DAX[,1]
SMI_forecast = forecast_var_model$fcst$SMI[,1]
CAC_forecast = forecast_var_model$fcst$CAC[,1]
FTSE_forecast = forecast_var_model$fcst$FTSE[,1]

# Get the last actual DAX value (scalar)
last_DAX_value = tail(EuStockMarkets[,1], 1)
last_SMI_value = tail(EuStockMarkets[,2], 1)
last_CAC_value = tail(EuStockMarkets[,3], 1)
last_FTSE_value = tail(EuStockMarkets[,4], 1)

# Convert the last DAX value into a time-series object, aligning it with forecast length
last_DAX_value_ts = ts(rep(last_DAX_value, length(DAX_forecast)), start = end(EuStockMarkets) + c(1/260), frequency = 260)
last_SMI_value_ts = ts(rep(last_SMI_value, length(SMI_forecast)), start = end(EuStockMarkets) + c(1/260), frequency = 260)
last_CAC_value_ts = ts(rep(last_CAC_value, length(CAC_forecast)), start = end(EuStockMarkets) + c(1/260), frequency = 260)
last_FTSE_value_ts = ts(rep(last_FTSE_value, length(FTSE_forecast)), start = end(EuStockMarkets) + c(1/260), frequency = 260)

# Re-integrate differenced forecast values by adding to the last DAX value
DAX_predicted = last_DAX_value_ts + cumsum(DAX_forecast)
SMI_predicted = last_SMI_value_ts + cumsum(SMI_forecast)
CAC_predicted = last_CAC_value_ts + cumsum(CAC_forecast)
FTSE_predicted = last_FTSE_value_ts + cumsum(FTSE_forecast)

# Combine the actual data and the forecasted values
DAX_combined_with_forecast = c(EuStockMarkets[,1], DAX_predicted)
SMI_combined_with_forecast = c(EuStockMarkets[,2], SMI_predicted)
CAC_combined_with_forecast = c(EuStockMarkets[,3], CAC_predicted)
FTSE_combined_with_forecast = c(EuStockMarkets[,4], FTSE_predicted)

# Create a zoo object for the combined data
DAX_zoo_combined = zoo(DAX_combined_with_forecast)
SMI_zoo_combined = zoo(SMI_combined_with_forecast)
CAC_zoo_combined = zoo(CAC_combined_with_forecast)
FTSE_zoo_combined = zoo(FTSE_combined_with_forecast)

# Zoom in on the last 100 values (75 original + 25 predicted)
last_100_DAX = zoo(DAX_zoo_combined[(length(DAX_zoo_combined)-99):length(DAX_zoo_combined)])
last_100_SMI = zoo(SMI_zoo_combined[(length(SMI_zoo_combined)-99):length(SMI_zoo_combined)])
last_100_CAC = zoo(CAC_zoo_combined[(length(CAC_zoo_combined)-99):length(CAC_zoo_combined)])
last_100_FTSE = zoo(FTSE_zoo_combined[(length(FTSE_zoo_combined)-99):length(FTSE_zoo_combined)])


# Plot DAX
xyplot(last_100_DAX, grid=TRUE, panel = function(x, y, ...) {
  # Plot both actual and forecasted data as a continuous line (blue + red)
  panel.xyplot(x, y, col="blue", ...)
  panel.xyplot(x[76:100], y[76:100], col="red", lwd=2, ...)
}, main = "DAX Forecast (Last 100)")


# Plot SMI
xyplot(last_100_SMI, grid=TRUE, panel = function(x, y, ...) {
  # Plot both actual and forecasted data as a continuous line (blue + red)
  panel.xyplot(x, y, col="blue", ...)
  panel.xyplot(x[76:100], y[76:100], col="red", lwd=2, ...)
}, main = "SMI Forecast (Last 100)")


# Plot CAC
xyplot(last_100_CAC, grid=TRUE, panel = function(x, y, ...) {
  # Plot both actual and forecasted data as a continuous line (blue + red)
  panel.xyplot(x, y, col="blue", ...)
  panel.xyplot(x[76:100], y[76:100], col="red", lwd=2, ...)
}, main = "CAC Forecast (Last 100)")


# Plot FTSE
xyplot(last_100_FTSE, grid=TRUE, panel = function(x, y, ...) {
  # Plot both actual and forecasted data as a continuous line (blue + red)
  panel.xyplot(x, y, col="blue", ...)
  panel.xyplot(x[76:100], y[76:100], col="red", lwd=2, ...)
}, main = "FTSE Forecast (Last 100)")

##############################################################################################################


