library(fpp2)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(tsibbledata)
library(tsibble)
## 
## Attaching package: 'tsibble'
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, union
library(USgas)
library(tsibble)
library(fable)
## Loading required package: fabletools
## 
## Attaching package: 'fabletools'
## The following objects are masked from 'package:forecast':
## 
##     accuracy, forecast
library(forecast)
Problem 7
### Topic 8.8 Problem 7: Exponential Smoothing


##Plot the time-series data of gas production in Australia

# Select the "Gas" column from aus_production
gas_col <- select(aus_production, Gas)

# Convert the selected column to a tsibble object
gas_tsbl <- as_tsibble(gas_col)

# Convert the tsibble object to a time series object
gas_ts <- ts(gas_tsbl)

# Plot the time series and add a title
autoplot(gas_ts) + labs(title = 'Gas Production in Australia over Time')


## Creating and plotting the ETS Model
ets_model <- aus_production %>%
  model(fit = ETS(Gas))
report(ets_model)
## Series: Gas 
## Model: ETS(M,A,M) 
##   Smoothing parameters:
##     alpha = 0.6528545 
##     beta  = 0.1441675 
##     gamma = 0.09784922 
## 
##   Initial states:
##      l[0]       b[0]      s[0]    s[-1]    s[-2]     s[-3]
##  5.945592 0.07062881 0.9309236 1.177883 1.074851 0.8163427
## 
##   sigma^2:  0.0032
## 
##      AIC     AICc      BIC 
## 1680.929 1681.794 1711.389
forecast_result <- forecast(ets_model, h = 4)
forecast::autoplot(forecast_result, data = aus_production)


print("Exponential smoothing uses level, trend, and seasonal components to forecast time series data. The seasonal component captures regular patterns of variation within a year, which can be either additive or multiplicative. When the seasonal variation changes over time, a multiplicative seasonal component should be used to avoid biased forecasts.")
## [1] "Exponential smoothing uses level, trend, and seasonal components to forecast time series data. The seasonal component captures regular patterns of variation within a year, which can be either additive or multiplicative. When the seasonal variation changes over time, a multiplicative seasonal component should be used to avoid biased forecasts."
## Creating and plotting the Damped Model
damped_model <- aus_production %>%
  model(fit = ETS(Gas  ~ trend('Ad', phi = 0.85)))
report(damped_model)
## Series: Gas 
## Model: ETS(M,Ad,M) 
##   Smoothing parameters:
##     alpha = 0.5664944 
##     beta  = 0.2579526 
##     gamma = 0.08891314 
##     phi   = 0.85 
## 
##   Initial states:
##      l[0]       b[0]      s[0]    s[-1]    s[-2]     s[-3]
##  5.683111 0.04853571 0.9272599 1.180129 1.073889 0.8187214
## 
##   sigma^2:  0.0035
## 
##      AIC     AICc      BIC 
## 1691.745 1692.610 1722.205
forecast_result1 <- forecast(damped_model, h = 4)
forecast::autoplot(forecast_result1, data = aus_production)


print("Comparison of summary report with reference to AIC, AICc and BIC, and comparison of plots reveal that damping the model does not seem to enhance the accuracy of the forecasts.")
## [1] "Comparison of summary report with reference to AIC, AICc and BIC, and comparison of plots reveal that damping the model does not seem to enhance the accuracy of the forecasts."
