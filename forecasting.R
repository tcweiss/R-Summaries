
###############################################
###############################################
###                                         ###
###               FORECASTING               ###
###                                         ###
###############################################
###############################################


######################
##      Basics      ##
######################

library(fpp2)

# Create plot with faceting.
autoplot(a10, facets = TRUE)

# Create plot with seasonal grouping. Setting polar=TRUE shows it as a circle.
ggseasonplot(a10, polar = TRUE)

# Create sub-series plot with small plots for each period. The blue lines
# indicate the mean of the respective series.
ggsubseriesplot(a10)

# Create plot of time series vs different lags of itself.
gglagplot(a10)

# Create classic autocorrelation plot.
ggAcf(a10)

# Perform Ljung-Box test to test if all autocorrelation up to lag 10 are zero.
Box.test(a10, lag = 10, type = "Ljung")

# Get subset of time series. This can be used to split the data into training
# and test set after you looked at it.
window(a10, start = 1992, end = 2005)



###########################
##      Forecasting      ##
###########################

# Get naive forecast for next 10 periods. The forecast is always the last value
# in the provided series, but the CIs around the forecasts differ between
# periods.
fc_naive <- naive(goog, 10)
summary(fc_naive)


# Get seasonal naive forecast for next 10 periods. This only works with data
# that also has smaller time intervals than years, e.g. years and quarters. The
# forecast in any given period will be the previous year's value in the same
# period.
fc_snaive <- snaive(ausbeer, 10)
summary(fc_snaive)


# Get a mean-forecast for the next 10 periods. This is just the mean of the
# provided series.
fc_mean <- meanf(ausbeer, 10)
summary(fc_mean)


# Get exponential smoothing (moving average) forecast. The first argument is a
# data set, the second one are the periods to forecast. The "alpha" argument is
# the smoothing coefficient, which is the same as lambda in the finance lecture
# notes. If it is not specified, it will be estimated. The "initial"
# argument stands for the method to compute the first value of the recursive
# series, either be "simple" (using the first value of the series) or "optimal"
# (estimated).
fc_ses <- ses(marathon, 10, alpha = NULL, initial = "simple")
summary(fc_ses)


# We can also do EMA with a (local) trend. The standard model above is 
# l_t = (α * y_t) + (1-α) * (l_t-1). With a linear trend, we instead have 
# l_t = (α * y_t) + (1-α) * (l_t-1 + b_t-1), where  b_t = β * (l_t) + (1-β) * b_t-1. 
# This means we need two more parameters: the smoothing parameter for the trend
# β, and the initial value for the trend b_0. The trend will be linear by
# default, but setting "damped=TRUE" gives a non-linear trend.
fc_holt <- holt(marathon, 10, alpha = NULL, beta = NULL, initial = "simple", damped = FALSE)
summary(fc_holt)


# We can also do EMA with trend and a seasonal component. The "additive" version
# of this model is l_t = (α * y_t - s_t-m) + (1-α) * (l_t-1), where as before
# b_t = β * (l_t) + (1-β) * b_t-1 and now also s_t =  γ(y_t-1 - l_t-1 - b_t-1) +
# (1- γ) * s_t-m. The "multiplicative" version is l_t = (α * (y_t/s_t-m)) +
# (1-α) * (l_t-1), where as before b_t = β * (l_t) + (1-β) * b_t-1 and γ(y_t /
# (l_t-1 - b_t-1)) + (1-γ) * s_t-m. You can choose with the seasonal argument.
# If the seasonal variation increases over time, the multiplicative version is better.
fc_hw <- hw(marathon, 10, alpha = NULL, beta = NULL, initial = "simple", damped = FALSE, seasonal = "additive")


# If we also include additive/multiplicative errors, we get an ETS model (error,
# trends, seasonality). The only argument is a series, while all other
# parameters will be estimated. The output is a model instead of a forecast, and
# the first line tells you the chosen specification. For the code below, we get
# ETS(M,Ad,N) (multiplicative errors, adjusted=damped trend, no seasonality).
# The get actual forecasts, you need to pass the output to forecast(). Note that
# this does not work well for all series, as the seasonality may be ignored.
ETS <- ets(marathon)
forecast(ETS)


# Get forecast from arima model. Here, all input parameters will be estimated
# with MLE. Setting "stepwise = FALSE" can make the estimation take longer, but
# may yield lower error measures.
auto.arima(marathon, stepwise = TRUE)


# Check residuals from forecast object. This takes the output from a forecast
# (like naive()) and returns a time plot, ACF plot, histogram and a Ljung-Box
# test of the residuals. If our forecast is good, the residuals should be
# uncorrelated (otherwise we can exploit more information), and they should have
# zero mean (otherwise it is biased and we can easily correct it). If we want to
# compute CIs, residuals should also have constant variance and be normally
# distributed. If the first three are satisfied, the residual ts will look like
# white noise; if all four are satisfied, it should look like Gaussian white
# noise.
checkresiduals()


# Get different error measures. The first argument is a forecast object, the
# second one is a ts. Among others, we get the MAE and MSE (depends on scale, so
# not good to compare series with different scales), the MAPE (good for
# comparisons, but only if data is all positive and values have no small or zero
# values), and the MASE (best for comparisons). Note that residuals come from
# IS-tests, while errors come from OOS-tests.
accuracy(fc, series)


# Plot observed values and add fitted values from forecast to plot.
autoplot(fc) +
  autolayer(fitted(fc))


################################
##      Cross-Validation      ##
################################

# Get cross-validated errors. The first argument is a data set, the second one a
# forecast function, and the third one the number of periods to forecast (to be
# passed to the forecast function). If we don't have enough data to split into
# test/training sets, cross-validation can be used to evaluate the accuracy of
# an approach. In time series, this is done by training a model with the first t
# values, and using this to forecast period t+1. Then we train the model again
# using the first t+1 values, and we use it to produce a forecast for period
# t+2, and so on. The function below does this. For example, running the code
# below will give a dataframe with 1000 rows (the number of observations in
# goog), and 5 columns (the number of forecasted periods). The values in the
# first row are the differences between: observed values in periods 2-6, and
# forecast using data in period 1.
tsCV(goog, forecastfunction = naive, h = 5)


# Note that the above function only gives "raw errors". If we want error
# measures, we need to compute them ourselves afterwards. For example, MSE can
# be computed as:
e <- tsCV(goog, forecastfunction = naive, h = 5)
mse <- colMeans(e^2, na.rm = TRUE)
data.frame("h" = 1:5, "MSE" = mse) %>%
  ggplot(aes(x = h, y = MSE)) +
    geom_point()


