
###############################################
###############################################
###                                         ###
###               ARIMA MODELS              ###
###                                         ###
###############################################
###############################################


######################
##      Basics      ##
######################

library(astsa)

# Create ACF & PACF plots. If ACF decays to zero and PACF drops to zero, try an
# AR. If ACF drops to zero and PACF decays to zero, try a MA. If both are
# decaying to zero for longer lags, and ARMA might make sense.
acf2(process)


######################
##       ARMA       ##
######################

# Estimate AR(1), MA(1) and ARMA(1,1) and get nice residual plots for free. Try
# multiple models to see what works best. The standardized residuals should look
# stationary, the ACF should be mostly within the CI, the QQ plot should be
# close to normality, and the values for the Ljung-Box test should be mostly
# above the blue line (suggesting residuals are white noise). Also, AIC and BIC
# should be as low as possible (not just as close to zero as possible).
sarima(process, p = 1, d = 0, q = 0)
sarima(process, p = 0, d = 0, q = 1)
sarima(process, p = 1, d = 0, q = 1)


######################
##       ARIMA      ##
######################

# Simulate 100 values from an  ARIMA(1,1,0) process. Note that a process
# exhibits ARIMA behavior if the differenced process shows ARMA behavior (as
# does the acf/pacf of the difference process). The second number indicates how
# many differences are required to get an ARMA process. In addition, need to
# specify the coefficients of the AR/MA parts of the process (here: ar = 0.9).
# So e.g. running the code below gives an ARIMA process, and taking first
# differences gives an ARMA(1,0) with slope coefficient 0.9.
process <- arima.sim(list(order = c(1,1,0), ar = 0.9), mean = 0, sd = 1, n = 100)
plot(process, main = "ARIMA(p = 1, d = 1, q = 0)")
plot(diff(process), main = "ARMA(p = 1, q = 0)")


# Estimate ARIMA(1,1,1) model and get nice plots for free. As with other models,
# try different specifications and choose the model with the best fit. Note that
# instead of estimating an ARIMA on the original data, we could also run an ARMA
# on the differenced data.
sarima(process, p = 1, d = 1, q = 1)


######################
##  Seasonal ARIMA  ##
######################

# Simulate 100 values from a SARIMA(0,0,0) x (1,1,0)_5 process. Note that a
# process with seasonal ARIMA behavior is like the unseasonal version, except
# that there is an additional autocorrelation component at a higher lag than 1.
# If the first three arguments (for the non-seasonal part) are zero, it is
# called a "pure" seasonal process (as below). For 5 lags, the acf/pacf will
# then look as described before, but the lags for anything but 5, 10, 15,...
# will be zero. If we also have a non-seasonal component, the acf/pacf will show
# autocorrelation at other lags too. 
process <- sarima.sim(ar = 0,     # Non-seasonal AR component
                      d = 0,      # Non-seasonal difference
                      ma = 0,     # Non-seasonal MA component
                      sar = 0.9,  # Seasonal AR component
                      D = 1,      # Seasonal difference
                      sma = NULL, # Seasonal MA component
                      S = 5,      # Seasonal lag
                      mean = 0, 
                      sd = 1, 
                      n = 100)


# Estimate SARIMA(1,1,0)_5 model and get nice plots for free. As with other
# models, try different specifications and choose the model with the best fit.
# Note that instead of estimating an ARIMA on the original data, we could also
# run an ARMA on the differenced data. The lowercase letters are for the
# original process, and can be set to zero. The uppercase letters are for
# seasonal models, i.e. *as if* we did not have seasonality. The uppercase S is
# for the number of lags at which this pattern occurs.
sarima(process, p = 0, d = 0, q = 0, P = 1, D = 1, Q = 0, S = 5)


######################
##     Forecasts    ##
######################

# Estimate a model, create forecast get nice plot for free. The grey areas around
# the forecast are ±1*RMSE and ±2*RMSE (the latter is like a 95% CI).
sarima.for(process, n.ahead = 20, p = 0, d = 0, q = 0, P = 1, D = 1, Q = 0, S = 5)


