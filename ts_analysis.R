
###############################################
###############################################
###                                         ###
###           TIME SERIES ANALYSIS          ###
###                                         ###
###############################################
###############################################


######################
##      Basics      ##
######################

# Create ts object from vector.
ts(data_vector, start = 1980, end = 2010, frequency = 4)


# Check if object is ts.
is.ts(AirPassengers)


######################
##        EDA       ##
######################

# Create simple plot.
plot(EuStockMarkets, 
     xlab = "Year",
     ylab = "Index Value",
     main = "Major European Stock Indices, 1991-1998",
     type = "l")


# Create more complex plot.
ts.plot(EuStockMarkets, 
        col = 1:4, 
        xlab = "Year", 
        ylab = "Index Value", 
        main = "Major European Stock Indices, 1991-1998")


# Add legend to plot.
legend("topleft", 
       colnames(EuStockMarkets), 
       lty = 1, 
       col = 1:4, 
       bty = "n")


# Find time values of first and last observations.
start(AirPassengers)
end(AirPassengers)


# Get vector of times at which each observation was sampled.
time(AirPassengers)


# Get time interval between observations time unit.
deltat(AirPassengers)


# Get number of observations per time unit.
frequency(AirPassengers)


# Get position in cycle of each observation.
cycle(AirPassengers)



###############################
##      Handling Trends      ##
###############################

# Linearize ts with exponential growth/decay (only works for positive series).
# This makes sense if you see that the variance of the process is not constant.
# Note that the interpretation of the ts changes (to logged values).
linear_growth <- log(rapid_growth)


# Remove linear trend from ts (may also be used after logging). This makes sense
# if you see that the mean of the process is not constant. Note that first
# differencing gives a new ts with one observation less. Also, the
# interpretation of the ts changes (to changes between dates).
d_linear <- diff(linear_growth)


# Remove seasonal trends from ts. For example, monthly data may show a strong
# twelve-month pattern, so the year-on-year change is more interesting than the
# month-on-month changes. Same as before, but use higher-order differences (s=12
# for monthly or s=4 for quarterly). Note that this gives a new ts with s
# observations less. Again, the interpretation changes.
ds_seasonal <- diff(seasonal, lag = s)



###############################
##         Processes         ##
###############################

# Simulate 100 values from Gaussian white noise process.
wn_process <- arima.sim(model = list(order = c(0,0,0)), mean = 0, sd = 1,  n = 100)


# Estimate white noise model. Note that this is just an intercept, so we can
# get the same result by taking the mean of the series.
arima(wn_process, order = c(0,0,0))


# Simulate random walk process. Note that this is equivalent to a cumulative-WN
# process. Taking first differences gives a WN-process again. (Likewise, the
# cumulative sum of a WN-process gives a random walk.)
random_walk <- arima.sim(model = list(order = c(0,1,0)), mean = 0, sd = 1, n = 100)


# Simulate 100 values from an AR(1) process with slope of 0.5. Note that the
# "order" argument is optional.
ar <- arima.sim(model = list(order = c(1,0,0), ar = 0.5), mean = 0, sd = 1, n = 100)


# Estimate AR(1) model.
arima(ar, order = c(1,0,0))


# Simulate 100 values from an MA(1) process with slope of 0.5. Note that the
# "order" argument is optional.
ma <- arima.sim(model = list(order = c(0,0,1), ma = 0.5), mean = 0, sd = 1, n = 100)


# Estimate MA(1) model.
arima(ma, order = c(0,0,1))


# Make predictions from model for n periods ahead. You can subset the output to
# either "pred" or "se".
fc <- predict(model, n.ahead = n)
fc$pred
fc$se


# Make plot with original values and predictions.
ts.plot(process)
model_fit <- observed - resid(model)
points(model_fit, type = "l", col = 2, lty = 2)





