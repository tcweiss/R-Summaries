
###############################################
###############################################
###                                         ###
###           TIME SERIES WITH XTS          ###
###                                         ###
###############################################
###############################################


######################
##      Basics      ##
######################

# Create an xts object. This is just a matrix with dates as row names. First
# argument must be a matrix, second one a vector of date variables in ascending
# order.

x <- xts(as.matrix(rnorm(20)), as.Date("2021-09-11") + 0:19)


# Get the index (=rownames in form of dates) of an xts object. Only argument
# must be an xts object.
 
index(x)


# Get the matrix of an xts object. Only argument must be an xts object.

coredata(x)


# Convert a time series to an xts object. Only argument must be an suitable time
# series. You may need to check the dates of the time series first, otherwise
# this will result in some garbage. For an example, run this function on the
# 'austres' dataset that ships with R.

as.xts(austres)


# Convert an xts object to a matrix again. Only argument must be an xts object.
# Dates as rownames will be preserved. This may be useful to send it to people
# who do not use xts or to apply functions that do not work with xts objects.

as.matrix(x)



###############################
##      Import & Export      ##
###############################

# You cannot directly import or export xts objects. However, you can import e.g.
# spreadsheets and then turn them into and xts as shown at the beginning.


# To export an xts object, use write.zoo. Required xts object and filename as
# argument. Unless this is not possible, I strongly recommend exporting to csv
# using the code below.

write.zoo(x, file = "x.csv", quote = FALSE, sep = ",")



######################################
##      Filtering Observations      ##
######################################

# You can filter xts object with row indexing using brackets. The row index will
# refer the the rownames (which are dates), and must be supplied in ISO
# standard. For example, getting observations from the year 2000:

x["2000"]

# Intervals are indicated by slashes For example, first day in year 2000 to last
# in 2001:

x["2000/2001"]

# Days also work. For example, 1 May 2000 to 31 Dec, 2001:

x["2000-05/2001"]

# Jan 1, 2000 to Feb 29, 2000:

x["'2000-01/02'"]

# For times, you have to put "T" befor the values. For example, all observations
# from 8:30 to 15:00:

x["T08:30/T15:00"]


# The xts equivalent to head() and tail() are first() and last(). You must
# supply and xts object and an indication of what should be extracted. The
# latter can either be an integer for the number of oberservations, or a string
# containing a number and the time range (e.g. weeks). If you use a string, time
# ranges like week or month will return the respective values like a calender.
# If a given week only has three days as entries, it will only return these three
# entries, not seven days.

first(x, 3)
last(x, 11)
first(x, "3 weeks")
last(x, "11 days")

# When using integers, put a minus in front to exclude only these values.

first(x, -3)
last(x, -11)


###############################
##      Joining Objects      ##
###############################

# You can use the classical join operations with xts object as well. However,
# you need to use the merge() function from base R, and all joins are made based
# on the rownames. The merge() function takes four arguments. The first two are
# two xts objects, the third is the type of join (inner, outer, left, right),
# and the fourth indicates which values should be used for non-matching
# observations. You can set it to nc.locf to replace missing values with the
# last non-missing value in a column.

merge(x, y, join = "inner", fill = 0)
merge(x, y, join = "left", fill = na.locf)


# You can also merge single integers, which creates a new column with this value
# only, or vector with the same number of values as the rows of the xts object.

merge(x, 3)
merge(x, c(1:4))


# The rbind() function works too, and it automatically inserts the object in the
# correct row based on the rowname (=date). You need two xts object with the
# same number of columns. The order in which they are supplied does not matter.

rbind(x, y)


##############################
##      Missing Values      ##
##############################

# Merging can often results in NAs. One first option to deal with this is
# na.locf(). You have seen it above already, but you can also apply it as a
# function directly to an xts object. It takes four arguments. The first is an
# xts object, the second indicates if NAs should be removed, the third indicates
# if the last non-missing value (forward replacing) or the next (backward
# replacing) should be used to fill missing values, the fourth argument sets an
# upper limit to how many adjacent NAs this should be applied.

na.locf(x, na.rm = TRUE, fromLast = FALSE, maxgap = Inf)


# You can also replace NAs directly like with this function. It takes two
# arguments; an xts object and the fill value.

na.fill(x, fill = 4)


# To remove NAs, you can use these two functions. The na.trim() function removes
# only rows with NAs that are at the beginning and at the end of the time
# series. The na.omit() function removes all rows that contain NAs. Both require
# an xts object as input.

na.trim(x)
na.omit(x)


# To replace missing values with estimates, you can use the na.approx()
# function. It makes linear estimates between existing values before and after
# the missing values, *based on the indexes*. If there are NAs at the beginning
# or the end of the time series, the function will automatically remove these
# rows. It requires an xts object as input.

na.approx(x)


##############################
##    Lags & Differences    ##
##############################

# You can create lagging or leading values with the lag() function. It requires
# two arguments. The first is an xts object, the second is an integer indicating
# by how many rows the values should be shifted. Positive ones shift the values
# down to create lags, i.e. each date will contain past values. Negative ones
# shift them up to create leads, i.e. each date will contain future values.  The
# indexes stay don't move.

lag(x, k = 1)
lag(x, k = -2)


# The diff() function can be used to compute differences between values in
# different rows. It takes three arguments. The first is an xts object, the
# second is the number of lags to be used to compute the difference (e.g. lag =
# 12 would subtract from every row the value from twelve rows in the past), and
# the third is the order of differences (differences = 2 is the second-order
# difference, or the difference of the difference).

diff(x, lag = 1, differences = 1)


###########################
##    Apply Functions    ##
###########################

# Apply functions require some endpoints as one of the arguments. The
# endpoints() function takes three arguments and returns  endpoints, i.e. the
# indexes of the last rows in a specified interval. The first argument is an xts
# objects, the second is the interval (like "minutes"/"hours"/"days" etc), and
# the third is an integer for how many intervals should be skipped at a time.
# The first endpoint is always zero, the last is always the last row (no matter
# of the skipped periods to this point). For example, you can get the endpoints
# of every second month from an object like this:

endpoints(x, on = "months", k = 2)


# Next, you can use the period.apply() function to compute a measure of each of
# these intervals. It takes three arguments. The first is an xts object, the
# second is a vector with endpoints, and the third is the function to be
# applied. There are multiple shortcut versions of this as well, which will do
# the endpoint step for you.

period.apply(x, INDEX = endpoints, FUN = fun)

apply.daily(x, FUN = fun)
apply.weekly(x, FUN = fun)
apply.monthly(x, FUN = fun)
apply.quarterly(x, FUN = fun)
apply.yearly(x, FUN = fun)


# You can also use the base R apply functions. Rather than setting endpoints,
# this first requires splitting your xts object into a list of intervals first.
# This is done using the split.xts() function, which takes two arguments. This
# first is your object, the second is the intervals by which to split.
# Afterwards, you can use lapply to get the same result as before.

y <- split.xts(x, f = "months", k = 3)

lapply(y, fun)


# Finally, rollapply() can be used to compute rolling measures over a time
# window. You must provide three arguments. The first is your object, the second
# is an integer for the time frame (i.e. 3 means three days for daily data but 3
# months for monthly), and the third is the function to apply.

rollapply(x, width = 3, FUN = fun)


#############################
##    OHLC Aggregations    ##
#############################

# You can compute Open-High-Low-Close aggregations by useing the to.period()
# function. You must provide two arguments. The first is an xts object, the
# second is the period interval to aggregate. Like before, there are shortcut
# functions as well.

to.period(x, period = "months")

to.daily(x)
to.hourly(x)
...



##################
##    Extras    ##
##################

# Get or change timezone of an xts object.

tzone(x)
tzone(x) <- "Europe/Zurich"


# Get or change time format of index of an xts object.

indexFormat(temps)
indexFormat(temps) <- "%b-%d-%Y"


# Count different time periods of an xts object.

ndays(x)
nweeks(x)
nmonths(x)
nyears(x)


# Create an index of e.g. weekend days of an xts object. Sunday is = 0 and
# Saturday is = 6.

index <- which(.indexwday(x) == 0 | .indexwday(x) == 6)

