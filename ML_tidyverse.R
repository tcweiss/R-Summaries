
###############################################
###############################################
###                                         ###
###           ML IN THE TIDYVERSE           ###
###                                         ###
###############################################
###############################################

library(gapminder)
library(broom)
library(rsample)


########################################
##              Nesting               ##
########################################

# Nest data. This creates a list column. It is advisable to
# first group the data before nesting it. The output will be a tibble, with one
# column for the group and a second one called "data" with tibbles containing
# all other columns.
gap_nested <- gapminder %>% 
                  group_by(country) %>% 
                  nest()

# Unnest data. To unnest columns, simply pipe the nested tibble into unnest().
# The only argument of nest if the index or name of the column to be unnested.
gap_nested %>% 
  unnest(2)


########################################
##              Mapping               ##
########################################

# Work with list columns. The map() function takes a vector or list and applies
# a function or formula to every element. The output will again be a list. The
# first argument is the nested column containing the tibble to use. The second
# argument defines what should be applied, and it can either come as a formula
# or a plain function. If the nested column you specified only contains tibbles
# with one column, you can simply pass the function (as in "apply"). Otherwise,
# use a formula like "~function(required_arguments)"; the data passed to the
# first argument can be accessed as an object called ".x", which you can use to
# specify the column in the nested tibble.
map(tibble_nested$data, mean)
map(tibble_nested$data, ~mean(.x$column))

# Type-specific map() function. If you know that the variable to be used has a
# certain type, you can use map_chr, map_lgl, etc. The output will then be a
# vector. Run the code below to see an example.
map_dbl(tibble_nested$data, ~mean(.x$column))

# Map with mutate. You can also use map to specify a function.
tibble_nested %>%
  mutate(new_column = map(data, ~mean(column)))


########################################
##      Extraction & Evaluation       ##
########################################

# Extract estimation results from model (e.g. coefficients).
tidy(model)

# Get summary of performance statistics.
glance(model)

# Adds prediction columns to the data being modeled.
augment(model)



########################################
##          Workflow Example          ##
########################################

# Below is an example of an efficient workflow that uses the above functions.
# First, we nest the gapminder data by country.
gap_nested <- gapminder %>%
                group_by(country) %>% 
                nest()

# Next, we estimate linear models for each country, regressing life expectancy
# on year. We use map() to store the models in a separate nested column.
gap_model <- gap_nested %>% 
                mutate(model = map(data, ~lm(lifeExp ~ year, .x)))

# We can now extract the coefficients. Use map() with tidy/glance/augment to
# create new columns.
gap_model <- gap_model %>% 
                mutate(coef = map(model, ~tidy(.x)),
                       perf = map(model, ~glance(.x)),
                       pred = map(model, ~augment(.x)))
                       
# Finally, we can unnest the new columns to get more insights. Below is an
# example.
worst <- gap_model %>% 
            unnest(perf) %>%
            arrange(r.squared) %>% 
            head(10) %>% 
            pull(country)

gap_model %>% 
  filter(country %in% worst) %>% 
  unnest(pred) %>% 
  ggplot(., aes(x = year)) +
    geom_point(aes(y = lifeExp)) +
    geom_line(aes(y = .fitted), color = "red") +
    facet_wrap(~country)
  

########################################
##              ML Workflow           ##
########################################

# This sections outlines a proper ML workflow. First, randomly create train and
# test set.
data_split <- initial_split(gapminder, prop = 0.75)
train <- training(data_split)
test <- testing(data_split)

# Manually prepare training data for cross-validation (if not possible
# automatically). This splits the training set from above into a training and
# validation set.
cv_data <- vfold_cv(train, v = FOLDS) %>% 
              mutate(train = map(splits, ~training(.x)),
                     validate = map(splits, ~testing(.x)))

# Train models using training data.
cv_models <- cv_data %>% 
               mutate(model = map(train, 
                                  ~MODEL_FUN(MODEL_INPUTS)))

# Create columns with actual and predicted values using validation data. The
# map2 function works like map(), but uses two data sources.
cv_prep <- cv_models %>% 
            mutate(validate_actual = map(validate, ~.x$y),
                   validate_predicted = map2(.x = model, 
                                             .y = validate, 
                                             ~predict(.x, .y)))

# Compute error measures using actual and predicted values.
cv_eval <- cv_prep %>% 
            mutate(validate_mae = map2_dbl(validate_actual, 
                                           validate_predicted, 
                                           ~ERROR_FUN(actual = .x, predicted = .y)))


# Tune model parameters. If everything seems to work, we can find the optimal
# parameter values for our model. This is done by simply trying different values
# and checking which model gives the best results. First, use crossing() to create
# a new column (same name as parameter to be tuned), and set it equal to a
# vector of parameter values to try. The dataset may get longer to guarantee
# that there are observations with each possible fold/parameter combination.
cv_tune <- cv_data %>% 
            crossing(MODEL_PARAM = 1:20) 

# Next, we build models for each fold & parameter combination.
cv_model_tune <- cv_tune %>% 
                    mutate(model = map2(.x = train, 
                                        .y = MODEL_PARAM, 
                                        ~MODEL_FUN(MODEL_INPUTS, data = .x, MODEL_PARAM = .y)))


# Generate predictions from each model using validation data.
cv_prep_tune <- cv_model_tune %>%
                    mutate(validate_predicted = map2(.x = model, 
                                                     .y = validate, 
                                                     ~predict(.x, .y)))

# Calculate error of each fold & parameter combination using validation data.
cv_eval_tune <- cv_prep_tune %>% 
                    mutate(validate_error = map2_dbl(.x = validate_actual, 
                                                     .y = validate_predicted,
                                                     ~ERROR_FUN(actual = .x, predicted = .y)))

# Calculate the mean error for each parameter used.
cv_eval_tune %>% 
  group_by(MODEL_PARAM) %>% 
  summarise(mean_error = mean(validate_error))


# Once you found the optimal parameter, use it to estimate the best model using
# the training data. Next, generate predictions using this model and the test
# data. Finally, compute the error from these predictions and the actual values
# in the test data.
best_model <- MODEL_FUN(MODEL_INPUTS, MODEL_PARAM)
test_actual <- test$y
test_predicted <- predict(best_model, test)
ERROR_FUN(test_actual, test_predicted)





