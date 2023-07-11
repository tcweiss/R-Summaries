
###############################################
###############################################
###                                         ###
###     SUPERVISED LEARNING: REGRESSION     ###
###                                         ###
###############################################
###############################################


########################################
##                Basics              ##
########################################

library(broom)
library(vtreat)

# Get summary of performance statistics.
glance(model)

# Make a prediction. Not passing any new data to the function returns the
# in-sample predictions.
predict(model, new_data)

# Interaction terms can be added with * or :. The former automatically adds main
# effects for both variables, the latter only adds the interaction. The
# following two formulas give the same results.
lm(y ~ x1*x2, data)
lm(y ~ x1 + x2 + x1:x2)

# To include powers of coefficients, wrap it into the I() function (treats
# it literally instead of adding interaction terms).
lm(y ~ I(x1^2) + I(x1^3), data)

# k-fold cross validation with linear regression. Simply set the number of folds
# (k) and run the code below.

  # Create split plan.
  splitPlan <- kFoldCrossValidation(nrow(data), k)

  # Initialize a column of the appropriate length
  dframe$pred.cv <- 0 
  
  for(i in 1:k) {
    
    # Get the ith split.
    split <- splitPlan[[i]]
    
    # Build a model using training data.
    model <- lm(y ~ x1 + x2, data = dframe[split$train,])
    
    # Make predictions using application (test) data.
    dframe$pred.cv[split$app] <- predict(model, newdata = dframe[split$app,])
    
  }

  
########################################
##          Logistic Regression       ##
########################################
  
# Run logistic regression. This works just as in classification, but we may have
# to transform the outcome variable so that it becomes binary (0/1 or FALSE/TRUE). 
logistic <- glm(y ~ x1 + x2, train, family = "binomial")
  
# Make predictions. First, run predict() using type = "response".  The output
# consists of probabilities for an observation being in the 1/TRUE class, *not*
# the values of your outcome variable. Recode them using ifelse(). This requires
# a cutoff probability, which you can choose by trying different values and
# evaluating their accuracy using AUC (see below).
test$success_prob <- predict(logistic, test, type = "response")
test$success_pred <- ifelse(test$sucess_prob > 0.5, "Y_SUCCESS", "Y_FAILURE")
  
# To evaluate the accuracy, we cannot use RMSE. An alternative is to compute the
# Gini coefficient (as in classification). Second, we can compute the pseudo
# R-squared. In both cases, a value closer to 1 indicates a better model.

  # Compute Gini coefficient (AUC).
  ROC <- roc(test$y, test$success_prob)
  auc(ROC)
  
  # Compute pseudo R-squared (better for final model).
  glance(logistic) %>% 
    summarize(pseudoR2 = 1 - deviance/null.deviance)
  


########################################
##          Poisson Regression       ##
########################################

# Check model type. Poisson regressions can be used if the outcome variable
# follows a Poisson, meaning that the outcome is a count or rate. The model then
# estimates the expected count or rate (λ). First, verify that the λ to be
# predicted will not be zero (otherwise, standard regression is sufficient).
# Second, verify whether mean(y) = var(y) using the code below. If this holds
# more or less, we can use Poisson regression (otherwise, we run a
# Quasi-Poisson).
train %>% 
    summarize(mean = mean(y),
              var = var(y))
  
# Run regression. The family must be chosen according to the output from before.
poiss <- glm(y ~ x1 + x2, train, family = c("poisson", "quasipoisson"))

# Check accuracy. We can either use RMSE or pseudo R-squared.

  # Compute RMSE.
  test$pred <- predict(poiss, test, type = "response")
  test %>% 
    mutate(residual = pred - y) %>% 
    summarize(rmse = sqrt(mean(residual^2)))
  
  # Compute pseudo R-squared.
  glance(poiss) %>% 
    summarize(pseudoR2 = 1 - deviance/null.deviance)


            
########################################
##       General additive models      ##
########################################

library(mgcv)

# Estimate GAM model. The outcome variable now depends on unknown smooth
# functions of coefficients. First, make sure your dataset is large enough
# (otherwise, you get overfitting). The family argument can be set to "gaussian"
# (standard regression), "probabilities" (logistic), or "poisson"/"quasipoisson".
# To indicate that a coefficient should be non-linear, you can wrap it in the
# s() function - do *not* use this with categorical variables.
gam_mod <- gam(y ~ s(x1) + x2, train, family = c("gaussian", "probabilities", "poisson", "quasipoisson"))

# Plot variable transformation the algorithm learned.
plot(gam_mod)

# Make predictions. For GAM models, the predict() function returns matrices. You
# can turn it into a vector by wrapping it  as.numeric.
predict(gam_mod, test, type = "response") %>% 
  as.numeric()



########################################
##          Trees & Forests           ##
########################################

library(ranger)

# Grow random forest. Depending on the outcome variables, the function will
# automatically use classification/regression. The "mtry" argument defines the
# minimum length. Setting respect.unordered.factors = "order" converts ordered
# factors to numbers, which speeds up the process. Printing the model
# automatically gives estimates of OOS performance and R2, but you should still
# evaluate it on test data.
forest <- ranger(y ~ x1 + x2, train, num.trees = 500, respect.unordered.factors = "order", seed = 1234)

# Make predictions and compute RMSE.
test$pred <- predict(forest, test)$predictions

pivot_longer(test, cols = c(y, pred), names_to = "modeltype", values_to = "value") %>% 
  mutate(residuals = y - pred) %>% 
  group_by(modeltype) %>% 
  summarize(rmse = sqrt(mean(residuals^2)))
  


########################################
##               XGBoost              ##
########################################

library(xgboost)
library(magrittr)

# Unlike most other model functions, xgboost does not convert categorical
# variables to numerical ones. Thus, we must first prepare our data.

# First, design a "treatment plan" from the training data. The first argument is
# the training data, the second one are the input variables (do *not* include the
# outcome variable). The third argument can be used to display progress
# messages.
treatplan <- designTreatmentsZ(train, varlist = c("x1", "x2"), verbose = FALSE)

# The output from above contains an element named "scoreFrame". This is a
# mapping from old to new variable names, and the type of each new variable.
scoreframe <- treatplan$scoreFrame %>% 
                select(varName, origName, code)

# Next, get the names of the new "lev" (contains indicators) and "clean"
# variables (clean numerical variables, i.e. without NA).
newvars <- scoreframe %>%
            filter(code %in% c("clean", "lev")) %>% 
            use_series(varName)

# Finally, we can use this to prepare the training data for modeling. The last
# argument is optional; it defines which variables to treat (default is all).
# The output is a new dataset with categorical variables as dummies, and
# numerical variables having no missing observations. Note that the outcome
# variable is *not* included in the treated data.
train.treated <- prepare(treatmentplan = treatplan, dframe = train, varRestriction = NULL)


# Xgboost first fits a tree and records the residuals. It then fits a second
# tree to the residuals from the first one, which gives a combined model
# composed of predictions from two trees. This is repeated many times, and each
# time a new tree is fit to the residuals from the previous one (using
# cross-validated errors to prevent overfitting). First run xgb.cv() with a
# large number of trees, which records the cross-validated errors of each number
# of trees. Then use the optimal number of trees as inputs in xgboost() to fit
# the corresponding model.

  # Find optimal number of trees. The "data" argument is the prepared input data
  # (a matrix), "label" is the outcome variable (a vector), "objective" is the
  # minimizing function (just set as below), nrounds is the maximum number of
  # trees to fit,  eta is the learning rate (goes from 0 to 1, higher is faster
  # but more prone to overfitting), "max_depth" is the maximum depth of
  # individual trees (0 is root only), and nfold is the number of folds for
  # cross validation. Extract the "evaluation_log" element from the output to
  # find the number of rounds with the lowest RMSE.
  cv <- xgb.cv(data = as.matrix(train.treated),
               label = train$y,
               objective = "reg:squarederror",
               nrounds = 100,
               eta = 0.3,
               max_depth = 5,
               nfold = 5)
    
   ntrees <- cv$evaluation_log %>% 
                  summarize(ntrees.train = which.min(train_rmse_mean),
                            ntrees.test  = which.min(test_rmse_mean)) 
  
  
    # Fit model. Replace the "nrounds" argument with the optimal value. The
    # other arguments stay the same (except for "nfolds", which disappears since
    # we don't have to use CV anymore)
    xgb <- xgboost(data = as.matrix(train.treated),
              label = train$y,
              objective = "reg:squarederror",
              nrounds = ntrees,
              nfold = 5,
              eta = 0.3,
              max_depth = 6)


# Make a prediction and compute RMSE.
test$pred <- predict(xgb, as.matrix(test.treated))

test %>% 
  mutate(residuals = y - pred) %>% 
  summarize(rmse = sqrt(mean(residuals^2)))



  
  