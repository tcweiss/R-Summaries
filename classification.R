
###############################################
###############################################
###                                         ###
###   SUPERVISED LEARNING: CLASSIFICATION   ###
###                                         ###
###############################################
###############################################


########################################
##        k-Nearest Neighbors         ##
########################################

library(class)

# Before using the kNN algorithm, it is common to normalize your data. Your
# dataset will usually have a categorical variable (to be predicted) and several
# numerical variables (for patterns to infer about the associated category). If
# one numerical variable is on a scale of 1-100 and another one is a dummy
# variable, the former may not contribute as much to the prediction (since its
# harder to define a cut-off value). Thus, we should perform a min-max
# normalization of all numerical variables. This means its minimum value will be
# zero and its maximum value will be one. this can be done with the following
# function.
normalize <- function(x) {
  return((x - min(x)) / (max(x)-min(x)))
}

# Estimate kNN model. The algorithm uses different levels of a categorical
# variables, e.g. street signs colors, and considers its levels as axes in a
# coordinate system, e.g. "blue", "red" and "green". We first train the model
# using test data, in which each observation (street sign) has a certain color
# and some numerical variables. When testing, we pass it another observation
# with unknown color but know values of the numerical predictors. The model checks
# the k shortest vector distances to other observations in the training data, so
# if k=5, it considers the five nearest neighbors. If a majority of them is
# "blue", the new observation is predicted to be blue (and chosen random with
# ties). The knn function has four arguments: "train" is the training data
# (without the categorical variable), "test" is the test data (also without
# categorical variable), "cl"  is the categorical variable from the training
# data (which serves as levels/labels when training),  and "k" is the number of
# neighbors to use. A smaller k covers more subtle patters, but is also more
# affected by noisy data. Always try different values, but a rule of thumb is k
# = sqrt(nrow(test_data)).
pred <- knn(train = train[-c("category")], test = test[-c("category")], cl = train["category"], k = 1, prob = FALSE)

# Check share of correct predictions.
mean(pred == test["category"])

# Check proportion of votes on the winning category of each observation (same
# code as above, but with prob = TRUE).
knn(train = train[-c("category")], test = test[-c("category")], cl = train["category"], k = 1, prob = TRUE)



########################################
##          Bayesian Methods          ##
########################################

library(naivebayes)

# Before using the (naive) Bayes algorithm, it is common to "bin" your data.
# Your data will have one or more categorical variable and perhaps also some
# numerical variables. Binning means that we transform the numerical predictors
# into categorical variables. For example, age values might be recoded as
# "child" and "adult", or we could use percentiles. Ideally, it should be a
# cut-off which makes it easier to differentiate between observations.

# Estimate naive Bayes model. Your data must be a data frame with at least two
# categorical variables (one to predict, one or more to condition on). The model
# is trained using the standard Bayes formula, but making some simplifying
# assumptions to speed up the process (with little impact on results). The first
# argument is a formula of the same format as in lm() (variable to be explained
# first), the second argument is the data. Note that if a certain combination of
# events never occurs in the data, the model will assign it a probability of
# zero by default. In reality, we might rather assign it a very low probability.
# To fix this, we should make a Laplace correction by adding a certain number of
# observations to each possible combination of events (typically 1). This is the
# purpose of the last argument.
nb <- naive_bayes(y ~ x1 + x2,
                  train, 
                  laplace = 1)

# Make a prediction. The input data should have the same format and colnames as
# the training data. Setting type = "prob" gives the predicted probabilities of
# each category.
test$pred <- predict(nb, test, type = NULL)

# Check share of correct predictions.
mean(test$pred == test$y)


########################################
##          Logistic Regression       ##
########################################

library(pROC)

# Before running a logistic regression, you should do two things. First, check
# if there are any numerical predictors which may also be recoded into factors.
# Second, impute any missing values (e.g. with the mean) and add an "imputed"
# variable.

# Run logistic regression. The first argument is a formula, the second one is
# your data, and the third one should be set equal "binomial".
logistic <- glm(y ~ x1 + x2, 
                train, 
                family = "binomial")

# Find optimal coefficients with stepwise forward selection. First, estimate a
# null and a full model. Then use the step() function, starting start with the
# former and add terms to get close to the latter. If AIC does not decrease
# anymore, the model will be estimated with the current selection.
null_model <- glm(y ~ 1, train, family = "binomial")
full_model <- glm(y ~ ., train, family = "binomial")
logistic <- step(null_model, 
                 scope = list(lower = null_model, upper = full_model), 
                 direction = "forward")

# Make a prediction. Note that the prediction comes as *probabilities*, not 0/1
# outcomes. Setting type = "response" turns logit-probabilities into normal
# ones. To turn them into actual 0/1 values, you need to recode them manually. A
# good cut-off value is the mean of the (observed) outcome variable in the
# training data.
test$success_prob <- predict(logistic, test, type = "response")
test$success_pred <- ifelse(test$sucess_prob > mean(train$y), 1, 0)

# Check accuracy using the roc() function. It computes a value between 0.5
# (amounts to random guessing) and 1 (perfect prediction). The first argument
# is the actual outcome (in 0/1), the second argument is the predictor (here the
# predicted probability).
ROC <- roc(test$y, test$success_prob)
auc(ROC)



########################################
##           Trees & Forests          ##
########################################

library(rpart)
library(rpart.plot)
library(randomForest)

# Grow a tree. The arguments work in much the same way as before. The control
# argument can be used to set "cp" (minimum decrease factor in lack of fit to
# make another split), "minsplit" (minimum splits per tree), and minbucket
# (minimum number of obs per leaf).
tree <- rpart(y ~ x1 + x2, train, method = "class",  control = rpart.control(cp = 0, minsplit = 0, minbucket = 0))

# Use post-pruning. First grow a complex tree by including all coefficients and
# setting cp = 0. Then look at the plot of "cp". Use the cp value where the
# error is minimal as input to the "prune" function.
tree <- rpart(y ~ .,  train,  method = "class", control = rpart.control(cp = 0))
plotcp(tree)
tree_pruned <- prune(tree, cp = 0.05)

# Make a prediction and check accuracy.
test$pred <- predict(tree_pruned, test, type = "class")
mean(test$pred == test$y)

# Plot the tree.
rpart.plot(tree)

# Grow random forest. This is based on a different package than the above. The
# "ntree" argument set the number of trees to grow, "mtry" set the number of
# predictors (p) per tree.
forest <- randomForest(y ~ x1 + x2, 
                       train,
                       ntree = 500,
                       mtry = sqrt(p))

# Make a prediction and check accuracy.
test$pred <- predict(forest, test)
mean(test$y == test$pred)


