
###############################################
###############################################
###                                         ###
###     MACHINE LEARNING WITH CARET         ###
###                                         ###
###############################################
###############################################

library(parallel)
library(doParallel)
library(caret)
library(ranger)



########################################
##                Basics              ##
########################################

# Create (and stop) parallel backend. 
n_cores <- detectCores() - 2
cluster <- makeCluster(n_cores, type = "FORK")
registerDoParallel(cluster)
stopCluster(cluster)

# Train model. The first two arguments work as in lm(), "method" specifies the
# model type. Instead of passing different hyperparameter value for your
# specific model, you can let the function find optimal values. The "tuneLength"
# indicates that k values should be tried for each parameter, so the algorithm
# will find the optimal value based on the these k values (default is to use
# this with 3 if no hyperparameters are specified). Higher k give better models,
# but take longer to train. You can also use "tuneGrid" to pass specific values
# to try for each parameter. The "preProcess" argument is optional, and it will
# execute any cleaning steps in the provided order. The following approach makes
# sense. First, use "zv" or "nzv" to remove constant or near-constant variables
# (this can mess up some models). Second, do "medianImpute" or "knnImpute" to
# impute randomly or non-randomly missing NAs. For linear models, you "center",
# "scale", and use "pca". For tree-based models, no further steps are necessary.
# After each additional input, you should run the model and check the performance; 
# these steps do not always improve the model. Finally, "trControl" specifies
# parameters related to the training process. If you set a parallel backend, the
# train() function will automatically use parallel computing.
train(y ~ x, train,
      method = "model_type", 
      tuneLength = k,
      tuneGrid = expand.grid(PARAM_NAME_1 = c(1:100),
                             PARAM_NAME_2 = c(2:30)),
      preProcess = c("zv"/"nzv", "medianImpute"/"knnImpute", "center", "scale", "pca"),
      trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))

# Print parameters and performance measures.
print(model$results)

# Plot estimated model.
plot(model$finalModel)
dnorm



########################################
##         Logistic Regression        ##
########################################

# Run logistic regression. By using the trainControl argument below, it will
# find the model with the highest AUC (= find the cutoff prob which maximizes
# accuracy on validation data, see also regression.R). The number of CV folds
# may be adjusted.
train(y ~ x, train, 
      method = "glm",
      trControl = trainControl(method = "cv", 
                               number = 10, 
                               summaryFunction = twoClassSummary, 
                               classProbs = TRUE, 
                               verboseIter = TRUE))


# Check accuracy with confusion matrix. Make sure your outcome variable is a
# factor before doing this. The "Accuracy" tells you how often the model was
# right, "No Information Rate" shows the performance of always predicting the
# dominant outcome.
test$success_prob <- predict(logistic, test, type = "response")
YES_NO <- ifelse(success_prob > 0.5, "YES", "NO")
y_pred <- factor(YES_NO, levels = levels(test[["y"]]))
confusionMatrix(y_pred, test$y)



########################################
##            Random Forest           ##
########################################

# Grow random forest.
train(y ~ x, train,
      method = "ranger", 
      tuneLength = 10,
      trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))



########################################
##              Elastic Net           ##
########################################

# Train elastic net model. This is a mixture of Lasso (shrinking) and Ridge
# (excluding whole variables). The "alpha" parameter defines the mixture, with 0
# being pure ridge and 1 being pure lasso. The "lambda" parameter defines the
# size of the penalty, with 0 giving all variables and infinity giving intercept
# only models. The tuning Grid below is quite efficient, since for a specified
# value of alpha, models for all lambda values are fit simultaneously (can use
# many lambda values).
train(y ~ ., train,
      method = "glmnet", 
      tuneGrid = expand.grid(alpha = c(0, 0.25, 0.5, 0.75, 1),
                             lambda = seq(0.0001, 0.1, length = 10)),
      trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))



########################################
##          Model Comparison          ##
########################################

# To easily compare models, use the following approach. First, create a list of
# folds.
myFolds <- createFolds(train$y, k = 5)

# Second, create reusable trainControl object, with "index" set equal the folds
# your created. This guarantees that all models use the exact same folds.
myControl <- trainControl(summaryFunction = twoClassSummary,
                          classProbs = TRUE, # IMPORTANT!
                          verboseIter = TRUE,
                          savePredictions = TRUE,
                          index = myFolds)

# Third, train all your models with "trControl" set equal to the trainControl
# object you created.
model <- train(y ~ x, train,
               method = "model_type", 
               trControl = myControl)

# Fourth, run the code below to get performance stats for all your models.
mod_all <- list(item1 = model_1, item2 = model_2, item3 = model_3) %>% 
              resamples()

summary(mod_all)
bwplot(mod_all)




