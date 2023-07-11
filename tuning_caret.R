
###############################################
###############################################
###                                         ###
###         HYPERPARAMETER TUNING           ###
###                                         ###
###############################################
###############################################

library(caret)
library(tictoc)

########################################
##                Workflow            ##
########################################

# Parameters are found during training (e.g. regression coefficients). They are
# the results of the training process. Hyperparameters are defined before
# training, e.g. the minimum leaf size in tree models. They define how the
# training process is run. To see what hyperparameters a model type has, go on
# http://topepo.github.io/caret/train-models-by-tag.html#generalized-linear-model.

# Train model. The tic() and toc() functions track the time
# required to train the model. Try a simple one first, and if it runs fast,
# increase the complexity. If you know how the parameters work and the model is
# simple, define a "tuneGrid" and in "trControl" set "search = grid". If you
# don't know how it works and/or the model is complex (takes long to train), use
# tuneLength and in "trControl" set "search = random". Third, you could also use
# adaptive resampling; this starts like the "random" search, but subsequent
# samples are drawn from a range close to values from previous samples that
# worked well. This is the fastest method, but not necessarily the most
# accurate. Use the pre-written trainControl object below and in "trControl" set
# "search = random".
tic()
set.seed(42)
model <- train(y ~ x, train,
               method = "model_type", 
               tuneLength = 100,
               tuneGrid = expand.grid(PARAM_NAME_1 = c(1:100),
                                      PARAM_NAME_2 = c(2:30)),
               preProcess = c("zv"/"nzv", 
                              "medianImpute"/"knnImpute", 
                              "center", "scale", 
                              "pca"),
               trControl = trainControl(method = "repeatedcv", 
                                        number = 5, 
                                        repeats = 3,
                                        search = c("grid", "random"),
                                        verboseIter = TRUE))
toc()

# [Training controls for adaptive resampling]
trControl_adaptive <- trainControl(method = "adaptive_cv",
                                   adaptive = list(min = 2, # min number of resamples per hyperpara (larger = slower & better) 
                                                   alpha = 0.05, # confidence level for removing hyperparas (not too important)
                                                   method = c("gls", "BT"), # resampling method (BT only for large number of hyperparas)
                                                   complete = TRUE), # Saves time
                                   search = "random")


# Plot hyperparameter tuning results. First, you can use the plot() function.
# The "highlight" argument indicates whether to highlight the optimal parameter
# values. Second, use can call ggplot() on the model. All usual ggplot layers
# can be added as well.
plot(model, 
     metric = c("RMSE", "Rsquared", "Accuracy", "Kappa"),
     plotType = c("scatter", "level", "line"),
     highlight = TRUE)

ggplot(model)


