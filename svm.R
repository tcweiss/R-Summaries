
###############################################
###############################################
###                                         ###
###         SUPPORT VECTOR MACHINES         ###
###                                         ###
###############################################
###############################################

library(e1071)
library(ggplot2)


########################################
##           Linear Kernels           ##
########################################

# Train linear SVM model. This algorithm tries to assign observations to one of
# two classes. With a linear kernel, this works as follows. Assume we have
# training data with two input variables and one binary outcome variable, which
# can be represented in a coordinate system (x1 vs x2 colored by y). The
# algorithm tries to draw a straight line through the point cloud, dividing the
# observations into one of two outcome classes. For a fixed value of y=0, such a
# line is given by all combinations of x1 and x2 which satisfy β_0 + β_1*x1 +
# β_2*x2 = 0. All points for which this equation gives a value >0 (above the
# line) are assigned to one group, and all <0 (below the line) are assigned to
# another. The best such line has largest possible distance to the closest point
# of each respective cloud. If the clouds are perfectly separable, the algorithm
# just finds points in each cloud which are closest to the other cloud, and
# draws two parallel lines through them (the margins). The optimal line is
# exactly in the middle (the decision boundary). If the clouds are not perfectly
# separable by a line, the model allows for some violations by making the
# margins wider (the strictness is given by a Lagrange-penalty and can be
# tuned). We can also have more than two input variables (just add them to the
# equation), and more than two outcomes (each possible binary class combination
# is estimated separately, and each point is assigned to the majority prediction).
# The function below does all of this. The outcome variable should be a factor.
# The "type" argument can be left as below for classification, "kernel" defines
# the form of the decision boundary, "scale" indicates whether to scale the
# data. The "cost" argument defines the penalty for a misclassification; the
# higher the value, the stricter the model (and the wider the margin boundaries
# around the decision boundary will be). For datasets which are not *exactly*
# linear, a lower value is recommended (but try different ones).
svm_model <- svm(y ~ x1 + x2, train, 
                 type = "C-classification",
                 kernel = "linear",
                 scale = TRUE,
                 cost = .)

# Check accuracy.
pred <- predict(svm_model, test)
mean(pred == test$y)

# Plot results.
plot(svm_model, train)



########################################
##          Polynomial Kernels        ##
########################################

# Train polynomial SVM model. This is used if your data is not (approximately)
# linearly separable. The idea is to then apply a transformation which makes the
# data linearly separable, to solve it using the linear SVM, and to then revert
# the transformation. For example, think of a horizontal line where the outer
# points are in class A and those in the middle are in class B. We could square
# this to get from a horizontal line to a parabola (positively curved). Class B
# points will be at the bottom and class A points at the top of the two "arms",
# so we can cut right through with a line. We can specify any number of
# polynomials (for circles, a degree of 2 works). The function inputs are
# similar to before, but the kernel argument is "polynomial". The "degree"
# argument sets the polynomial degree. A good idea is to first create
# scatterplot of the data (colored by outcome) and try to spot the ideal
# decision boundary. If it is symmetric about the x1/x2 axis, this requires an
# even-degree polynomial. The "gamma" and "coef0" argument are technical
# parameters; you can find them with the tune.svm() function below.
svm_model <- svm(y ~ x1 + x2, train, 
                 type = "C-classification",
                 kernel = "polynomial",
                 degree = 2,
                 scale = TRUE,
                 cost = .,
                 gamma = .,
                 coef0 = .)


# Find optimal parameter values. The first argument are the input variables, the
# second one is the outcome variable. You need to specify the "type", "kernel"
# and "degree" arguments as applicable. The last three argument are tuning
# parameters, and the function will try all combinations of the provided
# values; simply use the vectors in the code below. Access the output as shown
# below, and then use it to train your model as usual.
tune_out <- tune.svm(x = train[, -c("y")], y = train[, c("y")], 
                     type = "C-classification", 
                     kernel = "polynomial", 
                     degree = 2, 
                     cost = 10^(-1:3), 
                     gamma = c(0.1, 1, 10), 
                     coef0 = c(0.1, 1, 10))

tune_out$best.parameters$cost
tune_out$best.parameters$gamma
tune_out$best.parameters$coef0

# Check accuracy.
pred <- predict(svm_model, test)
mean(pred == test$y)

# Plot results.
plot(svm_model, train)



########################################
##             RBF Kernels            ##
########################################

# This kernel is based on a similar idea as KNN, i.e. it uses observations with
# similar features to decide on class assignments. The shape can be almost
# anything. To use it, set the kernel argument to "radial".
svm_model <- svm(y ~ x1 + x2, train, 
                 type = "C-classification",
                 kernel = "radial",
                 scale = TRUE,
                 cost = 1)

# Find optimal parameter values. Simply use the vectors defined below.
tune_out <- tune.svm(x = trainset[, -c("y")], y = trainset[, c("y")], 
                     gamma = 5*10^(-2:2), 
                     cost = c(0.01, 0.1, 1, 10, 100), 
                     type = "C-classification", kernel = "radial")

# Check accuracy.
pred <- predict(svm_model, test)
mean(pred == test$y)

# Plot results.
plot(svm_model, train)





