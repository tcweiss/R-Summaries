
###############################################
###############################################
###                                         ###
###         UNSUPERVISED LEARNING           ###
###                                         ###
###############################################
###############################################

library(dummies)

########################################
##        k-Means Clustering          ##
########################################

# Use k-Means clustering. This algorithm tries to sort observations into
# homogeneous groups, or clusters. Assume we have two variables, which can be
# represented in a coordinate system. Also assume we known that each observation
# comes from one of four clusters. The algorithm starts by assigning each
# observation randomly to one of the two clusters. Next, it identifies the
# center-most observation in each cluster-cloud. It then re-assigns all other
# observations to the cluster whose center-observation is closest. This is the
# first round. In the second round,, the algorithm again identifies the center
# of each cloud and reassigns all observations according to the closest
# center-observation. This process is repeated until no observation changes its
# assignment anymore (or for a predetermined number of rounds). The kmeans()
# function implements this. The first argument is the data (matrix or df),
# "centers" is the number of clusters, "nstart" is the number of intial random
# assignments to try (best one will be chosen, just use 20), and "iter.max"
# defines the maximum number of rounds to run (just use 50, usually stops before
# that). Note that the results differ slightly depending on the intial
# assignment, so setting seeds might make sense.
k_means <- kmeans(data, centers = k, nstart = 20, iter.max = 50)

# Get clusters.
k_means$cluster

# Get within-cluster sum of squared distances (measure of performance).
k_means$tot.withinss

# Create scatterplot of data colored by clusters. If your input data had more
# than two variables, you need to choose two.
ggplot(data, aes(x, y, col = as.factor(k_means$cluster))) +
  geom_point() +
  labs(x = "",
       y = "",
       col = "Cluster")

# Find optimal number of clusters. If you don't now the number of clusters
# beforehand, use the code below. Simply adjust the input data and run the code.
# The plot shows clusters vs within-cluster sum of squared distances. The
# optimal number of clusters is where the curve has a 45° downward slope. Use
# this value input to kmeans().
wss <- 0

for (i in 1:20) {
  km.out <- kmeans(data, centers = i, nstart = 20, iter.max = 50)
  wss[i] <- km.out$tot.withinss
}

plot(1:20, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Make sure to compute/plot descriptive statistics of your variables by your
# chosen cluster assignment. That way, you can get insights into why these
# clusters were selected.


########################################
##      Hierarchical Clustering       ##
########################################

# Prepare data 1. Before fitting the model, check whether each variable in your
# data has mean=0 and sd=1. If not, use the scale() function to standardize your
# data. With categorical variables (and no numerical ones), first make sure that
# none of them are factors. Second, run dummy.data.frame on the variables, which
# returns a 0/1 dataframe with all possible combinations. To compute the
# distance, use the dist() function, but set method="binary" (this gives
# the Jaccard-distance). Observations with the most differences between variables
# have a distance of 1, those with the most variables being equal have a
# distance of 0. Of the number of differing variable-pairs is in between, the
# distance will be somewhere between 0 and 1.
apply(data, 2, mean)
apply(data, 2, sd)
data.std <- scale(data)

dummy.data.frame(data) %>% 
  dist(., method = "binary")


# Use (top-down) hierarchical clustering. The main difference compared to
# k-means is that the number of clusters is not predefined - instead, this
# creates multiple clusters, and we can decide afterwards which one to use.
# First, the algorithm puts every observation into its own cluster. Next, it
# groups the two closest clusters (dots) to a single cluster, then the next two
# closest, and so on until everything is in one big cluster. The existing
# clusters (and the observations they contain) are recorded after each
# iteration. This series of different cluster-assignments can be depicted as a
# decision tree (but *only* depicted, since it's not used like the trees we
# know). The leaves represent the first round, where each observation is its own
# cluster. By connecting two observation as in the second round, we get a single
# branch. Keep doing this to get to the last iteration, which is the root. We
# can then prune the three to a stage where it only contains k clusters. To fit
# the model, use the hclust() function. The first argument are the euclidian
# distances (=vector length) between your observations; to get them, simply wrap
# your data in the dist() function. The second argument defines how the smallest
# distances between clusters are determined. Consider three clusters "A & B", "C
# & D", and "E", and we want to know which cluster is closest to "E". In the
# first cluster, we could compute the distance between "E" and "A" or "B", the
# in the second one to "C" or "D". The "complete" method uses the longest
# distance to any element in another cluster to to find the closest cluster,
# "single" uses the shortest distance, "average" uses the average, and
# "centroid" uses the center. The "complete" and "average" method produce rather
# symmetric trees, "single" and "centroid" don't. Asymmetric trees are rarely
# used, but can be helpful for identifying outliers (outliers are assigned to a
# smaller branch and the rest into a larger one).
h_cluster <- hclust(dist(data.std), method = c("complete", "average", "single", "centroid"))


# Plot the tree. You can either use the plot() function alone, or the pipe chain
# below. The k argument sets a desired number of branches, the tree will be
# colored accordingly (highlighting the tree version at that number of
# branches).
plot(h_cluster)

as.dendrogram(h_cluster) %>% 
  color_branches(., k = 3) %>% 
  plot()


# Prune the tree to get the clustering at a certain stage. The first argument is
# the model. The "k" argument is the number of clusters you want; you can also
# use "h to specify the tree height instead (which is the max/min/average/center
# euclidian distance among all observations in a cluster at that level). The
# latter makes more sense, since it gives you an indication of how close the
# cluster are to each other - if multiple branch splits occured at similar
# height values, the resulting clusters likely were not were different. Before
# you settle for a final height, compute/plot decriptive statistics of your
# variables grouped by cluster assignment to see if the results make sense.
cutree(h_cluster, k = 3, h = 6)

# Compare h-clustering with k-means. There are no clear guidelines on what to
# use, so you might try both clustering techniques. To check if they give
# similar results, run the code below.
table(k_means$cluster, h_cluster$cluster)



########################################
##    Principal Component Analysis    ##
########################################

# Prepare data. Make sure that there are not missing values (drop or impute),
# and that there are not categorical variables (drop or recode as 0/1). Your
# variables must also be normalized, but this can be done automatically later
# on.

# Use PCA. This reduces the dimensionality of our data while maintaining as much
# information as possible. If we start with 2 variables, the algorithm
# transforms our data to to have 1 variable, but still a similar amount of
# variance between individual observations. Think about a two-dimensional
# coordinate system, with each variable on one of the axes. The algorithm finds
# the linear combination between all dots which has the highest variance -
# visually, this means drawing a regression line through the cloud of dots. This
# linear combination is the first principal component. One could then draw lines
# from each dot the part of the line which is closest (so the tangential line
# has a 45° downward slope),, and mark the respective locations on the line. We
# could then just work with the distances of these locations on the line, i.e.
# with the first principal component. If we have e.g. ten variables, we might
# try to reduce it to 3 variables, so we need 3 principal components. The second
# principal component would be the linear combination which gives the second
# highest variance, and the third one is chosen to yield the third highest
# variance. The algorithm will always start with the first PC and record the
# variance, then add the second and record the new variance, and so on until there
# are as many PCs as initial variables (the maximum number).  Similar to
# hierarchical clustering, we can then choose which "level" to use. The prcomp()
# does this for you. The "scale" and "center" argument can be use to normalize
# your data, which is recommended.
pca <- prcomp(swiss, scale = TRUE, center = TRUE)

# Get each observation's PC values (x), center shift amount (center), scale
# shift amount (scale), and the transformation applied to get each PC
# (rotation).
pca$x
pca$center
pca$scale
pca$rotation

# Plot data as function of first two principal components. The plot also
# contains vectors of the initial variables, showing how the were used to create
# the PCs. If two vector almost overlap, this means the respective variable have
# about the same share in the two PCs. If you have many observations and the
# labels become annoying, create a normal plot instead.
biplot(pca)

plot(pca$x[, c(1, 2)], 
     xlab = "PC1", ylab = "PC2")


# Plot variance added by PC. Use the number of PCs for which the additional
# variance with a another PC is still overproportionately higher (tangential
# line has 45° downward slope). Alternatively, use the one which gives a desired
# level of variance (e.g. 80%); you can get the cumulative variance by calling
# summary().
pr_var <- pca$sdev^2
pve <- pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

summary(pca)

# Extract first n PCs. Once you determined the optimal number of PCs, you can
# extract the data underlying them with the code below. This can then be used as
# input data for other techniques, e.g. clustering. Using PCA before clustering
# can prevent overfitting and lead to improved performance. However, note that
# PCA also uncorrelates variables, which is not desired in models relying on
# that.
data_pca <- pca$x[, 1:n]


########################################
##               Remarks              ##
########################################

# Unsupervised methods are descriptive, not predictive - all it does is help
# use get more insights. Eventually, we have to decide ourselves what approach
# makes sense based on the problem.

# Hierachical clustering can handle virtually any distance, the results are the
# same every time we run it, and we can choose based on dendograms or the elbow
# plot. k-means clustering only handles euclidian distance, the results are not
# stable between runs, and we cannot rely on dendograms to choose the optimal k.
# However, k-means is less computationally complex and can handle larger amounts
# of data faster.

