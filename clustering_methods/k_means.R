# LASSO REGRESSION =====================================================================================================
# k-means clustering is a method of vector quantization, originally from signal processing, that aims to partition n
# observations into k clusters in which each observation belongs to the cluster with the nearest mean (cluster centers
# or cluster centroid), serving as a prototype of the cluster. This results in a partitioning of the data space into
# Voronoi cells. k-means clustering minimizes within-cluster variances (squared Euclidean distances), but not regular
# Euclidean distances, which would be the more difficult Weber problem: the mean optimizes squared errors, whereas only
# the geometric median minimizes Euclidean distances. For instance, better Euclidean solutions can be found using
# k-medians and k-medoids. The problem is computationally difficult (NP-hard); however, efficient heuristic algorithms
# converge quickly to a local optimum.

# Assumptions:
# - k-means assume the variance of the distribution of each attribute (variable) is spherical (i.i.d. Gaussian);
# - all variables have the same variance;
# - the prior probability for all k clusters are the same, i.e. each cluster has roughly equal number of observations;
# If any one of these 3 assumptions is violated, then k-means will fail.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(forecast)
library(cluster)
library(factoextra)

# Data =================================================================================================================
data("USArrests")

USArrests %>% str()
# 'data.frame':	50 obs. of  4 variables:
# $ Murder  : num  13.2 10 8.1 8.8 9 7.9 3.3 5.9 15.4 17.4 ...
# $ Assault : int  236 263 294 190 276 204 110 238 335 211 ...
# $ UrbanPop: int  58 48 80 50 91 78 77 72 80 60 ...
# $ Rape    : num  21.2 44.5 31 19.5 40.6 38.7 11.1 15.8 31.9 25.8 ...

USArrests %>% psych::describe()
#          vars  n   mean    sd median trimmed    mad  min   max range  skew kurtosis    se
# Murder      1 50   7.79  4.36   7.25    7.53   5.41  0.8  17.4  16.6  0.37    -0.95  0.62
# Assault     2 50 170.76 83.34 159.00  168.48 110.45 45.0 337.0 292.0  0.22    -1.15 11.79
# UrbanPop    3 50  65.54 14.47  66.00   65.88  17.79 32.0  91.0  59.0 -0.21    -0.87  2.05
# Rape        4 50  21.23  9.37  20.10   20.36   8.60  7.3  46.0  38.7  0.75     0.08  1.32

map(USArrests, shapiro.test)
# $Murder
#
# Shapiro-Wilk normality test
#
# data:  .x[[i]]
# W = 0.95703, p-value = 0.06674
#
#
# $Assault
#
# Shapiro-Wilk normality test
#
# data:  .x[[i]]
# W = 0.95181, p-value = 0.04052
#
#
# $UrbanPop
#
# Shapiro-Wilk normality test
#
# data:  .x[[i]]
# W = 0.97714, p-value = 0.4385
#
#
# $Rape
#
# Shapiro-Wilk normality test
#
# data:  .x[[i]]
# W = 0.94674, p-value = 0.0251

# Preprocessing ========================================================================================================
bc_list <- map(USArrests, BoxCox.lambda)
df_bc <-
  map2_df(USArrests, bc_list, function(x, y)
    BoxCox(x, lambda = y)) %>%
  as.data.frame()

df <-
  df_bc %>%
  mutate_if(is.numeric, scale)

df %>% psych::describe()
#          vars  n mean sd median trimmed  mad   min  max range  skew kurtosis   se
# Murder      1 50    0  1   0.21    0.09 1.03 -3.34 1.36  4.71 -0.93     0.67 0.14
# Assault     2 50    0  1   0.08    0.06 1.14 -1.98 1.54  3.52 -0.38    -0.93 0.14
# UrbanPop    3 50    0  1  -0.02    0.00 1.24 -2.07 1.90  3.97 -0.02    -1.00 0.14
# Rape        4 50    0  1   0.09    0.02 0.92 -2.13 1.94  4.07 -0.18    -0.59 0.14

row.names(df) <- row.names(USArrests)

# Model ================================================================================================================
# Vizualize distances
# For the distance measure can be used one of:
# "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" or "kendall".
get_dist(df, method = 'euclidean') %>%
  fviz_dist(gradient = list(
    low = "#00AFBB",
    mid = "white",
    high = "#FC4E07"
  ))

# k-means model
# x: numeric matrix, numeric data frame or a numeric vector
# centers: Possible values are the number of clusters (k) or a set of initial (distinct) cluster centers. If a number,
#          a random set of (distinct) rows in x is chosen as the initial centers.
# iter.max: The maximum number of iterations allowed. Default value is 10.
# nstart: The number of random starting partitions when centers is a number. Trying nstart > 1 is often recommended.
k2 <- kmeans(df, centers = 2, nstart = 25)
k2
str(k2)
# cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# centers: A matrix of cluster centers.
# totss: The total sum of squares.
# withinss: Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.

fviz_cluster(k2, data = df)

# Elbow method f# function to compute total within-cluster sum of square
# the basic idea behind cluster partitioning methods, such as k-means clustering, is to define clusters such that
# the total intra-cluster variation (known as total within-cluster variation or total within-cluster sum of square)
# is minimized
wss <- function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(
  k.values,
  wss_values,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)

# automatic function
fviz_nbclust(df, kmeans, method = "wss")

# Average silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(
  k.values,
  avg_sil_values,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Average Silhouettes"
)

# automatic function
fviz_nbclust(df, kmeans, method = "silhouette")

# classical silhouette plot
kmeans(df, centers = 2, nstart = 25)$cluster %>%
  silhouette(dist(df)) %>%
  fviz_silhouette()

# Gap statistic method
# The gap statistic compares the total intracluster variation for different values of k with their expected values under 
# null reference distribution of the data (i.e. a distribution with no obvious clustering). The reference dataset is 
# generated using Monte Carlo simulations of the sampling process. That is, for each variable (x_i) in the data set we 
# compute its range [min(x_i),max(x_j)] and generate values for the n points uniformly from the interval min to max.
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

fviz_gap_stat(gap_stat)

# classical silhouette plot
kmeans(df, centers = 5, nstart = 25)$cluster %>%
  silhouette(dist(df)) %>%
  fviz_silhouette()

final <- kmeans(df, 5, nstart = 25)
fviz_cluster(final, df)
