# HIERARCHICAL CLUSTERING ==============================================================================================
# In data mining and statistics, hierarchical clustering (also called hierarchical cluster analysis or HCA) is a method
# of cluster analysis that seeks to build a hierarchy of clusters. Strategies for hierarchical clustering generally fall
# into two categories:
# - Agglomerative: This is a "bottom-up" approach: Each observation starts in its own cluster, and pairs of clusters are 
#   merged as one moves up the hierarchy.
# - Divisive: This is a "top-down" approach: All observations start in one cluster, and splits are performed recursively
#   as one moves down the hierarchy.
# In general, the merges and splits are determined in a greedy manner. The results of hierarchical clustering are usually
# presented in a dendrogram.

# Assumptions:
# The distance or similarity measures used should be appropriate for the data analyzed (see the Proximities procedure 
# for more information on choices of distance and similarity measures). Also, you should include all relevant variables 
# in your analysis. Omission of influential variables can result in a misleading solution. Because hierarchical cluster
# analysis is an exploratory method, results should be treated as tentative until they are confirmed with an independent sample.

# Hierarchical clustering in R Programming Language is an Unsupervised non-linear algorithm in which clusters are created 
# such that they have a hierarchy(or a pre-determined ordering). For example, consider a family of up to three generations.
# A grandfather and mother have their children that become father and mother of their children. So, they all are grouped
# together to the same family i.e they form a hierarchy.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
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

# Model ================================================================================================================
# Finding distance matrix
# For the distance measure can be used one of:
# "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" or "kendall".
distance_mat <- get_dist(USArrests, method = "euclidean")
distance_mat %>% fviz_dist()

# Fitting Hierarchical clustering Model to dataset
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl
# Call:
# hclust(d = distance_mat, method = "average")
# 
# Cluster method   : average 
# Distance         : euclidean 
# Number of objects: 50 

# Plotting dendrogram
plot(Hierar_cl)
# Choosing no. of clusters with cutting tree by height
abline(h = 65, col = "blue")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3)
fit

table(fit)
# fit
#  1  2  3 
# 16 14 20
table(fit, rownames(USArrests))

# Final vizualizations
plot(Hierar_cl)
abline(h = 65, col = "blue")
rect.hclust(Hierar_cl, k = 3, border = "green")

ggplot(USArrests, aes(UrbanPop, Assault)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = fit) + 
  scale_color_manual(values = c('black', 'red', 'green'))
