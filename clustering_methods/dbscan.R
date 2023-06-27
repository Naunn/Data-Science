# DBSCAN CLUSTERING ====================================================================================================
# Density-based spatial clustering of applications with noise (DBSCAN) is a data clustering algorithm proposed by Martin
# Ester, Hans-Peter Kriegel, Jörg Sander and Xiaowei Xu in 1996. It is a density-based clustering non-parametric
# algorithm: given a set of points in some space, it groups together points that are closely packed together (points 
# with many nearby neighbors), marking as outliers points that lie alone in low-density regions (whose nearest neighbors
# are too far away). DBSCAN is one of the most common clustering algorithms and also most cited in scientific literature.

# Assumptions: DBSCAN works on the assumption that clusters are dense regions in space separated by regions of lower density

# Density-Based Clustering of Applications with Noise(DBScan) is an Unsupervised learning Non-linear algorithm. It does 
# use the idea of density reachability and density connectivity. The data is partitioned into groups with similar 
# characteristics or clusters but it does not require specifying the number of those groups in advance. A cluster is 
# defined as a maximum set of densely connected points. It discovers clusters of arbitrary shapes in spatial databases with noise.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(fpc)
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
# Determine optimal value for DBSCAN algorithm
dbscan::kNNdistplot(USArrests, k = 3)
abline(h = 29, lty = 2)

# Fitting DBScan clustering Model to dataset
Dbscan_cl <- dbscan(USArrests, 29)
Dbscan_cl

# Checking cluster
Dbscan_cl$cluster

# Table
table(Dbscan_cl$cluster)
table(Dbscan_cl$cluster, rownames(USArrests))

# Plotting Cluster
plot(Dbscan_cl, USArrests, main = "DBScan")
