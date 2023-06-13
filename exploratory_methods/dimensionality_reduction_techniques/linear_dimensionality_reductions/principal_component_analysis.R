# PRINCIPAL COMPONENT ANALYSIS =========================================================================================
# PCA is a statistical technique for reducing the dimensionality of a (quantitative) dataset. This is accomplished by
# linearly transforming the data into a new coordinate system where (most of) the variation in the data can be described
# with fewer dimensions than the initial data. Many studies use the first two principal components in order to plot
# the data in two dimensions and to visually identify clusters of closely related data points.

# Libraries ============================================================================================================
library(dplyr)
library(knitr)
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining with R
library(factoextra) # Print method for an object of class factoextra
library(corrr) # Package for correlation analysis that mainly focuses on creating and handling R data frames
library(corrplot)
library(gplots)

# Data =================================================================================================================
library(MultBiplotR)
data(Protein) # real-valued multivariate data set describing the average protein consumption by citizens of 25 European countries
detach("package:MultBiplotR", unload = TRUE)

Protein %>% str()
# 'data.frame':	25 obs. of  11 variables:
# $ Comunist         : Factor w/ 2 levels "No","Yes": 2 1 1 2 2 1 2 1 1 1 ...
# $ Region           : Factor w/ 3 levels "North","Center",..: 3 2 2 3 2 1 2 1 2 3 ...
# $ Red_Meat         : num  10.1 8.9 13.5 7.8 9.7 10.6 8.4 9.5 18 10.2 ...
# $ White_Meat       : num  1.4 14 9.3 6 11.4 10.8 11.6 4.9 9.9 3 ...
# $ Eggs             : num  0.5 4.3 4.1 1.6 2.8 3.7 3.7 2.7 3.3 2.8 ...
# $ Milk             : num  8.9 19.9 17.5 8.3 12.5 25 11.1 33.7 19.5 17.6 ...
# $ Fish             : num  0.2 2.1 4.5 1.2 2 9.9 5.4 5.8 5.7 5.9 ...
# $ Cereal           : num  42.3 28 26.6 56.7 34.3 21.9 24.6 26.3 28.1 41.7 ...
# $ Starch           : num  0.6 3.6 5.7 1.1 5 4.8 6.5 5.1 4.8 2.2 ...
# $ Nuts             : num  5.5 1.3 2.1 3.7 1.1 0.7 0.8 1 2.4 7.8 ...
# $ Fruits_Vegetables: num  1.7 4.3 4 4.2 4 2.4 3.6 1.4 6.5 6.5 ...

Protein %>% is.na() %>% colSums()
# Comunist          Region            Red_Meat          White_Meat        Eggs              Milk
# 0                 0                 0                 0                 0                 0
# Fish              Cereal            Starch            Nuts Fruits_Vegetables
# 0                 0                 0                 0                 0

# Preprocessing ========================================================================================================
min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

Protein_prep <-
  Protein %>%
  mutate_if(is.numeric, min_max)

cor_matrix <- cor(Protein_prep[3:11])
cor_matrix %>% corrplot(method = "number")

# Principal component analysis and interpretation ======================================================================
## princomp() approach =================================================================================================
# The calculation is done using eigen on the correlation or covariance matrix, as determined by cor. This is done for
# compatibility with the S-PLUS result. A preferred method of calculation is to use svd on x, as is done in prcomp."
pca.m <- princomp(cor_matrix)
summary(pca.m) # standard function for R models

pca.m$loadings # correlation of columns by each component
pca.m$loadings[, 1:2]
#                       Comp.1      Comp.2
# Red_Meat           0.2902937  0.09510150
# White_Meat         0.3131864  0.26392299
# Eggs               0.4131713  0.07419838
# Milk               0.3852342  0.15393922
# Fish               0.1165053 -0.69780771
# Cereal            -0.4342505  0.27531862
# Starch             0.2796041 -0.36379635
# Nuts              -0.4400339 -0.07686551
# Fruits_Vegetables -0.1567571 -0.43715631

fviz_eig(pca.m, addlabels = TRUE)

fviz_pca_biplot(pca.m, col.var = "black")
# Three main pieces of information can be observed from the previous plot (Note that this is essentially a loadings plot):
# - All the variables that are grouped together are positively correlated to each other, and that is the case  for
#   instance for white/red meat, milk, and eggs have a positive correlation to each. This result is surprising because
#   they have the highest values in the loading matrix with respect to the first principal component;
# - The higher the distance between the variable and the origin, the better represented that variable is.
#   From the biplot, eggs, milk, and white meat have higher magnitude compared to red meat, and hence are
#   well represented compared to red meat;
# - Variables that are negatively correlated are displayed to the opposite sides of the biplot’s origin.

# cos2 (the square cosine) - determines how much each variable is represented in a given component.
fviz_cos2(pca.m, choice = "var", axes = 1:2)

# Use repel = TRUE to avoid overplotting (slow if many points)
fviz_pca_var(
  pca.m,
  col.var = "cos2",
  gradient.cols = c("red", "orange", "green"),
  repel = TRUE
)

## prcomp() approach ===================================================================================================
# The calculation is done by a singular value decomposition of the (centered and possibly scaled) data matrix, not by 
# using eigen on the covariance matrix. This is generally the preferred method for numerical accuracy.
pca.m <- prcomp(Protein[3:11], center = TRUE, scale = TRUE)
summary(pca.m)

# new coordinates based on pca-axises
pca.m$x %>% kable()
dim(pca.m$x) # equivalent of python shape

fviz_eig(pca.m, addlabels = TRUE)

fviz_pca_biplot(pca.m, col.var = "black")

# cos2 (the square cosine) - determines how much each variable is represented in a given component.
fviz_cos2(pca.m, choice = "var", axes = 1:2)

# Use repel = TRUE to avoid overplotting (slow if many points)
fviz_pca_var(
  pca.m,
  col.var = "cos2",
  gradient.cols = c("red", "orange", "green"),
  repel = TRUE
)

# Finally, based on pca.m$x table with coordinates, we can build regression/clasiffication model depending on
# percentage of variance in each pca column. In this case, we can use first 3 columns, i.e. pca.m$x[,1:3] (~75.2%).
