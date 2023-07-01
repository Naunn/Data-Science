# RANDOM FOREST ALGORITHM ==============================================================================================
# Random forests or random decision forests is an ensemble learning method for classification, regression and other tasks
# that operates by constructing a multitude of decision trees at training time. For classification tasks, the output of
# the random forest is the class selected by most trees. For regression tasks, the mean or average prediction of the
# individual trees is returned. Random decision forests correct for decision trees' habit of overfitting to their training
# set. Random forests generally outperform decision trees, but their accuracy is lower than gradient boosted trees.
# However, data characteristics can affect their performance.
#
# The first algorithm for random decision forests was created in 1995 by Tin Kam Ho using the random subspace method,
# which, in Ho's formulation, is a way to implement the "stochastic discrimination" approach to classification proposed
# by Eugene Kleinberg.
#
# An extension of the algorithm was developed by Leo Breiman and Adele Cutler, who registered "Random Forests" as a
# trademark in 2006 (as of 2019, owned by Minitab, Inc.). The extension combines Breiman's "bagging" idea and random
# selection of features, introduced first by Ho and later independently by Amit and Geman in order to construct a
# collection of decision trees with controlled variance.
#
# Random forests are frequently used as black box models in businesses, as they generate reasonable predictions across
# a wide range of data while requiring little configuration.

# Assumptions:
# - main assumption is that samples (from bagging) are representative (independent variables and probability-based values)
# Note. Representative sampling and random sampling are two techniques used to help ensure data is free of bias.
# A representative sample is a group or set chosen from a larger statistical population according to specified
# characteristics. A random sample is a group or set chosen in a random manner from a larger population.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)

# Data =================================================================================================================
data(Glass, package = "mlbench")
# Additional Information:
# 1. Id number: 1 to 214
# 2. RI: refractive index
# 3. Na: Sodium (unit measurement: weight percent in corresponding oxide, as are attributes 4-10)
# 4. Mg: Magnesium
# 5. Al: Aluminum
# 6. Si: Silicon
# 7. K: Potassium
# 8. Ca: Calcium
# 9. Ba: Barium
# 10. Fe: Iron
# 11. Type of glass: (class attribute)
# -- 1 building_windows_float_processed
# -- 2 building_windows_non_float_processed
# -- 3 vehicle_windows_float_processed
# -- 4 vehicle_windows_non_float_processed (none in this database)
# -- 5 containers
# -- 6 tableware
# -- 7 headlamps

Glass %>% str()
# 'data.frame':	214 obs. of  10 variables:
# $ RI  : num  1.52 1.52 1.52 1.52 1.52 ...
# $ Na  : num  13.6 13.9 13.5 13.2 13.3 ...
# $ Mg  : num  4.49 3.6 3.55 3.69 3.62 3.61 3.6 3.61 3.58 3.6 ...
# $ Al  : num  1.1 1.36 1.54 1.29 1.24 1.62 1.14 1.05 1.37 1.36 ...
# $ Si  : num  71.8 72.7 73 72.6 73.1 ...
# $ K   : num  0.06 0.48 0.39 0.57 0.55 0.64 0.58 0.57 0.56 0.57 ...
# $ Ca  : num  8.75 7.83 7.78 8.22 8.07 8.07 8.17 8.24 8.3 8.4 ...
# $ Ba  : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Fe  : num  0 0 0 0 0 0.26 0 0 0 0.11 ...
# $ Type: Factor w/ 6 levels "1","2","3","5",..: 1 1 1 1 1 1 1 1 1 1 ...

Glass$Type %>% table()
# .
#  1  2  3  5  6  7
# 70 76 17 13  9 29

Glass %>% psych::describe()
#       vars   n  mean   sd median trimmed  mad   min   max range  skew kurtosis   se
# RI       1 214  1.52 0.00   1.52    1.52 0.00  1.51  1.53  0.02  1.60     4.72 0.00
# Na       2 214 13.41 0.82  13.30   13.38 0.64 10.73 17.38  6.65  0.45     2.90 0.06
# Mg       3 214  2.68 1.44   3.48    2.87 0.30  0.00  4.49  4.49 -1.14    -0.45 0.10
# Al       4 214  1.44 0.50   1.36    1.41 0.31  0.29  3.50  3.21  0.89     1.94 0.03
# Si       5 214 72.65 0.77  72.79   72.71 0.57 69.81 75.41  5.60 -0.72     2.82 0.05
# K        6 214  0.50 0.65   0.56    0.43 0.17  0.00  6.21  6.21  6.46    52.87 0.04
# Ca       7 214  8.96 1.42   8.60    8.74 0.66  5.43 16.19 10.76  2.02     6.41 0.10
# Ba       8 214  0.18 0.50   0.00    0.03 0.00  0.00  3.15  3.15  3.37    12.08 0.03
# Fe       9 214  0.06 0.10   0.00    0.04 0.00  0.00  0.51  0.51  1.73     2.52 0.01
# Type*   10 214  2.54 1.71   2.00    2.31 1.48  1.00  6.00  5.00  1.04    -0.29 0.12

GGally::ggpairs(Glass, aes(colour = Type))

df <-
  Glass %>%
  mutate_if(is.numeric, scale) %>%
  mutate_if(is.matrix, as.vector)

# Split the data into training (80%) and test set (20%)
training.samples.df <- df$Type %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.df <- df[training.samples.df,]
test.data.df <- df[-training.samples.df,]

# Random Forest classification model ===================================================================================
rf_model <-
  randomForest(Type ~ ., train.data.df)
# Call:
#   randomForest(formula = Type ~ ., data = train.data.df)
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
#
# OOB estimate of  error rate: 23.56%
# Confusion matrix:
#    1  2 3 5 6  7 class.error
# 1 50  5 1 0 0  0   0.1071429
# 2  9 45 1 3 2  1   0.2622951
# 3  5  4 5 0 0  0   0.6428571
# 5  0  4 0 6 0  1   0.4545455
# 6  0  2 0 0 6  0   0.2500000
# 7  1  2 0 0 0 21   0.1250000

pred <-
  predict(rf_model, test.data.df %>% select(!Type)) %>%
  as.vector() %>%
  factor(levels = c(1, 2, 3, 5, 6, 7))

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
#  0.87500        0.82095        0.73197        0.95814        0.40000        0.00000            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.87500     1.00000        1.00000        0.92308   1.00000 0.87500 0.93333      0.400
# Class: 2     0.86667     0.92000        0.86667        0.92000   0.86667 0.86667 0.86667      0.375
# Class: 3     1.00000     0.97368        0.66667        1.00000   0.66667 1.00000 0.80000      0.050
# Class: 5     0.00000     0.94872        0.00000        0.97368   0.00000 0.00000     NaN      0.025
# Class: 6     1.00000     1.00000        1.00000        1.00000   1.00000 1.00000 1.00000      0.025
# Class: 7     1.00000     1.00000        1.00000        1.00000   1.00000 1.00000 1.00000      0.125
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.350                0.350           0.93750
# Class: 2          0.325                0.375           0.89333
# Class: 3          0.050                0.075           0.98684
# Class: 5          0.000                0.050           0.47436
# Class: 6          0.025                0.025           1.00000
# Class: 7          0.125                0.125           1.00000

# Random Forest regression model =======================================================================================
rf_model <-
  randomForest(Type ~ ., train.data.df %>% mutate(Type = as.numeric(Type)), importance = TRUE)
rf_model
# Call:
#   randomForest(formula = Type ~ ., data = train.data.df %>% mutate(Type = as.numeric(Type))) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 0.746291
# % Var explained: 74.66

importance(rf_model)
#      %IncMSE IncNodePurity
# RI 17.172380      32.64881
# Na 14.372405      63.40418
# Mg 27.401645     140.20717
# Al 16.555531      77.00038
# Si 11.934158      27.18193
# K  14.198077      38.97041
# Ca 14.810018      29.40673
# Ba 17.003977      76.01539
# Fe  7.856711      10.24941

varImpPlot(rf_model)

pred <- predict(rf_model, test.data.df %>% select(!Type))

plot(pred)
lines(test.data.df$Type)
