# SUPPORT VECTOR MACHINE ALGORITHM =====================================================================================
# In machine learning, support vector machines (SVMs, also support vector networks) are supervised learning models with
# associated learning algorithms that analyze data for classification and regression analysis. Developed at AT&T Bell
# Laboratories by Vladimir Vapnik with colleagues (Boser et al., 1992, Guyon et al., 1993, Cortes and Vapnik, 1995,
# Vapnik et al., 1997) SVMs are one of the most robust prediction methods, being based on statistical learning frameworks
# or VC theory proposed by Vapnik (1982, 1995) and Chervonenkis (1974). Given a set of training examples, each marked as
# belonging to one of two categories, an SVM training algorithm builds a model that assigns new examples to one category
# or the other, making it a non-probabilistic binary linear classifier (although methods such as Platt scaling exist to
# use SVM in a probabilistic classification setting). SVM maps training examples to points in space so as to maximise the
# width of the gap between the two categories. New examples are then mapped into that same space and predicted to belong
# to a category based on which side of the gap they fall.

# In addition to performing linear classification, SVMs can efficiently perform a non-linear classification using what is
# called the kernel trick, implicitly mapping their inputs into high-dimensional feature spaces.

# The support vector clustering algorithm, created by Hava Siegelmann and Vladimir Vapnik, applies the statistics of
# support vectors, developed in the support vector machines algorithm, to categorize unlabeled data. These data sets
# require unsupervised learning approaches, which attempt to find natural clustering of the data to groups and, then,
# to map new data according to these clusters.

# Note:
# - SVM can be used to linear and non-linear data (depending on kernel),
# - SVM can be sensitive to outliers.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)

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
train.data.df <- df[training.samples.df, ]
test.data.df <- df[-training.samples.df, ]

# Linear SVM classifier ================================================================================================
linear_svm <-
  svm(
    Type ~ .,
    data = train.data.df,
    kernel = "linear",
    cost = 10,
    scale = FALSE
  )
linear_svm
# Call:
#   svm(formula = Type ~ ., data = train.data.df, kernel = "linear", cost = 10, scale = FALSE)
#
#
# Parameters:
#   SVM-Type:  C-classification
# SVM-Kernel:  linear
#       cost:  10
#
# Number of Support Vectors:  125
plot(linear_svm, data = train.data.df, RI ~ Na)

pred <- predict(linear_svm, test.data.df[,-ncol(df)])

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.65000        0.50442        0.48316        0.79372        0.50000        0.04035            NaN

cm$byClass %>% round(5)
# Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.80000     0.80000        0.57143        0.92308   0.57143 0.80000 0.66667      0.250
# Class: 2     0.60000     0.85000        0.80000        0.68000   0.80000 0.60000 0.68571      0.500
# Class: 3     0.33333     0.94595        0.33333        0.94595   0.33333 0.33333 0.33333      0.075
# Class: 5     0.50000     0.97368        0.50000        0.97368   0.50000 0.50000 0.50000      0.050
# Class: 6     0.50000     1.00000        1.00000        0.97436   1.00000 0.50000 0.66667      0.050
# Class: 7     1.00000     0.94595        0.60000        1.00000   0.60000 1.00000 0.75000      0.075
# Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.200                0.350           0.80000
# Class: 2          0.300                0.375           0.72500
# Class: 3          0.025                0.075           0.63964
# Class: 5          0.025                0.050           0.73684
# Class: 6          0.025                0.025           0.75000
# Class: 7          0.075                0.125           0.97297

linear_svm <-
  svm(
    Type ~ .,
    data = train.data.df,
    kernel = "polynomial",
    cost = 10,
    scale = FALSE
  )
linear_svm
# Call:
#   svm(formula = Type ~ ., data = train.data.df, kernel = "polynomial", cost = 10, scale = FALSE)
#
#
# Parameters:
#   SVM-Type:  C-classification
# SVM-Kernel:  polynomial
#       cost:  10
#     degree:  3
#     coef.0:  0
#
# Number of Support Vectors:  142
plot(linear_svm, data = train.data.df, RI ~ Na)

pred <- predict(linear_svm, test.data.df[,-ncol(df)])

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.62500        0.45255        0.45801        0.77274        0.50000        0.07693            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.69231     0.81481        0.64286        0.84615   0.64286 0.69231 0.66667      0.325
# Class: 2     0.60000     0.85000        0.80000        0.68000   0.80000 0.60000 0.68571      0.500
# Class: 3          NA     0.92500             NA             NA   0.00000      NA      NA      0.000
# Class: 5     0.33333     0.97297        0.50000        0.94737   0.50000 0.33333 0.40000      0.075
# Class: 6     0.00000     0.97436        0.00000        0.97436   0.00000 0.00000     NaN      0.025
# Class: 7     1.00000     0.94595        0.60000        1.00000   0.60000 1.00000 0.75000      0.075
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.225                0.350           0.75356
# Class: 2          0.300                0.375           0.72500
# Class: 3          0.000                0.075                NA
# Class: 5          0.025                0.050           0.65315
# Class: 6          0.000                0.025           0.48718
# Class: 7          0.075                0.125           0.97297

# Non-Linear SVM classifier ============================================================================================
nonlinear_svm <-
  svm(
    Type ~ .,
    data = train.data.df,
    kernel = "radial",
    cost = 10,
    scale = FALSE
  )
nonlinear_svm
# Call:
#   svm(formula = Type ~ ., data = train.data.df, kernel = "radial", cost = 10, scale = FALSE)
#
#
# Parameters:
#   SVM-Type:  C-classification
# SVM-Kernel:  radial
#       cost:  10
#
# Number of Support Vectors:  131
plot(nonlinear_svm, data = train.data.df, RI ~ Na)

pred <- predict(nonlinear_svm, test.data.df[,-ncol(df)])

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.65000        0.49367        0.48316        0.79372        0.42500        0.00343            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.73333     0.88000        0.78571        0.84615   0.78571 0.73333 0.75862      0.375
# Class: 2     0.58824     0.78261        0.66667        0.72000   0.66667 0.58824 0.62500      0.425
# Class: 3     0.50000     0.94737        0.33333        0.97297   0.33333 0.50000 0.40000      0.050
# Class: 5     1.00000     0.97436        0.50000        1.00000   0.50000 1.00000 0.66667      0.025
# Class: 6     0.00000     0.97436        0.00000        0.97436   0.00000 0.00000     NaN      0.025
# Class: 7     0.75000     0.94444        0.60000        0.97143   0.60000 0.75000 0.66667      0.100
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.275                0.350           0.80667
# Class: 2          0.250                0.375           0.68542
# Class: 3          0.025                0.075           0.72368
# Class: 5          0.025                0.050           0.98718
# Class: 6          0.000                0.025           0.48718
# Class: 7          0.075                0.125           0.84722

nonlinear_svm <-
  svm(
    Type ~ .,
    data = train.data.df,
    kernel = "sigmoid",
    cost = 10,
    scale = FALSE
  )
nonlinear_svm
# Call:
#   svm(formula = Type ~ ., data = train.data.df, kernel = "sigmoid", cost = 10, scale = FALSE)
#
#
# Parameters:
#   SVM-Type:  C-classification
# SVM-Kernel:  sigmoid
#       cost:  10
#     coef.0:  0
#
# Number of Support Vectors:  122
plot(nonlinear_svm, data = train.data.df, RI ~ Na)

pred <- predict(nonlinear_svm, test.data.df[,-ncol(df)])

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.45000        0.19340        0.29259        0.61509        0.57500        0.95996            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.50000     0.70000        0.35714        0.80769   0.35714 0.50000 0.41667      0.250
# Class: 2     0.43478     0.70588        0.66667        0.48000   0.66667 0.43478 0.52632      0.575
# Class: 3     0.00000     0.92308        0.00000        0.97297   0.00000 0.00000     NaN      0.025
# Class: 5     0.00000     0.94595        0.00000        0.92105   0.00000 0.00000     NaN      0.075
# Class: 6          NA     0.97500             NA             NA   0.00000      NA      NA      0.000
# Class: 7     1.00000     0.94595        0.60000        1.00000   0.60000 1.00000 0.75000      0.075
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.125                0.350           0.60000
# Class: 2          0.250                0.375           0.57033
# Class: 3          0.000                0.075           0.46154
# Class: 5          0.000                0.050           0.47297
# Class: 6          0.000                0.025                NA
# Class: 7          0.075                0.125           0.97297

# Linear SVM Regression ================================================================================================
linear_svm_reg <-
  svm(
    Type ~ .,
    data = train.data.df %>% mutate_all(as.numeric),
    kernel = "linear",
    cost = 10,
    scale = FALSE,
    type = 'eps-regression'
  )
pred <- predict(linear_svm_reg, test.data.df[,-ncol(df)])
plot(pred)
lines(test.data.df$Type)

linear_svm_reg <-
  svm(
    Type ~ .,
    data = train.data.df %>% mutate_all(as.numeric),
    kernel = "polynomial",
    cost = 10,
    scale = FALSE,
    type = 'eps-regression'
  )
pred <- predict(linear_svm_reg, test.data.df[,-ncol(df)])
plot(pred)
lines(test.data.df$Type)

# Non-Linear SVM Regression ============================================================================================
nonlinear_svm_reg <-
  svm(
    Type ~ .,
    data = train.data.df %>% mutate_all(as.numeric),
    kernel = "radial",
    cost = 10,
    scale = FALSE,
    type = 'eps-regression'
  )
pred <- predict(nonlinear_svm_reg, test.data.df[,-ncol(df)])
plot(pred)
lines(test.data.df$Type)

nonlinear_svm_reg <-
  svm(
    Type ~ .,
    data = train.data.df %>% mutate_all(as.numeric),
    kernel = "sigmoid",
    cost = 10,
    scale = FALSE,
    type = 'eps-regression'
  )
pred <- predict(nonlinear_svm_reg, test.data.df[,-ncol(df)])
plot(pred)
lines(test.data.df$Type)

# library(caret) =======================================================================================================
trainControl <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 3)
metric <- "Accuracy"
fit.cv.svm <- train(
  Type ~ .,
  data = train.data.df,
  method = "svmLinear",
  metric = metric ,
  trControl = trainControl
)
fit.cv.svm
pred <- predict(fit.cv.svm, test.data.df[,-ncol(df)])

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall[1]
# Accuracy
#     0.65

trainControl <-
  trainControl(method = "boot", number = 100)
metric <- "Accuracy"
fit.cv.svm <- train(
  Type ~ .,
  data = train.data.df,
  method = "svmLinear",
  metric = metric ,
  trControl = trainControl
)
fit.cv.svm
pred <- predict(fit.cv.svm, test.data.df[,-ncol(df)])

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall[1]
# Accuracy
#     0.65
