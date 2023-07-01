# PREPROCESSING ========================================================================================================
# Data preprocessing can refer to manipulation or dropping of data before it is used in order to ensure or enhance
# performance, and is an important step in the data mining process. The phrase "garbage in, garbage out" is particularly
# applicable to data mining and machine learning projects. Data-gathering methods are often loosely controlled, resulting
# in out-of-range values (e.g., Income: −100), impossible data combinations (e.g., Sex: Male, Pregnant: Yes), and missing values, etc.
#
# Analyzing data that has not been carefully screened for such problems can produce misleading results. Thus, the
# representation and quality of data is first and foremost before running any analysis. Often, data preprocessing is the
# most important phase of a machine learning project, especially in computational biology. If there is much irrelevant
# and redundant information present or noisy and unreliable data, then knowledge discovery during the training phase is
# more difficult. Data preparation and filtering steps can take considerable amount of processing time. Examples of data
# preprocessing include cleaning, instance selection, normalization, one hot encoding, transformation, feature extraction
# and selection, etc. The product of data preprocessing is the final training set.
#
# Data preprocessing may affect the way in which outcomes of the final data processing can be interpreted. This aspect
# should be carefully considered when interpretation of the results is a key point, such in the multivariate processing
# of chemical data (chemometrics).

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(boot)
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

train.data.df$Type %>% table()
#  1  2  3  5  6  7
# 56 61 14 11  8 24

# Simple K-NN for comparison
base_pred <-
  knn3Train(
    train = train.data.df %>% select(!Type),
    test = test.data.df %>% select(!Type),
    cl = train.data.df$Type,
    k = 5,
    prob = FALSE
  ) %>%
  factor(levels = c(1, 2, 3, 5, 6, 7))

base_cm <-
  confusionMatrix(test.data.df$Type %>% factor(levels = c(1, 2, 3, 5, 6, 7)), base_pred)

base_cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.60000        0.39394        0.43327        0.75135        0.50000        0.13409            NaN

base_cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision Recall      F1 Prevalence Detection Rate
# Class: 1       0.625     0.83333        0.71429        0.76923   0.71429  0.625 0.66667        0.4           0.25
# Class: 2       0.500     0.75000        0.66667        0.60000   0.66667  0.500 0.57143        0.5           0.25
# Class: 3          NA     0.92500             NA             NA   0.00000     NA      NA        0.0           0.00
# Class: 5          NA     0.95000             NA             NA   0.00000     NA      NA        0.0           0.00
# Class: 6          NA     0.97500             NA             NA   0.00000     NA      NA        0.0           0.00
# Class: 7       1.000     0.97222        0.80000        1.00000   0.80000  1.000 0.88889        0.1           0.10
#          Detection Prevalence Balanced Accuracy
# Class: 1                0.350           0.72917
# Class: 2                0.375           0.62500
# Class: 3                0.075                NA
# Class: 5                0.050                NA
# Class: 6                0.025                NA
# Class: 7                0.125           0.98611

# Bootstrapping ========================================================================================================
# Bootstrapping is any test or metric that uses random sampling with replacement (e.g. mimicking the sampling process),
# and falls under the broader class of resampling methods. Bootstrapping assigns measures of accuracy (bias, variance,
# confidence intervals, prediction error, etc.) to sample estimates. This technique allows estimation of the sampling
# distribution of almost any statistic using random sampling methods.
#
# Bootstrapping estimates the properties of an estimand (such as its variance) by measuring those properties when sampling
# from an approximating distribution. One standard choice for an approximating distribution is the empirical distribution
# function of the observed data. In the case where a set of observations can be assumed to be from an independent and
# identically distributed population, this can be implemented by constructing a number of resamples with replacement,
# of the observed data set (and of equal size to the observed data set).
#
# It may also be used for constructing hypothesis tests. It is often used as an alternative to statistical inference based
# on the assumption of a parametric model when that assumption is in doubt, or where parametric inference is impossible
# or requires complicated formulas for the calculation of standard errors.

boot_samples <- createResample(df$Type, times = 100, list = FALSE)
df[boot_samples,]$Type %>% table()
#    1    2    3    5    6    7
# 7079 7471 1770 1247  912 2921

# build model using bootstrapping
train.control <- trainControl(method = "boot", number = 100)
model <- train(Type ~ .,
               data = train.data.df,
               method = "knn",
               trControl = train.control)
pred <- predict(model, test.data.df %>% select(!Type))

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.60000        0.39394        0.43327        0.75135        0.50000        0.13409            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision Recall      F1 Prevalence Detection Rate
# Class: 1       0.625     0.83333        0.71429        0.76923   0.71429  0.625 0.66667        0.4           0.25
# Class: 2       0.500     0.75000        0.66667        0.60000   0.66667  0.500 0.57143        0.5           0.25
# Class: 3          NA     0.92500             NA             NA   0.00000     NA      NA        0.0           0.00
# Class: 5          NA     0.95000             NA             NA   0.00000     NA      NA        0.0           0.00
# Class: 6          NA     0.97500             NA             NA   0.00000     NA      NA        0.0           0.00
# Class: 7       1.000     0.97222        0.80000        1.00000   0.80000  1.000 0.88889        0.1           0.10
#          Detection Prevalence Balanced Accuracy
# Class: 1                0.350           0.72917
# Class: 2                0.375           0.62500
# Class: 3                0.075                NA
# Class: 5                0.050                NA
# Class: 6                0.025                NA
# Class: 7                0.125           0.98611

# Bagging ==============================================================================================================
# Bootstrap aggregating, also called bagging (from bootstrap aggregating), is a machine learning ensemble meta-algorithm
# designed to improve the stability and accuracy of machine learning algorithms used in statistical classification and
# regression. It also reduces variance and helps to avoid overfitting. Although it is usually applied to decision tree
# methods, it can be used with any type of method. Bagging is a special case of the model averaging approach. Bootstrap
# aggregation can be related to the posterior predictive distribution.

# In 1996, Leo Breiman introduced the bagging algorithm, which has three basic steps:
#  - Bootstrapping - Bagging leverages a bootstrapping sampling technique to create diverse samples. This resampling
#    method generates different subsets of the training dataset by selecting data points at random and with replacement.
#    This means that each time you select a data point from the training dataset, you are able to select the same
#    instance multiple times. As a result, a value/instance repeated twice (or more) in a sample.
#  - Parallel training - These bootstrap samples are then trained independently and in parallel with each other using
#    weak or base learners.
#  - Aggregation - Finally, depending on the task (i.e. regression or classification), an average or a majority of the
#    predictions are taken to compute a more accurate estimate. In the case of regression, an average is taken of all
#    the outputs predicted by the individual classifiers; this is known as soft voting. For classification problems,
#    the class with the highest majority of votes is accepted; this is known as hard voting or majority voting.

# Using random forest to make prediction
rf_model <-
  randomForest(Type ~ ., train.data.df)
rf_model
# Call:
#   randomForest(formula = Type ~ ., data = train.data.df)
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
#
# OOB estimate of  error rate: 20.11%
# Confusion matrix:
#    1  2 3 5 6  7 class.error
# 1 48  6 1 0 0  1   0.1428571
# 2  9 49 1 2 0  0   0.1967213
# 3  7  1 6 0 0  0   0.5714286
# 5  0  2 0 8 0  1   0.2727273
# 6  1  0 0 0 7  0   0.1250000
# 7  1  1 0 0 1 21   0.1250000

pred <-
  predict(rf_model, test.data.df %>% select(!Type)) %>%
  as.vector() %>%
  factor(levels = c(1, 2, 3, 5, 6, 7))

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.77500        0.67391        0.61549        0.89160        0.42500        0.00001            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.76471     0.95652        0.92857        0.84615   0.92857 0.76471 0.83871      0.425
# Class: 2     0.73333     0.84000        0.73333        0.84000   0.73333 0.73333 0.73333      0.375
# Class: 3     0.50000     0.94737        0.33333        0.97297   0.33333 0.50000 0.40000      0.050
# Class: 5     1.00000     0.97436        0.50000        1.00000   0.50000 1.00000 0.66667      0.025
# Class: 6          NA     0.97500             NA             NA   0.00000      NA      NA      0.000
# Class: 7     1.00000     1.00000        1.00000        1.00000   1.00000 1.00000 1.00000      0.125
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.325                0.350           0.86061
# Class: 2          0.275                0.375           0.78667
# Class: 3          0.025                0.075           0.72368
# Class: 5          0.025                0.050           0.98718
# Class: 6          0.000                0.025                NA
# Class: 7          0.125                0.125           1.00000

# using random forest as method of imputation (statistics)
p <- .5
df_na <-
  as.data.frame(apply(
    X = df %>% select(!Type),
    MARGIN = 1:2,
    FUN = \(x) sample(c(x, NA_integer_), 1, prob = c((1 - p), p))
  )) %>%
  mutate_all(as.numeric) %>%
  cbind(df$Type) %>%
  rename(Type = `df$Type`)

df_na %>% str()
# 'data.frame':	214 obs. of  10 variables:
# $ RI  : num  0.871 NA -0.72 NA NA ...
# $ Na  : num  0.284 NA 0.15 NA -0.169 ...
# $ Mg  : num  1.252 0.635 NA NA NA ...
# $ Al  : num  NA NA NA NA NA ...
# $ Si  : num  -1.124 0.102 0.438 NA 0.554 ...
# $ K   : num  -0.6701 -0.0262 -0.1641 NA NA ...
# $ Ca  : num  NA -0.792 NA -0.518 -0.623 ...
# $ Ba  : num  NA -0.352 NA -0.352 -0.352 ...
# $ Fe  : num  -0.585 NA -0.585 -0.585 -0.585 ...
# $ Type: Factor w/ 6 levels "1","2","3","5",..: 1 1 1 1 1 1 1 1 1 1 ...

df_imputed <- rfImpute(Type ~ ., data = df_na, iter = 10)

rf_model <-
  randomForest(Type ~ ., df_imputed, proximity = TRUE)
rf_model
# Call:
#   randomForest(formula = Type ~ ., data = df_imputed, proximity = TRUE)
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
#
# OOB estimate of  error rate: 42.99%
# Confusion matrix:
#    1  2 3 5 6  7 class.error
# 1 51 18 0 0 1  0   0.2714286
# 2 21 47 0 3 0  5   0.3815789
# 3  8  8 0 0 0  1   1.0000000
# 5  2  7 0 1 0  3   0.9230769
# 6  1  2 0 0 2  4   0.7777778
# 7  2  5 0 0 1 21   0.2758621

pred <-
  predict(rf_model, test.data.df %>% select(!Type)) %>%
  as.vector() %>%
  factor(levels = c(1, 2, 3, 5, 6, 7))

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.85000        0.78022        0.70165        0.94290        0.42500        0.00000            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.82353     1.00000        1.00000        0.88462   1.00000 0.82353 0.90323      0.425
# Class: 2     0.81250     0.91667        0.86667        0.88000   0.86667 0.81250 0.83871      0.400
# Class: 3     1.00000     0.94872        0.33333        1.00000   0.33333 1.00000 0.50000      0.025
# Class: 5     1.00000     0.97436        0.50000        1.00000   0.50000 1.00000 0.66667      0.025
# Class: 6          NA     0.97500             NA             NA   0.00000      NA      NA      0.000
# Class: 7     1.00000     1.00000        1.00000        1.00000   1.00000 1.00000 1.00000      0.125
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.350                0.350           0.91176
# Class: 2          0.325                0.375           0.86458
# Class: 3          0.025                0.075           0.97436
# Class: 5          0.025                0.050           0.98718
# Class: 6          0.000                0.025                NA
# Class: 7          0.125                0.125           1.00000

# Cross-validation ======================================================================================================
# Cross-validation, sometimes called rotation estimation or out-of-sample testing, is any of various similar model
# validation techniques for assessing how the results of a statistical analysis will generalize to an independent data set.
# Cross-validation is a resampling method that uses different portions of the data to test and train a model on different
# iterations. It is mainly used in settings where the goal is prediction, and one wants to estimate how accurately a
# predictive model will perform in practice. In a prediction problem, a model is usually given a dataset of known data on
# which training is run (training dataset), and a dataset of unknown data (or first seen data) against which the model is
# tested (called the validation dataset or testing set). The goal of cross-validation is to test the model's ability to
# predict new data that was not used in estimating it, in order to flag problems like overfitting or selection bias and
# to give an insight on how the model will generalize to an independent dataset (i.e., an unknown dataset, for instance
# from a real problem).
#
# One round of cross-validation involves partitioning a sample of data into complementary subsets, performing the analysis
# on one subset (called the training set), and validating the analysis on the other subset (called the validation set or
# testing set). To reduce variability, in most methods multiple rounds of cross-validation are performed using different
# partitions, and the validation results are combined (e.g. averaged) over the rounds to give an estimate of the model's
# predictive performance.
#
# In summary, cross-validation combines (averages) measures of fitness in prediction to derive a more accurate estimate
# of model prediction performance.

k_folds <- createFolds(df$Type, k = 10, list = TRUE)
k_folds
# $Fold01
# [1]   8  24  38  46  52  60  66  93 111 114 118 124 128 141 152 155 174 181 190 193 207
#
# $Fold02
# [1]   3  37  41  50  55  56  67  73  90  94 107 130 132 136 137 156 161 168 178 187 195 210
#
# $Fold03
# [1]  12  18  20  22  51  58  68  75  81  99 100 101 129 138 142 151 163 167 177 186 198
#
# $Fold04
# [1]   4   6  11  28  39  54  61  78  95 102 108 123 135 139 147 166 179 201 202 206
#
# $Fold05
# [1]   5  17  21  25  31  59  65  76  83  88  92  97  98 133 148 154 175 176 183 199 200 209
#
# $Fold06
# [1]   2   9  10  13  34  36  53  86  91  96 104 109 116 126 127 153 162 169 189 197 208
#
# $Fold07
# [1]   7  14  15  30  33  35  47  82  84  85 113 120 140 143 146 150 164 182 192 213 214
#
# $Fold08
# [1]  23  27  48  57  62  69  70  77  87 105 115 119 125 131 149 160 165 172 180 188 191 205
#
# $Fold09
# [1]  16  19  26  32  43  63  64  71  72  74 103 106 110 122 134 159 171 173 185 196 203 211
#
# $Fold10
# [1]   1  29  40  42  44  45  49  79  80  89 112 117 121 144 145 157 158 170 184 194 204 212

sapply(k_folds, \(x) table(df$Type[x]))
#   Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10
# 1      7      7      7      7      7      7      7      7      7      7
# 2      8      8      8      8      7      7      7      7      8      8
# 3      1      1      1      2      2      2      2      2      2      2
# 5      1      2      1      1      1      1      2      1      1      2
# 6      1      1      0      1      1      1      1      1      1      1
# 7      3      3      3      2      3      3      3      3      3      3

sapply(createMultiFolds(df$Type, k = 5), \(x) table(df$Type[x]))
#   Fold1.Rep1 Fold2.Rep1 Fold3.Rep1 Fold4.Rep1 Fold5.Rep1 Fold1.Rep2 Fold2.Rep2 Fold3.Rep2 Fold4.Rep2 Fold5.Rep2
# 1         56         56         56         56         56         56         56         56         56         56
# 2         61         61         61         60         61         61         61         61         61         60
# 3         13         14         14         14         13         13         14         14         13         14
# 5         10         10         11         10         11         10         10         10         11         11
# 6          8          7          7          7          7          7          7          8          7          7
# 7         23         23         23         24         23         23         23         24         23         23
#   Fold1.Rep3 Fold2.Rep3 Fold3.Rep3 Fold4.Rep3 Fold5.Rep3 Fold1.Rep4 Fold2.Rep4 Fold3.Rep4 Fold4.Rep4 Fold5.Rep4
# 1         56         56         56         56         56         56         56         56         56         56
# 2         60         61         61         61         61         61         61         60         61         61
# 3         13         14         13         14         14         13         14         14         14         13
# 5         11         10         10         11         10         11         10         10         10         11
# 6          7          7          7          7          8          7          7          7          8          7
# 7         23         23         23         23         24         23         23         23         23         24
#   Fold1.Rep5 Fold2.Rep5 Fold3.Rep5 Fold4.Rep5 Fold5.Rep5
# 1         56         56         56         56         56
# 2         61         61         61         60         61
# 3         14         13         14         14         13
# 5         11         10         10         11         10
# 6          7          7          7          7          8
# 7         23         23         23         24         23

# Build model using cross-validation (and repeated cv)
trainControl <-
  trainControl(method = "cv",
               number = 10)
metric <- "Accuracy"
fit.cv.knn <- train(
  Type ~ .,
  data = train.data.df,
  method = "knn",
  metric = metric ,
  trControl = trainControl
)

pred <- predict(fit.cv.knn, test.data.df %>% select(!Type))

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.57500        0.35667        0.40890        0.72957        0.47500        0.13392            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.58824     0.82609        0.71429        0.73077   0.71429 0.58824 0.64516      0.425
# Class: 2     0.47368     0.71429        0.60000        0.60000   0.60000 0.47368 0.52941      0.475
# Class: 3          NA     0.92500             NA             NA   0.00000      NA      NA      0.000
# Class: 5          NA     0.95000             NA             NA   0.00000      NA      NA      0.000
# Class: 6          NA     0.97500             NA             NA   0.00000      NA      NA      0.000
# Class: 7     1.00000     0.97222        0.80000        1.00000   0.80000 1.00000 0.88889      0.100
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1          0.250                0.350           0.70716
# Class: 2          0.225                0.375           0.59398
# Class: 3          0.000                0.075                NA
# Class: 5          0.000                0.050                NA
# Class: 6          0.000                0.025                NA
# Class: 7          0.100                0.125           0.98611

trainControl <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 5)
metric <- "Accuracy"
fit.cv.knn <- train(
  Type ~ .,
  data = train.data.df,
  method = "knn",
  metric = metric ,
  trControl = trainControl
)

pred <- predict(fit.cv.knn, test.data.df %>% select(!Type))

cm <- confusionMatrix(test.data.df$Type, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#  0.60000        0.39451        0.43327        0.75135        0.47500        0.07707            NaN

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 1     0.58824     0.82609        0.71429        0.73077   0.71429 0.58824 0.64516      0.425
# Class: 2     0.52632     0.76190        0.66667        0.64000   0.66667 0.52632 0.58824      0.475
# Class: 3          NA     0.92500             NA             NA   0.00000      NA      NA      0.000
# Class: 5          NA     0.95000             NA             NA   0.00000      NA      NA      0.000
# Class: 6          NA     0.97500             NA             NA   0.00000      NA      NA      0.000
# Class: 7     1.00000     0.97222        0.80000        1.00000   0.80000 1.00000 0.88889      0.100
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 1           0.25                0.350           0.70716
# Class: 2           0.25                0.375           0.64411
# Class: 3           0.00                0.075                NA
# Class: 5           0.00                0.050                NA
# Class: 6           0.00                0.025                NA
# Class: 7           0.10                0.125           0.98611
