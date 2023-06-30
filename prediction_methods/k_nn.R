# K-NEAREST NEIGHBORS ALGORITHM ========================================================================================
# In statistics, the k-nearest neighbors algorithm (k-NN) is a non-parametric supervised learning method first developed
# by Evelyn Fix and Joseph Hodges in 1951, and later expanded by Thomas Cover. It is used for classification and regression.
# In both cases, the input consists of the k closest training examples in a data set. The output depends on whether k-NN
# is used for classification or regression:
# - In k-NN classification, the output is a class membership. An object is classified by a plurality vote of its neighbors,
#   with the object being assigned to the class most common among its k nearest neighbors (k is a positive integer,
#   typically small). If k = 1, then the object is simply assigned to the class of that single nearest neighbor.
# - In k-NN regression, the output is the property value for the object. This value is the average of the values of k
#   nearest neighbors. If k = 1, then the output is simply assigned to the value of that single nearest neighbor.
# k-NN is a type of classification where the function is only approximated locally and all computation is deferred until
# function evaluation. Since this algorithm relies on distance for classification, if the features represent different
# physical units or come in vastly different scales then normalizing the training data can improve its accuracy dramatically.

# Both for classification and regression, a useful technique can be to assign weights to the contributions of the
# neighbors, so that the nearer neighbors contribute more to the average than the more distant ones. For example,
# a common weighting scheme consists in giving each neighbor a weight of 1/d, where d is the distance to the neighbor.

# The neighbors are taken from a set of objects for which the class (for k-NN classification) or the object property
# value (for k-NN regression) is known. This can be thought of as the training set for the algorithm, though no explicit
# training step is required.

# A peculiarity of the k-NN algorithm is that it is sensitive to the local structure of the data.

# Assumptions: KNN doesn’t make any assumptions about the data, meaning it can be used for a wide variety of problems.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(class)
library(forecast)
library(Metrics)
library(ggplot2)

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

map(Glass %>% select(!Type), shapiro.test) # all variables dont have normal distribiution

# Split the data into training (80%) and test set (20%)
training.samples <- Glass$Type %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- Glass[training.samples,]
test.data <- Glass[-training.samples,]

# Preprocessing ========================================================================================================
df <-
  Glass %>%
  mutate_if(is.numeric, scale) %>%
  mutate_if(is.matrix, as.vector)

df %>% psych::describe()
#       vars   n mean   sd median trimmed  mad   min  max range  skew kurtosis   se
# RI       1 214 0.00 1.00  -0.23   -0.12 0.62 -2.38 5.13  7.50  1.60     4.72 0.07
# Na       2 214 0.00 1.00  -0.13   -0.04 0.79 -3.28 4.86  8.14  0.45     2.90 0.07
# Mg       3 214 0.00 1.00   0.55    0.13 0.21 -1.86 1.25  3.11 -1.14    -0.45 0.07
# Al       4 214 0.00 1.00  -0.17   -0.07 0.62 -2.31 4.12  6.43  0.89     1.94 0.07
# Si       5 214 0.00 1.00   0.18    0.07 0.74 -3.67 3.56  7.23 -0.72     2.82 0.07
# K        6 214 0.00 1.00   0.09   -0.10 0.26 -0.76 8.76  9.52  6.46    52.87 0.07
# Ca       7 214 0.00 1.00  -0.25   -0.15 0.46 -2.48 5.08  7.56  2.02     6.41 0.07
# Ba       8 214 0.00 1.00  -0.35   -0.28 0.00 -0.35 5.98  6.34  3.37    12.08 0.07
# Fe       9 214 0.00 1.00  -0.59   -0.22 0.00 -0.59 4.65  5.23  1.73     2.52 0.07
# Type*   10 214 2.54 1.71   2.00    2.31 1.48  1.00 6.00  5.00  1.04    -0.29 0.12

GGally::ggpairs(df, aes(colour = Type))

# Split the data into training (80%) and test set (20%)
training.samples.df <- df$Type %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.df <- df[training.samples.df,]
test.data.df <- df[-training.samples.df,]

# Classification =======================================================================================================
# Build model using class::knn()
pred <-
  knn(
    train = train.data %>% select(!Type),
    test = test.data %>% select(!Type),
    cl = train.data$Type,
    k = 3
  )
confusionMatrix(test.data$Type, pred)

pred.df <-
  knn(
    train = train.data.df %>% select(!Type),
    test = test.data.df %>% select(!Type),
    cl = train.data.df$Type,
    k = 3
  )
confusionMatrix(test.data.df$Type, pred.df)
# Even with not the best number of neighbors, the normalized data model has better performance

# Build model using caret::train()
# Run algorithms using 10-fold cross validation
trainControl <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 3)
metric <- "Accuracy"
fit.cv.knn <- train(
  Type ~ .,
  data = train.data,
  method = "knn",
  metric = metric ,
  trControl = trainControl
)
fit.cv.knn
# k-Nearest Neighbors
#
# 174 samples
#   9 predictor
#   6 classes: '1', '2', '3', '5', '6', '7'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times)
# Summary of sample sizes: 156, 158, 155, 156, 156, 156, ...
# Resampling results across tuning parameters:
#
#   k  Accuracy   Kappa
#   5  0.6648036  0.5455893
#   7  0.6432182  0.5138322
#   9  0.6263281  0.4864443
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 5.

fit.cv.knn.df <- train(
  Type ~ .,
  data = train.data.df,
  method = "knn",
  metric = metric ,
  trControl = trainControl
)
fit.cv.knn.df$bestTune[1, 1] # 5

# Run algorithms using grid search method
grid <-
  expand.grid(.k = seq(2, floor(nrow(train.data) / 10), by = 1))
metric <- "Accuracy"
fit.grid.knn <- train(
  Type ~ .,
  data = train.data,
  method = "knn",
  metric = metric,
  tuneGrid = grid,
  trControl = trainControl
)
fit.grid.knn
# k-Nearest Neighbors
#
# 174 samples
#   9 predictor
#   6 classes: '1', '2', '3', '5', '6', '7'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times)
# Summary of sample sizes: 157, 157, 156, 157, 156, 157, ...
# Resampling results across tuning parameters:
#
#   k   Accuracy   Kappa
#   2  0.6651251  0.5445201
#   3  0.6577923  0.5308337
#   4  0.6553076  0.5291767
#   5  0.6734083  0.5550763
#   6  0.6535145  0.5293640
#   7  0.6253985  0.4915259
#   8  0.6481768  0.5176577
#   9  0.6355091  0.4992932
#   10  0.6249742  0.4836941
#   11  0.6206986  0.4740718
#   12  0.6049715  0.4507527
#   13  0.6135836  0.4601406
#   14  0.6093918  0.4560238
#   15  0.6171124  0.4656649
#   16  0.6174779  0.4638256
#   17  0.6196473  0.4665777
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 5.

fit.grid.knn.df <- train(
  Type ~ .,
  data = train.data.df,
  method = "knn",
  metric = metric,
  tuneGrid = grid,
  trControl = trainControl
)
fit.grid.knn.df$bestTune[1, 1] # 7

# Summary
pred <-
  knn3Train(
    train = train.data.df %>% select(!Type),
    test = test.data.df %>% select(!Type),
    cl = train.data.df$Type,
    k = 5,
    prob = FALSE
  ) %>%
  factor()
# pred <- predict(fit.cv.knn.df, newdata = test.data.df %>% select(!Type))
confusionMatrix(test.data.df$Type, pred)

# Regression ===========================================================================================================
df_reg <-
  df %>%
  mutate(Type = as.numeric(Type))

# Split the data into training (80%) and test set (20%)
training.samples.df.reg <- df_reg$Type %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.df.reg <- df_reg[training.samples.df.reg,]
test.data.df.reg <- df_reg[-training.samples.df.reg,]

pred <-
  knnregTrain(
    train = train.data.df.reg %>% select(!Type),
    test = test.data.df.reg %>% select(!Type),
    y = train.data.df.reg$Type,
    k = 5
  )
rmse(test.data.df.reg$Type, pred) # 0.9309493
