# Naive Bayes classifier ===============================================================================================
# In statistics, naive Bayes classifiers are a family of simple "probabilistic classifiers" based on applying Bayes'
# theorem with strong (naive) independence assumptions between the features (see Bayes classifier). They are among the
# simplest Bayesian network models, but coupled with kernel density estimation, they can achieve high accuracy levels.
# Naive Bayes classifiers are highly scalable, requiring a number of parameters linear in the number of variables
# (features/predictors) in a learning problem. Maximum-likelihood training can be done by evaluating a closed-form
# expression, which takes linear time, rather than by expensive iterative approximation as used for many other types of classifiers.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(caret)
library(e1071) # alternatively library(naivebayes) which is simpler
library(ggplot2)
# Data =================================================================================================================
data(iris)

iris %>% str()
# 'data.frame':	150 obs. of  5 variables:
# $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

# Split the data into training (80%) and test set (20%)
training.samples <- iris$Species %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- iris[training.samples,]
test.data <- iris[-training.samples,]

# Estimate preprocessing parameters (categorical variables are automatically ignored)
preproc.param <- train.data %>%
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

ggplot(train.transformed, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species))

# Model ================================================================================================================
# Fitting Naive Bayes Model to training dataset
nb <- naiveBayes(Species ~ ., data = train.transformed)
nb
# Naive Bayes Classifier for Discrete Predictors
#
# Call:
#   naiveBayes.default(x = X, y = Y, laplace = laplace)
#
# A-priori probabilities:
#   Y
# setosa versicolor  virginica
# 0.3333333  0.3333333  0.3333333
#
# Conditional probabilities:
#   Sepal.Length
# Y                  [,1]      [,2]
# setosa     -1.0294340 0.4519031
# versicolor  0.1972568 0.6618256
# virginica   0.8321771 0.7562816
#
# Sepal.Width
# Y                  [,1]      [,2]
# setosa      0.8482457 0.8395032
# versicolor -0.6317968 0.7314772
# virginica  -0.2164489 0.7848782
#
# Petal.Length
# Y                  [,1]       [,2]
# setosa     -1.3107241 0.09740682
# versicolor  0.3160375 0.27364023
# virginica   0.9946865 0.29606562
#
# Petal.Width
# Y                  [,1]      [,2]
# setosa     -1.2551054 0.1437688
# versicolor  0.1783742 0.2627586
# virginica   1.0767312 0.3523899

# Predicting on test data'
y_pred <- predict(nb, newdata = test.data[, 1:4])
y_pred

# Confusion Matrix
cm <- table(test.data$Species, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)
