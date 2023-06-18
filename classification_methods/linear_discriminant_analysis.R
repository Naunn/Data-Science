# Discriminant analysis ================================================================================================
# Linear discriminant analysis (LDA), normal discriminant analysis (NDA), or discriminant function analysis is a
# generalization of Fisher's linear discriminant, a method used in statistics and other fields, to find a linear
# combination of features that characterizes or separates two or more classes of objects or events. The resulting
# combination may be used as a linear classifier, or, more commonly, for dimensionality reduction before later
# classification. LDA uses continuous independent variables and a categorical dependent variable (i.e. the class label).

# In statistics, a quadratic classifier is a statistical classifier that uses a quadratic decision surface to separate 
# measurements of two or more classes of objects or events. It is a more general version of the linear classifier.
# Quadratic discriminant analysis (QDA) is closely related to linear discriminant analysis (LDA), where it is assumed that 
# the measurements from each class are normally distributed. Unlike LDA however, in QDA there is no assumption that the 
# covariance of each of the classes is identical.

# DA is like PCA (dimensionality reduction), but it focuses on maximizing the separatibility among know categories.

# The assumptions of discriminant analysis are the same as those for MANOVA. The analysis is quite sensitive to outliers
# and the size of the smallest group must be larger than the number of predictor variables.
# - Multivariate normality: Independent variables are normal for each level of the grouping variable.
# - Homogeneity of variance/covariance (homoscedasticity): Variances among group variables are the same across levels
#   of predictors. Can be tested with Box's M statistic. It has been suggested, however, that linear discriminant analysis
#   be used when covariances are equal, and that quadratic discriminant analysis may be used when covariances are not equal.
# - Multicollinearity: Predictive power can decrease with an increased correlation between predictor variables.
# - Independence: Participants are assumed to be randomly sampled, and a participant's score on one variable is assumed
#   to be independent of scores on that variable for all other participants.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(caret)
library(MASS)
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

# Linear discriminant analysis (LDA) ===================================================================================
# Fit the model
model <- lda(Species ~ ., data = train.transformed)
model
# Call:
#   lda(Species ~ ., data = train.transformed)
#
# Prior probabilities of groups:
#    setosa versicolor  virginica
# 0.3333333  0.3333333  0.3333333
#
# Group means:
#            Sepal.Length Sepal.Width Petal.Length
# setosa       -1.0258296   0.9030345   -1.3129790
# versicolor    0.1829636  -0.6807491    0.3196818
# virginica     0.8428660  -0.2222854    0.9932972
#            Petal.Width
# setosa      -1.2628632
# versicolor   0.2031563
# virginica    1.0597070
#
# Coefficients of linear discriminants:
#                     LD1        LD2
# Sepal.Length  0.7509330 -0.3635934
# Sepal.Width   0.6715377  1.0997302
# Petal.Length -4.2777884 -0.9684037
# Petal.Width  -1.8738891  1.9311526
#
# Proportion of trace:
#   LD1    LD2
# 0.9913 0.0087
plot(model)
# LDA determines group means and computes, for each individual, the probability of belonging to the different groups.
# The individual is then affected to the group with the highest probability score.
#
# The lda() outputs contain the following elements:
# - Prior probabilities of groups: the proportion of training observations in each group. For example, there are 31%
#   of the training observations in the setosa group
# - Group means: group center of gravity. Shows the mean of each variable in each group.
# - Coefficients of linear discriminants: Shows the linear combination of predictor variables that are used to form
#   the LDA decision rule. for example, LD1 = 0.91*Sepal.Length + 0.64*Sepal.Width - 4.08*Petal.Length - 2.3*Petal.Width.
#   Similarly, LD2 = 0.03*Sepal.Length + 0.89*Sepal.Width - 2.2*Petal.Length - 2.6*Petal.Width.

# vizualization
ggord::ggord(model, train.transformed$Species)

# prediction
predictions <- model %>% predict(test.transformed)
# The predict() function returns the following elements:
# - class: predicted classes of observations.
# - posterior: is a matrix whose columns are the groups, rows are the individuals and values are the posterior probability
#   that the corresponding observation belongs to the groups.
# - x: contains the linear discriminants, described above

ggplot(as.data.frame(predictions$x), aes(LD1, LD2)) +
  geom_point(aes(color = predictions$class))

mean(predictions$class==test.transformed$Species) # 1

# Quadratic discriminant analysis (QDA) ================================================================================
# Fit the model
model <- qda(Species ~ ., data = train.transformed)
model
# Call:
#   qda(Species ~ ., data = train.transformed)
# 
# Prior probabilities of groups:
#    setosa versicolor  virginica 
# 0.3333333  0.3333333  0.3333333 
# 
# Group means:
#            Sepal.Length Sepal.Width Petal.Length
# setosa       -1.0258296   0.9030345   -1.3129790
# versicolor    0.1829636  -0.6807491    0.3196818
# virginica     0.8428660  -0.2222854    0.9932972
#            Petal.Width
# setosa      -1.2628632
# versicolor   0.2031563
# virginica    1.0597070

# prediction
predictions <- model %>% predict(test.transformed)
# The predict() function returns the following elements:
# - class: predicted classes of observations.
# - posterior: is a matrix whose columns are the groups, rows are the individuals and values are the posterior probability
#   that the corresponding observation belongs to the groups.

mean(predictions$class==test.transformed$Species) # 1









