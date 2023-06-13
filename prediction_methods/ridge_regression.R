# RIDGE REGRESSION =====================================================================================================
# Ridge regression is a method of estimating the coefficients of multiple-regression models in scenarios where the
# independent variables are highly correlated. It has been used in many fields including econometrics, chemistry, and
# engineering.. Also known as Tikhonov regularization, named for Andrey Tikhonov, it is a method of regularization of
# ill-posed problems. It is particularly useful to mitigate the problem of multicollinearity in linear regression,
# which commonly occurs in models with large numbers of parameters. In general, the method provides improved efficiency
# in parameter estimation problems in exchange for a tolerable amount of bias.

# The mathematical term well-posed problem stems from a definition given by 20th-century French mathematician Jacques
# Hadamard. He believed that mathematical models of physical phenomena should have the properties that:
#   - a solution exists,
#   - the solution is unique,
#   - the solution's behaviour changes continuously with the initial conditions.
# Problems that are not well-posed in the sense of Hadamard are termed ill-posed.

# Ridge regression was developed as a possible solution to the imprecision of least square estimators when linear
# regression models have some multicollinear (highly correlated) independent variables—by creating a ridge regression
# estimator (RR). This provides a more precise ridge parameters estimate, as its variance and mean square estimator are
# often smaller than the least square estimators previously derived.

# Ridge regression’s advantage over least squares is rooted in the bias-variance trade-off. As λ increases, the
# flexibility of the ridge regression fit decreases, leading to decreased variance but increased bias.
#   - If λ = 0, the output is similar to simple linear regression.
#   - If λ = very large, the coefficients will become zero.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(rsample)
library(ggplot2)
library(patchwork)

# Data =================================================================================================================
credit <-
  readr::read_csv("./data/Credit.csv") %>%
  as.data.frame() %>%
  select(!1)

credit %>% str()
# 'data.frame':	400 obs. of  11 variables:
# $ Income   : num  14.9 106 104.6 148.9 55.9 ...
# $ Limit    : num  3606 6645 7075 9504 4897 ...
# $ Rating   : num  283 483 514 681 357 569 259 512 266 491 ...
# $ Cards    : num  2 3 4 3 2 4 2 2 5 3 ...
# $ Age      : num  34 82 71 36 68 77 37 87 66 41 ...
# $ Education: num  11 15 11 11 16 10 12 9 13 19 ...
# $ Gender   : chr  "Male" "Female" "Male" "Female" ...
# $ Student  : chr  "No" "Yes" "No" "No" ...
# $ Married  : chr  "Yes" "Yes" "No" "No" ...
# $ Ethnicity: chr  "Caucasian" "Asian" "Asian" "Asian" ...
# $ Balance  : num  333 903 580 964 331 ... # (average credit card debt for a number of individuals)

credit %>% select_if(is.character) %>% table()

min_max <- function(x) {
  return((x - min(x, na.rm = TRUE)) /
           (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

credit_prep <-
  credit %>%
  mutate_if(is.character, ~ factor(., ordered = FALSE)) %>%
  data.table::as.data.table() %>%
  mltools::one_hot() %>% 
  mutate_if(is.numeric, min_max)

credit_prep %>% str()
# Classes ‘data.table’ and 'data.frame':	400 obs. of  16 variables:
# $ Income                    : num  0.0257 0.5427 0.5346 0.7861 0.2583 ...
# $ Limit                     : num  0.211 0.443 0.476 0.662 0.31 ...
# $ Rating                    : num  0.214 0.439 0.474 0.661 0.297 ...
# $ Cards                     : num  0.125 0.25 0.375 0.25 0.125 0.375 0.125 0.125 0.5 0.25 ...
# $ Age                       : num  0.147 0.787 0.64 0.173 0.6 ...
# $ Education                 : num  0.4 0.667 0.4 0.4 0.733 ...
# $ Gender_Female             : num  0 1 0 1 0 0 1 0 1 1 ...
# $ Gender_Male               : num  1 0 1 0 1 1 0 1 0 0 ...
# $ Student_No                : num  1 0 1 1 1 1 1 1 1 0 ...
# $ Student_Yes               : num  0 1 0 0 0 0 0 0 0 1 ...
# $ Married_No                : num  0 0 1 1 0 1 1 1 1 0 ...
# $ Married_Yes               : num  1 1 0 0 1 0 0 0 0 1 ...
# $ Ethnicity_African American: num  0 0 0 0 0 0 1 0 0 1 ...
# $ Ethnicity_Asian           : num  0 1 1 1 0 0 0 1 0 0 ...
# $ Ethnicity_Caucasian       : num  1 0 0 0 1 1 0 0 1 0 ...
# $ Balance                   : num  0.167 0.452 0.29 0.482 0.166 ...
# - attr(*, ".internal.selfref")=<externalptr> 

# split into train and test
credit_split <- initial_split(credit_prep, prop = 0.8)

X_train <-
  credit_split %>%
  training() %>%
  select(!Balance) %>%
  data.matrix()

y_train <-
  credit_split %>%
  training() %>%
  select(Balance) %>%
  data.matrix()

X_test <-
  credit_split %>%
  testing() %>%
  select(!Balance) %>%
  data.matrix()

y_test <-
  credit_split %>%
  testing() %>%
  select(Balance) %>%
  data.matrix()

# The ridge regression model ===========================================================================================

# Setting the range of lambda values
# The glmnet function trains the model multiple times for all the different values of lambda, which we pass as a
# sequence of vector to the lambda = argument in the glmnet function.
lambda_seq <- 10 ^ seq(2, -2, by = -.1)

# traditional model
model <- glmnet(x = X_train, y = y_train, alpha = 0, lambda = lambda_seq)
summary(model)
coef(model)

# visualization of R square value
lambdas <-
  ggplot() +
  geom_line(aes(x = seq(1, length(lambda_seq)), y = lambda_seq))
r_squared <-
  ggplot() +
  geom_line(aes(x = seq(1, length(model$dev.ratio)), y = model$dev.ratio))
r_squared + lambdas

# Choosing Optimal Lambda Value using cross validation
model_cv <- cv.glmnet(x = X_train, y = y_train, alpha = 0, lambda = lambda_seq)

model_cv$glmnet.fit
model_cv$lambda.min # 0.01 - best lambda based on cross-validation

plot(model_cv)

# Build best model
best_model <-
  glmnet(x = X_train,
         y = y_train,
         alpha = 0,
         lambda = model_cv$lambda.min)
summary(best_model)
best_model$dev.ratio # 0.9494977
coef(best_model)

# Prediction
pred <- predict(best_model, s = model_cv$lambda.min, newx = X_test)

ggplot() +
  geom_line(aes(x = seq(1, length(y_test)), y = y_test), col = "blue", size =
              1.) +
  geom_line(aes(x = seq(1, length(pred)), y = pred))
