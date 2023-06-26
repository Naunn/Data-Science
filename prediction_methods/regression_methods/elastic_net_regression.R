# ELASTIC NET REGRESSION ===============================================================================================
# In statistics and, in particular, in the fitting of linear or logistic regression models, the elastic net is a
# regularized regression method that linearly combines the L1 and L2 penalties of the lasso and ridge methods.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(caret)
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

credit_prep <-
  credit %>%
  mutate_if(is.character, ~ factor(., ordered = FALSE)) %>%
  data.table::as.data.table() %>%
  mltools::one_hot()

credit_prep %>% str()
# Classes ‘data.table’ and 'data.frame':	400 obs. of  16 variables:
# $ Income                    : num  14.9 106 104.6 148.9 55.9 ...
# $ Limit                     : num  3606 6645 7075 9504 4897 ...
# $ Rating                    : num  283 483 514 681 357 569 259 512 266 491 ...
# $ Cards                     : num  2 3 4 3 2 4 2 2 5 3 ...
# $ Age                       : num  34 82 71 36 68 77 37 87 66 41 ...
# $ Education                 : num  11 15 11 11 16 10 12 9 13 19 ...
# $ Gender_Female             : int  0 1 0 1 0 0 1 0 1 1 ...
# $ Gender_Male               : int  1 0 1 0 1 1 0 1 0 0 ...
# $ Student_No                : int  1 0 1 1 1 1 1 1 1 0 ...
# $ Student_Yes               : int  0 1 0 0 0 0 0 0 0 1 ...
# $ Married_No                : int  0 0 1 1 0 1 1 1 1 0 ...
# $ Married_Yes               : int  1 1 0 0 1 0 0 0 0 1 ...
# $ Ethnicity_African American: int  0 0 0 0 0 0 1 0 0 1 ...
# $ Ethnicity_Asian           : int  0 1 1 1 0 0 0 1 0 0 ...
# $ Ethnicity_Caucasian       : int  1 0 0 0 1 1 0 0 1 0 ...
# $ Balance                   : num  333 903 580 964 331 ...
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
# Set training control
train_cont <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  search = "random",
  verboseIter = TRUE
)

# Train the model
elastic_reg <- train(
  Balance ~ .,
  data = cbind(X_train, y_train),
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneLength = 25,
  trControl = train_cont
)
summary(elastic_reg)

# visualization of RMSE and R squared
lambdas <-
  ggplot() +
  geom_line(aes(x = seq(1, length(
    elastic_reg$results$lambda
  )), y = elastic_reg$results$lambda)) +
  geom_line(aes(x = seq(1, length(
    elastic_reg$results$alpha
  )), y = elastic_reg$results$alpha), col = 'blue')
RMSE <-
  ggplot() +
  geom_line(aes(x = seq(1, length(
    elastic_reg$results$RMSE
  )), y = elastic_reg$results$RMSE)) +
  geom_line(aes(x = seq(
    1, length(elastic_reg$results$Rsquared)
  ), y = elastic_reg$results$Rsquared), col = 'blue')
RMSE + lambdas

# Checking the best model parameters
elastic_reg$bestTune
#    alpha      lambda
# 0.206034 0.001804582

# Prediction
pred <- predict(elastic_reg, X_test)

ggplot() +
  geom_line(aes(x = seq(1, length(y_test)), y = y_test), col = "blue", size =
              1.) +
  geom_line(aes(x = seq(1, length(pred)), y = pred))
