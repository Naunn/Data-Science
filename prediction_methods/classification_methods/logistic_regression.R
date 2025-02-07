# LOGISTIC REGRESSION ==================================================================================================
# In statistics, the logistic model (or logit model) is a statistical model that models the probability of an event
# taking place by having the log-odds for the event be a linear combination of one or more independent variables.
# In regression analysis, logistic regression (or logit regression) is estimating the parameters of a logistic model
# (the coefficients in the linear combination).

# Assumptions
# - First, binary logistic regression requires the dependent variable to be binary and ordinal logistic regression 
#   requires the dependent variable to be ordinal.
# - Second, logistic regression requires the observations to be independent of each other.  In other words, 
#   the observations should not come from repeated measurements or matched data.
# - Third, logistic regression requires there to be little or no multicollinearity among the independent variables.
#   This means that the independent variables should not be too highly correlated with each other.
# - Fourth, logistic regression assumes linearity of independent variables and log odds.  although this analysis does 
#   not require the dependent and independent variables to be related linearly, it requires that the independent 
#   variables are linearly related to the log odds.
# - Finally, logistic regression typically requires a large sample size.  A general guideline is that you need at 
#   minimum of 10 cases with the least frequent outcome for each independent variable in your model.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# Data =================================================================================================================
data("PimaIndiansDiabetes2", package = "mlbench")

PimaIndiansDiabetes2 %>% str()
# 'data.frame':	768 obs. of  9 variables:
# $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
# $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
# $ pressure: num  72 66 64 66 40 74 50 NA 70 96 ...
# $ triceps : num  35 29 NA 23 35 NA 32 NA 45 NA ...
# $ insulin : num  NA NA NA 94 168 NA 88 NA 543 NA ...
# $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 NA ...
# $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
# $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
# $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...

PimaIndiansDiabetes2 %>% is.na() %>% colSums()
# pregnant  glucose pressure  triceps  insulin     mass pedigree      age diabetes
#       0        5       35      227      374       11        0        0        0

df <- PimaIndiansDiabetes2 %>% drop_na()

traning_samples <-
  df$diabetes %>%
  createDataPartition(p = 0.8, list = FALSE)

df_train <- df[traning_samples, ]
df_test <- df[-traning_samples, ]

# Simple logistic regression ===========================================================================================
# Fit the model
model <- glm(diabetes ~ glucose, data = df_train, family = binomial)
summary(model)
# Call:
#   glm(formula = diabetes ~ glucose, family = binomial, data = df_train)
#
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)
#   (Intercept) -6.500356   0.732845  -8.870   <2e-16 ***
#   glucose      0.045477   0.005536   8.214   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 398.80  on 313  degrees of freedom
# Residual deviance: 300.27  on 312  degrees of freedom
# AIC: 304.27
#
# Number of Fisher Scoring iterations: 4

# Prediction and testing - use the option type = “response” to directly obtain the probabilities
probabilities <-
  model %>% predict(df_test %>% select(glucose), type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
(df_test$diabetes == predicted.classes) %>% table()
# .
# FALSE  TRUE
#    17    61

# (auto) visualization
df_train %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial")) +
  labs(title = "Logistic Regression Model",
       x = "Plasma Glucose Concentration",
       y = "Probability of being diabete-pos")
# The curve shows the probability of being diabetic versus glucose level.

# Multiple logistic regression =========================================================================================
best_model <-
  glm(diabetes ~ ., data = df_train, family = binomial) %>% step()
summary(best_model)
# Call:
#   glm(formula = diabetes ~ glucose + mass + pedigree + age, family = binomial,
#       data = df_train)
#
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)
#   (Intercept) -9.874326   1.198556  -8.239  < 2e-16 ***
#   glucose      0.036344   0.005634   6.450 1.12e-10 ***
#   mass         0.060795   0.022705   2.678 0.007415 **
#   pedigree     1.251908   0.471776   2.654 0.007964 **
#   age          0.056244   0.015319   3.672 0.000241 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 398.80  on 313  degrees of freedom
# Residual deviance: 277.75  on 309  degrees of freedom
# AIC: 287.75
#
# Number of Fisher Scoring iterations: 5

# Estimate:  the intercept (b0) and the beta coefficient estimates associated to each predictor variable
# Std.Error: the standard error of the coefficient estimates. This represents the accuracy of the coefficients.
#            The larger the standard error, the less confident we are about the estimate.
# z value:   the z-statistic, which is the coefficient estimate (column 2) divided by the standard error of the estimate (column 3).
# Pr(>|z|):  The p-value corresponding to the z-statistic. The smaller the p-value, the more significant the estimate is.

# For example, the regression coefficient for glucose is 0.036344. This indicate that one unit increase in the glucose
# concentration will increase the odds of being diabetes-positive by exp(0.036344) 1.037013 times.

# Prediction and testing - use the option type = “response” to directly obtain the probabilities
probabilities <-
  best_model %>% predict(df_test %>% select(names(best_model$coefficients)[-1]), type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
(df_test$diabetes == predicted.classes) %>% table()
# .
# FALSE  TRUE
#    15    63
