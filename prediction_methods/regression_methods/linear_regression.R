# LINEAR REGRESSION ====================================================================================================
# In statistics, linear regression is a linear approach for modelling the relationship between a scalar response and one
# or more explanatory variables (also known as dependent and independent variables). The case of one explanatory variable
# is called simple linear regression; for more than one, the process is called multiple linear regression.
# This term is distinct from multivariate linear regression, where multiple correlated dependent variables are predicted,
# rather than a single scalar variable.

# For choosing the unknown parameters in a linear regression model we use ordinary least squares (OLS) method.

# Assumptions
# Linear regression is an analysis that assesses whether one or more predictor variables explain the dependent 
# (criterion) variable.  The regression has five key assumptions:
# - Linear relationship
# - Multivariate normality
# - No or little multicollinearity
# - No auto-correlation
# - Homoscedasticity
# A note about sample size. In Linear regression the sample size rule of thumb is that the regression analysis requires 
# at least 20 cases per independent variable in the analysis.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# Data =================================================================================================================
marketing <-
  readr::read_csv("./data/Marketing_Data.csv") %>%
  as.data.frame()
# TV - is the amount of marketing dollars (in thousands) spent on TV advertising
# Internet - is the amount of marketing dollars (in thousands) spent on online advertising
# Mailing - is the amount of marketing dollars (in thousands) spend on advertising via mail
# Members - is the number of people who have signed up (in thousands) for health insurance given the various marketing campaigns
# Region - is the region of the United States that each marketing campaign was focused on

marketing %>% glimpse()
# Rows: 200
# Columns: 5
# $ TV       <dbl> 460.2, 89.0, 34.4, 303.0, …
# $ Internet <dbl> 75.6, 78.6, 91.8, 82.6, 21…
# $ Mailing  <dbl> 138.4, 90.2, 138.6, 117.0,…
# $ Members  <dbl> 44.2, 20.8, 18.6, 37.0, 25…
# $ Region   <chr> "Central", "South", "Centr…
marketing$Region %>% table()
# Central    East   South    West
#      59      44      51      46

marketing %>% is.na() %>% colSums()
# TV Internet  Mailing  Members   Region
# 0        0        0        0        0

marketing %>% psych::describe()

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

# Simple linear regression =============================================================================================
model <- lm(Members ~ TV, marketing)
summary(model)
# Call:
#   lm(formula = Members ~ TV, data = marketing)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -16.7720  -3.9090  -0.3825   4.1342  14.4247
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) 14.065187   0.915686   15.36   <2e-16 ***
# TV           0.047537   0.002691   17.67   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6.517 on 198 degrees of freedom
# Multiple R-squared:  0.6119,	Adjusted R-squared:  0.6099
# F-statistic: 312.1 on 1 and 198 DF,  p-value: < 2.2e-16


# T-test are made next to all variables, where *** are the info about p-values.
# As the p-value is much less than 0.05, we reject the null hypothesis that β = 0.
# Hence there is a significant relationship between the variables in the linear regression model of the data set faithful.

# Degrees of freedom are the maximum number of logically independent values, which may vary in a data sample.
model$df.residual # 198

model$coefficients[[1]] # 14.06519 - intercept Beta_0
model$coefficients[[2]] # 0.04753664 - coefficient Beta_1

# Adjusted r^2 is better, as it takes into account the number of degrees of freedom
summary(model)$adj.r.squared # 0.6099148

# visualization
x_axis <- "TV"
ggplot(marketing, aes(x = get(as.name(x_axis)), y = Members)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x,
    se = FALSE,
    color = "blue"
  )

# Multiple linear regression ===========================================================================================
model <- lm(Members ~ TV + Internet + Mailing, marketing)
summary(model)
# Call:
#   lm(formula = Members ~ TV + Internet + Mailing, data = marketing)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -17.6554  -1.7816   0.4836   2.3786   5.6584
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  5.877779   0.623816   9.422   <2e-16 ***
#   TV           0.045765   0.001395  32.809   <2e-16 ***
#   Internet     0.188530   0.008611  21.893   <2e-16 ***
#   Mailing     -0.001037   0.005871  -0.177     0.86
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 3.371 on 196 degrees of freedom
# Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8956
# F-statistic: 570.3 on 3 and 196 DF,  p-value: < 2.2e-16

# Let's leave out the 'Mailing' column, as it is irrelevant to the model.
model <- lm(Members ~ TV + Internet, marketing)
summary(model)
# Call:
#   lm(formula = Members ~ TV + Internet, data = marketing)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -17.5954  -1.7503   0.4844   2.3415   5.6657
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  5.84220    0.58898   9.919   <2e-16 ***
#   TV           0.04575    0.00139  32.909   <2e-16 ***
#   Internet     0.18799    0.00804  23.382   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 3.363 on 197 degrees of freedom
# Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8962
# F-statistic: 859.6 on 2 and 197 DF,  p-value: < 2.2e-16

model$df.residual # 197

model$coefficients[[1]] # 5.8422 - intercept Beta_0
model$coefficients[[2]] # 0.04575482 - coefficient Beta_1

# Adjusted r^2 is better, as it takes into account the number of degrees of freedom
summary(model)$adj.r.squared # 0.8961505

# visualization
x_axis <- "Internet"
ggplot(marketing, aes(x = get(as.name(x_axis)), y = Members)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x,
    se = FALSE,
    color = "blue"
  )

# Linear regression with quantitative variables ========================================================================
# One hot encoding -> When only 2 levels
model <- lm(Balance ~ ., credit_prep)
summary(model)
# Call:
#   lm(formula = Balance ~ ., data = credit_prep)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -161.64  -77.70  -13.49   53.98  318.20
#
# Coefficients: (4 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)                   -51.88739   39.14828  -1.325   0.1858
# Income                         -7.80310    0.23423 -33.314  < 2e-16 ***
#   Limit                           0.19091    0.03278   5.824 1.21e-08 ***
#   Rating                          1.13653    0.49089   2.315   0.0211 *
#   Cards                          17.72448    4.34103   4.083 5.40e-05 ***
#   Age                            -0.61391    0.29399  -2.088   0.0374 *
#   Education                      -1.09886    1.59795  -0.688   0.4921
# Gender_Female                 -10.65325    9.91400  -1.075   0.2832
# Gender_Male                          NA         NA      NA       NA
# Student_No                   -425.74736   16.72258 -25.459  < 2e-16 ***
#   Student_Yes                          NA         NA      NA       NA
# Married_No                      8.53390   10.36287   0.824   0.4107
# Married_Yes                          NA         NA      NA       NA
# `Ethnicity_African American`  -10.10703   12.20992  -0.828   0.4083
# Ethnicity_Asian                 6.69715   12.12244   0.552   0.5810
# Ethnicity_Caucasian                  NA         NA      NA       NA
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 98.79 on 388 degrees of freedom
# Multiple R-squared:  0.9551,	Adjusted R-squared:  0.9538
# F-statistic: 750.3 on 11 and 388 DF,  p-value: < 2.2e-16

# This suggests that these variables are perfectly or nearly perfectly collinear with other variables in the model, leading to singularity.

# Stepwise Regression: The step() function in R performs stepwise regression, which sequentially adds or removes
# predictors from the model based on certain criteria (e.g., AIC, BIC). It automatically searches through different
# combinations of predictors to find the best-fitting model.
# Note. This approach do not consider multiplication combinations of predictor variables.
model_step <- step(model)
summary(model_step)
# Call:
#   lm(formula = Balance ~ Income + Limit + Rating + Cards + Age +
#        Student_No, data = credit_prep)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -170.00  -77.85  -11.84   56.87  313.52
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -68.12425   28.84442  -2.362   0.0187 *
#   Income        -7.79508    0.23342 -33.395  < 2e-16 ***
#   Limit          0.19369    0.03238   5.981 4.98e-09 ***
#   Rating         1.09119    0.48480   2.251   0.0250 *
#   Cards         18.21190    4.31865   4.217 3.08e-05 ***
#   Age           -0.62406    0.29182  -2.139   0.0331 *
#   Student_No  -425.60994   16.50956 -25.780  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 98.61 on 393 degrees of freedom
# Multiple R-squared:  0.9547,	Adjusted R-squared:  0.954
# F-statistic:  1380 on 6 and 393 DF,  p-value: < 2.2e-16

model_ols_step <-
  olsrr::ols_step_all_possible(lm(Balance ~ ., credit)) # Test all (additive) combinations
model_ols_step

# Visualization
x_axis <- "Income"
ggplot(credit, aes(x = get(as.name(x_axis)), y = Balance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x,
    se = FALSE,
    color = "blue"
  )

# Testing regression assumtions
# Ideally, a plot of the residuals versus the predicted values will show no discernible pattern. A model that deviates
# from the linearity assumption will show kinks or curves in this chart. In order to address this situation, you will
# need to apply transformations to some or all of the variables or add polynomial terms.
plot(model_step, which = 1)

# Another key set of assumptions that must be met for a linear regression model to be valid center around the residuals.
plot(model_step, which = 2)

# Check for autocorrelation
res <- residuals(model_step)
acf(res)

# Another important assumption is that there is homoskedasticity in the errors. Homoskedasticity simply means that the
# variance of the residuals is constant. For example, the variances of the residuals may increase with the value of the
# response - this would not be desirable. You may be able to detect the presence of non-constant variance (heteroskedasticity)
# of the error terms by the presence of a funnel shape in the residual vs fitted plot. Heteroskedasticity will manifest
# as large kinks or curves in the red line or narrower/wider range of points in one or more regions. If both charts do
# not show a straight line, then the problem may simply be that one or more model terms need to be transformed.
plot(model_step, which = 3)

plot(model_step, which = 4)
plot(model_step, which = 5)
plot(model_step, which = 6)
