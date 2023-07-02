# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(caret)
library(forecast)
library(GGally)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(e1071)
library(randomForest)

# Data =================================================================================================================
some_data <- read_excel("data/some_data.xls")
some_data %>% str()
# tibble [304 × 6] (S3: tbl_df/tbl/data.frame)
# $ DBS    : num [1:304] 0 0.08 0.06 0.1 0.08 0.09 0.1 0.15 0.2 0 ...
# $ UIUX   : num [1:304] 0 0.08 0.06 0.1 0.08 0.15 0.1 0.02 0.14 0 ...
# $ ALG    : num [1:304] 0 0.1 0.05 0.15 0.08 0.4 0.43 0.34 0.35 0.5 ...
# $ OOP    : num [1:304] 0 0.24 0.25 0.65 0.98 0.1 0.29 0.4 0.72 0.2 ...
# $ DAT    : num [1:304] 0 0.9 0.33 0.3 0.24 0.66 0.56 0.01 0.25 0.85 ...
# $ CHANCES: chr [1:304] "very_low" "High" "Low" "Middle" ...

some_data %>% is.na() %>% colSums()
# DBS    UIUX     ALG     OOP     DAT CHANCES
# 0       0       0       0       0       0

some_data$CHANCES %>% table()
# High      Low   Middle Very Low very_low
# 73      102      100        5       24

# Analysis =============================================================================================================
df <-
  some_data %>%
  mutate(CHANCES = factor(
    gsub("Very Low", "very_low", CHANCES),
    levels = c("High", "Middle", "Low", "very_low")
  ))

df$CHANCES %>% table()
# High  Middle     Low very_low
# 73      100      102       29

ggpairs(df, aes(colour = CHANCES)) # seems like OOP vs DAT can be linearly separable

# Since the nominal data is ordered, so we can switch from strings to integers
df <-
  df %>%
  mutate(CHANCES = case_when(
    CHANCES == "High" ~ 4,
    CHANCES == "Middle" ~ 3,
    CHANCES == "Low" ~ 2,
    .default = 1
  ))

df %>% psych::describe()
#         vars   n mean   sd median trimmed  mad min  max range  skew kurtosis   se
# DBS        1 304 0.40 0.22   0.38    0.39 0.21   0 0.99  0.99  0.38    -0.55 0.01
# UIUX       2 304 0.35 0.21   0.31    0.34 0.20   0 0.90  0.90  0.57    -0.45 0.01
# ALG        3 304 0.45 0.25   0.40    0.45 0.31   0 0.95  0.95  0.07    -1.19 0.01
# OOP        4 304 0.43 0.26   0.33    0.42 0.24   0 0.99  0.99  0.46    -0.92 0.01
# DAT        5 304 0.45 0.25   0.45    0.45 0.31   0 0.98  0.98  0.16    -1.20 0.01
# CHANCES    6 304 2.71 0.94   3.00    2.76 1.48   1 4.00  3.00 -0.10    -0.97 0.05

# the relationship between linearly related variables
df %>% cor(method = "pearson")
#                 DBS      UIUX         ALG         OOP        DAT   CHANCES
# DBS      1.00000000 0.0989106 -0.02358626  0.12071867  0.2228907 0.2161619
# UIUX     0.09891060 1.0000000  0.14312016  0.12616931  0.1720621 0.2616858
# ALG     -0.02358626 0.1431202  1.00000000  0.06229136  0.1121188 0.1758559
# OOP      0.12071867 0.1261693  0.06229136  1.00000000 -0.1831190 0.1406347
# DAT      0.22289070 0.1720621  0.11211881 -0.18311900  1.0000000 0.9072993
# CHANCES  0.21616188 0.2616858  0.17585588  0.14063471  0.9072993 1.0000000

# non-parametric test that measures the strength of dependence between two variables
df %>% cor(method = "kendall")
#                 DBS       UIUX         ALG         OOP         DAT    CHANCES
# DBS      1.00000000 0.06458423 -0.01004462  0.05298002  0.13789518 0.15422578
# UIUX     0.06458423 1.00000000  0.09186704  0.06724199  0.09872808 0.18608168
# ALG     -0.01004462 0.09186704  1.00000000  0.04017239  0.06741193 0.12960411
# OOP      0.05298002 0.06724199  0.04017239  1.00000000 -0.15881360 0.08807247
# DAT      0.13789518 0.09872808  0.06741193 -0.15881360  1.00000000 0.78159456
# CHANCES  0.15422578 0.18608168  0.12960411  0.08807247  0.78159456 1.00000000

# non-parametric test that is used to measure the degree of association between two variables
df %>% cor(method = "spearman")
#                 DBS       UIUX         ALG         OOP         DAT   CHANCES
# DBS      1.00000000 0.09243724 -0.01594068  0.07901533  0.19033995 0.1913125
# UIUX     0.09243724 1.00000000  0.13530726  0.09974626  0.14146314 0.2403268
# ALG     -0.01594068 0.13530726  1.00000000  0.05861278  0.09541631 0.1663858
# OOP      0.07901533 0.09974626  0.05861278  1.00000000 -0.21107633 0.1113397
# DAT      0.19033995 0.14146314  0.09541631 -0.21107633  1.00000000 0.8997009
# CHANCES  0.19131248 0.24032679  0.16638584  0.11133973  0.89970090 1.0000000

# The null hypothesis of the Chi-Square test is that no relationship exists on the categorical variables
# in the population; they are independent.
for (i in 1:ncol(df)) {
  CHIS_DBS <-
    lapply(df[,-i], function(x)
      chisq.test(df[, i], x, simulate.p.value = TRUE))
  print(colnames(df[, i]))
  print(do.call(rbind, CHIS_DBS)[, c(1, 3)])
}
# [1] "DBS"
# statistic p.value
# UIUX    10785.51  0.09195402
# ALG     9392.335  0.2983508
# OOP     9367.486  0.3403298
# DAT     9414.773  0.02198901
# CHANCES 445.9674  0.0004997501
# [1] "UIUX"
# statistic p.value
# DBS     10785.51  0.07396302
# ALG     8675.378  0.05797101
# OOP     8416.65   0.2543728
# DAT     8021.978  0.1664168
# CHANCES 333.9621  0.007996002
# [1] "ALG"
# statistic p.value
# DBS     9392.335  0.3133433
# UIUX    8675.378  0.07046477
# OOP     8053.128  0.01999
# DAT     7189.895  0.2093953
# CHANCES 311.3549  0.005997001
# [1] "OOP"
# statistic p.value
# DBS     9367.486  0.3448276
# UIUX    8416.65   0.2533733
# ALG     8053.128  0.011994
# DAT     7459.998  0.07296352
# CHANCES 379.6578  0.0004997501
# [1] "DAT"
# statistic p.value
# DBS     9414.773  0.017991
# UIUX    8021.978  0.1824088
# ALG     7189.895  0.2168916
# OOP     7459.998  0.06596702
# CHANCES 663.7196  0.0004997501
# [1] "CHANCES"
# statistic p.value
# DBS  445.9674  0.0004997501
# UIUX 333.9621  0.009995002
# ALG  311.3549  0.002498751
# OOP  379.6578  0.0004997501
# DAT  663.7196  0.0004997501

# Conclusions:
# There is a strong relationship between the CHANCES and DAT variables, while other variables have a slight
# relationship with the CHANCES variable. In addition, the other variables do not seem to have a relationship with
# each other, so dimension reduction does not seem to be necessary (although we're going to try it too).

# Normalization ========================================================================================================
df_prep <-
  df %>%
  mutate(across(-CHANCES, scale)) %>%
  mutate_if(is.matrix, as.vector) %>%
  as.data.frame()

ggpairs(df_prep %>% mutate(CHANCES = factor(CHANCES, levels = c(4, 3, 2, 1))),
        aes(colour = CHANCES))
# OOP vs DAT plot seems to be worse than without scale() function

df_prep %>% psych::describe()
#         vars   n mean   sd median trimmed  mad   min  max range  skew kurtosis   se
# DBS        1 304 0.00 1.00  -0.11   -0.05 0.96 -1.86 2.70  4.56  0.38    -0.55 0.06
# UIUX       2 304 0.00 1.00  -0.22   -0.07 0.93 -1.68 2.58  4.26  0.57    -0.45 0.06
# ALG        3 304 0.00 1.00  -0.19   -0.01 1.24 -1.79 2.00  3.79  0.07    -1.19 0.06
# OOP        4 304 0.00 1.00  -0.40   -0.05 0.93 -1.69 2.17  3.86  0.46    -0.92 0.06
# DAT        5 304 0.00 1.00  -0.02   -0.02 1.22 -1.78 2.06  3.85  0.16    -1.20 0.06
# CHANCES    6 304 2.71 0.94   3.00    2.76 1.48  1.00 4.00  3.00 -0.10    -0.97 0.05

sapply(df_prep, shapiro.test)
# DBS
# statistic 0.9747375
# p.value   3.433161e-05
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# UIUX
# statistic 0.9522629
# p.value   2.17961e-08
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# ALG
# statistic 0.9564169
# p.value   7.132236e-08
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# OOP
# statistic 0.9403271
# p.value   9.875936e-10
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# DAT
# statistic 0.9464226
# p.value   4.548823e-09
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# CHANCES
# statistic 0.8712654
# p.value   2.956317e-15
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"

# Let's try to normalize the data using BoxCox transformation
bc_list <-
  map((df %>% dplyr::select(!CHANCES)) + 0.001, BoxCox.lambda)
df_bc <-
  map2_df(df %>% dplyr::select(!CHANCES), bc_list, function(x, y)
    BoxCox(x, lambda = y)) %>%
  cbind(df$CHANCES) %>%
  rename(CHANCES = `df$CHANCES`)

sapply(df_bc %>% dplyr::select(!CHANCES), shapiro.test)
# DBS
# statistic 0.9890251
# p.value   0.0215557
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# UIUX
# statistic 0.9791029
# p.value   0.0002036739
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# ALG
# statistic 0.9451327
# p.value   3.264255e-09
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# OOP
# statistic 0.9533572
# p.value   2.960411e-08
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"
# DAT
# statistic 0.9384882
# p.value   6.350904e-10
# method    "Shapiro-Wilk normality test"
# data.name "X[[i]]"

ggpairs(df_bc %>% mutate(CHANCES = factor(CHANCES, levels = c(4, 3, 2, 1))),
        aes(colour = CHANCES))

# Conclusion:
# Better but this is still far from normality. That's why, we need to apply classification method for non normal data.
# Also, OOP vs DAT plot seems to be worse than with scale() function.

# Dimension reduction ==================================================================================================
# Regression model + lasso method
model <- df_prep %>% lm(formula = CHANCES ~ .)
model
# Call:
#   lm(formula = CHANCES ~ ., data = .)
#
# Coefficients:
#   (Intercept)          DBS         UIUX          ALG          OOP          DAT
#       2.71382     -0.03729      0.05205      0.03750      0.29200      0.89888
summary(model)
# Call:
#   lm(formula = CHANCES ~ ., data = .)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -0.9849 -0.1172  0.0090  0.1530  0.7627
#
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)    2.71382    0.01464 185.397  < 2e-16 ***
#   DBS         -0.03729    0.01531  -2.436 0.015421 *
#   UIUX         0.05205    0.01520   3.424 0.000703 ***
#   ALG          0.03750    0.01494   2.510 0.012620 *
#   OOP          0.29200    0.01536  19.011  < 2e-16 ***
#   DAT          0.89888    0.01578  56.946  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2552 on 298 degrees of freedom
# Multiple R-squared:  0.9271,	Adjusted R-squared:  0.9258
# F-statistic: 757.5 on 5 and 298 DF,  p-value: < 2.2e-16

# Conclusions:
# The linear regression model matched the DAT and OOP variables as the most important (although all variables are relevant).
# The adjusted R-square is really high at 0.9258. In addition, the data does not have a normal distribution.

# PCA analysis
cor_matrix <- cor(df_prep)

pca.m <- princomp(cor_matrix)
summary(pca.m) # standard function for R models
# Importance of components:
#                           Comp.1    Comp.2    Comp.3    Comp.4      Comp.5 Comp.6
# Standard deviation     0.6146111 0.4246047 0.3407642 0.3226523 0.069944011      0
# Proportion of Variance 0.4823411 0.2302094 0.1482727 0.1329300 0.006246756      0
# Cumulative Proportion  0.4823411 0.7125505 0.8608232 0.9937532 1.000000000      1

pca.m$loadings[, 1:6] # correlation of columns by each component
#              Comp.1       Comp.2       Comp.3     Comp.4      Comp.5      Comp.6
# DBS      0.04233097  0.649967108  0.322448008  0.4646505  0.50156295  0.06567054
# UIUX    -0.02561200 -0.181164500 -0.808913421  0.4127418  0.36021890  0.10985946
# ALG     -0.08815610 -0.701133868  0.456652808  0.0971859  0.51526403  0.13101867
# OOP     -0.45222624  0.230107917 -0.150374205 -0.6679693  0.41348219  0.32059139
# DAT      0.69850280  0.004461021 -0.009081236 -0.1437950 -0.01742938  0.70072155
# CHANCES  0.54531264  0.012803398 -0.102272685 -0.3707277  0.42650747 -0.61046119

fviz_eig(pca.m, addlabels = TRUE)

fviz_pca_biplot(pca.m, col.var = "black")
# Three main pieces of information can be observed from the previous plot (Note that this is essentially a loadings plot):
# - All the variables that are grouped together are positively correlated to each other, and that is the case  for
#   instance for white/red meat, milk, and eggs have a positive correlation to each. This result is surprising because
#   they have the highest values in the loading matrix with respect to the first principal component;
# - The higher the distance between the variable and the origin, the better represented that variable is.
#   From the biplot, eggs, milk, and white meat have higher magnitude compared to red meat, and hence are
#   well represented compared to red meat;
# - Variables that are negatively correlated are displayed to the opposite sides of the biplot’s origin.

# Classification (with all variables) ==================================================================================
training.samples  <-
  df %>% mutate(CHANCES = factor(CHANCES, levels = c(4, 3, 2, 1))) %>%
  .$CHANCES %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <-
  mutate(df, CHANCES = factor(CHANCES, levels = c(4, 3, 2, 1)))[training.samples,]
test.data <-
  mutate(df, CHANCES = factor(CHANCES, levels = c(4, 3, 2, 1)))[-training.samples,]

train.data$CHANCES %>% table()
#  4  3  2  1
# 59 80 82 24

# Random Forest model
rf_model <-
  randomForest(CHANCES ~ ., train.data, proximity = TRUE)
rf_model

pred <- predict(rf_model, test.data %>% select(!CHANCES))

cm <- confusionMatrix(test.data$CHANCES, pred)

cm$overall %>% round(5)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
#  0.93220        0.90288        0.83541        0.98122        0.40678        0.00000            NaN 

cm$byClass %>% round(5)
#          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision  Recall      F1 Prevalence
# Class: 4     1.00000     1.00000            1.0        1.00000       1.0 1.00000 1.00000    0.23729
# Class: 3     1.00000     0.95122            0.9        1.00000       0.9 1.00000 0.94737    0.30508
# Class: 2     0.83333     1.00000            1.0        0.89744       1.0 0.83333 0.90909    0.40678
# Class: 1     1.00000     0.96429            0.6        1.00000       0.6 1.00000 0.75000    0.05085
#          Detection Rate Detection Prevalence Balanced Accuracy
# Class: 4        0.23729              0.23729           1.00000
# Class: 3        0.30508              0.33898           0.97561
# Class: 2        0.33898              0.33898           0.91667
# Class: 1        0.05085              0.08475           0.98214

# SVM classification model
