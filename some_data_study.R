# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(caret)
library(forecast)
library(GGally)
library(ggplot2)

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

ggpairs(df, aes(colour = CHANCES))

# Since the nominal data is ordered, so we can switch from strings to integers
df_prep <-
  df %>%
  mutate(CHANCES = case_when(
    CHANCES == "High" ~ 4,
    CHANCES == "Middle" ~ 3,
    CHANCES == "Low" ~ 2,
    .default = 1
  )) %>%
  mutate_if(is.numeric, scale) %>%
  mutate_if(is.matrix, as.vector) %>%
  as.data.frame()

ggpairs(df_prep, aes(colour = df$CHANCES)) # OOP vs DAT plot seems to be worse than without scale() function

df_prep %>% psych::describe()
#         vars   n mean   sd median trimmed  mad   min  max range  skew kurtosis   se
# DBS        1 304 0.00 1.00  -0.11   -0.05 0.96 -1.86 2.70  4.56  0.38    -0.55 0.06
# UIUX       2 304 0.00 1.00  -0.22   -0.07 0.93 -1.68 2.58  4.26  0.57    -0.45 0.06
# ALG        3 304 0.00 1.00  -0.19   -0.01 1.24 -1.79 2.00  3.79  0.07    -1.19 0.06
# OOP        4 304 0.00 1.00  -0.40   -0.05 0.93 -1.69 2.17  3.86  0.46    -0.92 0.06
# DAT        5 304 0.00 1.00  -0.02   -0.02 1.22 -1.78 2.06  3.85  0.16    -1.20 0.06
# CHANCES    6 304 2.71 0.94   3.00    2.76 1.48  1.00 4.00  3.00 -0.10    -0.97 0.05

# the relationship between linearly related variables
df_prep %>% cor(method = "pearson")
#                 DBS      UIUX         ALG         OOP        DAT   CHANCES
# DBS      1.00000000 0.0989106 -0.02358626  0.12071867  0.2228907 0.2161619
# UIUX     0.09891060 1.0000000  0.14312016  0.12616931  0.1720621 0.2616858
# ALG     -0.02358626 0.1431202  1.00000000  0.06229136  0.1121188 0.1758559
# OOP      0.12071867 0.1261693  0.06229136  1.00000000 -0.1831190 0.1406347
# DAT      0.22289070 0.1720621  0.11211881 -0.18311900  1.0000000 0.9072993
# CHANCES  0.21616188 0.2616858  0.17585588  0.14063471  0.9072993 1.0000000

# non-parametric test that measures the strength of dependence between two variables
df_prep %>% cor(method = "kendall")
#                 DBS       UIUX         ALG         OOP         DAT    CHANCES
# DBS      1.00000000 0.06458423 -0.01004462  0.05298002  0.13789518 0.15422578
# UIUX     0.06458423 1.00000000  0.09186704  0.06724199  0.09872808 0.18608168
# ALG     -0.01004462 0.09186704  1.00000000  0.04017239  0.06741193 0.12960411
# OOP      0.05298002 0.06724199  0.04017239  1.00000000 -0.15881360 0.08807247
# DAT      0.13789518 0.09872808  0.06741193 -0.15881360  1.00000000 0.78159456
# CHANCES  0.15422578 0.18608168  0.12960411  0.08807247  0.78159456 1.00000000

# non-parametric test that is used to measure the degree of association between two variables
df_prep %>% cor(method = "spearman")
#                 DBS       UIUX         ALG         OOP         DAT   CHANCES
# DBS      1.00000000 0.09243724 -0.01594068  0.07901533  0.19033995 0.1913125
# UIUX     0.09243724 1.00000000  0.13530726  0.09974626  0.14146314 0.2403268
# ALG     -0.01594068 0.13530726  1.00000000  0.05861278  0.09541631 0.1663858
# OOP      0.07901533 0.09974626  0.05861278  1.00000000 -0.21107633 0.1113397
# DAT      0.19033995 0.14146314  0.09541631 -0.21107633  1.00000000 0.8997009
# CHANCES  0.19131248 0.24032679  0.16638584  0.11133973  0.89970090 1.0000000

# Conclusions:
# There is a strong relationship between the CHANCES and DAT variables, while other variables have a slight
# relationship with the CHANCES variable. In addition, the other variables do not seem to have a relationship with
# each other, so dimension reduction does not seem to be necessary.

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

# Conclusions:
# The linear regression model matched the DAT and OOP variables as the most important.
# The adjusted R-square is really high at 0.9258. In addition, the data does not have a normal distribution.

# Preprocessing ========================================================================================================
# Let's try to normalize the data using BoxCox transformation
bc_list <- map(df %>% dplyr::select(!CHANCES), BoxCox.lambda)
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

ggpairs(df_bc, aes(colour = CHANCES))

# Conclusion:
# Better but this is still far from normality. That's why, we need to apply classification method for non normal data.
# Also, OOP vs DAT plot seems to be worse than with scale() function.

# Classification =======================================================================================================
