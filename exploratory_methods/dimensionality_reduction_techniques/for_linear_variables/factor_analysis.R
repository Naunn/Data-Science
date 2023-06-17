# FACTOR ANALYSIS ======================================================================================================
# Factor analysis is a statistical method used to describe variability among observed, correlated variables in terms of
# a potentially lower number of unobserved variables called factors. For example, it is possible that variations in
# six observed variables mainly reflect the variations in two unobserved (underlying) variables. Factor analysis
# searches for such joint variations in response to unobserved latent variables. The observed variables are modelled as
# linear combinations of the potential factors plus "error" terms, hence factor analysis can be thought of as a special
# case of errors-in-variables models.
#
# Simply put, the factor loading of a variable quantifies the extent to which the variable is related to a given factor.
#
# A common rationale behind factor analytic methods is that the information gained about the interdependencies between
# observed variables can be used later to reduce the set of variables in a dataset. Factor analysis is commonly used
# in psychometrics, personality psychology, biology, marketing, product management, operations research, finance,
# and machine learning. It may help to deal with data sets where there are large numbers of observed variables that
# are thought to reflect a smaller number of underlying/latent variables. It is one of the most commonly used
# inter-dependency techniques and is used when the relevant set of variables shows a systematic inter-dependence and
# the objective is to find out the latent factors that create a commonality.

# There are two types of FA:
# - Exploratory factor analysis
# Exploratory factor analysis (EFA) is used to identify complex interrelationships among items and group items that are
# part of unified concepts. The researcher makes no a priori assumptions about relationships among factors.

# - Confirmatory factor analysis
# Confirmatory factor analysis (CFA) is a more complex approach that tests the hypothesis that the items are
# associated with specific factors. CFA uses structural equation modeling to test a measurement model whereby
# loading on the factors allows for evaluation of relationships between observed variables and unobserved variables.
# Structural equation modeling approaches can accommodate measurement error and are less restrictive than least-squares
# estimation. Hypothesized models are tested against actual data, and the analysis would demonstrate loadings of
# observed variables on the latent variables (factors), as well as the correlation between the latent variables.

# FA vs PCA
# The differences between PCA and factor analysis (FA) are further illustrated by Suhr (2009):
# - PCA results in principal components that account for a maximal amount of variance for observed variables;
#   FA accounts for common variance in the data.
# - PCA inserts ones on the diagonals of the correlation matrix;
#   FA adjusts the diagonals of the correlation matrix with the unique factors.
# - PCA minimizes the sum of squared perpendicular distance to the component axis;
#   FA estimates factors that influence responses on observed variables.
# - The component scores in PCA represent a linear combination of the observed variables weighted by eigenvectors;
#   the observed variables in FA are linear combinations of the underlying and unique factors.
# - In PCA, the components yielded are uninterpretable, i.e. they do not represent underlying ‘constructs’;
#   in FA, the underlying constructs can be labelled and readily interpreted, given an accurate model specification.

# Assumptions
# - The variables’ linear relationships
# - Absence of multicollinearity
# - Relevance of the variables
# - The existence of a true correlation between factors and variables

# Libraries ============================================================================================================
library(dplyr)
library(psych) # A package for personality, psychometric, and psychological research
library(corrplot)
library(ggplot2)
library(car)

# Data =================================================================================================================
# Dataset
data_survey <-
  read.csv(
    "https://raw.githubusercontent.com/housecricket/data/main/efa/sample1.csv",
    sep = ","
  ) %>%
  select(!ID)

data_survey %>% str()
# 'data.frame':	259 obs. of  14 variables:
# $ KM1: int  5 3 2 4 4 1 2 2 4 2 ...
# $ KM2: int  5 3 2 3 4 1 4 3 5 2 ...
# $ KM3: int  5 3 2 3 4 1 4 3 5 2 ...
# $ QC1: int  5 4 2 4 2 2 2 4 4 2 ...
# $ QC2: int  2 5 2 3 3 5 4 3 2 2 ...
# $ QC3: int  1 3 1 4 4 3 2 3 5 2 ...
# $ CT1: int  1 4 3 4 4 5 4 4 5 2 ...
# $ CT2: int  3 5 3 4 4 5 4 4 4 4 ...
# $ CT3: int  1 4 3 4 4 5 4 4 5 2 ...
# $ PC1: int  4 2 4 1 3 4 3 3 4 5 ...
# $ PC2: int  1 2 3 1 3 3 1 4 4 3 ...
# $ PC3: int  3 2 5 3 5 5 4 3 5 5 ...
# $ QD : int  4 4 2 3 4 3 4 3 4 2 ...

describe(data_survey) # psych function
# vars   n       mean    sd median trimmed   mad min max range  skew kurtosis   se
# KM1    2 259   3.31  1.29      3    3.39  1.48   1   5     4 -0.27    -0.89 0.08
# KM2    3 259   3.36  1.37      3    3.45  1.48   1   5     4 -0.35    -1.02 0.09
# KM3    4 259   3.40  1.31      3    3.49  1.48   1   5     4 -0.38    -0.90 0.08
# QC1    5 259   3.78  1.35      4    3.96  1.48   1   5     4 -0.93    -0.50 0.08
# QC2    6 259   3.41  1.17      3    3.48  1.48   1   5     4 -0.30    -0.62 0.07
# QC3    7 259   3.34  1.23      3    3.42  1.48   1   5     4 -0.27    -0.81 0.08
# CT1    8 259   3.69  1.12      4    3.80  1.48   1   5     4 -0.54    -0.24 0.07
# CT2    9 259   3.73  1.08      4    3.84  1.48   1   5     4 -0.56    -0.21 0.07
# CT3   10 259   3.36  1.23      3    3.44  1.48   1   5     4 -0.28    -0.73 0.08
# PC1   11 259   3.42  1.20      3    3.51  1.48   1   5     4 -0.40    -0.59 0.07
# PC2   12 259   3.09  1.34      3    3.11  1.48   1   5     4 -0.13    -1.05 0.08
# PC3   13 259   3.71  1.27      4    3.87  1.48   1   5     4 -0.72    -0.49 0.08
# QD    14 259   3.55  1.05      4    3.61  1.48   1   5     4 -0.33    -0.39 0.07

data_survey %>% is.na() %>% colSums()
# KM1 KM2 KM3 QC1 QC2 QC3 CT1 CT2 CT3 PC1 PC2 PC3  QD
#  0   0   0   0   0   0   0   0   0   0   0   0   0

# Exploration ==========================================================================================================
# Divide into explanatory and explicative
X <- data_survey %>% select(!QD)
y <- data_survey %>% select(QD)

# Determine if factor analysis is appropriate using correlation matrix and Bartlett’s Test of Sphericity
X %>%
  cor() %>%
  corrplot(method = 'number')

# Bartlett’s Test of Sphericity compares an observed correlation matrix to the identity matrix. Essentially it checks
# to see if there is a certain redundancy between the variables that we can summarize with a few number of factors.
# The null hypothesis of the test is that the variables are orthogonal, i.e. not correlated. The alternative hypothesis
# is that the variables are not orthogonal, i.e. they are correlated enough to where the correlation matrix diverges
# significantly from the identity matrix.
cortest.bartlett(X)$p.value # 8.846246e-290 < 0.05 - analysis may be useful with our data

# The Kaiser–Meyer–Olkin (KMO) test is a statistical measure to determine how suited data is for factor analysis.
# The test measures sampling adequacy for each variable in the model and the complete model.
KMO(r = cor(X))
# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = cor(X))
# Overall MSA =  0.83
# MSA for each item =
#   KM1  KM2  KM3  QC1  QC2  QC3  CT1  CT2  CT3  PC1  PC2  PC3
# 0.87 0.84 0.82 0.88 0.86 0.86 0.83 0.85 0.85 0.70 0.78 0.80
# According to Kaiser’s (1974) guidelines, a suggested cutoff for determining the factorability of the sample data is KMO ≥ 60

fa.parallel(X)
# Parallel analysis suggests that the number of factors =  4  and the number of components =  3

# Build model ==========================================================================================================
# FA model
# Note. Factor weights are computed to extract the maximum possible variance, with successive factoring continuing until 
# there is no further meaningful variance left.[4] The factor model must then be rotated for analysis.
fa.model <- fa(
  r = X,
  nfactors = 4,
  # covar = FALSE, SMC = TRUE,
  fm = "pa",
  # type of factor analysis we want to use (“pa” is principal axis factoring)
  max.iter = 100,
  # (50 is the default, but we have changed it to 100
  rotate = "varimax"
) # none rotation

fa.diagram(fa.model)

# Regression model
df <- 
  as.data.frame(fa.model$scores) %>% 
  cbind(y)

lm.model.org <- lm(QD~., data_survey)
summary(lm.model.org) # Multiple R-squared:  0.9359,	Adjusted R-squared:  0.9328 

lm.model <- lm(QD~., df)
summary(lm.model) # Multiple R-squared:  0.8658,	Adjusted R-squared:  0.8637 while all variables have high significant level
