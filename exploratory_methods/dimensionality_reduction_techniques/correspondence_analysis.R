# CORRESPONDENCE ANALYSIS ==============================================================================================
# Correspondence analysis (CA) is a multivariate statistical (descriptive) technique, conceptually similar to principal
# component analysis, but applies to categorical rather than continuous data.  If more than two categorical variables
# are to be summarized, a variant called multiple correspondence analysis should be chosen instead.
# Notice that we can only compare two categorical variables.

## Important terms:
# - contingency table - (also known as a cross tabulation or crosstab) is a type of (2 dimensional) table in a matrix
#   format that displays the (multivariate) frequency distribution of the variables. They provide a basic picture of
#   the interrelation between two variables and can help find interactions between them;
# - residuals - quantifies the difference between the observed data and the data we would expect - assuming there is
#   no relationship between the row and column categories (here, those would be brand and attribute). 
#   A positive residual shows us that the count for that brand attribute pairing is much higher than expected,
#   suggesting a strong relationship; correspondingly, a negative residual shows a lower value than expected,
#   suggesting a weaker relationship. Let's walk through calculating these residuals.

# Libraries ============================================================================================================
library(dplyr)
library(knitr)
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining with R
library(factoextra) # Print method for an object of class factoextra
library(gplots)
library(corrplot)

# Data =================================================================================================================
data("housetasks") # House tasks contingency table
knitr::kable(housetasks) # contingency table
# |           | Wife| Alternating| Husband| Jointly|
# |:----------|----:|-----------:|-------:|-------:|
# |Laundry    |  156|          14|       2|       4|
# |Main_meal  |  124|          20|       5|       4|
# |Dinner     |   77|          11|       7|      13|
# |Breakfeast |   82|          36|      15|       7|
# |Tidying    |   53|          11|       1|      57|
# |Dishes     |   32|          24|       4|      53|
# |Shopping   |   33|          23|       9|      55|
# |Official   |   12|          46|      23|      15|
# |Driving    |   10|          51|      75|       3|
# |Finances   |   13|          13|      21|      66|
# |Insurance  |    8|           1|      53|      77|
# |Repairs    |    0|           3|     160|       2|
# |Holidays   |    0|           1|       6|     153|

prop.table(housetasks) %>% kable()
# |           |      Wife| Alternating|   Husband|   Jointly|
# |:----------|---------:|-----------:|---------:|---------:|
# |Laundry    | 0.0894495|   0.0080275| 0.0011468| 0.0022936|
# |Main_meal  | 0.0711009|   0.0114679| 0.0028670| 0.0022936|
# |Dinner     | 0.0441514|   0.0063073| 0.0040138| 0.0074541|
# |Breakfeast | 0.0470183|   0.0206422| 0.0086009| 0.0040138|
# |Tidying    | 0.0303899|   0.0063073| 0.0005734| 0.0326835|
# |Dishes     | 0.0183486|   0.0137615| 0.0022936| 0.0303899|
# |Shopping   | 0.0189220|   0.0131881| 0.0051606| 0.0315367|
# |Official   | 0.0068807|   0.0263761| 0.0131881| 0.0086009|
# |Driving    | 0.0057339|   0.0292431| 0.0430046| 0.0017202|
# |Finances   | 0.0074541|   0.0074541| 0.0120413| 0.0378440|
# |Insurance  | 0.0045872|   0.0005734| 0.0303899| 0.0441514|
# |Repairs    | 0.0000000|   0.0017202| 0.0917431| 0.0011468|
# |Holidays   | 0.0000000|   0.0005734| 0.0034404| 0.0877294|

housetasks %>% str()
#  'data.frame':	13 obs. of  4 variables:
# $ Wife       : int  156 124 77 82 53 32 33 12 10 13 ...
# $ Alternating: int  14 20 11 36 11 24 23 46 51 13 ...
# $ Husband    : int  2 5 7 15 1 4 9 23 75 21 ...
# $ Jointly    : int  4 4 13 7 57 53 55 15 3 66 ...

# Correspondence analysis and interpretation ===========================================================================
# factoextra functions:
# get_eigenvalue(res.famd): Extract the eigenvalues/variances retained by each dimension (axis).
# fviz_eig(res.famd): Visualize the eigenvalues/variances.
# get_famd_ind(res.famd): Extract the results for individuals.
# get_famd_var(res.famd): Extract the results for quantitative and qualitative variables.
# fviz_famd_ind(res.famd), fviz_famd_var(res.famd): Visualize the results for individuals and variables, respectively.

# fancy graphic comparison
t(as.table(as.matrix(housetasks))) %>%
  balloonplot(
    main = "housetasks",
    xlab = "",
    ylab = "",
    label = FALSE,
    show.margins = FALSE
  )

# Correspondence Analysis and interpretation ===========================================================================
# For a small contingency table, you can use the Chi-square test to evaluate whether there is
# a significant dependence between row and column categories. Because CA is a descriptive technique,
# it can be applied to tables regardless of a significant chi-squared test.

# The null hypothesis of the Chi-Square test is that no relationship exists on the categorical variables
# in the population, so they are independent. If p < 0.05 then we reject null hypothesis.
chisq.test(housetasks)

# CA model
res.ca <- CA(housetasks, graph = FALSE)
# The way that correspondence analysis works means that we can compare between row labels based on distances. We can also 
# compare between column labels based on distances. However, if we want to compare a row label to a column label, we need to:
#   1. Look at the length of the line connecting the row label to the origin (0,0). Longer lines indicate that the row label
#     is highly associated with some of the column labels (i.e., it has at least one high residual).
#   2. Look at the length of the label connecting the column label to the origin (0,0). Longer lines again indicate
#     a high association between the column label and one or more row labels.
#   3. Look at the angle formed between these two lines. Really small angles indicate association. 
#     90 degree angles indicate no relationship. Angles near 180 degrees indicate negative associations.
summary(res.ca) # standard function for R models

fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)

# Eigen values
get_eigenvalue(res.ca)
#       eigenvalue variance.percent cumulative.variance.percent
# Dim.1  0.5428893         48.69222                    48.69222 # (relatively) high variance.percentage of 1 dimension
# Dim.2  0.4450028         39.91269                    88.60491 # (relatively) high variance.percentage of 2 dimension
# Dim.3  0.1270484         11.39509                   100.00000

# - Eigenvalues correspond to the amount of information retained by each axis. Dimensions are ordered decreasingly and 
#   listed according to the amount of variance explained in the solution. Dimension 1 explains the most variance in the solution,
#   followed by dimension 2 and so on. Eigenvalues can be used to determine the number of axes to retain.
# - The cumulative percentage explained is obtained by adding the successive proportions of variation explained to
#   obtain the running total. 
# Note that, a good dimension reduction is achieved when the the first few dimensions account for a large proportion of the variability.
fviz_eig(res.ca)

# Contributions of rows to the dimensions
corrplot(res.ca$row$contrib, is.corr=FALSE)    
# row$coord: coordinates of each row point in each dimension (1, 2 and 3). Used to create the scatter plot.
# row$cos2: quality of representation of rows.
# var$contrib: contribution of rows (in %) to the definition of the dimensions.

# Contributions of rows to dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)

