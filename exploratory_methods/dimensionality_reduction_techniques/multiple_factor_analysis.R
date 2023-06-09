# MULTIPLE FACTOR ANALYSIS =============================================================================================
# Multiple factor analysis (MFA) is a multivariate data analysis method for summarizing and visualizing a complex data
# table in which individuals are described by several sets of variables (quantitative and /or qualitative) structured
# into groups. It takes into account the contribution of all active groups of variables to define the distance between
# individuals. The number of variables in each group may differ and the nature of the variables (qualitative or
# quantitative) can vary from one group to the other but the variables should be of the same nature in a given group.

# Multiple factor analysis can be used in a variety of fields (J. Pagès 2002), where the variables are organized into groups:

# - Survey analysis, where an individual is a person; a variable is a question. Questions are
#   organized by themes (groups of questions).

# - Sensory analysis, where an individual is a food product. A first set of variables includes sensory variables
#   (sweetness, bitterness, etc.); a second one includes chemical variables (pH, glucose rate, etc.).

# - Ecology, where an individual is an observation place. A first set of variables describes soil characteristics;
#   a second one describes flora.

# - Times series, where several individuals are observed at different dates. In this situation, there is commonly
#   two ways of defining groups of variables:
#     - generally, variables observed at the same time (date) are gathered together.
#     - When variables are the same from one date to the others, each set can gather the different dates for one variable.

# Libraries ============================================================================================================
library(dplyr)
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining with R
library(factoextra) # Print method for an object of class factoextra

# Data =================================================================================================================
data(wine)

wine %>% str()
# 'data.frame':	21 obs. of  31 variables:
# $ Label                        : Factor w/ 3 levels "Saumur","Bourgueuil",..: 1 1 2 3 1 2 2 1 3 1 ...
# $ Soil                         : Factor w/ 4 levels "Reference","Env1",..: 2 2 2 3 1 1 1 2 2 3 ...
# $ Odor.Intensity.before.shaking: num  3.07 2.96 2.86 2.81 3.61 ...
# $ Aroma.quality.before.shaking : num  3 2.82 2.93 2.59 3.43 ...
# $ Fruity.before.shaking        : num  2.71 2.38 2.56 2.42 3.15 ...
# $ Flower.before.shaking        : num  2.28 2.28 1.96 1.91 2.15 ...
# $ Spice.before.shaking         : num  1.96 1.68 2.08 2.16 2.04 ...
# $ Visual.intensity             : num  4.32 3.22 3.54 2.89 4.39 ...
# $ Nuance                       : num  4 3 3.39 2.79 4.04 ...
# $ Surface.feeling              : num  3.27 2.81 3 2.54 3.38 ...
# $ Odor.Intensity               : num  3.41 3.37 3.25 3.16 3.54 ...
# $ Quality.of.odour             : num  3.31 3 2.93 2.88 3.36 ...
# $ Fruity                       : num  2.88 2.56 2.77 2.39 3.16 ...
# $ Flower                       : num  2.32 2.44 2.19 2.08 2.23 ...
# $ Spice                        : num  1.84 1.74 2.25 2.17 2.15 ...
# $ Plante                       : num  2 2 1.75 2.3 1.76 ...
# $ Phenolic                     : num  1.65 1.38 1.25 1.48 1.6 ...
# $ Aroma.intensity              : num  3.26 2.96 3.08 2.54 3.62 ...
# $ Aroma.persistency            : num  2.96 2.81 2.8 2.58 3.3 ...
# $ Aroma.quality                : num  3.2 2.93 3.08 2.48 3.46 ...
# $ Attack.intensity             : num  2.96 3.04 3.22 2.7 3.46 ...
# $ Acidity                      : num  2.11 2.11 2.18 3.18 2.57 ...
# $ Astringency                  : num  2.43 2.18 2.25 2.18 2.54 ...
# $ Alcohol                      : num  2.5 2.65 2.64 2.5 2.79 ...
# $ Balance                      : num  3.25 2.93 3.32 2.33 3.46 ...
# $ Smooth                       : num  2.73 2.5 2.68 1.68 3.04 ...
# $ Bitterness                   : num  1.93 1.93 2 1.96 2.07 ...
# $ Intensity                    : num  2.86 2.89 3.07 2.46 3.64 ...
# $ Harmony                      : num  3.14 2.96 3.14 2.04 3.64 ...
# $ Overall.quality              : num  3.39 3.21 3.54 2.46 3.74 ...
# $ Typical                      : num  3.25 3.04 3.18 2.25 3.44 ...

wine %>% psych::describe()
# vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
# Label*                           1 21 1.67 0.80   1.00    1.59 0.00 1.00 3.00  2.00  0.61    -1.23 0.17
# Soil*                            2 21 2.10 1.00   2.00    2.00 1.48 1.00 4.00  3.00  0.40    -1.07 0.22
# Odor.Intensity.before.shaking    3 21 3.11 0.29   3.07    3.09 0.26 2.64 3.71  1.07  0.71    -0.39 0.06
# Aroma.quality.before.shaking     4 21 3.05 0.21   3.11    3.05 0.26 2.59 3.43  0.84 -0.25    -0.74 0.04
# Fruity.before.shaking            5 21 2.71 0.20   2.73    2.71 0.23 2.38 3.15  0.78  0.26    -0.67 0.04
# Flower.before.shaking            6 21 2.06 0.14   2.04    2.06 0.17 1.83 2.28  0.45  0.06    -1.39 0.03
# Spice.before.shaking             7 21 1.99 0.24   1.96    1.97 0.17 1.67 2.67  1.00  0.90     0.90 0.05
# Visual.intensity                 8 21 3.96 0.55   4.07    4.01 0.42 2.61 4.71  2.11 -0.84     0.01 0.12
# Nuance                           9 21 3.73 0.52   3.89    3.76 0.42 2.54 4.54  2.00 -0.58    -0.38 0.11
# Surface.feeling                 10 21 3.18 0.28   3.26    3.23 0.19 2.44 3.46  1.02 -1.36     0.94 0.06
# Odor.Intensity                  11 21 3.40 0.20   3.37    3.40 0.16 2.89 3.74  0.85 -0.33     0.10 0.04
# Quality.of.odour                12 21 3.24 0.22   3.35    3.25 0.13 2.80 3.50  0.70 -0.66    -1.14 0.05
# Fruity                          13 21 2.85 0.22   2.88    2.86 0.17 2.39 3.18  0.79 -0.47    -0.83 0.05
# Flower                          14 21 2.16 0.15   2.19    2.17 0.16 1.77 2.44  0.67 -0.55     0.11 0.03
# Spice                           15 21 2.18 0.21   2.16    2.18 0.17 1.74 2.61  0.87 -0.10    -0.29 0.05
# Plante                          16 21 1.96 0.16   1.96    1.95 0.18 1.75 2.30  0.55  0.47    -0.60 0.04
# Phenolic                        17 21 1.54 0.15   1.54    1.54 0.10 1.25 1.91  0.66  0.35    -0.07 0.03
# Aroma.intensity                 18 21 3.20 0.25   3.25    3.22 0.23 2.54 3.62  1.07 -0.84     0.52 0.06
# Aroma.persistency               19 21 2.98 0.25   3.04    3.01 0.18 2.31 3.30  0.99 -0.94     0.56 0.05
# Aroma.quality                   20 21 3.06 0.32   3.19    3.08 0.29 2.48 3.46  0.98 -0.55    -1.13 0.07
# Attack.intensity                21 21 3.16 0.31   3.25    3.20 0.21 2.18 3.52  1.34 -1.53     2.44 0.07
# Acidity                         22 21 2.39 0.24   2.39    2.36 0.21 2.11 3.18  1.07  1.53     3.11 0.05
# Astringency                     23 21 2.44 0.20   2.50    2.46 0.21 1.96 2.67  0.70 -0.65    -0.64 0.04
# Alcohol                         24 21 2.74 0.17   2.78    2.76 0.11 2.25 2.96  0.71 -1.18     1.28 0.04
# Balance                         25 21 3.13 0.33   3.21    3.17 0.16 2.33 3.50  1.17 -1.08     0.00 0.07
# Smooth                          26 21 2.67 0.41   2.82    2.71 0.27 1.68 3.29  1.61 -0.91     0.13 0.09
# Bitterness                      27 21 2.07 0.19   2.04    2.05 0.16 1.68 2.67  0.99  1.14     3.10 0.04
# Intensity                       28 21 3.17 0.37   3.25    3.21 0.32 2.18 3.67  1.49 -0.91     0.46 0.08
# Harmony                         29 21 3.15 0.44   3.21    3.21 0.32 2.04 3.79  1.75 -1.07     0.81 0.10
# Overall.quality                 30 21 3.33 0.43   3.46    3.38 0.37 2.37 3.93  1.56 -0.87    -0.35 0.09
# Typical                         31 21 3.20 0.43   3.30    3.24 0.30 2.25 3.93  1.68 -0.78    -0.18 0.09

# We have 6 groups of variables, which can be specified to the FactoMineR as follow:
wine[, 1:2] # origin of the wines
wine[, 3:7] # odor of the wines before shaking
wine[, 8:10] # visual inspection of the wines
wine[, 11:20] # odor of the wines after shaking
wine[, 21:29] # taste of the wines
wine[, 30:31] # overall judgement of the wines

# Build model ==========================================================================================================
# MFA model
# MFA()
# base : a data frame with n rows (individuals) and p columns (variables)
# group: a vector with the number of variables in each group.
# type: the type of variables in each group. By default, all variables are quantitative and scaled to unit variance.
#   Allowed values include:
#   “c” or “s” for quantitative variables. If “s”, the variables are scaled to unit variance.
#   “n” for categorical variables.
#   “f” for frequencies (from a contingency tables).
# ind.sup: a vector indicating the indexes of the supplementary individuals.
# name.group: a vector containing the name of the groups (by default, NULL and the group are named group.1, group.2 and so on).
# num.group.sup: the indexes of the illustrative groups (by default, NULL and no group are illustrative).
# graph : a logical value. If TRUE a graph is displayed.

# build model
res.mfa <-
  MFA(
    base = wine,
    group = c(2, 5, 3, 10, 9, 2),
    type = c("n", "s", "s", "s", "s", "s"),
    name.group = c(
      "origin",
      "odor",
      "visual",
      "odor.after.shaking",
      "taste",
      "overall"
    ),
    num.group.sup = c(1, 6),
    graph = FALSE
  )

# get eigen values
get_eigenvalue(res.mfa)
#         eigenvalue variance.percent cumulative.variance.percent
# Dim.1  3.461950436      49.37838239                    49.37838
# Dim.2  1.366768270      19.49444613                    68.87283
# Dim.3  0.615429078       8.77796864                    77.65080
# Dim.4  0.372199671       5.30874662                    82.95954
# Dim.5  0.270382485       3.85651094                    86.81605
# Dim.6  0.202403306       2.88691245                    89.70297
# Dim.7  0.175713405       2.50622990                    92.20920
# Dim.8  0.125898681       1.79571410                    94.00491
# Dim.9  0.105275528       1.50156260                    95.50647
# Dim.10 0.078791202       1.12381220                    96.63029
# Dim.11 0.073892411       1.05393990                    97.68423
# Dim.12 0.060338440       0.86061733                    98.54484
# Dim.13 0.028705009       0.40942438                    98.95427
# Dim.14 0.021962548       0.31325552                    99.26752
# Dim.15 0.019163991       0.27333923                    99.54086
# Dim.16 0.010940292       0.15604323                    99.69691
# Dim.17 0.009156013       0.13059375                    99.82750
# Dim.18 0.006370966       0.09087016                    99.91837
# Dim.19 0.003303413       0.04711714                    99.96549
# Dim.20 0.002419756       0.03451339                   100.00000
fviz_screeplot(res.mfa)


# GROUPS OF VARIABLES
# extract the results for groups of variables
group <- get_mfa_var(res.mfa, "group")
group$coord
# Dim.1      Dim.2      Dim.3      Dim.4      Dim.5
# odor               0.7820738 0.61977283 0.37353451 0.17260092 0.08553276
# visual             0.8546846 0.04014481 0.01438360 0.04550736 0.02966750
# odor.after.shaking 0.9247734 0.46892047 0.18009116 0.10139051 0.11589439
# taste              0.9004187 0.23793016 0.04741982 0.05270088 0.03928784

# Coordinates of groups
head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the  dimensions
head(group$contrib)

# Plot of the correlation between groups and dimensions:
#   red color = active groups of variables
#   green color = supplementary groups of variables
fviz_mfa_var(res.mfa, "group")
# The coordinates of the four active groups on the first dimension are almost identical. This means that they
# contribute similarly to the first dimension. Concerning the second dimension, the two groups - odor and
# odor.after.shake - have the highest coordinates indicating a highest contribution to the second dimension.

# Contribution to the first dimension
fviz_contrib(res.mfa, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.mfa, "group", axes = 2)

# QUANTITATIVE VARIABLES
quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var$coord %>% row.names()
# [1] "Odor.Intensity.before.shaking" "Aroma.quality.before.shaking"
# [3] "Fruity.before.shaking"         "Flower.before.shaking"
# [5] "Spice.before.shaking"          "Visual.intensity"
# [7] "Nuance"                        "Surface.feeling"
# [9] "Odor.Intensity"                "Quality.of.odour"
# [11] "Fruity"                        "Flower"
# [13] "Spice"                         "Plante"
# [15] "Phenolic"                      "Aroma.intensity"
# [17] "Aroma.persistency"             "Aroma.quality"
# [19] "Attack.intensity"              "Acidity"
# [21] "Astringency"                   "Alcohol"
# [23] "Balance"                       "Smooth"
# [25] "Bitterness"                    "Intensity"
# [27] "Harmony"

# Plot of the correlation between groups and dimensions
fviz_mfa_var(
  res.mfa,
  "quanti.var",
  palette = "jco",
  col.var.sup = "violet",
  repel = TRUE,
  geom = c("point", "text"),
  # instead of geom = c(“arrow”, “text”)
  legend = "bottom"
)

# Contributions to dimension 1
fviz_contrib(
  res.mfa,
  choice = "quanti.var",
  axes = 1,
  top = 20,
  palette = "jco"
)

# Contributions to dimension 2
fviz_contrib(
  res.mfa,
  choice = "quanti.var",
  axes = 2,
  top = 20,
  palette = "jco"
)

