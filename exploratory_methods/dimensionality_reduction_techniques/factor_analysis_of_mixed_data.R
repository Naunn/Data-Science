# FACTOR ANALYSIS OF MIXED DATA ========================================================================================
# FAMD is the factorial method devoted to data tables in which a group of individuals is described both by
# quantitative and qualitative variables.

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

df_wine <-
  wine %>%
  select(Label,
         Soil,
         Plante,
         Acidity,
         Harmony,
         Intensity,
         Overall.quality)

# Factor analysis of mixed data ========================================================================================
# factoextra functions:
# get_eigenvalue(res.famd): Extract the eigenvalues/variances retained by each dimension (axis).
# fviz_eig(res.famd): Visualize the eigenvalues/variances.
# get_famd_ind(res.famd): Extract the results for individuals.
# get_famd_var(res.famd): Extract the results for quantitative and qualitative variables.
# fviz_famd_ind(res.famd), fviz_famd_var(res.famd): Visualize the results for individuals and variables, respectively.

# FAMD model
# FAMD(
# base : a data frame with n rows (individuals) and p columns (variables).
# ncp: the number of dimensions kept in the results (by default 5)
# sup.var: a vector indicating the indexes of the supplementary variables.
# ind.sup: a vector indicating the indexes of the supplementary individuals.
# graph : a logical value. If TRUE a graph is displayed.
# )

# build model
res.famd <- FAMD(df_wine, graph = FALSE)

# get eigen values
get_eigenvalue(res.famd)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1  4.1005774        41.005774                    41.00577
# Dim.2  1.8278868        18.278868                    59.28464
# Dim.3  1.5808161        15.808161                    75.09280
# Dim.4  1.0543736        10.543736                    85.63654
# Dim.5  0.6511132         6.511132                    92.14767
fviz_eig(res.famd)

res.famd$ind$coord # Scores (i.e. principal coordinates)

# Plot representation of individuals
fviz_famd_ind(
  res.famd,
  col.ind = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) # qualitative data are plotted in black

# Plot representation of individuals with grouping on qualitative data
fviz_mfa_ind(res.famd, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
)

# Plot representation of individuals with grouping separated by qualitative data
fviz_ellipses(res.famd, c("Label", "Soil"), repel = TRUE)

# Plot correlation of coords (scores/variables) to axises (res.famd$var$coord)
fviz_famd_var(res.famd, repel = TRUE) # Dim1: Overall.quality, Harmony, Plante, Intensity; Dim2: Soil

# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)
# The red dashed line on the graph above indicates the expected average value, If the contributions were uniform.

# Plot correlation between quantitative variables
fviz_famd_var(
  res.famd,
  "quanti.var",
  repel = TRUE,
  col.var = "cos2",
  # cos2 (the square cosine) - determines how much each variable is represented in a given component.
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)# "black")
# The graph of variables (correlation circle) shows the relationship between variables, the quality of the
# representation of variables, as well as, the correlation between variables and the dimensions (like in PCA).

# Plot correlation between qualitative variables
fviz_famd_var(
  res.famd,
  "quali.var",
  repel = TRUE,
  col.var = "cos2",
  # cos2 (the square cosine) - determines how much each variable is represented in a given component.
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
) 
