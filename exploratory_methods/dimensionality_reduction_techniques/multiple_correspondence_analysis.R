# MULTIPLE CORRESPONDENCE ANALYSIS =====================================================================================
# Multiple correspondence analysis (MCA) is a data analysis technique for nominal categorical data, used to detect and
# represent underlying structures in a data set, mainly for dimension reduction. It does this by representing data as
# points in a low-dimensional Euclidean space. The procedure thus appears to be the counterpart of principal component
# analysis for categorical data. MCA can be viewed as an extension of simple correspondence analysis (CA) in that it is
# applicable to a large set of categorical variables.

# https://www.displayr.com/correspondence-analysis-versus-multiple-correspondence-analysis-use/
# The core difference between correspondence analysis and multiple correspondence analysis:
# - multiple correspondence analysis is a technique for analyzing categorical variables. It is essentially a form of factor
#   analysis for categorical data. You should use it when you want a general understanding of how categorical variables are related.
# - Correspondence analysis is a technique for summarizing relativities in tables. As tables are ubiquitous in data analysis,
#   it is a technique that can be used widely.
# The reason for the word "multiple" is that multiple correspondence can be applied to a table that has more than
# two dimensions (e.g., a cube), whereas correspondence analysis requires as an input a table with only two dimensions.

# Libraries ============================================================================================================
library(dplyr)
library(knitr)
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining with R
library(factoextra) # Print method for an object of class factoextra
library(gplots)
library(corrplot)

# Data =================================================================================================================
data(mpg) # Fuel economy data from 1999 to 2008 for 38 popular models of cars
kable(mpg)
# |manufacturer |model                  | displ| year| cyl|trans      |drv | cty| hwy|fl |class      |
# |:------------|:----------------------|-----:|----:|---:|:----------|:---|---:|---:|:--|:----------|
# |audi         |a4                     |   1.8| 1999|   4|auto(l5)   |f   |  18|  29|p  |compact    |
# |audi         |a4                     |   1.8| 1999|   4|manual(m5) |f   |  21|  29|p  |compact    |
# |audi         |a4                     |   2.0| 2008|   4|manual(m6) |f   |  20|  31|p  |compact    |
# |audi         |a4                     |   2.0| 2008|   4|auto(av)   |f   |  21|  30|p  |compact    |
# |audi         |a4                     |   2.8| 1999|   6|auto(l5)   |f   |  16|  26|p  |compact    |
# |audi         |a4                     |   2.8| 1999|   6|manual(m5) |f   |  18|  26|p  |compact    |
# |audi         |a4                     |   3.1| 2008|   6|auto(av)   |f   |  18|  27|p  |compact    |
# |audi         |a4 quattro             |   1.8| 1999|   4|manual(m5) |4   |  18|  26|p  |compact    |
# |audi         |a4 quattro             |   1.8| 1999|   4|auto(l5)   |4   |  16|  25|p  |compact    |
# |audi         |a4 quattro             |   2.0| 2008|   4|manual(m6) |4   |  20|  28|p  |compact    |
# |audi         |a4 quattro             |   2.0| 2008|   4|auto(s6)   |4   |  19|  27|p  |compact    |

mpg %>% str()
# tibble [234 × 11] (S3: tbl_df/tbl/data.frame)
# $ manufacturer: chr [1:234] "audi" "audi" "audi" "audi" ...                       # manufacturer name
# $ model       : chr [1:234] "a4" "a4" "a4" "a4" ...                               # model name
# $ displ       : num [1:234] 1.8 1.8 2 2 2.8 2.8 3.1 1.8 1.8 2 ...                 # engine displacement, in litres
# $ year        : int [1:234] 1999 1999 2008 2008 1999 1999 2008 1999 1999 2008 ... # year of manufacture
# $ cyl         : int [1:234] 4 4 4 4 6 6 6 4 4 4 ...                               # number of cylinders
# $ trans       : chr [1:234] "auto(l5)" "manual(m5)" "manual(m6)" "auto(av)" ...   # type of transmission
# $ drv         : chr [1:234] "f" "f" "f" "f" ...                                   # the type of drive train, where f = front-wheel drive, r = rear wheel drive, 4 = 4wd
# $ cty         : int [1:234] 18 21 20 21 16 18 18 18 16 20 ...                     # city miles per gallon
# $ hwy         : int [1:234] 29 29 31 30 26 26 27 26 25 28 ...                     # highway miles per gallon
# $ fl          : chr [1:234] "p" "p" "p" "p" ...                                   # fuel type
# $ class       : chr [1:234] "compact" "compact" "compact" "compact" ...           # "type" of car

data(poison)
poison %>% str()
# 'data.frame':	55 obs. of  15 variables:
# $ Age       : int  9 5 6 9 7 72 5 10 5 11 ...
# $ Time      : int  22 0 16 0 14 9 16 8 20 12 ...
# $ Sick      : Factor w/ 2 levels "Sick_n","Sick_y": 2 1 2 1 2 2 2 2 2 2 ...
# $ Sex       : Factor w/ 2 levels "F","M": 1 1 1 1 2 2 1 1 2 2 ...
# $ Nausea    : Factor w/ 2 levels "Nausea_n","Nausea_y": 2 1 1 1 1 1 1 2 2 1 ...
# $ Vomiting  : Factor w/ 2 levels "Vomit_n","Vomit_y": 1 1 2 1 2 1 2 2 1 2 ...
# $ Abdominals: Factor w/ 2 levels "Abdo_n","Abdo_y": 2 1 2 1 2 2 2 2 2 1 ...
# $ Fever     : Factor w/ 2 levels "Fever_n","Fever_y": 2 1 2 1 2 2 2 2 2 2 ...
# $ Diarrhae  : Factor w/ 2 levels "Diarrhea_n","Diarrhea_y": 2 1 2 1 2 2 2 2 2 2 ...
# $ Potato    : Factor w/ 2 levels "Potato_n","Potato_y": 2 2 2 2 2 2 2 2 2 2 ...
# $ Fish      : Factor w/ 2 levels "Fish_n","Fish_y": 2 2 2 2 2 1 2 2 2 2 ...
# $ Mayo      : Factor w/ 2 levels "Mayo_n","Mayo_y": 2 2 2 1 2 2 2 2 2 2 ...
# $ Courgette : Factor w/ 2 levels "Courg_n","Courg_y": 2 2 2 2 2 2 2 2 2 2 ...
# $ Cheese    : Factor w/ 2 levels "Cheese_n","Cheese_y": 2 1 2 2 2 2 2 2 2 2 ...
# $ Icecream  : Factor w/ 2 levels "Icecream_n","Icecream_y": 2 2 2 2 2 2 2 2 2 2 ...

# Preprocessing ========================================================================================================
mpg_prep <- mpg %>%
  transmute(
    manufacturer = factor(manufacturer, ordered = FALSE),
    displ = factor(
      case_when(displ < 2.0 ~ "<2.0",
                displ < 3.0 ~ "2.0<=x<3.0",
                .default = ">=3.0"),
      ordered = FALSE
    ),
    year = factor(
      case_when(year < 2000 ~ "old",
                year < 2015 ~ "modern",
                .default = "new"),
      ordered = FALSE
    ),
    cyl = factor(cyl, ordered = FALSE),
    # factor(ordered = TRUE) when categorical data are ordered (i.e. education: higher > middle > lower)
    # > factor(c(1,2,3), ordered=TRUE)
    # [1] 1 2 3
    # Levels: 1 < 2 < 3
    # > factor(c(1,2,3), ordered=FALSE)
    # [1] 1 2 3
    # Levels: 1 2 3
    trans = factor(case_when(grepl("auto", trans) ~ "auto",
                             .default = "manual"), ordered = FALSE),
    drv = factor(drv, ordered = FALSE),
    combustion = factor(
      case_when((cty + hwy) / 2 < 7 ~ "low",
                (cty + hwy) / 2 < 12 ~ "average",
                .default = "high"),
      ordered = FALSE
    ),
    fl = factor(fl, ordered = FALSE),
    class = factor(class, ordered = FALSE)
  ) %>%
  as.data.frame()

mpg_prep %>% str()
# 'data.frame':	234 obs. of  9 variables:
# $ manufacturer: Factor w/ 15 levels "audi","chevrolet",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ displ       : Factor w/ 3 levels "<2.0",">=3.0",..: 1 1 3 3 3 3 2 1 1 3 ...
# $ year        : Factor w/ 2 levels "modern","old": 2 2 1 1 2 2 1 2 2 1 ...
# $ cyl         : Factor w/ 4 levels "4","5","6","8": 1 1 1 1 3 3 3 1 1 1 ...
# $ trans       : Factor w/ 2 levels "auto","manual": 1 2 2 1 1 2 1 2 1 2 ...
# $ drv         : Factor w/ 3 levels "4","f","r": 2 2 2 2 2 2 2 1 1 1 ...
# $ combustion  : Factor w/ 2 levels "average","high": 2 2 2 2 2 2 2 2 2 2 ...
# $ fl          : Factor w/ 5 levels "c","d","e","p",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ class       : Factor w/ 7 levels "2seater","compact",..: 2 2 2 2 2 2 2 2 2 2 ...
sapply(mpg_prep, nlevels) %>% sum()

poison_prep <- poison %>% select(!c(Age, Time))
sapply(poison_prep, nlevels) %>% sum()

# Multiple correspondence Analysis and interpretation ==================================================================
# MCA model
res.mca <- MCA(mpg_prep, graph = FALSE)
res.mca$eig
# eigenvalue percentage of variance cumulative percentage of variance
# dim 1  0.446460617             11.8180752                          11.81808
# dim 2  0.258007994              6.8296234                          18.64770
# dim 3  0.238808217              6.3213940                          24.96909
# dim 4  0.228564258              6.0502304                          31.01932
# dim 5  0.209059192              5.5339198                          36.55324
# dim 6  0.188350723              4.9857544                          41.53900
# dim 7  0.175026501              4.6330544                          46.17205
# dim 8  0.160597998              4.2511235                          50.42318
# dim 9  0.145373328              3.8481175                          54.27129
# dim 10 0.144887692              3.8352624                          58.10655
# dim 11 0.129349818              3.4239658                          61.53052
# dim 12 0.127708255              3.3805126                          64.91103
# dim 13 0.119998131              3.1764211                          68.08745
# dim 14 0.114478380              3.0303101                          71.11776
# dim 15 0.113580325              3.0065380                          74.12430
# dim 16 0.110335551              2.9206469                          77.04495
# dim 17 0.103752375              2.7463864                          79.79134
# dim 18 0.091635723              2.4256515                          82.21699
# dim 19 0.086320935              2.2849659                          84.50195
# dim 20 0.082145019              2.1744270                          86.67638
# dim 21 0.076027658              2.0124968                          88.68888
# dim 22 0.066478171              1.7597163                          90.44859
# dim 23 0.059448146              1.5736274                          92.02222
# dim 24 0.051996221              1.3763706                          93.39859
# dim 25 0.045096466              1.1937300                          94.59232
# dim 26 0.044250751              1.1713434                          95.76366
# dim 27 0.036865197              0.9758434                          96.73951
# dim 28 0.029085220              0.7699029                          97.50941
# dim 29 0.024948321              0.6603967                          98.16981
# dim 30 0.020648321              0.5465732                          98.71638
# dim 31 0.018105379              0.4792600                          99.19564
# dim 32 0.014147246              0.3744859                          99.57013
# dim 33 0.008527964              0.2257402                          99.79587
# dim 34 0.007711683              0.2041328                         100.00000
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 15))

# draw the biplot of individuals and variable categories
fviz_mca_biplot(res.mca,
                repel = TRUE, # Avoid text overlapping (slow if many point
                ggtheme = theme_minimal())

# visualize the correlation between variables and MCA principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
# The plot above helps to identify variables that are the most correlated with each dimension. The squared correlations
# between variables and the dimensions are used as coordinates. It can be seen that, the variables cyl and displ are
# the most correlated with dimension 1. When the variables drv, class and manufacturer are correlated with dim 1 and dim 2.

# Contributions of rows to dimension 1
fviz_contrib(res.mca,
             choice = "var",
             axes = 1,
             top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.mca,
             choice = "var",
             axes = 2,
             top = 10)

res.mca <- MCA(poison_prep, graph = FALSE)
res.mca$eig
#        eigenvalue percentage of variance cumulative percentage of variance
# dim 1  0.35400698              35.400698                          35.40070
# dim 2  0.11321219              11.321219                          46.72192
# dim 3  0.09360912               9.360912                          56.08283
# dim 4  0.08218096               8.218096                          64.30093
# dim 5  0.07415156               7.415156                          71.71608
# dim 6  0.06628886               6.628886                          78.34497
# dim 7  0.05973746               5.973746                          84.31871
# dim 8  0.05264927               5.264927                          89.58364
# dim 9  0.04545157               4.545157                          94.12880
# dim 10 0.03461955               3.461955                          97.59075
# dim 11 0.01164482               1.164482                          98.75523
# dim 12 0.01037235               1.037235                          99.79247
# dim 13 0.00207531               0.207531                         100.00000
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 15))
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

