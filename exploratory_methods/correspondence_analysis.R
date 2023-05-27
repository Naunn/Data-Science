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

# Preprocessing ========================================================================================================
mpg_prep <- mpg %>%
  transmute(
    manufacturer,
    displ = case_when(displ < 2.0 ~ "<2.0",
                      displ < 3.0 ~ "2.0<=x<3.0",
                      .default = ">=3.0"),
    year = case_when(year < 2000 ~ "old",
                     year < 2015 ~ "modern",
                     .default = "new"),
    cyl = as.character(cyl),
    # as.factor() when categorical data are ordered (i.e. education: higher > middle > lower)
    trans = case_when(grepl("auto", trans) ~ "auto",
                      .default = "manual"),
    drv,
    combustion = case_when((cty + hwy) / 2 < 7 ~ "low",
                           (cty + hwy) / 2 < 12 ~ "average",
                           .default = "high"),
    fl,
    class
  ) %>%
  as.data.frame()

mpg_prep %>% str()
# 'data.frame':	234 obs. of  9 variables:
# $ manufacturer: chr  "audi" "audi" "audi" "audi" ...
# $ displ       : chr  "<2.0" "<2.0" "2.0<=x<3.0" "2.0<=x<3.0" ...
# $ year        : chr  "old" "old" "modern" "modern" ...
# $ cyl         : chr  "4" "4" "4" "4" ...
# $ trans       : chr  "auto" "manual" "manual" "auto" ...
# $ drv         : chr  "f" "f" "f" "f" ...
# $ combustion  : chr  "high" "high" "high" "high" ...
# $ fl          : chr  "p" "p" "p" "p" ...
# $ class       : chr  "compact" "compact" "compact" "compact" ...

mpg_cont <- table(mpg_prep$manufacturer, mpg_prep$trans) 
mpg_cont %>% kable()
# |           | auto| manual|
# |:----------|----:|------:|
# |audi       |   11|      7|
# |chevrolet  |   16|      3|
# |dodge      |   30|      7|
# |ford       |   17|      8|
# |honda      |    4|      5|
# |hyundai    |    7|      7|
# |jeep       |    8|      0|
# |land rover |    4|      0|
# |lincoln    |    3|      0|
# |mercury    |    4|      0|
# |nissan     |    8|      5|
# |pontiac    |    5|      0|
# |subaru     |    7|      7|
# |toyota     |   20|     14|
# |volkswagen |   13|     14|

mpg_cont %>% kable()
# |           |      auto|    manual|
# |:----------|---------:|---------:|
# |audi       | 0.0470085| 0.0299145|
# |chevrolet  | 0.0683761| 0.0128205|
# |dodge      | 0.1282051| 0.0299145|
# |ford       | 0.0726496| 0.0341880|
# |honda      | 0.0170940| 0.0213675|
# |hyundai    | 0.0299145| 0.0299145|
# |jeep       | 0.0341880| 0.0000000|
# |land rover | 0.0170940| 0.0000000|
# |lincoln    | 0.0128205| 0.0000000|
# |mercury    | 0.0170940| 0.0000000|
# |nissan     | 0.0341880| 0.0213675|
# |pontiac    | 0.0213675| 0.0000000|
# |subaru     | 0.0299145| 0.0299145|
# |toyota     | 0.0854701| 0.0598291|
# |volkswagen | 0.0555556| 0.0598291|

# Visualization ========================================================================================================
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

fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)
# Eigen values
get_eigenvalue(res.ca)
#       eigenvalue variance.percent cumulative.variance.percent
# Dim.1  0.5428893         48.69222                    48.69222 # (relatively) high variance.percentage of 1 dimension
# Dim.2  0.4450028         39.91269                    88.60491 # (relatively) high variance.percentage of 2 dimension
# Dim.3  0.1270484         11.39509                   100.00000
fviz_eig(res.ca)









