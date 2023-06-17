# ANOVA ================================================================================================================
# Analysis of variance (ANOVA) is a collection of statistical models and their associated estimation procedures
# (such as the "variation" among and between groups) used to analyze the differences among means. ANOVA was developed
# by the statistician Ronald Fisher. ANOVA is based on the law of total variance, where the observed variance in a
# particular variable is partitioned into components attributable to different sources of variation. In its simplest
# form, ANOVA provides a statistical test of whether two or more population means are equal, and therefore generalizes
# the t-test beyond two means. In other words, the ANOVA is used to test the difference between two or more means.
# ANOVA attempt to express one dependent variable as a linear combination of other features or measurements, also
# it uses categorical independent variables and a continuous dependent variable,

# The ANOVA is used to compare groups (in practice, 3 or more groups). More generally, it is used to:
# - study whether measurements are similar across different modalities (also called levels or treatments in the context
#   of ANOVA) of a categorical variable,
# - compare the impact of the different levels of a categorical variable on a quantitative variable,
# - explain a quantitative variable based on a qualitative variable.

# The assumptions of the ANOVA are:
# - Variable type: ANOVA requires a mix of one continuous quantitative dependent variable (which corresponds to the
#   measurements to which the question relates) and one qualitative independent variable (with at least 2 levels which
#   will determine the groups to compare).
# - Independence: the data, collected from a representative and randomly selected portion of the total population, should be
#   independent between groups and within each group.
# - Normality:
#   i) In case of small samples, residuals should follow approximately a normal distribution.
#   ii) In case of large samples, normality is not required (by the central limit theorem).
# - Equality of variances: the variances of the different groups should be equal in the populations.
# - Outliers: There should be no significant outliers in the different groups, or the conclusions of your ANOVA may be flawed.

# Libraries ============================================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) # qqPlot(); leveneTest()
library(multcomp) # glht()

# Data =================================================================================================================
data("penguins", package = "palmerpenguins")

penguins %>% str()
# tibble [344 × 8] (S3: tbl_df/tbl/data.frame)
# $ species          : Factor w/ 3 levels "Adelie","Chinstrap",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ island           : Factor w/ 3 levels "Biscoe","Dream",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ bill_length_mm   : num [1:344] 39.1 39.5 40.3 NA 36.7 39.3 38.9 39.2 34.1 42 ...
# $ bill_depth_mm    : num [1:344] 18.7 17.4 18 NA 19.3 20.6 17.8 19.6 18.1 20.2 ...
# $ flipper_length_mm: int [1:344] 181 186 195 NA 193 190 181 195 193 190 ...
# $ body_mass_g      : int [1:344] 3750 3800 3250 NA 3450 3650 3625 4675 3475 4250 ...
# $ sex              : Factor w/ 2 levels "female","male": 2 1 1 NA 1 2 1 2 NA NA ...
# $ year             : int [1:344] 2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...

penguins %>% is.na() %>% colSums()
# species            island    bill_length_mm     bill_depth_mm flipper_length_mm
#       0                 0                 2                 2                 2
# body_mass_g               sex              year
#           2                11                 0

df <- penguins %>% select(species, flipper_length_mm) %>% drop_na()

# ANOVA analysis =======================================================================================================
## One-way ANOVA
res_aov <- aov(flipper_length_mm ~ species, data = df)
res_aov
# Call:
#   aov(formula = flipper_length_mm ~ species, data = df)
#
# Terms:
#   species Residuals
# Sum of Squares  52473.28  14953.26
# Deg. of Freedom        2       339
#
# Residual standard error: 6.641529
# Estimated effects may be unbalanced

## Check the normality of residuals
# The null-hypothesis of this test is that the population is normally distributed.
shapiro.test(res_aov$residuals)
# Shapiro-Wilk normality test
# data:  res_aov$residuals
# W = 0.99452, p-value = 0.2609
# vizualization
hist(res_aov$residuals, breaks = 20)
qqPlot(res_aov$residuals, id = FALSE) # id = FALSE to remove point identification)

## Check the similarity of variances
# The null-hypothesis of this test is that the variances are equal.
# (alternative - at least one variance is different)
leveneTest(flipper_length_mm ~ species, data = df)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   2  0.3306 0.7188
#       339
# vizualization
boxplot(flipper_length_mm ~ species, data = df) # no significant outliers

## Model
aggregate(flipper_length_mm ~ species,
          data = df,
          function(x)
            round(c(mean = mean(x), sd = sd(x)), 2))
#     species flipper_length_mm.mean flipper_length_mm.sd
# 1    Adelie                 189.95                 6.54
# 2 Chinstrap                 195.82                 7.13
# 3    Gentoo                 217.19                 6.48

summary(res_aov)
#              Df Sum Sq Mean Sq F value Pr(>F)
# species       2  52473   26237   594.8 <2e-16 *** # <- reject the null hypothesis that all groups are equal
# Residuals   339  14953      44
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Alternative model
oneway.test(flipper_length_mm ~ species,
            data = df,
            var.equal = TRUE) # assuming equal variances
# One-way analysis of means
# data:  flipper_length_mm and species
# F = 594.8, num df = 2, denom df = 339, p-value < 2.2e-16

## Welch ANOVA
oneway.test(flipper_length_mm ~ species,
            data = df,
            var.equal = FALSE) # assuming unequal variances
# One-way analysis of means (not assuming equal variances)
# data:  flipper_length_mm and species
# F = 614.01, num df = 2.00, denom df = 172.76, p-value < 2.2e-16

# How to tell which group is different from who?
# Tukey HSD - used to compare all groups to each other (so all possible comparisons of 2 groups).
# Dunnett - used to make comparisons with a reference group. For example, consider 2 treatment groups and one control group. 
#   If you only want to compare the 2 treatment groups with respect to the control group, and you do not want to compare 
#   the 2 treatment groups to each other, the Dunnett’s test is preferred.

## Tukey HSD
post_test <- glht(res_aov, linfct = mcp(species = "Tukey"))
summary(post_test)
# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: Tukey Contrasts
# 
# Fit: aov(formula = flipper_length_mm ~ species, data = df)
# 
# Linear Hypotheses:
#                         Estimate Std. Error t value Pr(>|t|)    
# Chinstrap - Adelie == 0   5.8699     0.9699   6.052   <1e-08 ***
# Gentoo - Adelie == 0     27.2333     0.8067  33.760   <1e-08 ***
# Gentoo - Chinstrap == 0  21.3635     1.0036  21.286   <1e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)
plot(post_test)

# alternative
TukeyHSD(res_aov) 
plot(TukeyHSD(res_aov))

## Dunnett
post_test <- glht(res_aov, linfct = mcp(species = "Dunnett"))
summary(post_test)
# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: Dunnett Contrasts
# 
# Fit: aov(formula = flipper_length_mm ~ species, data = df)
# 
# Linear Hypotheses:
#                         Estimate Std. Error t value Pr(>|t|)    
# Chinstrap - Adelie == 0   5.8699     0.9699   6.052 7.59e-09 ***
# Gentoo - Adelie == 0     27.2333     0.8067  33.760  < 1e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)
