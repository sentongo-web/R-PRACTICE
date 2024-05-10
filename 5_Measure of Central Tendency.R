############## MEASURES OF CENTRAL TENDENCY AND DISPERSION ############
### 1. Described by Mode, Median and Mean
## 2. A form of surmising the data
## 3. Statistical tests for comparing means (central tendency) (T-test, Wilcoxon, ANOVA and Kruskal-Wallis test)
setwd()
library(tidyverse)
library(tidyr)
library(dplyr)
ggplot2::diamonds
### Calculating mean and weighted mean
# Consider the following example of a list of temperatures:
Temperature <- c(24, 26, 34, 27, 37, 22)
mean <- mean(Temperature)
# The above gives the arithmetic mean
#However, if we have class intervals of these temperatures e.g.(<20, 20-25, 25-30, 30-35, 35-40),
# And the frequencies of the temperatures, (Absolute frequencies: 0, 2, 2, 1, 1) and relative frequencies (0/6, 2/6, 2/6, 1/6, 1/6)
# The R function for calculating weighted means requires the middle values of the class intervals and relative frequencies
weightmean <- weighted.mean(c(22.5,27.5,32.5, 37.5),c(2/6, 2/6, 1/6, 1/6))

## Calculating Median ###
# Divides the observations into two equal parts such that at least 50% of the values >= to the median and 50% =< median
median <-median(Temperature)

#Comparing mean and median
# If mean = median, then we have normal distribution of data
?diamonds
#Calculating the mean and median of the variable "price" in the diamonds dataset
mean(diamonds$price)
median(diamonds$price)
# This can be recalculated removing any missing data
mean(diamonds$price, na.rm = TRUE)
median(diamonds$price, na.rm = TRUE)
# The results show that the mean > that median (they differ), showed skewdness in the data
#Therefore, you'd have to use the median and not the mean as it's sensitive to outliers

#Mode is not innately in R.
#So create a function for it.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
v <- diamonds$price
mode_price <- getmode(v)
mode_price

## 2. SURMISING THE DATA
#Using the variable "price" in the dataset diamonds
head(diamonds, 10)
#Statistical summary of "price"
Summ_price <- summary(diamonds$price)
Summ_price
# Min.: the minimum value
#1st Qu.: The first quartile. 25% of values are lower than this.
# Median: the median value. Half the values are lower; half are higher.
#3rd Qu.: the third quartile. 75% of values are higher than this.
#Max.: the maximum value


################### 3. STATISTICALLY TESTING MEANS  ##############
#####3.1. Comparing one-sample mean to a standard known mean
###3.1.1 One-sample T-test (parametric)
#What is one-sample t-test?
# Compares the mean of one sample to a known standard (or theoretical/hypothetical) mean (μ).

# Research questions and statistical hypotheses: 
#Is the mean (m) of the sample = theoretical mean (μ)?
#Is the mean (m) of the sample <  theoretical mean (μ)?
#Is the mean (m) of the sample > the theoretical mean (μ)?
# corresponding null hypothesis (H0) as follow:
#H0:m=μ
#H0:m≤μ
#H0:m≥μ
# corresponding alternative hypotheses (Ha) are as follow:
#Ha:m≠μ (different)
#Ha:m>μ (greater)
#Ha:m<μ (less)
### Hypotheses 1) are called two-tailed tests
### Hypotheses 2) and 3) are called one-tailed tests

####Formula of one-sample t-test
# If p-value =< 0.05, reject the null hypothesis and accept the alternative hypothesis

# Visualize data and compute one-sample t-test in R
#R function to compute one-sample t-test
install.packages("ggpubr")
library(ggpubr)
if(!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("kassambara/ggpubr")
library(devtools)

###Preliminary test to check one-sample t-test assumptions
# Visualize data using box plots
ggboxplot(diamonds$price, 
          ylab = "Price", xlab = FALSE,
          ggtheme = theme_minimal())

# Check to see if the data is normally distributed using the Shapiro-Wilk test (For sample size between 3 and 5000)
#Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed
# p-value =<0.05 are not normally distributed, 
#p-value >= 0.05 are normally distributed
shapiro.test(diamonds$price)
#Since the diamonds dataset is over 5000 observations, we can use the mpg dataset
ggplot2::mpg
?mpg # Looking at the variable "cty" the city miles per gallon
ggboxplot(mpg$cty, 
          ylab = "City Miles Per Gallon", xlab = FALSE,
          ggtheme = theme_minimal())
shapiro.test(mpg$cty)
# The p-value is (1.74e-6) which is much less than 0.05. Hence data is not normally distributed
mean(mpg$cty)
median(mpg$cty)
# Distribution can also be visualised using a  Q-Q plots (quantile-quantile plots). 
#Q-Q plots draw the correlation between a given sample and the normal distribution.
ggqqplot(mpg$cty, ylab = "Miles per gallon",
         ggtheme = theme_minimal())
# The data is not normally distributed, it’s recommended to use the non parametric one-sample Wilcoxon rank test.



###Compute one-sample t-test
# We want to know, if the average miles per gallon differs from the expected average (two-tailed test)?
T_test <- t.test(mpg$cty, mu = 25)
T_test
# t is the t-test statistic value (t = -29.078),
#df is the degrees of freedom (df= 233),
#p-value is the significance level of the t-test (p-value < 7.95310^{-6}).
#conf.int is the confidence interval of the mean at 95% (conf.int = [17.8172, 20.6828]);
#sample estimates is he mean value of the sample (mean = 16.86).

###Interpretation of the result
### print the p-value
res$p.value
### print the mean
res$estimate
# print the confidence interval
res$conf.int


