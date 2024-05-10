########COVARIANCE AND CORRELATION###############
#Relation between variables
#Both used in regression analysis
#One shows how variables differ
#Later shows how variables are related

####1. COVARIANCE (DIFFER) #######
#Measure of how two random variables vary together.
#Measure of change in one variable associated to change in another variable

##Measuring  Covariance by Hand using two vectors; x and y###
x <- c(25, 27, 29)
y <- c(5, 15, 9)
#Calculate the difference of each value and the mean for the variables:
xdev <- x - mean(x)
ydev <- y - mean(y)

#Calculate the product of the above differences:
xdev_ydev <- xdev * ydev
xdev_ydev

# Find the sum of the products
sum_xdev_ydev <- sum(xdev_ydev)

#complete the covariance calculation by dividing the numerator by the denominator
cov_xy <- sum_xdev_ydev / (3 - 1)
cov_xy

##Even shorter to compute covariance in R ###
cov(x, y)

##1.2. Computing covariance in the diamonds dataset##
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
install.packages("car")
install.packages("rstatix")
install.packages("ggpubr")
library(car)
library(rstatix)
library(ggpubr)
ggplot2::diamonds
?diamonds

##1.2.1 Covariance between two continuous variables ##
## E.g. price and width of the diamonds
cov (diamonds$price, diamonds$y)
#Result = 3943.271
#he cov() function requires use="complete.obs" to remove NA entries.
cov (diamonds$price, diamonds$y, use = "complete.obs")
# The covariance of price and diamond width is positive, 
#indicating the two variables are positively related/ varying over time.
#Therefore, as width increases, price also increases

##1.2.2 Covariance between a continuous and categorical variables#
#E.g. price and diamond cut
cov (diamonds$price, diamonds$cut, use = "complete.obs")
# Result negative
#To compare strengths of association, covariance is standardized by running correlations.

##### 2. CORRELATION (strength of relationship)
#Correlation standardizes covariance on a scale of -1 to +1, 
#whereby the magnitude from zero indicates strength of relationship
#r<0.3, weak correlation
#0.3<r<0.7, moderate correlation
#r>0.7, high correlation

#####2.1. Correlation by Hand##
#The product of the standard deviations of variables x and y for the denominator:
stnd.dev <- sd(x)*sd(y)
#find the quotient of the covariance numerator and standard deviations denominator:
cov_xy/stnd.dev

####2.2. Correlation Tests in R: ##
#Computing correlation between price and width
cor(diamonds$price, diamonds$y, use = "complete.obs")
#Result = 0.8654209, strong positive correlation.
#However, this leads to a new hypothesis/question: 
#is there a relationship between price and width? 
#To test this we employ Pearson’s product-moment correlation available via the cor.test() function. 
#Our testing hypotheses are:
#-H0: the true correlation coefficient is zero
#-H1: the true correlation coefficient is not zero
cor.test(diamonds$price, diamonds$y, use="complete.obs")
#The correlation test yields a p-value < α = 0.05, 
#thereby the null hypothesis is rejected such that the true correlation coefficient is not zero

#Visualise the relationship between price and width
ggplot(diamonds, aes(x = y, y = price)) +
  geom_point(shape = 1)
#OR
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = y, y = price))
#OR
ggplot(data = diamonds) +
  geom_smooth(mapping = aes(x = y, y = price))
#The result shows an overlap of points

#Use the geom_jitter function instead of the geom_point function to avoid overlap
ggplot(diamonds, aes(x = y, y = price)) +
  geom_jitter(shape = 1)
#OR
ggplot(diamonds, aes(x = price, y = y)) +
  geom_jitter(shape = 1)
#The result shows some correlation between price and width
#Verify this by checking the correlation:
diamonds %>%
  dplyr::select(price, y) %>%
  cor(use = "complete.obs")

#Add a trendline to determine the direction of correlation
ggplot(diamonds, aes(x = y, y = price)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  geom_jitter(shape = 1)
#method "lm" (linear model) draws a regression line for the data points
#There's also the "loess" (local regression) method that uses local fitting to fit a regression model to a dataset
ggplot(diamonds, aes(x = y, y = price)) +
  geom_point(shape = 1) +
  geom_smooth(method = loess) +
  geom_jitter(shape = 1)
#Note:Loess is a non-parametric approach used for data that isn't normally distributed.

#Correlation can be further differentiated by other variables, 
# such as cut, to examine potential difference amongst diamond cuts
ggplot(diamonds, aes(x = y, y = price, color = cut)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  geom_jitter(shape = 1)
#The "cut" variable is now the trendline.

#2.2.1 Correlation between a continuous variables##
#Use the Pearson’s correlation coefficient
view(diamonds)



##2.2.2. Correlation between a categorical and continuous variable
#Use a point biserial correlation
#Only if:
#correlation between a binary categorical variable (a variable that can only take on two values) 
#and a continuous variable and has the following properties:
#Point biserial correlation can range between -1 and 1.
#For each group created by the binary variable, it is assumed that the continuous variable is normally distributed with equal variances.
#For each group created by the binary variable, it is assumed that there are no extreme outliers.
#For instance, a class of student grades for two genders
#define values for gender
gender <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

#define values for score
score <- c(77, 78, 79, 79, 82, 84, 85, 88, 89, 91, 91, 94,
           84, 84, 84, 85, 85, 86, 86, 86, 89, 91, 94, 98)

#calculate point-biserial correlation
cor.test(gender, score)
# cor=0.2810996, p-value= 0.1833. Positive correlation 



##2.2.3. Correlation between a categorical variables
#a) Pearson's Chi-square test (X2)
chisq.test(table(Class,Rating))$statistic

#b)Cramer’s V
#Overcomes issues of sample size with Pearson's X2
install.packages("vcd")
library(vcd)
assocstats(xtabs(∼Class+Rating))

#Bonferroni correction


