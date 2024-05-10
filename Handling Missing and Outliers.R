## ***HANDLING MISSING DATA AND OUTLIERS ** ##

#Set working directory
setwd("E:/Documents/Personal Learning/Data Science 2022/Test Data")
#Import dataset
Data=read.csv("E:/Documents/Personal Learning/Data Science 2022/Test Data/wn_bothhouses_2.csv")

#libraries
library(readxl)
library(tidyverse)
library(dplyr)

#Renaming a variable country to countries
# Load library
library("dplyr")
Data %>% 
  rename("countries" = "country")

#This won't remain permanently unless you generate it as a different dataframe
Data = Data %>% 
  rename("countries" = "country")

#Generating a correlation matrix for the years 1945-2021
install.packages("Hmisc")
library("Hmisc")
#First generate the correlation scores
#After removing the "character" column countries
Data2 <- Data[,-1]
mydata.cor = cor(Data2, method = c("spearman"))

#The generate the correlative matrix
mydata.rcorr = rcorr(as.matrix(Data2))
mydata.rcorr

#Generate a correlation table with p-values
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

#Generate a correlation plot
install.packages("corrplot")
library(corrplot)

#A correlogram
corrplot(mydata.cor)

#Or
corrplot(cor(Data2))

# 1. Show how you would sort the dataset to exclude missing information.
# First view the dataset for missing values
sum(is.na (Data)) 

#HANDLING MISSING DATA ######
#Replace the missing values for continuous data using;
#1. Omit missing values if not more than 30% of dataset
Data3 = na.omit(Data)

#2. Mean (if normally distributed)
Data$X1945[is.na(Data$X1945)] <- mean(Data$X1945, na.rm = TRUE)
head(Data)

#To replace each column of missing data with mean., use a dataset excluding the character variables
install.packages("zoo")                                    
library("zoo")
Data4 <- na.aggregate(Data2)
head(Data4)

#3. Median (if not normally distributed)
Data$X1946[is.na(Data$X1946)] <- median(Data$X1946, na.rm = TRUE)
head(Data)
# Imputing with median across all columns
Data %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))



###HANDLING OUTLIERS/ UNUSUAL DATAPOINTS####
#Detect outliers
outlier_values <- boxplot.stats(Data$X1945)$out  # outlier values in 1945
boxplot(Data$X1945, main="Representation in 1945", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Outliers for the entire dataset
All_outliers <- boxplot.stats(Data2)$out
boxplot(Data2, main="Representation from 1945-2021", boxwex=0.1)
mtext(paste("Outliers: ", paste(All_outliers, collapse=", ")), cex=0.6)

#Detect using histograms and Q-Q plots
hist(Data$X1945, main = "Histogram")
qqnorm(Data$X1945, main = "Normal Q-Q plot")

#Treating outliers using mean and SD
# get mean and Standard deviation
mean = mean(Data$X1945)
std = sd(Data$X1945)

# get threshold values for outliers
Tmin = mean-(3*std)
Tmax = mean+(3*std)

# find outliers
Data$X1945[which(Data$X1945 < Tmin | Data$X1945 > Tmax)]

# remove outliers
Data$X1945[which(Data$X1945 > Tmin & Data$X1945 < Tmax)]

#Visualise the boxplot after outlier removal
boxplot(Data$X1945, main="Representation in 1945", boxwex=0.1)

### Removing outliers using IQR (Interquartile range)
# get values of Q1, Q3, 
summary(Data$X1945)

# get IQR
IQR(Data$X1945)
#Result 0

# get threshold values for outliers
Tmin = 2.874-(1.5*5.5) 
Tmax = 2.874+(1.5*5.5) 

# find outliers
Data$X1945[which(Data$X1945 < Tmin | Data$X1945 > Tmax)]

# remove outliers
Data$X1945[which(Data$X1945 > Tmin & Data$X1945 < Tmax)]

#Visualise the boxplot
boxplot(Data$X1945)
