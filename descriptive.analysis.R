##### Tserpes Marios #####
#### Research Methods in Data Science #####
#### Assignment 2 (Part1)####

#Load necessary libraries
library(ISLR)
library(MASS)
library(car)          
library(tidyverse)          
library(caret)          


#Understanding the structure of dataset
attach(Carseats)
?Carseats
View(Carseats)          
str(Carseats)
sapply(Carseats, class)



#descriptive analysis and visualization to get an initial insight about data
#For the numeric variables
require(psych)
index <- sapply(Carseats, class) == 'numeric'
numeric.columns <- Carseats[ , index]
summary(numeric.columns)

round(t(describe(numeric.columns)), 3) #descriptive statistics



#visualizing numeric columns distribution
hist(Carseats$Sales, main = 'Unit sales(in thousands) at each location',
     xlab = 'Sales in thousands', labels = TRUE)
hist(Carseats$CompPrice, main = 'Price by competitor',
     xlab = 'CompPrice', col = 'white', labels = TRUE)
hist(Carseats$Income, main = 'Community income level (in thousands of dollars)',
     xlab = 'Income', col = 'white', labels = TRUE)
hist(Carseats$Advertising, main = 'Advertising(in thousand dollars)',
     xlab = 'Advertising(in thousand dollars)', col = 'white', labels = TRUE) #does not approach normal distribution 
hist(Carseats$Population, main = 'Population size in region (in thousands)',
     xlab = 'Population', col = 'white', labels = TRUE)
hist(Carseats$Price, main = 'Price for carseats in each site',
     xlab = 'Price', col = 'white', labels = TRUE)
hist(Carseats$Age, main = 'Average age of the local population',
     xlab = 'Age', col = 'white', labels = TRUE)
hist(Carseats$Education, main = 'Education level at each location',
     xlab = 'Education', col = 'white', labels = TRUE)



#Descriptive analysis and visualizations for the factors 

factor.columns <- Carseats[ , !index] #three variables 
summary(factor.columns)


#Visualizing factor columns
plot(factor.columns$ShelveLoc, col = 'red',
     main = 'A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site',
     xlab = 'Shelving Location') #most frequent 'medium'

plot(factor.columns$Urban, col = 'red',
     main = 'Indicate whether the store is in an urban or rural location',
     xlab = 'Urban') #most frequent 'Yes'

plot(factor.columns$US, col = 'red',
     main = 'Indicate whether the store is in the US or not',
     xlab = 'Us') #most frequent 'yes'

