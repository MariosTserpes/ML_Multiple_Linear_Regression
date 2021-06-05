        ##### Tserpes Marios #####
#### Research Methods in Data Science #####
         #### Assignment 2 ####

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



#################
###Question 1.i####
#################

#Model estimation with Sales dependent and independent of the rest.
model1 <- lm(Sales~., data = Carseats)
print(model1)



#################
###Question 1.ii####
#################

#Summary of the results of model1
summary(model1)
print(anova(model1)) #analysis of variance table



#################
###Question 1.iii####
#################
#95% confidence interval for the coefficient estimates of the model1.
confint(model1, level = 0.95)



#################
###Question 1.iv####
#################
#prediction when the numeric variables are equal to their mean value
#and categorical variables equal to their prevailing value.
summary(Carseats)
predict(model1, data.frame(CompPrice = mean(Carseats$CompPrice),
                           Income = mean(Carseats$Income),
                           Advertising = mean(Carseats$Advertising),
                           Population = mean(Carseats$Population),
                           Price = mean(Carseats$Price),
                           Age = mean(Carseats$Age),
                           Education = mean(Carseats$Education),
                           ShelveLoc = 'Medium',
                           Urban = 'Yes',
                           US = 'Yes'))



#95% confidence interval for the above prediction
predict(model1, data.frame(CompPrice = mean(Carseats$CompPrice),
                           Income = mean(Carseats$Income),
                           Advertising = mean(Carseats$Advertising),
                           Population = mean(Carseats$Population),
                           Price = mean(Carseats$Price),
                           Age = mean(Carseats$Age),
                           Education = mean(Carseats$Education),
                           ShelveLoc = 'Medium',
                           Urban = 'Yes',
                           US = 'Yes'), interval = 'confidence')


#95%  prediction interval for the above prediction
predict(model1, data.frame(CompPrice = mean(Carseats$CompPrice),
                           Income = mean(Carseats$Income),
                           Advertising = mean(Carseats$Advertising),
                           Population = mean(Carseats$Population),
                           Price = mean(Carseats$Price),
                           Age = mean(Carseats$Age),
                           Education = mean(Carseats$Education),
                           ShelveLoc = 'Medium',
                           Urban = 'Yes',
                           US = 'Yes'), interval = 'prediction')




#################
###Question 1.v####
#################

#Backward stepwise procedure according to AIC
step.model2 <- stepAIC(model1, direction = 'backward')
step.model2$anova

summary(step.model2)

#Checking assumption about normality of the residuals of model2
names(step.model2)
#Distribution of residuals
hist(step.model2$residuals, main = 'distribution of residuals',
     xlab = 'residuals', ylab = 'frequency', labels = TRUE, 
     col = 'white')
#four plots in order to examine assumptions of the final model
par(mfrow = c(2, 2))
plot(step.model2)


#################
###Question 1.vi(a)####
#################

# Split the data into training and test set
set.seed(123)
training.samples <- Carseats$Sales %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data <- Carseats[training.samples, ]
test.data <- Carseats[-training.samples, ]

#Build with most significant predictors and target variable from step.model2
model3 <- update(step.model2, ~., data = train.data)
print(summary(model3))
anova(model3) #in order to take a look about SSRegression and SSResiduals.
summary(model3)$r.sq


#Predictions and compute R^2
predictions <- model3 %>% predict(test.data)
data.frame(R2 = R2(predictions, test.data$Sales),
           RMSE = RMSE(predictions, test.data$Sales))

#a data frame with 2 columns : 1)y_test 2)y_pred 
y.df <- data.frame(test.data$Sales, predictions)
plot(y.df$test.data.Sales, col = 'red', pch = '+',
     main = 'Sales in test set')
plot(y.df$predictions, col = 'blue', pch = '*', 
     main = 'Predictions')



#################
###Question 1.vi(b)####
#################



# 5-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = 'cv', number = 5)
#training the model
model4 <- train(Sales~.-Population-Education-Urban-US, data = Carseats,
                method = 'lm', trControl = train.control)
print(model4)
model4$results
model4$resample
