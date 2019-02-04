#Chapter 4Diversity CASE Group level data (1).sav
# https://www.youtube.com/watch?v=G-hOmmtIf90

# Load the libraries
setwd("~/Google Drive/Data Analytics/Predictive HR Analytics/Diversity Analysis")
#install.packages("corrplot")
#install.packages("ggpubr")
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)
library(caret)
library(skimr)
library(ggplot2)
library(ggthemes) 
library(gridExtra) 
library(vcd)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(UsingR)
library(GGally)
library(foreign)

# Load the MASS library
library(MASS)
library(car)

#1.Read SPSS File ----

dataset <- read.spss("ethnicdiversitydata.sav",to.data.frame = TRUE)
summary(dataset)
glimpse(dataset)


####
#2.Determine Predictor Variables ----
# Create a list of interesting predictor variables
####

# How do I restrict my data frame to just these columns that interest me?
# Make a vector of the names;

keep.vars <- c('LondonorNot','Function','GroupSize','NumberFeMaleTeamLeads','PercentMale','BAME');


# Note that the R data frame is a (rectangular) list object which means that it can be 
# accessed in two ways - as a matrix or as a list;
# Note that the keep.vars are the COLUMNS that we want to keep, not the rows;

skinny.df <- dataset[,keep.vars];
glimpse(skinny.df)


####
# 3 Omit Missing Values ----
# Delete observations with missing values
####
sample.df <- na.omit(skinny.df);
summary(sample.df)

####
# 4 Train and Test Set ----
####
set.seed(123)
sample.df$u <- runif(n=dim(sample.df)[1],min=0,max=1);
train.df <- subset(sample.df,u<0.70);
test.df  <- subset(sample.df,u>=0.70);


# Check the data

dim(sample.df)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1] + dim(test.df)[1]


# 5 Fit a MLR model ----
model <- lm(BAME ~ ., data=train.df)

# Display model summary
summary(model)
# London and Professional Service Function are key drivers in variation of BAME


# Panel the plots
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
plot(model)
par(mfrow = c(1, 1), oma = c(0, 0, 2, 0))

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model$residuals)
qqline(model$residuals)

# Make a scatterplot
plot(train.df$BAME,model$residuals)
title('Residual vs Predictor')

# Anova Test

P <- aov(BAME ~ .,data = train.df)
summary(P)




# 6 AIC  ----
Measurement <- data.frame(AIC(model))

# 7 MSE and MAE ----

Measurement$MSE <- mean(model$residuals^2)
Measurement$MAE <- mean(abs(model$residuals))

# 7 BIC ----

Measurement$BIC <- BIC(model)


# 8 Adjusted R Square ----

Measurement$Rsquare  <- as.vector(summary(model)$adj.r.squared)
Measurement


# 9 Predictive Accuracy ----

test  <- predict(model,newdata = test.df)

# 10. Mean Square Error (MSE) and Mean Absolute Error (MAE) ----
# calculated as the mean of (Predicted Values - Observed Values)^2? 
#The observed values here are the response variable from the testing dataset.


Measurement$MSETest <- mean((test.df$BAME - predict(model, test.df)) ^ 2)
Measurement$MAETest <- mean(abs((test.df$BAME - predict(model, test.df)) ))
Measurement

#10. How do the 2 significant predictors impact BAME on a plot

dataset %>% 
  dplyr::select(BAME,LondonorNot,Function)%>%
    ggpairs()




