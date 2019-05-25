setwd("~/Google Drive/Data Analytics/Predictive HR Analytics/People-Analytics-Project/06 - Training Analytics")

# Libraries
#install.packages("interplot")
library(readxl)
library(car)
library(tidyverse)
library(ggthemes)
library(interplot)

# 1. Choose Training Type A or Training Type B ----

# 1.1 Load Data ----

training_data <- read_excel("New Starter Training.xlsx")
glimpse(training_data)

names(training_data)
attach(training_data)


# 1.2 Conducting Levene's test to test equality of variances ----

# One important assumption of the Independent-Samples t Test is that the variances in the 
# sample groups are approximately equal or that the samples have homogeneity of variance. 
# Levene’s Test for Equality of Variances is a test of whether the variances of the 
# two samples/groups are approximately equal. 
# Null Hypothesis : There is no difference in the variance between Training methods

#Levene's needs Training method to be a factor variable

Method <- factor(`Training Method`,
                      labels = c("Training Method A", "Training Method B"))
Freq1=table(Method)
Freq1

leveneTest(`Test Scores`, group = Method, center = mean)

# If you leave out the center=mean it will use the median which is also known as 
# the Brown-Forsythe test for variances

leveneTest(`Test Scores`, group = Method)

# 1.3 Conducting Independent T Test - Equality of Means ----

# Assuming variances are equal based on Levene's Test
# Null Hypothesis : Their is no difference in mean scores from the 2 training methods

t.test(`Test Scores`~`Training Method`, var.equal=TRUE)


attach(training_data)

# 2. Trainings Impact on Performance based on Gender ----

Train_Intervention <-read.table("TrainingIntervention.txt",header=T,sep="\t")
glimpse(Train_Intervention)


# 2.1 Create a regression Model ----

model1 =lm(ScanPminuteTime2 ~ educ + disability + Gender + Store + Weeks 
              + age1 + age2 + age4 + age5 + age6 + age7 + age8 + age9 + age10 + Training + ScanPminute,data = Train_Intervention)

model1
anova(model1)
summary(model1)
coef(model1)

# 2.2 Convert Gender values to a different value label ----

Train_Intervention$Gender = factor(Train_Intervention$Gender,
                                   levels = c(1, 2),
                                   labels = c("Male","Female"))

# 2.3 Run a model with interaction variable of Training ----

model2 = lm(ScanPminuteTime2 ~ Training*Gender,data = Train_Intervention)
summary(model2)

#interplot(model2,var1 = "Training",var2 = "Gender") +theme_few()

# 2.4 Run a standard two-way interaction Plot ----

library(sjPlot)
library(sjmisc)
library(ggplot2)

plot1 <- plot_model(model = model2,type = "int",axis.title = "Scan Rates After Training Event",
                    title = "Employees' check out performance with or Without Training") + 
                    theme(plot.title = element_text(hjust = 0.5))

plot1 + labs(x = "Training -  0 : Not Taken , 1 : Taken")



# References

# https://www.youtube.com/watch?v=4mkEZxgxMRA

# https://www.youtube.com/watch?v=GLbCuaI8SOQ


