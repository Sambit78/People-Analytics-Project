# Set up Directory

setwd("~/Google Drive/Data Analytics/Predictive HR Analytics/People-Analytics-Project/05 - Recruiting Bias Analysis")


# 1. Loading Libraries , data files  ----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)
# install.packages('caTools')
library(caTools)
library(ROCR) # for the ROC/ AUC measure
library(pROC) # for the ROC/ AUC measure
library(rattle) # Visualization of Decision Trees
library(rpart.plot)
library(RColorBrewer)
library(psych) # for point biserial correlation
library("caret")
library("GGally")
library(janitor)
#install.packages("corrplot")
library(corrplot)
#install.packages('e1071', dependencies=TRUE)
#install.packages("car")
library(car)
#install.packages("expss")
library(expss)

# Load Data

my_data <- read.delim("Chapter8APPLICANTS.txt")
glimpse(my_data)

# 2 Descriptive Statistics ----

# 2.1 Applicant Pool by Gender ----

my_data %>%
  select(ApplicantCode,Gender)%>%
  group_by(Gender) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(Percent = n / sum(n)*100) %>%
    mutate(Gender = factor(Gender, labels = c("Male","Female"))) %>%
    adorn_totals("row")


# 2.2. Applicant Pool by Aboriginal Torres Strait Islander Yes or No ----

my_data %>%
  select(ApplicantCode,ATSIyn)%>%
  group_by(ATSIyn) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Percent = n / sum(n)*100) %>%
  mutate(ATSIyn = factor(ATSIyn, labels = c("Aboriginal Torres Strait Islander","General")))%>%
  adorn_totals("row")


# 2.3. Applicants Rejected or Shortlisted ----

my_data %>%
  select(ApplicantCode,ShortlistedNY)%>%
  group_by(ShortlistedNY) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Percent = n / sum(n)*100) %>%
  adorn_totals("row")

# 2.4. Applicants Interviewed ----

my_data %>%
  select(ApplicantCode,Interviewed)%>%
  group_by(Interviewed) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Percent = n / sum(n)*100) %>%
  mutate(Interviewed = factor(Interviewed, labels = c("Not Interviewed","Interviewed")))%>%
  adorn_totals("row")

# 2.5. Female Member on the interview panel ----

my_data %>%
  select(ApplicantCode,FemaleONpanel)%>%
  na.omit()%>%
  group_by(FemaleONpanel) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Percent = n / sum(n)*100) %>%
  mutate(FemaleONpanel = factor(FemaleONpanel, labels = c("Male Only","Female Panel member")))%>%
  adorn_totals("row")

# 2.6. Made an Offer ----

my_data %>%
  select(ApplicantCode,OfferNY)%>%
  na.omit()%>%
  group_by(OfferNY) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Percent = n / sum(n)*100) %>%
  mutate(OfferNY = factor(OfferNY, labels = c("Offer Not Made","Offer Made")))%>%
  adorn_totals("row")

# 2.7. Accepted ----

my_data %>%
  select(ApplicantCode,AcceptNY)%>%
  na.omit()%>%
  group_by(AcceptNY) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Percent = n / sum(n)*100) %>%
  mutate(AcceptNY = factor(AcceptNY, labels = c("Declined","Accepted")))%>%
  adorn_totals("row")

# 2.8. Joined or Not  ----

my_data %>%
  select(ApplicantCode,JoinYN)%>%
  na.omit()%>%
  group_by(JoinYN) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Percent = n / sum(n)*100) %>%
  mutate(JoinYN = factor(JoinYN, labels = c("Not Joined","Joined")))%>%
  adorn_totals("row")

# 3 Influence of ATSI on applicants shortlisting ----
# Creating crosstab table 

tbl = table(factor(my_data$ATSIyn,labels = c("Aboriginal Torres Strait Islander","General Applicant")),factor(my_data$ShortlistedNY,
                                                                      labels = c("Not Shortlisted","Shortlisted")))
addmargins(tbl)                 # the contingency table 
#prop.table(tbl)

?balloonplot
balloonplot(t(tbl), main ="Applicants Ethnicity background relation with Shortlisted status ", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Chi-square statistic can be easily computed using the function chisq.test() as follow:
# Result is statistically significant. Hence we can reject the hypothesis and conclude
# bias exists 

expectedp <- c(0.5,0.5)
chisq <- chisq.test(tbl,p=expectedp)
# Determine the expected and observed results
round(chisq$expected,0)
round(chisq$observed,0)
chisq


# Residuals

#If you want to know the most contributing cells to the total Chi-square score, 
#you just have to calculate the Chi-square statistic for each cell:

#Positive residuals are in blue. Positive values in cells specify an attraction 
#(positive association) between the corresponding row and column variables.
#Negative residuals are in red. This implies a repulsion (negative association) 
#between the corresponding row and column variables. 

round(chisq$residuals, 3)
corrplot(chisq$residuals, is.cor = FALSE)

# Contibution in percentage (%) to the chi-square score
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

# 4 Influence of Gender on shortlisting  ----
# Creating crosstab table 

tbl = table(factor(my_data$Gender,labels = c("Male","Female")),factor(my_data$ShortlistedNY,
                                                                      labels = c("Not Shortlisted","Shortlisted")))
addmargins(tbl)                 # the contingency table 
#prop.table(tbl)

?balloonplot
balloonplot(t(tbl), main ="Applicants Shortlisted by Gender", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Chi-square statistic can be easily computed using the function chisq.test() as follow:
# Result is statistically significant. Hence we can reject the hypothesis and conclude
# bias exists 

expectedp <- c(0.5,0.5)
chisq <- chisq.test(tbl,p=expectedp)
# Determine the expected and observed results
round(chisq$expected,0)
round(chisq$observed,0)
chisq

# Residuals

#If you want to know the most contributing cells to the total Chi-square score, 
#you just have to calculate the Chi-square statistic for each cell:

#Positive residuals are in blue. Positive values in cells specify an attraction 
#(positive association) between the corresponding row and column variables.
#Negative residuals are in red. This implies a repulsion (negative association) 
#between the corresponding row and column variables. 

round(chisq$residuals, 3)
corrplot(chisq$residuals, is.cor = FALSE)

# Contibution in percentage (%) to the chi-square score
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

# 5 Logistic Regression to predict Shortlisting of candidates ----

# 5.1 Fitting Logistic Regression to the Training set ----
set.seed(7)
fit.glm <- glm(ShortlistedNY ~ Gender + ATSIyn,family=binomial("logit"),data = my_data)
summary(fit.glm)

# Check Collinearity

sort(vif(fit.glm), decreasing = TRUE)

# 5.2 Check Anova to analyze the model of deviance ----

anova(fit.glm, test="Chisq")


#for nagelkereke install and load rcompanion package
#install.packages("rcompanion") 
library(rcompanion)
nagelkerke(fit.glm, null = NULL, restrictNobs = TRUE)





