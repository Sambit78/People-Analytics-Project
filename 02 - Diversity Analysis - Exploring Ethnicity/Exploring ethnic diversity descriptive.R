
#Chapter 4Diversity CASE Group level data (1).sav
# https://www.youtube.com/watch?v=G-hOmmtIf90

# Load the libraries
setwd("~/Google Drive/Data Analytics/Predictive HR Analytics/Diversity Analysis")
#install.packages("corrplot")
#install.packages("ggpubr")
#install.packages("plotly")
#library("migest")
library(plotly)
library(plyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(graphics)
library(tidyquant)
library(gplots)
#install.packages("vcd")
library(vcd)
library(RColorBrewer)
library(foreign)
#install.packages("qwraps2")
library(qwraps2)
library("ggpubr")

# Plotly settings

Sys.setenv("plotly_username"="sambitdas")
Sys.setenv("plotly_api_key"="pinPJiUim7v1SM3N5kz1")


p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
#options(browser = 'false')
api_create(p, filename = "r-docs-midwest-boxplots")


library("migest")
demo(cfplot_reg, package = "migest", ask = FALSE)


#1.Read SPSS File ----

dataset <- read.spss("ethnicdiversitydata.sav",to.data.frame = TRUE)
summary(dataset)
glimpse(dataset)


# 2.Explore Team Diversity ----
# In order to determine if specific ethnic groups are clustered together
# and if there are variations in representation of BAME across teams within the Organization
# Some teams are more welcoming and some teams have low proportion. Can we identify factors ?
# Black , Asian or Minority Ethnic (BAME)

# 2.1 Percentage of Male and BAME ees in Units ----

# Use Tab_Summary to produce summary data 
#For numeric variables, tab_summary will provide the formulae for the min, median (iqr), mean (sd), 
#and max. factor and character vectors will have calls to qwraps2::n_perc for all levels provided.
#https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html

dataset %>%
  select(DepartmentGroupNumber,PercentMale,BAME) %>%
  summarise_all(funs(sum(!is.na(.))))
summary(dataset[,3:4])  

# 3.Compare ethnicity and Gender across 2 functions ----
# Sample T Test to compare if mean of BAME is different across 2 Functions

  
# 3.1 Subset the data into 2 groups of functions ----

ProfessionalService_dataset <- dataset%>%
  dplyr::select(Function, BAME,PercentMale)%>%
  filter(Function =='Professional Service')

Sales_dataset <- dataset%>%
  dplyr::select(Function, BAME,PercentMale)%>%
  filter(Function =='Sales')

# 3.2 BAME analysis across 2 functions ----
# Showcase descriptive stats of BAME across both groups 

dataset %>%
  dplyr::select(Function,BAME) %>%
  filter(Function!='NA')%>%
  ggboxplot(x = "Function", y = "BAME", 
            color = "Function", palette = c("#00AFBB", "#E7B800"),
            ylab = "BAME", xlab = "Function")


dataset %>%
  dplyr::select(Function, BAME)%>%
  filter(BAME!='NA')%>%
  group_by(Function)%>%
  dplyr :: summarise(N=n(),
            Mean = mean(BAME),
            StdDeviation = StdDev(BAME))

# Variation of BAME between the Functions is statistically significant

var.test1 <- var.test(ProfessionalService_dataset$BAME, Sales_dataset$BAME)
var.test1

# Hence we use the Independent sample T test with Var Equal to False

res <- t.test(Sales_dataset$BAME,ProfessionalService_dataset$BAME,  var.equal = FALSE)
res

# 3.3 Gender analysis across 2 functions ----
# Showcase descriptive stats of BAME across both groups 

dataset %>%
  dplyr::select(Function, PercentMale)%>%
  filter(PercentMale!='NA')%>%
  group_by(Function)%>%
  dplyr :: summarise(N=n(),
                     Mean = mean(PercentMale),
                     StdDeviation = StdDev(PercentMale))

# Variation of BAME between the Functions is statistically significant
var.test1 <- var.test(ProfessionalService_dataset$PercentMale, Sales_dataset$PercentMale)
var.test1

# Hence we use the Independent sample T test with Var Equal to False
res <- t.test(Sales_dataset$PercentMale,ProfessionalService_dataset$PercentMale,  var.equal = FALSE)
res

#https://www.rdocumentation.org/packages/lawstat/versions/3.2/topics/levene.test

