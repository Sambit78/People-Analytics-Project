# Load the libraries
setwd("~/Google Drive/Data Analytics/Predictive HR Analytics/People-Analytics-Project/Diversity Analytics")
#install.packages("corrplot")
#install.packages("kableExtra")
library(ggthemes)  # To call solarized theme
library(plyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(graphics)
library(tidyquant)
library(gplots)
library(tidyr)  # Using for spread and gather function
#install.packages("vcd")
library(vcd)
library(RColorBrewer)
#install.packages("janitor")
library(janitor)   # Good at cleaning column names
library(kableExtra)
library(scales)
library(foreign)

# BM Data set


dataset <- read.spss("Chapter4Diversity1.sav",to.data.frame = TRUE)
glimpse(dataset)

# dim provides number of rows and number of dimensions in the table
dim(dataset)


# A Nicer color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")


# 1. DIVERSITY ANALYTICS ----

# Check country equality , diversity and inclusion rules of Australia
# 7 Eleven Diversity statistics from Australian website
# https://www.wgea.gov.au/sites/default/files/public_reports/tempPublicReport_a7yeftm5kt.pdf
# https://www.legislation.gov.au/Details/F2014L00365
# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
# http://data.wgea.gov.au/overview
# Predictive HR Analytics - Mastering the HR Metric by Martin R Edwards & Kirsten Edwards


# Step 1.1 - Workplace Profile of Active Employees ----

dataset %>%
  dplyr::select(JobGrade,Gender,Status) %>%
  filter(Status == "active" & Gender != "NA") %>%
  group_by(JobGrade,Gender)%>%
  dplyr::summarise(TotalEmployees = n()) %>%
  spread(Gender,TotalEmployees) %>%
  mutate(Total = Male + Female) %>%
  adorn_totals() %>%
  kable() %>%
  kable_styling(bootstrap_options = c("hover","striped","condensed"),full_width = F)


# Step 1.2 Gender Composition of Active workforce----

dataset %>%
  dplyr::select(Gender,Status) %>%
  filter(Status == "active" & Gender != "NA") %>%
  count() %>%
  mutate(Proportion = prop.table(freq)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("hover","striped","condensed"),full_width = F)


# Step 1.3 Gender Composition of Terminated workforce----

dataset %>%
  dplyr::select(Gender,Status) %>%
  filter(Status == "left" & Gender != "NA") %>%
  count() %>%
  mutate(Proportion = prop.table(freq)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("hover","striped","condensed"),full_width = F)


# Step 2.1 Gender and Job Grade Analysis ----
# Let's determine number of employees by Job Level and Job Role
# Female proportion in Job levels has steadily increased with Job levels but is under-represented at 
# the highest Job Level 5 which are the leadership positions in the company
# This is most analysis companies do. But is D&I a problem . Is this a statistically significant result


dataset %>% 
  filter(Status == "active" & Gender != "NA") %>%
  group_by(JobGrade,Gender) %>% 
  dplyr::summarise(Total = n()) %>%
  mutate(Percentage = Total /sum(Total)) %>%
  arrange(desc(Total)) %>% 
  ggplot() + 
  geom_bar(aes(JobGrade,Percentage, fill = Gender), stat = 'identity') +
  #facet_grid(.~GenderSelect)  + 
  theme_solarized() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Distribution by Job Level - M/F') +
  coord_flip()


# Step 2.2 Create contigency table ----

# Lets filter active employees from the dataset

diversity <- filter(dataset, Status == "active")
# Below command is used for resetting attrition column so that 
# terminated employees do not show up at all
diversity$Status <- factor(diversity$Status)  
tbl = table(diversity$JobGrade,diversity$Gender)
#tbl <- addmargins(table(dataset$JobRole,dataset$Gender), 2)
addmargins(tbl)                 # the contingency table 
prop.table(tbl)


# Step 2.3 Create ballon , mosaic and association plots ----

balloonplot(t(tbl), main ="Job Level distribution by Gender", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

mosaicplot(tbl, shade = TRUE,
           main = "Job Level distribution by Gender",las = 2)



# Plot just a subset of the table
#assoc(head(tbl, 5), shade = TRUE, las=3)
assoc(head(tbl,5), shade = TRUE, las=1)


# Step 2.4 Chi-Square Test and Residual analysis ----
# The test checks whether the data pattern found above is what we would expect
# to find if there was no link between gender and seniority in our organization
# In other words does gender bias exist
# From a purely statistical point of view , if gender bias does not exist then
# Male and Female proportions would have a close to 50:50 proportion in any job grade as it should be
# across the Organization
# Chi square test enables to check the actual frequencies of males and females in each job level
# within our organization against what we would expect to have if there were no bias.
# Bigger the bias , bigger the chi square statistic result


# Chi-square statistic can be easily computed using the function chisq.test() as follow:
# Result is statistically significant. Hence we can reject the hypothesis and conclude
# bias exists 

expectedp <- c(0.5,0.5)
chisq <- chisq.test(tbl,p=expectedp)
# Determine the expected and observed results
chisq$expected
chisq$observed


# Step 2.5 Residuals ----

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









