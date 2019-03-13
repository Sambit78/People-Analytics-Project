# Set up Directory

setwd("~/Google Drive/Data Analytics/NorthWestern/MSDS 460/Final Project")


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

# Load Data


path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)
test_raw_tbl  <- read_excel("00_Data/telco_test.xlsx", sheet = 1)
productivity_cost_level_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
train_raw_tbl
test_raw_tbl
productivity_cost_level_tbl


# Data Subset

dept_job_role_tbl <- rbind(train_raw_tbl, test_raw_tbl) %>%
    select(EmployeeNumber, ClaimLevel, PerformanceRating, Attrition)

dept_job_role_tbl


# 2. Active Headcount breakdown for each claim category ----

dept_job_role_tbl %>% 
    group_by(ClaimLevel,Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(pct = n / sum(n)) %>%
  filter(Attrition %in% c("No"))

# 3. Overall Attrition Level at company level ----

dept_job_role_tbl %>% 
  group_by(Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))

# 3. Attrition Rates at each claim category ----

dept_job_role_tbl %>% 
    
    group_by(ClaimLevel, Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    
    group_by(ClaimLevel) %>%
    mutate(Attrition_percentage = n / sum(n))%>%

  filter(Attrition %in% c("Yes"))

# 4. Compare with Industry KPI : 8.8%

dept_job_role_tbl %>% 
    
    group_by(ClaimLevel, Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    
    group_by(ClaimLevel) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    
    filter(Attrition %in% c("Yes")) %>%
    arrange(desc(pct)) %>%
    mutate(
        above_industry_avg = case_when(
            pct > 0.088 ~ "Yes",
            TRUE ~ "No"
        )
    )


# 4. Attrition Cost Function ----

calculate_attrition_cost <- function (
    
    # Employee
    n                    = 1,
    salary               = 80000,
    
    # Direct Costs
    separation_cost      = 500,
    vacancy_cost         = 10000,
    acquisition_cost     = 4900,
    placement_cost       = 3500,
    
    # Productivity Costs
    net_revenue_per_employee = 250000,
    workdays_per_year        = 250,
    workdays_position_open   = 40,
    workdays_onboarding      = 60,
    onboarding_efficiency    = 0.50
    
) {
    
    # Direct Costs
    direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
    
    # Lost Productivity Costs
    productivity_cost <- net_revenue_per_employee / workdays_per_year * 
        (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
    
    # Savings of Salary & Benefits (Cost Reduction)
    salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
    
    # Estimated Turnover Per Employee
    cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
    
    # Total Cost of Employee Turnover
    total_cost <- n * cost_per_employee
    
    return(total_cost)
    
}

# 5. Calculate Attrition Costs at Claim Level ----

  dept_job_role_tbl %>% 
    
    group_by(ClaimLevel, Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    
    group_by(ClaimLevel) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    
    filter(Attrition %in% c("Yes")) %>%
    arrange(desc(pct)) %>%
    mutate(
        above_industry_avg = case_when(
            pct > 0.088 ~ "Yes",
            TRUE ~ "No"
        )
    ) %>%
  left_join(productivity_cost_level_tbl,by = c("ClaimLevel") ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n=n,
                                              salary = Salary_Average,
                                              net_revenue_per_employee = Revenue_employee
                                              )
  )
              
    

# 6 Visualization of Attrition Cost ----


dept_job_role_tbl %>% 
  
  group_by(ClaimLevel, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(ClaimLevel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% c("Yes")) %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  left_join(productivity_cost_level_tbl,by = c("ClaimLevel") ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n=n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_employee
    )
  )  %>%
  

  # Data Manipulation
  mutate(name = str_c("Claim Category",ClaimLevel, sep = ": ") %>% as_factor()) %>% 
  mutate(name = fct_reorder(name, cost_of_attrition)) %>%
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2), 
                           "M", sep = "")) %>%
    
    # Plotting
    ggplot(aes(x = cost_of_attrition, y = name)) +
    geom_segment(aes(xend = 0, yend = name), color = palette_light()[[1]]) +
    geom_point(aes(size = cost_of_attrition), color = palette_light()[[1]]) +
    scale_x_continuous(labels = scales::dollar) +
    geom_label(aes(label = cost_text, size = cost_of_attrition), 
               hjust = "inward", color = palette_light()[[1]]) +
    theme_tq() +
    scale_size(range = c(3, 5)) +
    labs(title = "Based on Employees already left , the estimated Cost of Attrition by Claim category Level",
         y = "", x = "Cost of Attrition") +
    theme(legend.position = "none")



# 7. ggpair Plotting

train_raw_tbl %>%
  select(Attrition,Age,Gender,DistanceFromHome,NumCompaniesWorked,ClaimLevel,PerformanceRating,TotalWorkingYears,OverTime)%>%
  ggpairs(aes(color = Attrition),lower = "blank",legend = 1,diag = list(continous = wrap("densityDiag",alpha = 0.5))) +
  theme(legend.position = "bottom")



# 8. Data preparation and Logistic Regression Modelling ----

train_raw_tbl$Gender    <- as.factor(train_raw_tbl$Gender)
train_raw_tbl$OverTime  <- as.factor(train_raw_tbl$OverTime)
train_raw_tbl$Attrition <- factor(train_raw_tbl$Attrition)
test_raw_tbl$Attrition  <- as.factor(test_raw_tbl$Attrition)

# Run algorithms using 10-fold cross validation

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Fitting Logistic Regression to the Training set
set.seed(7)

fit.glm <- train(as.factor(Attrition) ~ Age + DistanceFromHome + Gender + ClaimLevel + OverTime + PerformanceRating + TotalWorkingYears, 
                 data=train_raw_tbl, method="glm", metric=metric, trControl=control)



# summarize accuracy of models
results <- resamples(list(glm=fit.glm))
summary(results)

dotplot(results)

# Kappa interpretation

#< 0         = less than chance agreement
#0.01 - 0.20 = slight agreement
#0.21 - 0.40 = Fair agreement
#0.41 - 0.60 = Moderate agreement
#0.61 - 0.8  = Substantial agreement
#0.81 - 0.99 = Almost perfect agreement

# compare accuracy of models

dotplot(results)




# 9. Ranking feature importance of glm model ----

coef(summary(fit.glm))
# We will now sort this by p-values in ascending order
fit.glm_coeff = coef(summary(fit.glm))
ranked_params = row.names(fit.glm_coeff)[order(abs(fit.glm_coeff[,"z value"]), decreasing=T)]
ranked_params = ranked_params[!(ranked_params %in% "(Intercept)")]
ranked_params


# 10. Predictions on Test data set and Confusion Matrix ----

predictions <- predict(fit.glm, test_raw_tbl[-21])
confusionMatrix(predictions, test_raw_tbl$Attrition)

ActiveEmployees2019          <- rbind(subset(train_raw_tbl,Attrition == "No"),subset(test_raw_tbl,Attrition == "No"))
glimpse(ActiveEmployees2019)


ActiveEmployees2019$Age<-ActiveEmployees2019$Age+1
ActiveEmployees2019$TotalWorkingYears <- ActiveEmployees2019$TotalWorkingYears+1
ActiveEmployees2019$TrainingTimesLastYear <- ActiveEmployees2019$TrainingTimesLastYear+1
ActiveEmployees2019$YearsAtCompany <- ActiveEmployees2019$YearsAtCompany+1
ActiveEmployees2019$YearsInCurrentRole <- ActiveEmployees2019$YearsInCurrentRole+1
ActiveEmployees2019$YearsSinceLastPromotion <- ActiveEmployees2019$YearsSinceLastPromotion+1
ActiveEmployees2019$YearsWithCurrManager <- ActiveEmployees2019$YearsWithCurrManager+1


# 11. Predicted Attrition Next Year ----

ActiveEmployees2019$PredictedSTATUS2019 <-predict(fit.glm,ActiveEmployees2019)
PredictedTerminatedEmployees2019 <- subset(ActiveEmployees2019,PredictedSTATUS2019 == "Yes" )
PredictedTerminatedEmployees2019


# 12. Calculate Predicted Cost of Turn over next year  ---- 

PredictedTerminatedEmployees2019 %>% 
  
  group_by(ClaimLevel, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(ClaimLevel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  left_join(productivity_cost_level_tbl,by = c("ClaimLevel") ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n=n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_employee
    )
  ) %>%
  
  
  # Data Manipulation
  mutate(name = str_c("Claim Category",ClaimLevel, sep = ": ") %>% as_factor()) %>% 
  mutate(name = fct_reorder(name, cost_of_attrition)) %>%
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2), 
                           "M", sep = "")) %>%
  
  # Plotting
  ggplot(aes(x = cost_of_attrition, y = name)) +
  geom_segment(aes(xend = 0, yend = name), color = palette_light()[[1]]) +
  geom_point(aes(size = cost_of_attrition), color = palette_light()[[1]]) +
  scale_x_continuous(labels = scales::dollar) +
  geom_label(aes(label = cost_text, size = cost_of_attrition), 
             hjust = "inward", color = palette_light()[[1]]) +
  theme_tq() +
  scale_size(range = c(3, 5)) +
  labs(title = "Based on Employees predicted to leave next year, the estimated Cost of Attrition is :",
       y = "", x = "Cost of Attrition") +
  theme(legend.position = "none")

# 13. Calculate Predicted Cost of Turn over next year  ---- 

optimization_level_tbl <- read_excel("00_Data/optimization.xlsx")

optimization_level_tbl %>% 
  
  group_by(ClaimLevel, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(ClaimLevel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% c("Yes")) %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  left_join(productivity_cost_level_tbl,by = c("ClaimLevel") ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n=n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_employee
    )
  )  %>%
  
  
  # Data Manipulation
  mutate(name = str_c("Claim Category",ClaimLevel, sep = ": ") %>% as_factor()) %>% 
  mutate(name = fct_reorder(name, cost_of_attrition)) %>%
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2), 
                           "M", sep = "")) %>%
  
  # Plotting
  ggplot(aes(x = cost_of_attrition, y = name)) +
  geom_segment(aes(xend = 0, yend = name), color = palette_light()[[1]]) +
  geom_point(aes(size = cost_of_attrition), color = palette_light()[[1]]) +
  scale_x_continuous(labels = scales::dollar) +
  geom_label(aes(label = cost_text, size = cost_of_attrition), 
             hjust = "inward", color = palette_light()[[1]]) +
  theme_tq() +
  scale_size(range = c(3, 5)) +
  labs(title = "The estimated Cost of Attrition by Claim category Level for the Optimization Problem",
       y = "", x = "Cost of Attrition") +
  theme(legend.position = "none")


