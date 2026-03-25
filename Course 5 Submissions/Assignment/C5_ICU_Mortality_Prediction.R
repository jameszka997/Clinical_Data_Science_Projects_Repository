#Clinical Data Science - Course 5 
#Module 5 Assignment - Practical Application: Developing a Clinical prediction model
#By Janos Kondri
#The aim of this code base is to develop & evaluate prediction models to determine which one best assesses the risk of death of patients who are admitted to the ICU/Intensive-care Unit.

#Setting up Environment & Datasets
#Download and install of packages only need to be done on the first run if they are already not installed

# install.packages('tidyverse')
# install.packages('bigrquery')
# install.packages('rsample')
# install.packages('plotROC')


library(tidyverse)
library(bigrquery)
library(rsample)
library(plotROC)


#Online connection, commented out due to local use
#con <- DBI::dbConnect(drv = bigquery(),
# project = "learnclinicaldatascience")

#admissions <- tbl(con, 'mimic3_demo.ADMISSIONS') %>% 
#collect()
#patients  <- tbl(con, 'mimic3_demo.PATIENTS') %>% 
#  collect()
#icustays <- tbl(con, "mimic3_demo.ICUSTAYS") %>% 
#  collect()
#labevents <- tbl(con, "mimic3_demo.LABEVENTS") %>% 
#  collect()


#Setting up Directory path to the folder holding the local files
setwd("C:/Users/U1061617/OneDrive - Sanofi/WIFI Update Back up/Data Projects/Clinical Data Science/mimic3 - demo files/mimic-iii-clinical-database-demo-1.4")

admissions <- read.csv("ADMISSIONS.csv")  
patients <- read.csv("PATIENTS.csv")  
icustays <- read.csv("ICUSTAYS.csv")
labevents <- read.csv("LABEVENTS.csv")




#Filtering out and setting prediction model variables (Date of Birth/DOB, Date of Death, lactate level)

dob <- patients %>% 
  select(subject_id, dob) %>% 
  filter(is.na(dob) == FALSE)
dob

death <- admissions %>% 
  select(hadm_id, hospital_expire_flag)
death

lactate <- labevents %>% 
  filter(itemid == 50813) %>% 
  select(hadm_id, valuenum, charttime)
lactate





#Data Preprocessing
#Step to construct the analytic dataset from the available and extracted data
#1. Merging Death and DOB data
#2. Calculating age at the time of admission 
#3. Selecting correct hospital expiration flags in patients with multiple-admissions
#4. Identifying the earliest lactate measurement within the first 24 hours
#5. Then last filtering for variables of interest: Age, Lactate measurement, Hospital Expiry Flag

analytic_dataset <- icustays %>% 
  select(subject_id, icustay_id, hadm_id, intime, outtime) %>% 
  inner_join(dob) %>% 
  mutate(
    intime = ymd_hms(intime),
    outtime = ymd_hms(outtime),
    dob = ymd_hms(dob)
  ) %>% 
  mutate(age = round(as.numeric((intime - dob) / 365.25))) %>% 
  left_join(death) %>% 
  group_by(hadm_id) %>% 
  mutate(hospital_expire_flag = case_when(outtime != max(outtime) ~ 0, TRUE ~ as.numeric(hospital_expire_flag))) %>% 
  ungroup() %>% 
  left_join(lactate) %>% 
  mutate(charttime = ymd_hms(charttime)) %>% 
  rename(lactate = valuenum) %>% 
  group_by(icustay_id) %>% 
  mutate(time_diff = charttime - intime) %>% 
  filter(time_diff >= 0) %>% 
  filter(time_diff == min(time_diff)) %>% 
  ungroup() %>% 
  filter(time_diff <= 86400) %>% 
  select(age, lactate, hospital_expire_flag)


#Dataset includes 69 patients 
analytic_dataset




#Analytic_dataset is done, now we need to split our dataset with a 7/3 ration into training and testing dataset
data_split <- initial_split(analytic_dataset, prop = 7/10)

training_data <- training(data_split)
testing_data <- testing(data_split)

training_data
testing_data




#Fitting a binomial logistic regression model to the training data 
#Earliest lactate within the first 24h (lactate) and age at the time of admission (age) are used as predictors, as well as doing the summary statistics of the model
model <- training_data %>% 
  glm(formula = hospital_expire_flag ~ lactate + age, 
      family = "binomial")

summary(model)


#Results of regression model
#Lactate has a p-value below >0.05, thus it can be a significant predictor utilizing this model
#However, the model was only fit to 43 (42+1) points, thus it is fairly low population
#Assing the two predictors, the deviance was resudiced by ~15 points (58.466 - > 42.942)

# Call:
# glm(formula = hospital_expire_flag ~ lactate + age, family = "binomial", 
#       data = .)
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   -3.46067    2.11827  -1.634  0.10232   
# lactate        0.89055    0.32314   2.756  0.00585 **
#   age          0.01573    0.02624   0.599  0.54893   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 58.466  on 42  degrees of freedom
# Residual deviance: 42.942  on 40  degrees of freedom
# AIC: 48.942
# 
# Number of Fisher Scoring iterations: 5









# Generating predicted probabilities of hospital mortality for training and testing datasets
# and evaluating model performance via ROC curves and AUC scores

# Training data - predicting outcomes and plotting ROC curve
training_data$predicted_outcome <- predict(model, training_data, type = "response")

training_roc <- training_data %>% 
  ggplot(aes(m = predicted_outcome, d = hospital_expire_flag)) +
  geom_roc(n.cuts = 10, labels=F, labelround = 4) +
  style_roc(theme = theme_grey) +
  labs(title = "Training Data ROC curve")

training_roc

# Training AUC score as a percentage
calc_auc(training_roc)$AUC*100

# Testing data - predicting outcomes and plotting ROC curve

testing_data$predicted_outcome <- predict(model, testing_data, type = "response")

testing_roc <- testing_data %>% 
  ggplot(aes(m = predicted_outcome, d = hospital_expire_flag)) +
  geom_roc(n.cuts = 10, labels=F, labelround = 4) +
  style_roc(theme = theme_grey) +
  labs(title = "Testing Data ROC curve")

testing_roc

# Testing AUC score as a percentage
calc_auc(testing_roc)$AUC*100





#End of the script