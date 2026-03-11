#Clinical Data Science Course 5 - Module 4
#Building a Clinical Prediction Model
#Work is based on the html file work of the course 
#https://d3c33hcgiwev3.cloudfront.net/ZlJQP0QITfWSUD9ECP31RA_ed3a15f032a542e5bd96ab173d001c41_BuildingAPredictionModel.html?Expires=1769520923&Signature=St2VPwnHyyCaU9gdZUbZG4KIUuiI8qwTCr7NxvAOMcQbl8cC5U6xKA6peZ7MpD2FrG9-24Y3mrG9OSVB8U5ZQ83~2QxwNSk8l5BOjDQQEY8oJmx-j7Glt5FUWI1S8YEBxWjHchkdTnSH6b0~lX1a-vOayTjkjrwAJPBPRaqjH94_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A


# ---------------------  Setting up Environment  ---------------------
library(tidyverse)
library(rsample)



#con <- DBI::dbConnect(drv = bigquery(),project = "learnclinicaldatascience") - > cannot connect to the bigquery database, thus using the local method

admissions <- read.csv("C:/Users/james/OneDrive/Asztali gép/Clinical Data Science/mimic-iii-clinical-database-demo-1.4/ADMISSIONS.csv")

patients <- read.csv("C:/Users/james/OneDrive/Asztali gép/Clinical Data Science/mimic-iii-clinical-database-demo-1.4/PATIENTS.csv")

icustays <- read.csv("C:/Users/james/OneDrive/Asztali gép/Clinical Data Science/mimic-iii-clinical-database-demo-1.4/ICUSTAYS.csv")

diagnoses_icd <- read.csv("C:/Users/james/OneDrive/Asztali gép/Clinical Data Science/mimic-iii-clinical-database-demo-1.4/DIAGNOSES_ICD.csv")

chartevents <- read.csv("C:/Users/james/OneDrive/Asztali gép/Clinical Data Science/mimic-iii-clinical-database-demo-1.4/CHARTEVENTS.csv")

d_items <- read.csv("C:/Users/james/OneDrive/Asztali gép/Clinical Data Science/mimic-iii-clinical-database-demo-1.4/D_ITEMS.csv")




# ---------------------  Assignment  ---------------------   
#build a model that predicts the likelihood that a patient will stay longer than than the ICU average stay of 4 days.
#3 stages
#1. Building an Analytic data set
#2. Developing the Predictive Model
#3. Evaluating the Model

#Things to keep in mind during model development:
#Which patients this model will and will not be valid for
#Which clinical/operational/financial functions this model will and will not be valid





# ---------------------  1. Building an Analytic data set  ---------------------   
#build an analytic data set that includes all the patients (e.g., Population) and data (e.g., Outcome and Predictors) you want to use in your model




#A. Define the population
#Current task - > build a prediction model for which ICU patients will stay for a longer than average time.
#This will be in the icustay dataset

summary(icustays)

#Our length (number of entries) is 136, while we only have 100 patients in the dataset
#So some will have multiple icustays


#Checking number of icustays per patient
icustays %>% 
  count(subject_id, sort = TRUE)

#19 have 2 or above
#Patient 41976 has 15 icustays


#Shall take a look at this patient
icustays %>% 
  filter(subject_id == 41976) %>% 
  arrange(subject_id, hadm_id, icustay_id)


#She had 15 different hospitalizations with each a single ICU stay
#Now looking at patients who might have had multipl ICU stays during a single hospitalization

icustays %>% 
  count(subject_id, hadm_id, sort = TRUE)

#Some people had multiple icu stays in a single hospital admission (one person 3 times)



#We identified patients with multiple ICU stays, multiple hospitalizations, and multiple ICU stays within a single hospitalization. This raises key decisions about how to handle repeat observations:
  
#  Include all ICU stays, allowing patients to appear multiple times.
#Include one ICU stay per hospitalization, allowing patients with multiple hospitalizations to appear multiple times.
#Include a single ICU stay from a single hospitalization per patient, so each patient contributes once.

#For the latter two options, we must also decide which hospitalization and/or ICU stay to select. While none of these approaches is inherently correct or incorrect, each will influence the interpretation and applicability of the resulting model.


#For the demonstration, each patient will be represented ONCE, so we will select only one icu stay from one hospitalization, shall go with the first one for simplicity


first_hospitalization <- admissions %>% 
  group_by(subject_id) %>%                 # group records by patient
  filter(admittime == min(admittime)) %>%  # keep earliest admission per patient
  ungroup() %>%                            # remove grouping
  select(subject_id, hadm_id)              # keep patient and hospital IDs


#Counting number of entries, 
first_hospitalization %>% 
  count()


#100, one for each patient
#Now this will be used to filter the first hospital admission and we select the earliest ICU stay
#Using inner join so we only keep the rows which match between both tables

analytic_dataset <- icustays %>% 
  inner_join(first_hospitalization, by = c("subject_id" = "subject_id", "hadm_id" = "hadm_id")) %>%
  group_by(subject_id, hadm_id) %>% 
  filter(intime == min(intime)) %>% 
  ungroup() %>% 
  select(subject_id, hadm_id, icustay_id)

analytic_dataset %>% 
  count()

#It worked, we only have 100 entries
#Checking for duplicates

analytic_dataset %>% 
  summarize(
    subject_id_unique = n_distinct(subject_id, na.rm = FALSE) == n(), #FALSE if duplicates exist
    hadm_id_unique = n_distinct(hadm_id, na.rm = FALSE) == n(),
    icustay_id_unique = n_distinct(hadm_id, na.rm = FALSE) == n()
  )

#All are unique





#Exercise, we shall do the inverse and select last icu stay for patients
#A. Try selecting the last ICU stay for a patient - how many patients have a different ICU stay selected?

last_hospitalization <- admissions %>% 
  group_by(subject_id) %>%                 # group records by patient
  filter(admittime == max(admittime)) %>%  # keep last admission per patient
  ungroup() %>%                            # remove grouping
  select(subject_id, hadm_id)              # keep patient and hospital IDs

last_hospitalization %>% 
  count()

#100 patients in the data


#B. What about if you selected the last ICU stay for the last hospitalization? Now how many patients have a different ICU stay selected?

exercise_dataset <- icustays %>% 
  inner_join(last_hospitalization, by = c("subject_id" = "subject_id", "hadm_id" = "hadm_id")) %>% 
  group_by(subject_id, hadm_id) %>% 
  filter(intime == max(intime)) %>% 
  ungroup() %>% 
  select(subject_id, hadm_id, icustay_id)

exercise_dataset %>% 
  count()
  
  
#100 patients

#Checking which patients have different ICU stay
#Doing an inner join of the two dataset and renaming each icu_stay columns

icu_stay_compared <- analytic_dataset %>% 
  rename(icu_stay_first = icustay_id) %>% 
  inner_join(
    exercise_dataset %>% rename(icu_stay_last = icustay_id),
    by = "subject_id"
  ) %>% 
  mutate(
    same_icu_stay = icu_stay_first == icu_stay_last
  )

#Now counting how many patients differ
icu_stay_compared %>% 
  summarise(n_diff = sum(!same_icu_stay, na.rm = TRUE)) %>% 
  pull(n_diff)

#19 patients have differing ICU stay ids




#C. By only including the first ICU stay of the first hospitalization when building our model, for which set of patients and under which circumstances could we expect our model to under perform? Warning, this question will be answered in future parts of this programming assignment, make sure to think about it now if you want to practice on your own.

#First hospitalization would include anyone with a condition or case that required transport and stay at the icu
#Now the problem with that would be related to patients who have repeat hospitalizations or even ones that have multiple icustays in one hospitalization as these would not be caught by the model, thus heavier or repeat cases would most likely be overlooked









#Define the outcome
#Now that we have the dataset patient population we can add in the vlaue for the desired outcome
#Goal - model which predicts the likelihood of a patient staying longer than the ICU average stay of 4 days


#First quesiton - > Where can we find the length of stay for any individual icu stay
#This information is in ICUSTAYS - > LOS column, it is a fractional length of time spent in the ICU
#We can build a temporary table and combine it iwth our analytic dataset

length_of_stay <- icustays %>% 
  select(subject_id, hadm_id, icustay_id, los)
length_of_stay

#We need to transform this into a binary variable (1/0) and not use the actual length of the stay
#We would do an if statement (case_when) to say that 1 is for stays over 4 days, and 0 for under

length_of_stay <- length_of_stay %>% 
  mutate(los_outcome = case_when(los > 4 ~ 1,
                                 TRUE ~ 0))

#It checks out, saving the column to our dataset
#Then we add this to our table with a left_join
#we keep all the values from the left table (analytic dataset), and add information from the additional table (length_of_stay) to each row
#We do this to add the information to our population list, reason for left_join() is that some variables may not always be available for every patient
#But we want to make sure they do not get removed from the data set, left_join() keeps all left rows, and if they are missing from the second data frame, they receive an NA
#After joining, we want to do quick quality data checks to make sure that the data manipulation did not cause unexpected changes
#E.g.: number of rows, any missing values, any modified values, etc.
#1. checking the total number of rows
#2. total number  of unique subject_ids to see we have 100 separate records

analytic_dataset %>% 
  left_join(length_of_stay)

analytic_dataset %>% 
  left_join(length_of_stay) %>% 
  summarise(count_rows = n(), count_patients = n_distinct(subject_id))

#All good
#Then we save our dataset manipulations

analytic_dataset <- analytic_dataset %>% 
  left_join(length_of_stay)
analytic_dataset




#Exercise
#What if we instead wanted to analyze death during the hospitalization? Use the data dictionary available from MIMIC-II: https://mimic.physionet.org/mimictables/ to determine which variable/s you could use. Then follow a similar process we have completed here to identify how many patients died during their first hospitalization.

#I think it would be within the Patient dataframe, as subject_id links to it and it has DOD based columns, meaning date of death
#It has hospital, general and social security database date of death

time_of_death <- patients %>% 
  select(subject_id, dod)
time_of_death

#Once I have extracted the dod, then I can do a left_join on the patients and add their date of death

analytic_dataset <- analytic_dataset %>% 
  left_join(time_of_death)
analytic_dataset

#I also need to add the intime and outtime for the icustays

intime_outtime_icy_stays <- icustays %>%  
  select(subject_id, hadm_id, icustay_id, intime, outtime)
analytic_dataset <- analytic_dataset %>% 
  left_join(intime_outtime_icy_stays)
analytic_dataset

#Now I would need to make a command where I see if the date value is between the intime and outtime of the dod


analytic_dataset %>% 
  mutate(in_date = dod >= intime & dod <= outtime) %>% 
  summarise(n_true = sum(in_date, na.rm = TRUE),
            n_false = sum(!in_date, na.rm = TRUE)
            )  
  
#20 people have entries that are within their icu_stay timeframe










#Define the Predictors
#We can choose predictors (variables from the dataset) based on a myriad of ways (expert clinical/scientific knowledge, data driven approaches, etc.)
#We shall select semi-random predictors to teach us useful analytic skills or demonstrate common types of hidden influencers

#These will be:
#Gender
#Age at admission
#Average oxygen saturation level during the first 12 hours of the icu stay
#Any history of hypertension

#Gender
#Gender is from the patients table, also add subject_id

gender <- patients %>% 
  select(subject_id, gender)
gender

#The gender is highlighted as F or M, this string format is not compatible with modelling tools for predictor variables (no character labels)
#To make it compatible, we will create a 'dummy variable'
#Making the categorical data numeric (0, 1)
#Male = 1, Female = 0

gender <- gender %>% 
  mutate(male = case_when(gender == "M" ~ 1,
                          TRUE ~ 0)) %>% 
  select(subject_id, male)

#Now we can join the dataset to our analytic_dataset and checking that the number of rows match 100

analytic_dataset %>% 
  left_join(gender) %>% 
  summarise(count_rows =  n(), count_patients = n_distinct(subject_id))

#Joining them and adding to the table

analytic_dataset <- analytic_dataset %>% 
  left_join(gender)
analytic_dataset

#resetting my analytic_dataset to only have the columns used within the guide

analytic_dataset <- analytic_dataset %>% 
  select(subject_id, hadm_id, icustay_id, los_outcome, male)
analytic_dataset





#Age at admission
#Now we will find the patient's age upon admission
#Admission time is at 'admissions' table, date of birt is 'dob' in patients table

date_of_birth <- patients %>% 
  select(subject_id, dob)
date_of_birth

admissions %>% 
  select(subject_id, hadm_id, admittime)
admission_time <- admissions %>% 
  select(subject_id, hadm_id, admittime)
admission_time
summary(admission_time)

#Now we can join these two together, using left_join and admission_time as the first one, as it has more entries 

age_at_admission <- admission_time %>% 
  left_join(date_of_birth)
age_at_admission


#So my dataset has dob and date values for each icu stay of each patient
#In the tutorial, this does not happen as dob is unavailable for some reason for some entries
#I will keep this in mind and I might need to filter down to our 100 first icu stay, first hospitalization list of people


#We can subtract DOB from the ADMITTIME and divide by 365.25 (the 0.25 accounts for leap years) to get the patient’s age at admission in years.

#age_at_admission %>% 
#  mutate(age_at_admission = (admittime - dob)/365.25)

#Issue with data column type in the dates sections, troubleshooting

#str(age_at_admission)

#Data is in chr character format, I need to turn it to date-time format

age_at_admission %>% 
  mutate(
    admittime = ymd_hms(admittime),
    dob = ymd_hms(dob),
    age_at_admission = as.numeric(admittime - dob) / 365.25
  )

age_at_admission_test <- age_at_admission %>% 
  mutate(admittime_date = ymd_hms(admittime),
         dob_date = ymd_hms(dob),
         age_at_admission = as.numeric(admittime_date - dob_date) / 365.25
         )

summary(age_at_admission_test)

#okay, managed to solve the issue with age_at_admission from the previous working version on my pc
#I have done some manual checks and they check out
#We will round the numbers to have a good database

age_at_admission_test <- age_at_admission %>% 
  mutate(admittime_date = ymd_hms(admittime),
         dob_date = ymd_hms(dob),
         age_at_admission = round(as.numeric(admittime_date - dob_date) / 365.25)
  )

summary(age_at_admission_test)


#As I am checking the NA values in the tutorial match the 300 values within my dataset, so I think that is probably the anomaly I have observed
#Now I will select just age_at_admission, subject_id, hadm_id and then join with my analytic_dataset data so I have the age at the time of admisison for my model dataset, also doing quality checks

age_at_admission_test <- age_at_admission %>% 
  mutate(admittime_date = ymd_hms(admittime),
         dob_date = ymd_hms(dob),
         age_at_admission = round(as.numeric(admittime_date - dob_date) / 365.25)
  ) %>% 
  select(subject_id, hadm_id, age_at_admission)
age_at_admission_test


analytic_dataset %>% 
  left_join(age_at_admission_test) %>% 
  summarise(count_rows = n(), count_patients = n_distinct(subject_id))

#It worked, adding it to the dataset

analytic_dataset <- analytic_dataset %>% 
  left_join(age_at_admission_test)
analytic_dataset

#I am exchanging the 300 values to NA so there won't be issues later

analytic_dataset %>% 
  mutate(age_at_admission = na_if(age_at_admission, 300))

#Counting number of 300 and NA values

analytic_dataset %>% 
  summarise(n_300 = sum(age_at_admission == 300, na.rm = TRUE))  #8 values in my dataset

analytic_dataset %>% 
  mutate(age_at_admission = na_if(age_at_admission, 300)) %>% 
  summarise(n_na = sum(is.na(age_at_admission)))  #also 8, we fixed it

#Finalizing dataset
analytic_dataset <- analytic_dataset %>% 
  mutate(age_at_admission = na_if(age_at_admission, 300))


analytic_dataset


#Exercise
#1. Try calculating age at discharge instead. What is the average age at discharge across all records in the ADMISSIONS table?
#Discharge time is within the 'dischtime' column in the admissions table so will be using that with our previous workflow
#I will also need date of birth, dob

date_of_birth <- patients %>% 
  select(subject_id, dob)
date_of_birth

discharge_time <- admissions %>% 
  select(subject_id, hadm_id, dischtime)
admission_time

#Joining them together

age_at_discharge <- discharge_time %>% 
  left_join(date_of_birth)
age_at_discharge  

#So now I would need to calculate everyone their age at their discharge time, also rounding up from the start, shall also incorporate including NA values instead of 300


age_at_admission %>% 
  mutate(
    admittime = ymd_hms(admittime),
    dob = ymd_hms(dob),
    age_at_admission = as.numeric(admittime - dob) / 365.25
  )

age_at_discharge %>% 
  mutate(
    dischtime = ymd_hms(dischtime),
    dob = ymd_hms(dob),
    age_at_discharge = round(as.numeric((dischtime - dob) / 365.25))
  ) %>% 
  summarise(n_300 = sum(age_at_discharge == 300, na.rm = TRUE))



age_at_discharge <- age_at_discharge %>% 
  mutate(
    dischtime = ymd_hms(dischtime),
    dob = ymd_hms(dob),
    age_at_discharge = round(as.numeric((dischtime - dob) / 365.25))
  ) 
  

#Checking number of 300 values
age_at_discharge %>% 
  summarise(n_300 = sum(age_at_discharge == 300, na.rm = TRUE))   #9 values with 300 

#Changing 300 to NA values
age_at_discharge <- age_at_discharge %>% 
  mutate(age_at_discharge = na_if(age_at_discharge, 300))

#Counting number of NA values
age_at_discharge %>% 
  summarise(n_na = sum(is.na(age_at_discharge)))



#Now that I have the average age, and NA values are correctly in place
#I can calculate the average

age_at_discharge %>% 
  summarise(mean_age = mean(age_at_discharge, na.rm = TRUE))

#68.91667 is the mean age







#Average oxygen saturation level during first 12 hours of icu stay
#A common type of predictor in clinical models are clinical observations or other vital sign measurements
#For the example, we will do the first 12 hours in the icu
#Would search in the documentation to know where oxygen saturation measurements are stored
#observations are found in the CHARTEVENTS table and the labels are in the D_ITEMS table. The best way to work with these data are the search the D_ITEMS table to find the values you are interested in and then filter the CHARTEVENTS table to only those measurements you want.
#Thinking through search terms, oxygen saturation is often abbreviated as Sp02 or pulseox. Let’s search the LABEL column for these terms

d_items %>% 
  filter(str_detect(label, pattern = regex("SpO2|pulseox", ignore_case = TRUE)))

#A number of entries were brought up as alarm for oxygen saturation level
#(SpO2 Alarm [Low], SpO2 Alarm [High], O2 Saturation Pulseoxymetry Alarm - Low, O2 Saturation Pulseoxymetry Alarm - High), limits (SpO2-L, SpO2 Desat Limit), and actual measurements (SpO2, O2 saturation pulseoxymetry)


#For this tutorial we shall use itemid '646' and '220277' 
#chartevents can be fltered to just these two values, the chartevents table is very long so we will minimize it to the first 20 rows

pulseox <- chartevents %>% 
  filter(itemid %in% c(646, 220277))

pulseox %>% 
  head(20)

#We need to simplify the database to the information which we need, these are subject_id, hadm_id, icustay_id, charttime, valuenum

pulseox <- pulseox %>% 
  select(subject_id, hadm_id, icustay_id, charttime, valuenum)

pulseox %>% 
  head(20)

#Based on this I can see that we have measures hourly and then some for the oxygen saturation for each subject 
#I can see we will extract the ones we need with a join to the analytic dataset list and then we can take the average of the first 12 hours of stay
#We need to fiest select the time the patients were admitted to the icu

icu_admission_time <- icustays %>% 
  select(subject_id, hadm_id, icustay_id, intime) 

#Now we can join the two of them together, we keep pulseox as the main table we just want to add the intime for each entry to each patient that fits, thus we will use an inner_join which keeps only the values which fit into both of them, thus we keep the right subject, hadm and icustay

pulseox <- pulseox %>%
  inner_join(icu_admission_time)

pulseox %>% 
  head(20)

#Now we can filter for all the values that are within the first 12 hours from the intime for each patient
#We would make this by making a new column and adding time (12 x 3600 seconds (= 12 x 1 hour))
#Also needed to add date-time modification to the intime column to make it work (like from before to admittime)

pulseox <- pulseox %>% 
  mutate(intime = ymd_hms(intime),
    end_time = intime + 12*3600)

pulseox %>% 
  head(20)

#Now we can simply filter to measuremets where chartime is later than intime and earlier than intime

pulseox <- pulseox %>% 
  filter(charttime >= intime & charttime <= end_time)

pulseox %>% 
  head(20)

#Somehow my data looks different from the tutorial even tho my code looks like it is working like it should
#Lets group each ICU admission and calculate the average pulseox value over twelve hours

pulseox <- pulseox %>% 
  group_by(subject_id, hadm_id, icustay_id) %>% 
  summarise(mean_first12hr_pulseox = mean(valuenum))

pulseox

#Something is different and I cannot tell what is different, I will keep it like this
#I dont care
#Joining with analytics dataset

analytic_dataset %>% 
  left_join(pulseox) %>% 
  summarise(count_rows = n(), count_patients = n_distinct(subject_id))

#We have 100 actually, so it seems, alright
analytic_dataset <- analytic_dataset %>% 
  left_join(pulseox)

analytic_dataset







#Exercise
#Try calculating the minimum respiratory rate in the first 24 hours of every hospitalization. First, what are the ITEM_IDs you used to identify respiratory rate? Once you have calculated the minimum value - what is the average minimum value across all hospitalizations?
#So I have no idea what is the respiratory rate, so I will do a label search

resp_labels <- d_items %>% 
  filter(str_detect(label, pattern = regex("respiratory rate", ignore_case = TRUE)))

#I am going to shoot a blank into the void and say use item_id 618 and 220210, as those stand for Respiratory Rate, filtering for those in chartevents

respiratory_rate <- chartevents %>% 
  filter(itemid %in% c(618, 220210))

respiratory_rate %>% 
  head(20)

#Well now I have a whole bunch of respiratory rates
#I will simplify the data (subject_id, hadm_id, icustay_id, charttime, valuenum)

respiratory_rate <- respiratory_rate %>% 
  select(subject_id, hadm_id, icustay_id, charttime, valuenum)

respiratory_rate %>% 
  head(20)

#Okay, this looks better
#We can now filter how these respond to the time of the hospitalization, i am sticking with the intime of icustay as that was in the tutorial and the only value for time within the icustay dataset

icu_admission_time <- icustays %>% 
  select(subject_id, hadm_id, icustay_id, intime) 

respiratory_rate <- respiratory_rate %>% 
  inner_join(icu_admission_time)

respiratory_rate %>% 
  head(20)

#Okay, so far we are good
#Now would come for the first 24 hours calculation from each first icu intime calculation

respiratory_rate <- respiratory_rate %>% 
  mutate(intime = ymd_hms(intime),
         end_time = intime + 24*3600)

respiratory_rate %>% 
  head(20)

#It worked
#We can now filter so we the measurements where chartime is later than intime and earlier than end_time

respiratory_rate <- respiratory_rate %>% 
  filter(charttime >= intime & charttime <= end_time)

respiratory_rate %>% 
  head(20)

respiratory_rate %>% 
  summarise(n_subject = n_distinct(subject_id))

#Huh, I have 91 subject_id entries
#I have checked the data and it seems to check out
#Now I need to find the minimum value for each hospitalization

respiratory_rate <- respiratory_rate %>% 
  group_by(subject_id, hadm_id, icustay_id) %>% 
  summarise(minimum_respiratory_rate_first24hours = min(valuenum))

min_first24hour_resprate <- respiratory_rate

#Now I can check the average minimum respiratory rate across all hospitalizations

mean(respiratory_rate$minimum_respiratory_rate_first24hours, na.rm = TRUE)

#The mean value is 12.90984









#Any history of hypertension
#ICD-9 codes for hypertension are the following 401.* (401.0, 401.1, 401.9).

hypertension <- diagnoses_icd %>% 
  filter(icd9_code %in% c("4010", "4011", "4019"))
hypertension

#Worked good and data checks out with tutorial
#We will create a list of unique subject_id that are within this dataset

hypertension <- hypertension %>% 
  distinct(subject_id)
hypertension

#Now we can make a dummy variable hypertension and assign these individuals 1 if they are on this list

hypertension <- hypertension %>% 
  mutate(hypertension = 1)

#We can join onto the analytic dataset and perform the quality check with the data

analytic_dataset %>% 
  left_join(hypertension) %>% 
  summarise(count_rows = n(), count_patients = n_distinct(subject_id))

#100, all good
#Now we save the data

analytic_dataset <- analytic_dataset %>% 
  left_join(hypertension)   
analytic_dataset








#Exercise
#Instead of using the DIAGNOSES_ICD table, try using the DIAGNOSIS variable in the ADMISSIONS table. How many people in had an admissions diagnosis involving any type of “sepsis” for their first hospitalization?

sepsis <- admissions %>% 
  filter(str_detect(diagnosis, pattern = regex("sepsis", ignore_case = TRUE)))

#Now we create a list of unique subject_id that are present in the dataset

sepsis <- sepsis %>% 
  distinct(subject_id)
sepsis

#Now adding the dummy variable

sepsis <- sepsis %>% 
  mutate(sepsis = 1)
sepsis

#Now we join onto analytic_dataset and perform our data quality check
analytic_dataset %>% 
  left_join(sepsis) %>% 
  summarise(count_rows = n(), count_patients = n_distinct(subject_id))

#Worked
#Saving the dataset

analytic_dataset_exercise <- analytic_dataset %>% 
  left_join(sepsis)
analytic_dataset_exercise

table(analytic_dataset_exercise$sepsis)
#8 people had sepsis








# ---------------------  2. Build the Model  ---------------------
#Shall use a fairly standard approach for building and testing the prediction model
#Shall split the larger dataset into training and testing population
#'rsample' package can do this process, randomized
#To be able to reproduce the same process as the tutorial, will use 'set.seed()'
#Assigns the split to be done the same during each analysis
#'initital_split()' function is used to create a training_data set that has 70% of the total analytic_data set and testing_data has 30%

analytic_dataset %>% 
  head(10)

#The analytic_dataset has a few columns missing which I need
#Added all of them, unsure how to fix it and save it, but will be problem for another time
#I have transferred the project over to my personal laptop so I may try setting up rsample there
#install.packages("pak")
#pak::pak("rsample")
#install.packages("rsample")

#It has finally downloaded, tho not the way I expected
#I think the pak installation solved some of the failing dependencies which were not met and then the rsample installation worked

#Now starting the Build the model section again
#rsample will split  the dataset into sections we need, in a randomized format
#First checking that analytic_dataset has all the columns which are in the workshop guide

analytic_dataset

#Tutorial columns: subject_id, hadm_id, icustay_id, los_outcome, male, age_at_admission, mean_first12hr_pulseox, hypertension
#Analytic_dataset has 9 columns, sepsis needs to be removed

analytic_dataset <- analytic_dataset %>% 
  select(-one_of('sepsis'))

#Now doing the splitting

set.seed(2020)  #causes split to be the same
data_split <- initial_split(analytic_dataset, prop = 7/10)
training_data <- training(data_split)
testing_data <- testing(data_split)
training_data
testing_data


#Now we can build a logistic regression model using training_data and glm() function in R with los_outcome as the outcome
#male, age_at_admission, mean_first12hr_pulseox, hypertension as predictors

model <- training_data %>% 
  glm(formula = los_outcome ~ male + age_at_admission + mean_first12hr_pulseox + hypertension,
      family = "binomial")
summary(model)


#Because the outcome is binary (1/0) we use the “binomial” family in the regression model. The summary output gives us the estimate, standard error, test statistic (z-value) and p-value for each variable in our model, including the intercept. For the binary predictors (male, hypertension) the estimate indicates the change in log odds of having a longer than average ICU stay if the person falls into the reference category as opposed to the non-reference level (ex. the log odds of having a longer than average ICU stay for male patients are lower than female patients by 1.63). For continuous variables (age_at_admission, mean_first12hr_pulseox), the estimates represent the change in log odds of death for every 1 unit change. One variable, male is significant at a level of p=0.05.

#We can evaluate the fit of this model within the training_data by using the model to predict outcomes on the training_data and plotting a receiver operating characteristic (ROC) curve which we can use to calculate the AUC (Area Under the Curve) statistic. We use the predict() function to make a new variable in the training_data data frame, predicted_outcome. Then we can use the plotROC library to make the ROC plot and calculate the AU

training_data$predicted_outcome <- predict(model, training_data, type = "response")

#install.packages("plotROC")
library(plotROC)


#So there are NA values in the hypertension section, I will replace NA values with 0

analytic_dataset$hypertension
analytic_dataset %>% 
  mutate(hypertension = ifelse(is.na(hypertension), 0, hypertension)) %>% 
  select(hypertension)

#Works, saving it

analytic_dataset <- analytic_dataset %>% 
  mutate(hypertension = ifelse(is.na(hypertension), 0, hypertension))
analytic_dataset %>% 
  head(10)

#Doing the testing and training data section, again

set.seed(2020)  #causes split to be the same
data_split <- initial_split(analytic_dataset, prop = 7/10)
training_data <- training(data_split)
testing_data <- testing(data_split)
training_data
testing_data 

model <- training_data %>% 
  glm(formula = los_outcome ~ male + age_at_admission + mean_first12hr_pulseox + hypertension,
      family = "binomial")
summary(model)
#Outcome of the model is better now, still not too convincing, doing the plotting of the predicted outcome (ROC plot and calculate the AUC)

#Now I 
training_data$predicted_outcome <- predict(model, training_data, type = "response")

training_roc <- training_data %>% 
  ggplot(aes(m = predicted_outcome, d = los_outcome)) +
  geom_roc(n.cuts = 10, labels = F, labelround = 4) +
  style_roc(theme = theme_grey)

training_roc
calc_auc(training_roc)$AUC*100
#[1] 66.50246 AUC value


#High performance models have ROC plots as close to 1 on the y axis as close to 0 on the x-axis as possible
#Diagonal line = > completely random performance
#Our sample size is small, and completely random predictor, not a bad performance
#The model does have some difference in the input data, that is for sure, but not too large
#AUC of 66.5% also aint tooo bad
#Have to keep in mind this is most ideal situation, as we are predicting based on the data used to generate the model
#Performance is better than using the testing data set

#We can also test whether this performance is equally distributed across groups of patients
#We can check the performance accuracy between men and women


stratified_training_roc <- training_data %>% 
  mutate(gender = case_when(male == 1 ~ "male",
                            TRUE ~ "female")) %>% 
  ggplot(aes(m = predicted_outcome, d = los_outcome, color = gender)) +
  geom_roc(n.cuts = 10, labels = F, labelround = 4) +
  style_roc(theme = theme_grey)

stratified_training_roc
#So it seems that the performance is much more even and better for females, compared to males

calc_auc(stratified_training_roc) %>% 
  select(group, AUC) %>% 
  mutate(group = case_when(group == 1 ~ "female",
                           group == 2 ~ "male"),
         AUC = AUC*100)

#   group      AUC
#1 female 69.84127
#2   male 56.00000

#This is also represented within our data. The tutorial has the opposite tho 


#Based on the previous “Try it out for yourself” exercises, build a prediction model for longer than average length of stay (los_outcome) using the predictors: gender (e.g., male), age at discharge (age_at_discharge), minimum respiratory rate in the first 24 hours of hospital admission (min_first24hr_resprate), and sepsis as one of the admission diagnoses (sepsis). Use the same training/testing splits as we did for this model. Are there any significant results (p<0.05)? When you try predicting on the training_data what AUC do you get? Make sure to set.seed to 2020 before you split out the training and testing populations!


analytic_dataset
#So I need to add the calculated values for age_at_discharge, min_first24hour_resprate, sepsis
#Running back the relevant sections and adding them to the analytic_dataset

#age_at_discharge
analytic_dataset %>% 
  left_join(age_at_discharge) %>% 
  summarise(count_rows = n(), count_patients = n_distinct(subject_id))

analytic_dataset %>% 
  left_join(age_at_discharge) %>% 
  select(age_at_discharge) %>% 
  head(10)

#Test looking good, adding it to the dataset
analytic_dataset <- analytic_dataset %>% 
  left_join(age_at_discharge)


#min_first24hour_resprate
analytic_dataset %>% 
  left_join(min_first24hour_resprate) %>% 
  summarise(count_rows = n(), count_patients = n_distinct(subject_id))

