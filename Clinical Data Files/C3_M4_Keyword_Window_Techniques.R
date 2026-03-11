#Course 4 - Module 4 - Keyword Window Techniques
#This will be about the Module 4 of the Clinical Data Science course based on the tutorial for the rstudio portion

#Source: https://d3c33hcgiwev3.cloudfront.net/7nnyizArS5W58oswK4uVmA_8637c46bdf464944bf4d32e2271c6e72_KeywordWindow_Technique.html?Expires=1768567942&Signature=aSk7wls6B8bUISoEege20KXKlnCUqUCjhOO8pyYh8xPNQlZBqIl05MvH7x5b~Co5goMT1Sx0oC6ldTcJUaHa~XT9e0wYc~083pvOgQMUD7AXruPIP2Zn~ltbLeRybklxlQ7UNJC7gfT~GY-bjecyF9ONVO~N26pxPiJFYiG-6I8_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A


#Setting up Environment
library(tidyverse)
library(magrittr)

library(bigrquery)
install.packages("bigrquery")

#con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")
#radiology_reports <- tbl(con, "course4_data.radiology_reports") %>% collect()
#discharge_summaries <- tbl(con, "course4_data.discharge_summaries") %>% collect()


#As I have issues with access to course specific data, I might have found just the discharge_summaries data as file.csv on the UniofColCDS github
  
#In our case, I shall just follow and solve the discharge summary cases, will include as notes, the radiology_reports examples used for the data
  
discharge_summaries <- read.csv("discharge_summary.csv")
summary(discharge_summaries)


#So we need to develop the necessary strategy for our keyword window development
#These include:
#1. Note Type
#2. Keyword selection
#3. Window Size Selection
#4. Search selection
#Example -> looks for all mentione of statin and statin drugs 
#2. For cyst, example, the previously developed regex can be used  "(?<![a-zA-Z])cyst(s|ic)?(?![a-zA-z])" to find the necessary keywords
#3. Window size - Start by selecting an initial window size based on how close the target information is likely to appear to your keyword, then adjust as needed during processing.
#For the task we are focused on, identifying cyst location, it probably will be in the same sentence as the word cyst. In this case a medium-size window, say 20 words (10 words before and 10 after) would be a good starting point. Once you have this initial window size identified and extracted you will move on start processing the text window. During that process you may find that your initial guess isn’t working as well as you had hoped (maybe you’re not finding many matches or you have a lot of off-target matches). If that’s the case, then you want to move on to the next step of adjusting the window size.
#4. Plan your search based on task type: confirm keyword, identify related info, or extract details, then filter for negation/unrelated context and extract location if needed





#Exercise
#1. Use discharge summaries to identify patients who have hypertension.
 


discharge_summaries %>%
  filter(str_detect(TEXT, regex("(?<![a-zA-Z])hypertens(ion|ive)(s)?(?![a-zA-Z])", ignore_case = TRUE))) %>%    #regex filter 
  mutate(matched_word = str_extract_all(TEXT, regex("(?<![a-zA-Z])hypertens(ion|ive)(s)?(?![a-zA-Z])", ignore_case = TRUE)))  %>%   #extracts the matching words in TEXT
  select(NOTE_ID, matched_word) #only selects NOTE ID and matching words

#Patients with hypertension n = 7 (1, 3, 5, 7, 10, 12, 15)


#Manually looking for BP measure
install.packages("DT")
library(DT)
datatable(discharge_summaries)



#2. Extract any blood pressure measurements listed in those same discharge summaries.


#What keyword/s would you use for each goal?
#Hypertension and blood pressure or BP


# For the goal of identifying which patients have hypertension (Goal #1) - what initial window size would you select? Small (<10 words), Medium (10-25 words), or Large (26+ words)?
#For hypertesion I would use a medium sized one as it could be information whether they have hypertension or not would be within such a context within the sentence

#For the goal of extracting any blood pressure measurements mentioned in the discharge summary (Goal #2) - what initial window size would you select? Small (<10 words), Medium (10-25 words), or Large (26+ words)?
#I would use small, as I think that blood pressure measurement would be right around as number where bp is mentioned

#Which search approach would you use for each goal?
#For the first, I would confirm the keyword to the person having hypertension and then from context around it as not
#For the second, I would probably extract keyword information to get the number measured for blood pressure measurements








#EXTRACT TEXT WINDOW

#Prepare Note and Select Windows of Interest

#I shall extract one note to use as an example, using #3 as it has 4 mentions of hypertension
example_note <- discharge_summaries %>% 
  filter(NOTE_ID == 3)
example_note


#One method to extract keyword windows, break a note into each individual word
#We can break up the note with separate_rows() and break up the sections
#WORDS column will contain each word
example_note %>% 
  mutate(WORDS = TEXT) %>% 
  separate_rows(WORDS, sep = "[ \n]+")

#Now we will build an index column
#Assigns number to each row - > this aids to defined the window size around the keyword 
example_note %>% 
  mutate(WORDS = TEXT) %>% 
  separate_rows(WORDS, sep = "[ \n]+") %>% 
  mutate(INDEX = seq(from = 1, to = n(), by = 1.0))


#We shall start by defining a 10 word window size
example_note %>% 
  mutate(WORDS = TEXT) %>% 
  separate_rows(WORDS, sep = "[ \n]+") %>% 
  mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
      WINDOW_START = INDEX - 10,
      WINDOW_END = INDEX + 10)


#Now that we know the start and end of each window, we can use a handy function in the stringr package called word() that extracts words based on word indices and separating characters. Let’s try it out and put the results in a new column called WINDOW.

ex_note <- example_note %>% 
  mutate(WORDS = TEXT) %>% 
  separate_rows(WORDS, sep = "[ \n]+") %>% 
  mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
         WINDOW_START = INDEX - 10,
         WINDOW_END = INDEX + 10,
         WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+"))


datatable(ex_note)



#Hmm, that gave us an error and the first and last few rows have blank text windows - what’s going on? The first window that actually appears has a window start of 1 - negative numbers and 0 seem to give word() a problem. Similarly, the last 10 rows all have problems because WINDOW_END is a index that doesn’t exist (since they go beyond the end of the content in TEXT). We can add a couple of checks to make sure that windows that extend beyond the start or the finish of the note default to the first or last word, respectively.


ex_note <- example_note %>% 
  mutate(WORDS = TEXT) %>% 
  separate_rows(WORDS, sep = "[ \n]+") %>% 
  mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
         WINDOW_START = case_when(INDEX - 10 < 1 ~ 1,
                                  TRUE ~ INDEX - 10),
         WINDOW_END = case_when(INDEX + 10 > max(INDEX) ~ max(INDEX),
                                TRUE ~ INDEX + 10),
         WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+"))

datatable(ex_note)


#Now we would filter using our regex function to only showcase the words we would like to see, in Note_ID 3's case, 4 cases of hypertension

ex_note <- example_note %>% 
  mutate(WORDS = TEXT) %>% 
  separate_rows(WORDS, sep = "[ \n]+") %>% 
  mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
         WINDOW_START = case_when(INDEX - 10 < 1 ~ 1,
                                  TRUE ~ INDEX - 10),
         WINDOW_END = case_when(INDEX + 10 > max(INDEX) ~ max(INDEX),
                                TRUE ~ INDEX + 10),
         WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+")) %>% 
  filter(str_detect(string = WORDS, pattern = regex("(?<![a-zA-Z])hypertens(ion)(s)?(?![a-zA-Z])", ignore_case = TRUE)))

datatable(ex_note)


#Now we would extend this filtering mechanism to all the notes within discharge_summaries
discharge_summaries %>% 
  group_by(NOTE_ID) %>% 
  mutate(WORDS = TEXT) %>% 
  separate_rows(WORDS, sep = "[ \n]+") %>% 
  mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
         WINDOW_START = case_when(INDEX - 10 < 1 ~ 1,
                                  TRUE ~ INDEX - 10),
         WINDOW_END = case_when(INDEX + 10 > max(INDEX) ~ max(INDEX),
                                TRUE ~ INDEX + 10),
         WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+")) %>% 
  filter(str_detect(string = WORDS, pattern = regex("(?<![a-zA-Z])hypertens(ion)(s)?(?![a-zA-Z])", ignore_case = TRUE)))

#We have 13 cases of hypertension in 7 patients


#Now doing it for blood pressure
discharge_summaries



#discharge_summaries %>% 
#  group_by(NOTE_ID) %>% 
#  mutate(WORDS = TEXT) %>% 
#  separate_rows(WORDS, sep = "[ \n]+") %>% 
#  mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
#         WINDOW_START = case_when(INDEX - 10 < 1 ~ 1,
#                                  TRUE ~ INDEX - 10),
#         WINDOW_END = case_when(INDEX + 10 > max(INDEX) ~ max(INDEX),
#                                TRUE ~ INDEX + 10),
#         WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+"))  %>% 
#  filter(str_detect(string = WORDS, pattern = regex("(?<![a-zA-Z])blood?(?![a-zA-Z])", ignore_case = TRUE)))


#datatable(bp_notes)


#BP gave false results and right now I have no idea how to search for blood pressure, as they have a space between them, they are two separate rows and entries within the database's WORDS column, I have used blood and then from the window, pressure can be inferred


#I have checked the quiz after and it very well did say that bp filtering with the current method is not possible
#It has a solution to it, will investigate this
#It looks at the first word, then uses the lead() function to look at the next row's keyword
#Also adjusting so the keyword window is extended by 1 to account for the extra row word filter

extract_2words_text_window <- function(dataframe, keyword1, keyword2, half_window_size){
  dataframe %>% 
    group_by(NOTE_ID) %>% 
    mutate(WORDS = TEXT) %>% 
    separate_rows(WORDS, sep = "[ \n]+") %>% 
    mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
           WINDOW_START = case_when(INDEX - half_window_size < 1 ~ 1,
                                    TRUE ~ INDEX - half_window_size),
           WINDOW_END = case_when(INDEX + half_window_size > max(INDEX) ~ max(INDEX),
                                  TRUE ~ INDEX + half_window_size),
           WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+")) %>% 
    ungroup() %>% 
    filter(str_detect(string = WORDS, pattern = regex(keyword1, ignore_case = TRUE)),
           str_detect(string = lead(WORDS), pattern = regex(keyword2, ignore_case = TRUE))) %>%
    mutate(WINDOW_END = WINDOW_END + 1)
}


blood_pressure_2words_results <- extract_2words_text_window(discharge_summaries, "blood", "pressure", 10) %>% 
  distinct(NOTE_ID)

datatable(blood_pressure_2words_results)

#We have 13 finds for blood pressure


#Exercise 1. - Extract keyword windows from the discharge summaries that match keywords for hypertension and blood pressure.
(?<![a-zA-Z])hypertens(ion)(s)?(?![a-zA-Z])
(?<![a-zA-Z])blood?(?![a-zA-Z])

#Exercise 2. - How many notes mention hypertension? How about blood pressure (or BP)?
#hypertension = 13 notes
#blood pressure = 13 notes
#Exercise 3. - How many keyword windows that mention hypertension or blood pressure did you find?
#I dont know, I am lazy to answer this one












#Making Window Extraction Function
#Once thing that helps make the window extraction process faster is to turn the code into a single function that you can easily adjust the window size or search string without having to manually adjust both the window start and end values every time you want to try something new. Let’s learn how to do that! To build an R function you want to use this general format:

#name <- function(variables) {}

extract_text_window <- function(dataframe, keyword, half_window_size) {
  
}

#Then we would make a few modifications to our code to extract hypertension finds within discharge summaries

extract_text_window <- function(dataframe, keyword, half_window_size) {
  dataframe %>% 
    group_by(NOTE_ID) %>% 
    mutate(WORDS = TEXT) %>% 
    separate_rows(WORDS, sep = "[ \n]+") %>% 
    mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
           WINDOW_START = case_when(INDEX - half_window_size < 1 ~ 1,
                                    TRUE ~ INDEX - half_window_size),
           WINDOW_END = case_when(INDEX + half_window_size > max(INDEX) ~ max(INDEX),
                                  TRUE ~ INDEX + half_window_size),
           WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+")) %>% 
    ungroup() %>% 
    filter(str_detect(string = WORDS, pattern = regex(keyword, ignore_case = TRUE)))
}

extract_text_window(discharge_summaries, keyword = "(?<![a-zA-Z])hypertens(ion)(s)?(?![a-zA-Z])", 10)

#Function works













#PROCESS TEXT WINDOW
#Confirming Keyword
#As described in the “Identifying Search Strategy” section, our goal with this search task is to confirm that they keyword we selected is accurate for the patient in question. To do that, we will:
#Initially assume that any mention of cyst/s or cystic in the report of a CT scan of the abdomen/pelvis indicates the patient has a cyst in the abdomen/pelvis.
#Search cyst keyword windows for evidence of negation or unrelated disease/patient. If present, remove that text window from consideration.
#If a note has one or more cyst keyword windows that have not been removed in Step 2 -> conclude scan identified cyst in the abdomen and/or pelvis.




#Step 1 - We assume the text window that has the keyword text is positive (no negation)

results <- extract_text_window(discharge_summaries, keyword = "(?<![a-zA-Z])hypertens(ion)(s)?(?![a-zA-Z])", 10)

datatable(results)


#Step 2 - Looking at the results to look for any negation

#Entry #8 has the following window
#white female with a history of fevers. No history of hypertension or cardiovascular risk factors. For further details of the admission,

#We amend our function with an additional regex filter to look for any no history of

results <- extract_text_window(discharge_summaries, keyword = "(?<![a-zA-Z])hypertens(ion)(s)?(?![a-zA-Z])", 20) %>% 
  mutate(EXCLUSE = case_when(str_detect(WINDOW, regex(pattern  = "no history of hypertension?", ignore_case = TRUE)) ~ 1,
                             TRUE ~ 0))

datatable(results)

#(\w+ ){2} = “two words in a row, each followed by a space”
#I have checked and no other mention of hypertension is negated



#Step 3 - Creating list of non-negated hypertension occurances

results <- extract_text_window(discharge_summaries, keyword = "(?<![a-zA-Z])hypertens(ion)(s)?(?![a-zA-Z])", 10) %>% 
  mutate(EXCLUDE = case_when(str_detect(WINDOW, regex(pattern  = "no history of hypertension?", ignore_case = TRUE)) ~ 1,
                             TRUE ~ 0)) %>%  
  filter(EXCLUDE != 1)  %>% 
  distinct(NOTE_ID)

datatable(results)

#   NOTE_ID
#1	1
#2	3
#3	5
#4	10
#5	12
#6	15


#Exercise
#How many discharge summaries had hypertension windows that were negated?
#Only 1









#Identifying Keyword Information
#There is no analogous exercise for this with hypertension so I will just have to go through and paste here the tutorial information
#Looking for splenic cysts
#Search all cyst keyword windows for evidence that the cyst occurs in the spleen. If evidence that the cyst is in the spleen then keep the keyword window. If the cyst located elsewhere or location is not specified then remove the text window from consideration.
#Search all splenic cyst keyword windows for evidence of negation or unrelated disease/patient. If present, remove that text window from consideration.
#If a note has one or more splenic cyst keyword windows that have not been removed in Step 2 -> conclude the scan identified a splenic cyst.


#Step 1 - We shall look at the first few windows of cysts to see how spleen is written or referred to
radiology_reports %>% 
  extract_text_window(keyword = "(?<![a-zA-Z])cyst(s|ic)?(?![a-zA-z])", half_window_size = 10)

#Based on my observation, we may find 'splenic' or 'in the spleen' 

#NOTE_ID 3, INDEX = 108 uses 'splenic'
#Shall use make a new column where splenic cysts is looked for

radiology_reports %>% 
  extract_text_window(keyword = "(?<![a-zA-Z])cyst(s|ic)?(?![a-zA-z])", half_window_size = 10) %>% 
  mutate(SPLEEN_CYST = case_when(str_detect(WINDOW, regex("splenic cyst(s)?", ignore_case = TRUE)) ~ 1,
                                 TRUE ~ 0)) %>% 
  arrange(desc(SPLEEN_CYST))



#It is the only window which uses this phrase
#After your search you likely found another window (NOTE_ID = 3, INDEX = 368), that uses the phrase “Low-attenuation lesions in the spleen may represent cyst”. We can add that to our regular expression:


radiology_reports %>% 
  extract_text_window(keyword = "(?<![a-zA-Z])cyst(s|ic)?(?![a-zA-z])", half_window_size = 10) %>% 
  mutate(SPLEEN_CYST = case_when(str_detect(WINDOW, regex("splenic cyst(s)?", ignore_case = TRUE)) ~ 1,
                                 str_detect(WINDOW, regex("spleen may represent cyst(s)?", ignore_case = TRUE)) ~ 1,
                                 TRUE ~ 0)) %>% 
  arrange(desc(SPLEEN_CYST))

#You may also have seen that there were a couple of windows that didn’t have any location information. Specifically:

#NOTE_ID = 7, INDEX = 73 - Location outside of the window, looking at the full note (TIP: you can hover over the TEXT field in the table and it will show you the full note), it appears this is on the pancreas.
#NOTE_ID = 10, INDEX = 57 - A second cyst is mentioned in the window without a clear location. However that cyst mention is captured by the next window (NOTE_ID = 10, INDEX = 62), which does have a location mentioned.
#NOTE_ID = 10, INDEX = 153 - Cyst mentioned without a clear location. There are 7 other cyst windows in this note, location is probably captured in one of the other windows.

#These three cases actually are great examples about how to judge when to increase a window size. If I were doing this with a much larger data set of hundreds or thousands of notes, the only missing location window that would cause me to consider increasing window size is the one from Note 7. In that particular case the location is just outside my window (3 words away). I wouldn’t use either window from Note 10 to influence my decision to increase my window for two reasons: First, for window 57 the second cyst location is actually captured in another window (INDEX = 62) thus it isn’t that the window is too small, just that it also captured part of another window. Second, for window 153 from the context of the window it is clear that there is no location to be found near this cyst mention. Increasing my window size won’t help in this situation.



#Step 2
#Now that we have identified which windows have a cyst in the spleen, you will want to do a quick check of those windows to ensure that there are no negated mentions.


radiology_reports %>% 
  extract_text_window(keyword = "(?<![a-zA-Z])cyst(s|ic)?(?![a-zA-z])", half_window_size = 10) %>% 
  mutate(SPLEEN_CYST = case_when(str_detect(WINDOW, regex("splenic cyst(s)?", ignore_case = TRUE)) ~ 1,
                                 str_detect(WINDOW, regex("spleen may represent cyst(s)?", ignore_case = TRUE)) ~ 1,
                                 TRUE ~ 0)) %>% 
  filter(SPLEEN_CYST == 1)

#No negation

#Step 3 
#Since no windows were removed in Step 2, we can simply look at the unique NOTE_ID and know which notes identified a splenic cyst!

radiology_reports %>% 
  extract_text_window(keyword = "(?<![a-zA-Z])cyst(s|ic)?(?![a-zA-z])", half_window_size = 10) %>% 
  mutate(SPLEEN_CYST = case_when(str_detect(WINDOW, regex("splenic cyst(s)?", ignore_case = TRUE)) ~ 1,
                                 str_detect(WINDOW, regex("spleen may represent cyst(s)?", ignore_case = TRUE)) ~ 1,
                                 TRUE ~ 0)) %>% 
  filter(SPLEEN_CYST == 1) %>% 
  distinct(NOTE_ID)

#Only Note ID = 3









#EXTRACTING KEYWORD INFORMATION
#Now let’s try the extraction task to identify where the cyst is located using the following approach:
  
#Search cyst keyword windows for evidence of negation or unrelated disease/patient. If present, remove that text window from consideration.
#For all remaining cyst keyword windows, write regular expressions to extract location of the cyst. Depending on your goal, you may need to map specific locations to organ groups (e.g., “left anterior mid pole region” -> “Kidney”).
#In our case, I shall follow along with the blood pressure measurement where both systolic and diastolic numbers are listed


#Step 1
#This is just what we did in the “Confirming Keyword” section - let’s just grab that code and reuse it here!
#For this, we will use the extract_2words_text_window function

bp_results <- extract_2words_text_window(discharge_summaries, "blood", "pressure", half_window_size = 10)
datatable(bp_results)


#Don't need to negate blood pressure as this is specific as it is and the next filtering will take our positive results

#Step 2
#I will try to write an expression which first looks and finds whether systolic is within it

bp_results <- extract_2words_text_window(discharge_summaries, "blood", "pressure", half_window_size = 10) %>% 
  mutate(SYSTOLIC = case_when(str_detect(WINDOW, regex("systolic?", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0))
                              
datatable(bp_results)
 


#I think I will do a diastolic column and then I would filter so I have entries which both have TRUE (1) entries



bp_results <- extract_2words_text_window(discharge_summaries, "blood", "pressure", half_window_size = 10) %>% 
  mutate(SYSTOLIC = case_when(str_detect(WINDOW, regex("systolic?", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0)) %>% 
  mutate(DIASTOLIC = case_when(str_detect(WINDOW, regex("diastolic?", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0)) %>% 
  filter(SYSTOLIC == 1, DIASTOLIC == 1) %>% 
  distinct(NOTE_ID)

datatable(bp_results)
                              
#Patient 3 (NOTE_ID) has both diastolic and systolic blood pressure measurement information                            
  












#Exercise
#1. Using the discharge summaries, extract all blood pressure measurements where both systolic and diastolic numbers are listed.

bp_results <- extract_2words_text_window(discharge_summaries, "blood", "pressure", half_window_size = 10) %>% 
  mutate(SYSTOLIC = case_when(str_detect(WINDOW, regex("systolic?", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0)) %>% 
  mutate(DIASTOLIC = case_when(str_detect(WINDOW, regex("diastolic?", ignore_case = TRUE)) ~ 1,
                               TRUE ~ 0)) %>% 
  filter(SYSTOLIC == 1, DIASTOLIC == 1) %>% 
  distinct(NOTE_ID)





bp_results <- discharge_summaries %>% 
  extract_2words_text_window(keyword1 = "blood", keyword2 = "pressure", half_window_size = 10) %>% 
  mutate(BLOOD_PRESSURE = case_when(str_detect(WINDOW, regex("blood pressure ([0-9]+/[0-9]+)", ignore_case = TRUE)) ~ str_match(WINDOW, regex("blood pressure ([0-9]+/[0-9]+)", ignore_case = TRUE))[,2],
                                    TRUE ~ "")) %>% 
  filter(BLOOD_PRESSURE != "")

datatable(bp_results)
