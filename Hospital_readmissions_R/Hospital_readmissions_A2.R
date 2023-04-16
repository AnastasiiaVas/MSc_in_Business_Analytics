#' Author: Anastasia Vasileva
#' Data: April 8th, 2023
#' Purpose: Hospital Data, EDA&Modeling
#' 

# libs
library(DataExplorer)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(powerjoin)
library(lattice)
library(caret)
library(scorecard)
library(vtreat)
library(scales)


#update.packages()

# Set WD
setwd("C:/Users/anast/OneDrive/Рабочий стол/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Data look up on github
diabetesPatientTrain <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv')
diabetesMedsTrain <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv')
diabetesHospitalInfoTrain <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv')

diabetesPatientTest <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv')
diabetesMedsTest <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv')
diabetesHospitalInfoTest <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv')



#Dimensions of DF's
dim(diabetesPatientTrain) #7500 16
str(diabetesPatientTrain) 
dim(diabetesMedsTrain) #7500 23
str(diabetesMedsTrain) 
dim(diabetesHospitalInfoTrain) #7500 7
str(diabetesHospitalInfoTrain) 


##Firstly, I wil work with each file separately

############################# diabetesPatientTrain #############################
################################################################################

#Starting with diabetesPatientTrain csv.

dim(diabetesPatientTrain) #7500 rows 7 columns
str(diabetesPatientTrain)
head(diabetesPatientTrain)
plot_str(diabetesPatientTrain)
introduce(diabetesPatientTrain)

#Checking all columns we have
names(diabetesPatientTrain)

#To check how many NAN's  we have- we transform values such as "?" and "" to NAN's:
diabetesPatientTrain$race <- ifelse(diabetesPatientTrain$race == "", NA, diabetesPatientTrain$race)
diabetesPatientTrain$gender <- ifelse(diabetesPatientTrain$gender == "", NA, diabetesPatientTrain$gender)
diabetesPatientTrain$age <- ifelse(diabetesPatientTrain$age == "", NA, diabetesPatientTrain$age)
diabetesPatientTrain$wgt <- ifelse(diabetesPatientTrain$wgt == "", NA, diabetesPatientTrain$wgt)
diabetesPatientTrain$payer_code <- ifelse(diabetesPatientTrain$payer_code == "", NA, diabetesPatientTrain$payer_code)

diabetesPatientTrain$race <- ifelse(diabetesPatientTrain$race == "?", NA, diabetesPatientTrain$race)
diabetesPatientTrain$gender <- ifelse(diabetesPatientTrain$gender == "?", NA, diabetesPatientTrain$gender)
diabetesPatientTrain$age <- ifelse(diabetesPatientTrain$age == "?", NA, diabetesPatientTrain$age)
diabetesPatientTrain$wgt <- ifelse(diabetesPatientTrain$wgt == "?", NA, diabetesPatientTrain$wgt)
diabetesPatientTrain$payer_code <- ifelse(diabetesPatientTrain$payer_code == "?", NA, diabetesPatientTrain$payer_code)

#Checking columns for NAN's one more time
plot_missing(diabetesPatientTrain)

#Dropping "payer_code"- too many NAN's
diabetesPatientTrain <- select(diabetesPatientTrain,-payer_code)

#Checking null values for each column
sum(is.na(diabetesPatientTrain$tmpID)) #no null
sum(is.na(diabetesPatientTrain$race)) #172
sum(is.na(diabetesPatientTrain$gender)) #no null
sum(is.na(diabetesPatientTrain$age)) #no null
sum(is.na(diabetesPatientTrain$wgt)) #no null
sum(is.na(diabetesPatientTrain$payer_code)) #no null

#Checking values in each column
table(diabetesPatientTrain$race, useNA="ifany") #176 nan
table(diabetesPatientTrain$gender, useNA="ifany") #all good- all values are filled
table(diabetesPatientTrain$age, useNA="ifany") #all good
table(diabetesPatientTrain$wgt, useNA="ifany") #all good


#Checking classes for each column
class(diabetesPatientTrain$race)
diabetesPatientTrain$race <- as.factor(diabetesPatientTrain$race)
class(diabetesPatientTrain$gender)
diabetesPatientTrain$gender <- as.factor(diabetesPatientTrain$gender)

#Transforming readmitted_y to 0 and 1
diabetesPatientTrain$readmitted_y <- ifelse(diabetesPatientTrain$readmitted_y == "FALSE", 0, 1)

#Filling missing values for race 
set.seed(123)
prop <- prop.table(table(diabetesPatientTrain$race, useNA = "ifany"))
diabetesPatientTrain$race[is.na(diabetesPatientTrain$race)] <- sample(c("AfricanAmerican", "Asian", "Caucasian", "Hispanic", "Other"), size = sum(is.na(diabetesPatientTrain$race)), replace = TRUE, prob = c(prop["AfricanAmerican"], prop["Asian"], prop["Caucasian"],prop["Hispanic"],prop["Other"]))

#Doing one hot encoding
OHE_PatientTrain <- one_hot(diabetesPatientTrain, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE)
str(diabetesPatientTrain)
str(OHE_PatientTrain)

################# We are done with diabetesPatientTrain file ###################
#################### Moving to diabetesMedsTrain file ##########################


dim(diabetesMedsTrain) #7500 rows 23 columns
str(diabetesMedsTrain)
head(diabetesMedsTrain)
plot_str(diabetesMedsTrain)
introduce(diabetesMedsTrain) #total_missing_values = 0 

#Checking all columns we have
names(diabetesMedsTrain)

#We are not checking missing values for each column because total_missing_values = 0

#Checking values in each column- all good. No values such as "", "?", "Unknown" etc
table(diabetesMedsTrain$max_glu_serum, useNA="ifany")
table(diabetesMedsTrain$A1Cresult, useNA="ifany") 
table(diabetesMedsTrain$metformin, useNA="ifany") 
table(diabetesMedsTrain$repaglinide, useNA="ifany") 
table(diabetesMedsTrain$nateglinide, useNA="ifany")
table(diabetesMedsTrain$chlorpropamide, useNA="ifany") 
table(diabetesMedsTrain$glimepiride, useNA="ifany")
table(diabetesMedsTrain$acetohexamide, useNA="ifany") 
table(diabetesMedsTrain$glipizide, useNA="ifany") 
table(diabetesMedsTrain$glyburide, useNA="ifany")
table(diabetesMedsTrain$tolbutamide, useNA="ifany") 
table(diabetesMedsTrain$pioglitazone, useNA="ifany") 
table(diabetesMedsTrain$rosiglitazone, useNA="ifany") 
table(diabetesMedsTrain$acarbose, useNA="ifany") 
table(diabetesMedsTrain$miglitol, useNA="ifany") 
table(diabetesMedsTrain$troglitazone, useNA="ifany") 
table(diabetesMedsTrain$tolazamide, useNA="ifany")
table(diabetesMedsTrain$examide, useNA="ifany") 
table(diabetesMedsTrain$citoglipton, useNA="ifany") 
table(diabetesMedsTrain$insulin, useNA="ifany") 
table(diabetesMedsTrain$change, useNA="ifany")
table(diabetesMedsTrain$diabetesMed, useNA="ifany") 

#This change I am making after I merged the files for test and train- and i noticed the uneven amount of columns-
#i will drop the ones i am missing
diabetesMedsTrain <- select(diabetesMedsTrain,-miglitol, -nateglinide, -tolbutamide, -troglitazone, -chlorpropamide)


#Doing one hot encoding

OHE_MedsTrain <- one_hot(diabetesMedsTrain, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE)
str(OHE_MedsTrain)



################## We are done with diabetesMedsTrain file #####################
################ Moving to diabetesHospitalInfoTrain file ######################

dim(diabetesHospitalInfoTrain) #7500 rows 16 columns
str(diabetesHospitalInfoTrain)
head(diabetesHospitalInfoTrain)
plot_str(diabetesHospitalInfoTrain)
introduce(diabetesHospitalInfoTrain) #total_missing_values = 0 


#To check how many NAN's do we have- we transform values such as "" to NAN's:
diabetesHospitalInfoTrain$admission_type_id <- ifelse(diabetesHospitalInfoTrain$admission_type_id == "", NA, diabetesHospitalInfoTrain$admission_type_id)
diabetesHospitalInfoTrain$discharge_disposition_id <- ifelse(diabetesHospitalInfoTrain$discharge_disposition_id == "", NA, diabetesHospitalInfoTrain$discharge_disposition_id)
diabetesHospitalInfoTrain$admission_source_id <- ifelse(diabetesHospitalInfoTrain$admission_source_id == "", NA, diabetesHospitalInfoTrain$admission_source_id)
diabetesHospitalInfoTrain$medical_specialty <- ifelse(diabetesHospitalInfoTrain$medical_specialty == "?", NA, diabetesHospitalInfoTrain$medical_specialty)
#Checking columns for NAN's one more time
plot_missing(diabetesHospitalInfoTrain)

#Checking all columns we have and categories in it. Removing irrelevant/not significant data:

table(diabetesHospitalInfoTrain$admission_type_id, useNA="ifany") 
#In admission_type_id column there is only one "Newborn" data- removing it
diabetesHospitalInfoTrain <- diabetesHospitalInfoTrain %>%
  filter(!grepl("Newborn", admission_type_id))

table(diabetesHospitalInfoTrain$discharge_disposition_id, useNA="ifany") 
#In discharge_disposition_id column there is only one "Admitted as an inpatient to this hospital" data- removing it.
diabetesHospitalInfoTrain <- diabetesHospitalInfoTrain %>%
  filter(!grepl("Admitted as an inpatient to this hospital", discharge_disposition_id))
#The main idea of analys is to find top 100 patients that expected to be readmitted, so categories such as "Expired", 
#"Hospice / home", "Hospice / medical facility"- are irrelevant. These people either already died or dying. We will drop them.
diabetesHospitalInfoTrain <- diabetesHospitalInfoTrain %>%
  filter(!grepl("Expired", discharge_disposition_id))
diabetesHospitalInfoTrain <- diabetesHospitalInfoTrain %>%
  filter(!grepl("Hospice / home", discharge_disposition_id))
diabetesHospitalInfoTrain <- diabetesHospitalInfoTrain %>%
  filter(!grepl("Hospice / medical facility", discharge_disposition_id))

table(diabetesHospitalInfoTrain$admission_source_id, useNA="ifany") 
#Logically, the admission source cannot influence patience's health and the probability of if he will be readmitted or not.
#I will drop this column.
diabetesHospitalInfoTrain <- select(diabetesHospitalInfoTrain,-admission_source_id)

table(diabetesHospitalInfoTrain$time_in_hospital, useNA="ifany") 
#Everything is ok

table(diabetesHospitalInfoTrain$medical_specialty, useNA="ifany")
#To many missing values- dropping the column
diabetesHospitalInfoTrain <- select(diabetesHospitalInfoTrain,-medical_specialty)

table(diabetesHospitalInfoTrain$num_lab_procedures, useNA="ifany") 
#Everything is ok
table(diabetesHospitalInfoTrain$num_procedures, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTrain$num_medications, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTrain$number_outpatient, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTrain$number_emergency, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTrain$number_diagnoses, useNA="ifany") 
#Everything is ok

table(diabetesHospitalInfoTrain$diag_1_desc, useNA="ifany") 
table(diabetesHospitalInfoTrain$diag_2_desc, useNA="ifany") 
table(diabetesHospitalInfoTrain$diag_3_desc, useNA="ifany")
#Checking how many categories have diag_1_desc, diag_2_desc,diag_3_desc have.
unique(diabetesHospitalInfoTrain$diag_1_desc)
unique(diabetesHospitalInfoTrain$diag_2_desc)
unique(diabetesHospitalInfoTrain$diag_3_desc)
#I will drop them- The only way how i can extract something beneficial from these columns is NLP- i do not posses this skill. 
diabetesHospitalInfoTrain <- select(diabetesHospitalInfoTrain,-diag_1_desc, -diag_2_desc, -diag_3_desc)

#Transforming columns. Changing categories in these columns.
#Starting with discharge_disposition_id column- we will leave only 5 categories: "Discharged to home", "Discharged/transferred to another medical institution/facility,
#"Discharged/transferred to home under care/home health service"," Left AMA" , "Not Mapped"
diabetesHospitalInfoTrain$discharge_disposition_id <- ifelse(diabetesHospitalInfoTrain$discharge_disposition_id %in% c("Discharged/transferred to a long term care hospital.", 
                                                                                                                       "Discharged/transferred to a nursing facility certified under Medicaid but not certified under Medicare", 
                                                                                                                       "Discharged/transferred to another  type of inpatient care institution", 
                                                                                                                       "Discharged/transferred to another rehab fac including rehab units of a hospital.", 
                                                                                                                       "Discharged/transferred to another short term hospital", 
                                                                                                                       "Discharged/transferred to ICF", 
                                                                                                                       "Discharged/transferred to SNF", 
                                                                                                                       "Discharged/transferred within this institution to Medicare approved swing bed", 
                                                                                                                       "Discharged/transferred/referred another institution for outpatient services", 
                                                                                                                       "Discharged/transferred/referred to a psychiatric hospital of a psychiatric distinct part unit of a hospital"), "Discharged/transferred to another medical institution/facility", diabetesHospitalInfoTrain$discharge_disposition_id)
diabetesHospitalInfoTrain$discharge_disposition_id <- ifelse(diabetesHospitalInfoTrain$discharge_disposition_id %in% c("Discharged/transferred to home under care of Home IV provider", 
                                                                                                                       "Discharged/transferred to home with home health service"), "Discharged/transferred to home under care/home health service", diabetesHospitalInfoTrain$discharge_disposition_id)

#Filling NAN's
#Using the percentage distribution of categories
set.seed(123)
prop <- prop.table(table(diabetesHospitalInfoTrain$admission_type_id, useNA = "ifany"))
diabetesHospitalInfoTrain$admission_type_id[is.na(diabetesHospitalInfoTrain$admission_type_id)] <- sample(c("Elective", "Emergency", "Not Available", "Not Mapped", "Urgent" ), size = sum(is.na(diabetesHospitalInfoTrain$admission_type_id)), replace = TRUE, prob = c(prop["Elective"], prop["Emergency"], prop["Not Available"],prop["Not Mapped"],prop["Urgent"]))

set.seed(123)
prop <- prop.table(table(diabetesHospitalInfoTrain$discharge_disposition_id, useNA = "ifany"))
diabetesHospitalInfoTrain$discharge_disposition_id[is.na(diabetesHospitalInfoTrain$discharge_disposition_id)] <- sample(c("Discharged to home",
                                                                                                                          "Discharged/transferred to another medical institution/facility",
                                                                                                                          "Discharged/transferred to home under care/home health service",
                                                                                                                          "Left AMA",
                                                                                                                          "Not Mapped"), size = sum(is.na(diabetesHospitalInfoTrain$discharge_disposition_id)), replace = TRUE, prob = c(prop["Discharged to home"], prop["Discharged/transferred to another medical institution/facility"], prop["Discharged/transferred to home under care/home health service"],prop["Not Mapped"],prop["Left AMA"]))

#Do one hot encoding
OHE_HospTrain <- one_hot(diabetesHospitalInfoTrain, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE)
str(OHE_HospTrain)




################## We are done with diabetesHospitalInfoTrain file, now I do the same thing for Test #############
##################################################################################################################


#Starting with diabetesPatientTest csv.

dim(diabetesPatientTest) #7500 rows 7 columns
str(diabetesPatientTest)
head(diabetesPatientTest)
plot_str(diabetesPatientTest)
introduce(diabetesPatientTest)

#Checking all columns we have
names(diabetesPatientTest)

#To check how many NAN's  we have- we transform values such as "?" and "" to NAN's:
diabetesPatientTest$race <- ifelse(diabetesPatientTest$race == "", NA, diabetesPatientTest$race)
diabetesPatientTest$gender <- ifelse(diabetesPatientTest$gender == "", NA, diabetesPatientTest$gender)
diabetesPatientTest$age <- ifelse(diabetesPatientTest$age == "", NA, diabetesPatientTest$age)
diabetesPatientTest$wgt <- ifelse(diabetesPatientTest$wgt == "", NA, diabetesPatientTest$wgt)
diabetesPatientTest$payer_code <- ifelse(diabetesPatientTest$payer_code == "", NA, diabetesPatientTest$payer_code)

diabetesPatientTest$race <- ifelse(diabetesPatientTest$race == "?", NA, diabetesPatientTest$race)
diabetesPatientTest$gender <- ifelse(diabetesPatientTest$gender == "?", NA, diabetesPatientTest$gender)
diabetesPatientTest$age <- ifelse(diabetesPatientTest$age == "?", NA, diabetesPatientTest$age)
diabetesPatientTest$wgt <- ifelse(diabetesPatientTest$wgt == "?", NA, diabetesPatientTest$wgt)
diabetesPatientTest$payer_code <- ifelse(diabetesPatientTest$payer_code == "?", NA, diabetesPatientTest$payer_code)

#Checking columns for NAN's one more time
plot_missing(diabetesPatientTest)

#Dropping "payer_code"- too many NAN's
diabetesPatientTest <- select(diabetesPatientTest,-payer_code)

#Checking null values for each column
sum(is.na(diabetesPatientTest$tmpID)) #no null
sum(is.na(diabetesPatientTest$race)) #172
sum(is.na(diabetesPatientTest$gender)) #no null
sum(is.na(diabetesPatientTest$age)) #no null
sum(is.na(diabetesPatientTest$wgt)) #no null
sum(is.na(diabetesPatientTest$payer_code)) #no null

#Checking values in each column
table(diabetesPatientTest$race, useNA="ifany") #176 nan
table(diabetesPatientTest$gender, useNA="ifany") #all good- all values are filled
table(diabetesPatientTest$age, useNA="ifany") #all good
table(diabetesPatientTest$wgt, useNA="ifany") #all good


#Checking classes for each column
class(diabetesPatientTest$race)
diabetesPatientTest$race <- as.factor(diabetesPatientTest$race)
class(diabetesPatientTest$gender)
diabetesPatientTest$gender <- as.factor(diabetesPatientTest$gender)

#Transforming readmitted_y to 0 and 1
diabetesPatientTest$readmitted_y <- ifelse(diabetesPatientTest$readmitted_y == "FALSE", 0, 1)

#Filling missing values for race 
set.seed(123)
prop <- prop.table(table(diabetesPatientTest$race, useNA = "ifany"))
diabetesPatientTest$race[is.na(diabetesPatientTest$race)] <- sample(c("AfricanAmerican", "Asian", "Caucasian", "Hispanic", "Other"), size = sum(is.na(diabetesPatientTest$race)), replace = TRUE, prob = c(prop["AfricanAmerican"], prop["Asian"], prop["Caucasian"],prop["Hispanic"],prop["Other"]))

#Doing one hot encoding
OHE_PatientTest <- one_hot(diabetesPatientTest, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE)
str(diabetesPatientTest)
str(OHE_PatientTest)

################# We are done with diabetesPatientTest file ###################
#################### Moving to diabetesMedsTest file ##########################


dim(diabetesMedsTest) #7500 rows 23 columns
str(diabetesMedsTest)
head(diabetesMedsTest)
plot_str(diabetesMedsTest)
introduce(diabetesMedsTest) #total_missing_values = 0 

#Checking all columns we have
names(diabetesMedsTest)

#We are not checking missing values for each column because total_missing_values = 0

#Checking values in each column- all good. No values such as "", "?", "Unknown" etc
table(diabetesMedsTest$max_glu_serum, useNA="ifany")
table(diabetesMedsTest$A1Cresult, useNA="ifany") 
table(diabetesMedsTest$metformin, useNA="ifany") 
table(diabetesMedsTest$repaglinide, useNA="ifany") 
table(diabetesMedsTest$nateglinide, useNA="ifany")
table(diabetesMedsTest$chlorpropamide, useNA="ifany") 
table(diabetesMedsTest$glimepiride, useNA="ifany")
table(diabetesMedsTest$acetohexamide, useNA="ifany") 
table(diabetesMedsTest$glipizide, useNA="ifany") 
table(diabetesMedsTest$glyburide, useNA="ifany")
table(diabetesMedsTest$tolbutamide, useNA="ifany") 
table(diabetesMedsTest$pioglitazone, useNA="ifany") 
table(diabetesMedsTest$rosiglitazone, useNA="ifany") 
table(diabetesMedsTest$acarbose, useNA="ifany") 
table(diabetesMedsTest$miglitol, useNA="ifany") 
table(diabetesMedsTest$troglitazone, useNA="ifany") 
table(diabetesMedsTest$tolazamide, useNA="ifany")
table(diabetesMedsTest$examide, useNA="ifany") 
table(diabetesMedsTest$citoglipton, useNA="ifany") 
table(diabetesMedsTest$insulin, useNA="ifany") 
table(diabetesMedsTest$change, useNA="ifany")
table(diabetesMedsTest$diabetesMed, useNA="ifany") 

#This change I am making after I merged the files for test and train- and i noticed the uneven amount of columns-
#i will drop the ones i am missing
diabetesMedsTest <- select(diabetesMedsTest,-miglitol, -nateglinide, -tolbutamide, -troglitazone, -chlorpropamide)

  
#Doing one hot encoding

OHE_MedsTest <- one_hot(diabetesMedsTest, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE)
str(OHE_MedsTest)



################## We are done with diabetesMedsTest file #####################
################ Moving to diabetesHospitalInfoTest file ######################

dim(diabetesHospitalInfoTest) #7500 rows 16 columns
str(diabetesHospitalInfoTest)
head(diabetesHospitalInfoTest)
plot_str(diabetesHospitalInfoTest)
introduce(diabetesHospitalInfoTest) #total_missing_values = 0 


#To check how many NAN's do we have- we transform values such as "" to NAN's:
diabetesHospitalInfoTest$admission_type_id <- ifelse(diabetesHospitalInfoTest$admission_type_id == "", NA, diabetesHospitalInfoTest$admission_type_id)
diabetesHospitalInfoTest$discharge_disposition_id <- ifelse(diabetesHospitalInfoTest$discharge_disposition_id == "", NA, diabetesHospitalInfoTest$discharge_disposition_id)
diabetesHospitalInfoTest$admission_source_id <- ifelse(diabetesHospitalInfoTest$admission_source_id == "", NA, diabetesHospitalInfoTest$admission_source_id)
diabetesHospitalInfoTest$medical_specialty <- ifelse(diabetesHospitalInfoTest$medical_specialty == "?", NA, diabetesHospitalInfoTest$medical_specialty)
#Checking columns for NAN's one more time
plot_missing(diabetesHospitalInfoTest)

#Checking all columns we have and categories in it. Removing irrelevant/not significant data:

table(diabetesHospitalInfoTest$admission_type_id, useNA="ifany") 
#In admission_type_id column there is only one "Newborn" data- removing it
diabetesHospitalInfoTest <- diabetesHospitalInfoTest %>%
  filter(!grepl("Newborn", admission_type_id))

table(diabetesHospitalInfoTest$discharge_disposition_id, useNA="ifany") 
#In discharge_disposition_id column there is only one "Admitted as an inpatient to this hospital" data- removing it.
diabetesHospitalInfoTest <- diabetesHospitalInfoTest %>%
  filter(!grepl("Admitted as an inpatient to this hospital", discharge_disposition_id))
#The main idea of analys is to find top 100 patients that expected to be readmitted, so categories such as "Expired", 
#"Hospice / home", "Hospice / medical facility"- are irrelevant. These people either already died or dying. We will drop them.
diabetesHospitalInfoTest <- diabetesHospitalInfoTest %>%
  filter(!grepl("Expired", discharge_disposition_id))
diabetesHospitalInfoTest <- diabetesHospitalInfoTest %>%
  filter(!grepl("Hospice / home", discharge_disposition_id))
diabetesHospitalInfoTest <- diabetesHospitalInfoTest %>%
  filter(!grepl("Hospice / medical facility", discharge_disposition_id))

table(diabetesHospitalInfoTest$admission_source_id, useNA="ifany") 
#Logically, the admission source cannot influence patience's health and the probability of if he will be readmitted or not.
#I will drop this column.
diabetesHospitalInfoTest <- select(diabetesHospitalInfoTest,-admission_source_id)

table(diabetesHospitalInfoTest$time_in_hospital, useNA="ifany") 
#Everything is ok

table(diabetesHospitalInfoTest$medical_specialty, useNA="ifany")
#To many missing values- dropping the column
diabetesHospitalInfoTest <- select(diabetesHospitalInfoTest,-medical_specialty)

table(diabetesHospitalInfoTest$num_lab_procedures, useNA="ifany") 
#Everything is ok
table(diabetesHospitalInfoTest$num_procedures, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTest$num_medications, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTest$number_outpatient, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTest$number_emergency, useNA="ifany")
#Everything is ok
table(diabetesHospitalInfoTest$number_diagnoses, useNA="ifany") 
#Everything is ok

table(diabetesHospitalInfoTest$diag_1_desc, useNA="ifany") 
table(diabetesHospitalInfoTest$diag_2_desc, useNA="ifany") 
table(diabetesHospitalInfoTest$diag_3_desc, useNA="ifany")
#Checking how many categories have diag_1_desc, diag_2_desc,diag_3_desc have.
unique(diabetesHospitalInfoTest$diag_1_desc)
unique(diabetesHospitalInfoTest$diag_2_desc)
unique(diabetesHospitalInfoTest$diag_3_desc)
#I will drop them- The only way how i can extract something beneficial from these columns is NLP- i do not posses this skill. 
diabetesHospitalInfoTest <- select(diabetesHospitalInfoTest,-diag_1_desc, -diag_2_desc, -diag_3_desc)

#Transforming columns. Changing categories in these columns.
#Starting with discharge_disposition_id column- we will leave only 5 categories: "Discharged to home", "Discharged/transferred to another medical institution/facility,
#"Discharged/transferred to home under care/home health service"," Left AMA" , "Not Mapped"
diabetesHospitalInfoTest$discharge_disposition_id <- ifelse(diabetesHospitalInfoTest$discharge_disposition_id %in% c("Discharged/transferred to a long term care hospital.", 
                                                                                                                       "Discharged/transferred to a nursing facility certified under Medicaid but not certified under Medicare", 
                                                                                                                       "Discharged/transferred to another  type of inpatient care institution", 
                                                                                                                       "Discharged/transferred to another rehab fac including rehab units of a hospital.", 
                                                                                                                       "Discharged/transferred to another short term hospital", 
                                                                                                                       "Discharged/transferred to ICF", 
                                                                                                                       "Discharged/transferred to SNF", 
                                                                                                                       "Discharged/transferred within this institution to Medicare approved swing bed", 
                                                                                                                       "Discharged/transferred/referred another institution for outpatient services", 
                                                                                                                       "Discharged/transferred/referred to a psychiatric hospital of a psychiatric distinct part unit of a hospital", "Discharged/transferred to a federal health care facility.",
                                                                                                                     "Discharged/transferred/referred to this institution for outpatient services"), "Discharged/transferred to another medical institution/facility", diabetesHospitalInfoTest$discharge_disposition_id)
diabetesHospitalInfoTest$discharge_disposition_id <- ifelse(diabetesHospitalInfoTest$discharge_disposition_id %in% c("Discharged/transferred to home under care of Home IV provider", 
                                                                                                                       "Discharged/transferred to home with home health service"), "Discharged/transferred to home under care/home health service", diabetesHospitalInfoTest$discharge_disposition_id)

#Filling NAN's
#Using the percentage distribution of categories
set.seed(123)
prop <- prop.table(table(diabetesHospitalInfoTest$admission_type_id, useNA = "ifany"))
diabetesHospitalInfoTest$admission_type_id[is.na(diabetesHospitalInfoTest$admission_type_id)] <- sample(c("Elective", "Emergency", "Not Available", "Not Mapped", "Urgent" ), size = sum(is.na(diabetesHospitalInfoTest$admission_type_id)), replace = TRUE, prob = c(prop["Elective"], prop["Emergency"], prop["Not Available"],prop["Not Mapped"],prop["Urgent"]))

set.seed(123)
prop <- prop.table(table(diabetesHospitalInfoTest$discharge_disposition_id, useNA = "ifany"))
diabetesHospitalInfoTest$discharge_disposition_id[is.na(diabetesHospitalInfoTest$discharge_disposition_id)] <- sample(c("Discharged to home",
                                                                                                                          "Discharged/transferred to another medical institution/facility",
                                                                                                                          "Discharged/transferred to home under care/home health service",
                                                                                                                          "Left AMA",
                                                                                                                          "Not Mapped"), size = sum(is.na(diabetesHospitalInfoTest$discharge_disposition_id)), replace = TRUE, prob = c(prop["Discharged to home"], prop["Discharged/transferred to another medical institution/facility"], prop["Discharged/transferred to home under care/home health service"],prop["Not Mapped"],prop["Left AMA"]))

#Do one hot encoding
OHE_HospTest <- one_hot(diabetesHospitalInfoTest, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE)
str(OHE_HospTest)
#############################################################################################################################
# The data is now clean, time to join them together


#Now we merge transformed and cleaned files
#Left join dataframes on 'tmpID'
Train <- OHE_HospTrain %>%
  left_join(OHE_MedsTrain, by = "tmpID") %>%
  left_join(OHE_PatientTrain, by = "tmpID")

Test <- OHE_HospTest %>% # This is the TEST DATA that i will use to implement the trained model
  left_join(OHE_MedsTest, by = "tmpID") %>%
  left_join(OHE_PatientTest, by = "tmpID")



# Now we partition the Train set to have a validation set and avoid overfitting with the full data
set.seed(2022)
idxPrep        <- sample(1:nrow(Train),.6*nrow(Train))
train    <- Train[idxPrep,]
valid <- Train[-idxPrep,]


str(train) 
str(valid)
    
train$readmitted_y <- factor(train$readmitted_y, levels = c(1,0), labels = c("Y_1", "Y_0"))
valid$readmitted_y <- factor(valid$readmitted_y, levels = c(1,0), labels = c("Y_1", "Y_0"))
Test$readmitted_y <- factor(Test$readmitted_y, levels = c(1,0), labels = c("Y_1", "Y_0"))

# realized that my column names contain special signs that need to be removed
# Get the column names of the data frame
cols <- names(train)
# Use gsub to replace all occurrences of "" / < > with _
cols <- gsub("[\"\\/<>]", "_", cols)
# Assign the updated column names back to the data frame
names(train) <- cols
colnames(train) <- gsub(" ", "_", colnames(train))

cols <- names(Test)
cols <- gsub("[\"\\/<>]", "_", cols)
names(Test) <- cols
colnames(Test) <- gsub(" ", "_", colnames(Test))

cols <- names(valid)
cols <- gsub("[\"\\/<>]", "_", cols)
names(valid) <- cols
colnames(valid) <- gsub(" ", "_", colnames(valid))
names(valid)

#################################################################################
#################################### Modeling ###################################
################################# Random Forest #################################
#################################################################################

library(ranger) # activate ranger model
# fit a random forest model with ranger

# hyperparameter grid
grid <- expand.grid(.mtry = c(1,2,4,6),
                    .splitrule = c('extratrees', 'gini'),
                    .min.node.size = c(1,2,3,5))

fitcontrol <- trainControl(method = "cv",
                           number = 4,
                           returnData = FALSE,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           verboseIter = TRUE,
                           returnResamp = "final",
                           allowParallel = TRUE)

fit_rf <- train(as.factor(readmitted_y) ~ ., data = train,
             method = 'ranger',
             num.trees = 250,
             tuneGrid = grid,
             trControl = fitcontrol)

fit_rf$finalModel
# selecting tuning parameters
# fitting mtry = 4, splitrule = gini, min.node.size = 5 on full training set

fitpredict <- predict(fit_rf, Test)
confusionMatrix(fitpredict, Test$readmitted_y)
# I will evaluate my models based on which one achieves more True Positives 
# This model gets 240 True Positives



# now lets optimize
fit_rf <- train(as.factor(readmitted_y) ~ ., data = train,
             method = 'ranger',
             tuneGrid = grid,
             trControl = fitcontrol)
fit_rf$finalModel$num.trees # max number of trees 500
fit_rf$finalModel$prediction.error # 22% prediction error 

# tuning
numtreesvec <- vector()
ooberror  <- vector()
ntreesearch <- seq(from = 100, to = 500, by=50)
for(i in 1:length(ntreesearch)){
  print(i)
  fit_rf <- train(as.factor(readmitted_y) ~ ., data = train,
                method = 'ranger',
                num.trees = ntreesearch[i],
                tuneGrid = grid,
                trControl = fitcontrol)
  numtreesvec[i] <- fit_rf$finalModel$num.trees
  ooberror[i] <- fit_rf$finalModel$prediction.error
}

results <- data.frame(ntrees =numtreesvec,
                       oobError = ooberror)
ggplot(results, aes(x=ntrees,y=ooberror)) + geom_line(alpha =0.25, color = 'red') +
   theme_gdocs()+
   geom_smooth(method = "loess")
# best choice 250 ---> change for the model above 


#################################################################################
############################### Logistic regression #############################
#################################################################################

fitControl <- trainControl(method = "cv",
                           number = 4, # number of folds
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           verboseIter = TRUE)

fit_glmnet <- train(readmitted_y ~ ., 
                    data = train, 
                    method = "glmnet", 
                    trControl = fitControl, 
                    tuneLength = 10, # number of lambda values to try
                    family = "binomial") 

glmnet_pred <- predict(fit_glmnet, Test)
confusionMatrix(glmnet_pred, Test$readmitted_y)
# This model gets 200 True Positives

#################################################################################
################################## Decision tree ################################
#################################################################################

library(rpart)

fitControl <- trainControl(method = "cv",
                           number = 4,
                           returnData = FALSE,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           verboseIter = TRUE,
                           returnResamp = "final",
                           allowParallel = TRUE)

grid <- expand.grid(.cp = c(0.0001, 0.001, 0.005, 0.01))

fit_dt <- train(readmitted_y ~., 
                data = train, 
                method = "rpart", 
                tuneGrid = grid, 
                trControl = fitControl,
                control = rpart.control(minsplit = 100, minbucket = 50)) 

# Predict on valid
dtpredict <- predict(fit_dt, valid)
confusionMatrix(dtpredict, valid$readmitted_y)
# This model gets 400 True Positives

dtprob <- predict(fit_dt, valid, type= "prob")
nCutoff <- .5
dtpred <- ifelse(dtprob[, "Y_1"] >= nCutoff, "Y_1", "Y_0")
confusionMatrix(factor(dtpred, levels = c("Y_1", "Y_0")), valid$readmitted_y)

head(dtpred)

# Predict on Test
dtpredict <- predict(fit_dt, Test)
confusionMatrix(dtpredict, Test$readmitted_y)
dtprob <- predict(fit_dt, Test, type= "prob")
nCutoff <- .5
dtpred <- ifelse(dtprob[, "Y_1"] >= nCutoff, "Y_1", "Y_0")
confusionMatrix(factor(dtpred, levels = c("Y_1", "Y_0")), Test$readmitted_y)

head(dtpred)

library(rpart.plot)

# fit the rpart model on the training data
fit_dt <- rpart(readmitted_y ~., data=train, control=rpart.control(minsplit=100, minbucket=50))

# create the feature importance plot
rpart.plot(fit_dt, type=4, extra=106, under=TRUE, main="Feature Importance")

# This model gets 400 True Positives
# Accuracy : 63% 
#I will use this model to extract top 100 patients with the highest prediction level
##################################################################################
# Sorting probability and creating a df
probability <- dtprob[,"Y_1"]
results <- data.frame(tmpID = Test$tmpID,
                      actuals = Test$readmitted_y,
                      classes = dtpred,
                      probability = probability)


top_100 <- head(results[order(-results$probability), ], n = 100)

# order results df by descending order 
# and then head 100 to get the top 100 rows filtered by probability

# Save as csv
write.csv(top_100, "top_100.csv", row.names = FALSE)



##################################################################################
########################### Extracting insights ##################################
##################################################################################

# Now we need to identify trends and patters among these 100 patients with the highest
# probability of being readmitted to the hospital. 

# I will take cleaned data as it has all necessary transformations.
# I will left join them with my top_100 csv.

full_top <- top_100 %>%
  left_join(diabetesPatientTest, by = "tmpID") %>%
  left_join(diabetesMedsTest, by = "tmpID") %>%
  left_join(diabetesHospitalInfoTest, by = "tmpID")
#For my analysis I will also take full test data set filtered by "1" readmitted patients 
test_df <- diabetesPatientTest  %>% 
  left_join(diabetesMedsTest, by = "tmpID") %>%
  left_join(diabetesHospitalInfoTest, by = "tmpID")
# test_df <- test_df %>% filter(readmitted_y == 1)
# 
# test_df_notr <- diabetesPatientTest  %>% 
#   left_join(diabetesMedsTest, by = "tmpID") %>%
#   left_join(diabetesHospitalInfoTest, by = "tmpID")
# test_df_notr <- test_df_notr %>% filter(readmitted_y == 0)
test_df$readmitted_y <- as.factor(test_df$readmitted_y)
full_top$readmitted_y <- as.factor(full_top$readmitted_y)

table(full_top$age_groups, useNA="ifany")
(full_top)
head(full_top)
str(full_top)
names(full_top)


# Charts

################################################################################
# Creating  bar charts for top 100 predicted patients and for patients form full 
# test dataset- comparing them. 
################################################################################

# creating bins for age for full_top
breaks <- c(30, 40, 50, 60, 70, 80, 90, 100)
labels <- c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-100")

# column with age groups for full_top
age_groups <- cut(full_top$age, breaks = breaks, labels = labels)
full_top$age_groups <- age_groups

# creating bins for age for test_df
breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-100")

# column with age groups for test_df
age_groups_test <- cut(test_df$age, breaks = breaks, labels = labels)
test_df$age_groups <- age_groups_test


# function to plot the bar chart
plot_barchart <- function(data, title) {
  # percentages
  perc <- data %>% 
    group_by(age_groups, .drop = FALSE) %>% 
    summarise(n = n()) %>%
    mutate(perc = n/sum(n) * 100)
  
  total_count <- sum(perc$n)
  
  ggplot(perc, aes(x = age_groups, y = n)) +
    geom_col(position = "dodge", fill = "#0072B2") +
    ggtitle(title) +
    xlab("Age") +
    ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", n/total_count*100), "%\n", n)), 
              position = position_dodge(width = 1),
              vjust = -0.5,
              size = 3)
}
  # bar chart for top 100 patients 
plot_barchart(full_top, "Age for Patients from top 100 list")
  # bar chart for patients on full test dataset
plot_barchart(test_df,  "Age for Patients from full dataset")
  

plot_barchart <- function(data, title) {
  # calculate percentages by age group and readmission status
  perc <- data %>% 
    group_by(age_groups, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(age_groups = factor(age_groups)) %>%
    left_join(data %>% 
                group_by(age_groups) %>% 
                summarise(total = n())) %>%
    mutate(perc = count / total)
  
  # create  bar chart with fill by readmission status
  ggplot(perc, aes(x = age_groups, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Age") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) 
} 


# bar chart for top 100 patients 
plot_barchart(full_top, "Age and readmission status for Patients from top 100 list")


# bar chart for patients on full test dataset
plot_barchart(test_df,  "Age and readmission status for Patients from full dataset")


################################### GENDER #####################################
################################################################################
#function to plot the bar chart
plot_barchart <- function(data, title) {
  # percentages by gender 
  perc <- data %>% 
    group_by(gender, .drop = FALSE) %>% 
    summarise(n = n()) %>%
    group_by(gender, .drop = FALSE) %>%
    mutate(perc = n/sum(n) * 100)
  
  ggplot(perc, aes(x = gender, y = n)) +
    geom_col(position = "dodge") +
    ggtitle(title) +
    xlab("Age") +
    ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", perc), "%\n", n)), 
              position = position_dodge(width = 1),
              vjust = -0.5,
              size = 3)
}

## bar chart for top 100 patients 
plot_barchart(full_top, "Gender for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df, "Gender for Patients from top 100 list")

# plot for gender
plot_barchart <- function(data, title) {
  # calculate percentages by gender and readmission status
  perc <- data %>% 
    group_by(gender, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(gender = factor(gender)) %>%
    left_join(data %>% 
                group_by(gender) %>% 
                summarise(total = n())) %>%
    mutate(perc = count / total)
  
  # bar chart with fill by readmission status
  ggplot(perc, aes(x = gender, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Gender") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) 
}



# bar chart for top 100 patients 
plot_barchart(full_top, "Gender and readmission status for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Gender and readmission status for Patients from full dataset")

####################################RACE########################################
# plot for race
plot_barchart <- function(data, title) {
  # calculate percentages 
  perc <- data %>% 
    group_by(race, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(gender = factor(race)) %>%
    left_join(data %>% 
                group_by(race) %>% 
                summarise(total = n())) %>%
    mutate(perc = count / total)
  
  # bar chart with fill by readmission status
  ggplot(perc, aes(x = race, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Race") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) 
}


# bar chart for top 100 patients 
plot_barchart(full_top, "Race and readmission status for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Race and readmission status for Patients from full dataset")


########################## AGE AND GENDER ######################################
############################# TOGETHER #########################################

# function to plot the bar chart
plot_barchart <- function(data, title) {
  # percentages by gender and age group
  perc <- data %>% 
    group_by(age_groups, gender, .drop = FALSE) %>% 
    summarise(n = n()) %>%
    group_by(age_groups, .drop = FALSE) %>%
    mutate(perc = n/sum(n) * 100)
  
  ggplot(perc, aes(x = age_groups, y = n, fill = gender)) +
    geom_col(position = "dodge") +
    ggtitle(title) +
    xlab("Age") +
    ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", perc), "%\n", n)), 
              position = position_dodge(width = 1),
              vjust = -0.5,
              size = 3)
}

## bar chart for top 100 patients 
plot_barchart(full_top, "Age and gender for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Age and gender for Patients from full dataset")

# plot the bar chart
plot_barchart <- function(data, title) {
  # calculate percentages by gender, age group and readmission status
  perc <- data %>% 
    group_by(gender, age_groups, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(gender = factor(gender), age_groups = factor(age_groups)) %>%
    left_join(data %>% 
                group_by(gender, age_groups) %>% 
                summarise(total = n())) %>%
    mutate(perc = count / total)
  
  # create stacked bar chart with fill by readmission status
  ggplot(perc, aes(x = age_groups, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Age") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) +
    facet_grid(rows = vars(gender), scales = "free_x") # add facet grid by gender
}

# bar chart for top 100 patients 
plot_barchart(full_top, "Age and gender, and readmission status for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Age and gender, and readmission status for Patients from full dataset")

########################## time_in_hospital ######################################
##################################################################################
# function to plot the bar chart
plot_barchart <- function(data, title) {
  # percentages
  perc <- data %>% 
    group_by(time_in_hospital, .drop = FALSE) %>% 
    summarise(n = n()) %>%
    mutate(perc = n/sum(n) * 100)
  
  total_count <- sum(perc$n)
  
  ggplot(perc, aes(x = time_in_hospital, y = n)) +
    geom_col(position = "dodge", fill = "#0072B2") +
    ggtitle(title) +
    xlab("Time in hospital") +
    ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", n/total_count*100), "%\n", n)), 
              position = position_dodge(width = 1),
              vjust = -0.5,
              size = 3)
}

# bar chart for top 100 patients 
plot_barchart(full_top, "Time in hospital for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Time in hospital for Patients from full dataset")

plot_barchart <- function(data, title) {
  # calculate percentages 
  perc <- data %>% 
    group_by(time_in_hospital, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(time_in_hospital = factor(time_in_hospital)) %>%
    left_join(data %>% 
                group_by(time_in_hospital) %>% 
                summarise(total = n()) %>% 
                mutate(time_in_hospital = factor(time_in_hospital))) %>%
    mutate(perc = count / total)
  
  # bar chart with fill by readmission status
  ggplot(perc, aes(x = time_in_hospital, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Time in hospital") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) 
}

# bar chart for top 100 patients 
plot_barchart(full_top, "Time in hospital and readmission status for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Time in hospital and readmission status for Patients from full dataset")

########################## admission_type_id ######################################
##################################################################################
# plot the bar chart
plot_barchart <- function(data, title) {
  # percentages
  perc <- data %>% 
    group_by(admission_type_id, .drop = FALSE) %>% 
    summarise(n = n()) %>%
    mutate(perc = n/sum(n) * 100)
  
  total_count <- sum(perc$n)
  
  ggplot(perc, aes(x = admission_type_id, y = n)) +
    geom_col(position = "dodge", fill = "#0072B2") +
    ggtitle(title) +
    xlab("Admission type") +
    ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", n/total_count*100), "%\n", n)), 
              position = position_dodge(width = 1),
              vjust = -0.5,
              size = 3)
}


# bar chart for top 100 patients 
plot_barchart(full_top, "Admission type for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Admission type for Patients from full dataset")

# plot
plot_barchart <- function(data, title) {
  # calculate percentages
  perc <- data %>% 
    group_by(admission_type_id, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(admission_type_id = factor(admission_type_id)) %>%
    left_join(data %>% 
                group_by(admission_type_id) %>% 
                summarise(total = n())) %>%
    mutate(perc = count / total)
  
  # bar chart with fill by readmission status
  ggplot(perc, aes(x = admission_type_id, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Admission type") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) 
}

# bar chart for top 100 patients 
plot_barchart(full_top, "Admission type and readmission status for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Admission type and readmission status for Patients from full dataset")

########################## discharge_disposition_id ##############################
##################################################################################
# function to plot the bar chart
plot_barchart <- function(data, title) {
  # percentages
  perc <- data %>% 
    group_by(discharge_disposition_id, .drop = FALSE) %>% 
    summarise(n = n()) %>%
    mutate(perc = n/sum(n) * 100)
  
  total_count <- sum(perc$n)
  
  ggplot(perc, aes(x = discharge_disposition_id, y = n)) +
    geom_col(position = "dodge", fill = "#0072B2") +
    ggtitle(title) +
    xlab("Discharge Disposition") +
    ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", n/total_count*100), "%\n", n)), 
              position = position_dodge(width = 1),
              vjust = -0.5,
              size = 3)
}

# bar chart for top 100 patients 
plot_barchart(full_top, "Discharge Disposition for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Discharge Disposition for Patients from full dataset")


# plot
plot_barchart <- function(data, title) {
  # calculate percentages 
  perc <- data %>% 
    group_by(discharge_disposition_id, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(discharge_disposition_id = factor(discharge_disposition_id)) %>%
    left_join(data %>% 
                group_by(discharge_disposition_id) %>% 
                summarise(total = n())) %>%
    mutate(perc = count / total)
  
  
  # bar chart with fill by readmission status
  ggplot(perc, aes(x = discharge_disposition_id, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Discharge Disposition") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) 
}
# bar chart for top 100 patients 
plot_barchart(full_top, "Discharge Disposition and readmission status for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Discharge Disposition and readmission status for Patients from full dataset")

########################## weight ################################################
##################################################################################
bins <- seq(56, 378, by = 30)
test_df$weight <- cut(test_df$wgt, bins, include.lowest = TRUE)
full_top$weight <- cut(full_top$wgt, bins, include.lowest = TRUE)


# function to plot the bar chart
plot_barchart <- function(data, title) {
  # calculate percentages by age group and readmission status
  perc <- data %>% 
    group_by(weight, readmitted_y) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(weight = factor(weight)) %>%
    left_join(data %>% 
                group_by(weight) %>% 
                summarise(total = n())) %>%
    mutate(perc = count / total)
  
  #  bar chart with fill by readmission status
  ggplot(perc, aes(x = weight, y = total, fill = readmitted_y)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    xlab("Weight") +
    ylab("Count") +
    scale_y_continuous(labels = comma_format()) +
    scale_fill_manual(values = c("#E69F00", "#0072B2")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    geom_text(aes(label = paste0(sprintf("%.1f", .data$perc * 100), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3) 
}


# bar chart for top 100 patients 
plot_barchart(full_top, "Weight and readmission status for Patients from top 100 list")
# bar chart for patients on full test dataset
plot_barchart(test_df,  "Weight and readmission status for Patients from full dataset")

#End