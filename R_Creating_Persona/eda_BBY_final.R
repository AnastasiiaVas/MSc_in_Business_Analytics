#' Author: Anastasia Vasileva
#' Data: March 25th,2023
#' Purpose: BedBathYonder EDA
#' 

# libs
library(radiant.data)
library(DataExplorer)
library(ggplot2)
library(ggthemes)
library(rbokeh)
library(dplyr)
library(powerjoin)
library(DataExplorer)


# Set WD
setwd("C:/Users/anast/OneDrive/Рабочий стол/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Data look up on github
consumerData <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv')
donationsData <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/DonationsData_training15K_studentVersion.csv')
inHouse <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv')
magazineData <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/magazineData_training15K_studentVersion.csv')
politicalData <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/politicalData_training15K_studentVersion.csv')

#Dimensions of DF's
dim(consumerData$tmpID) #15000 26
dim(donationsData) #15000 15
dim(inHouse) #10332 20
dim(magazineData) #15000 10
dim(politicalData) #15000 13

##Firstly, I wil work with each file separately

############################# ConsumerData #####################################
################################################################################

#Starting with consumerData csv.

dim(consumerData) #15000 rows 25 columns
head(consumerData)
plot_str(consumerData)
introduce(consumerData)

#Checking all columns we have
names(consumerData)

#Work with columns- starting with "BroadEthnicGroupings" and "EthnicDescription"
table(consumerData$BroadEthnicGroupings) #... of empty spaces
sum(is.na(consumerData$BroadEthnicGroupings)) #no null values BUT we have empty spaces

table(consumerData$EthnicDescription) #... of empty spaces
sum(is.na(consumerData$EthnicDescription))#no null values BUT we have empty spaces

#I want to leave only one ethnic column- BroadEthnicGroupings
#This column contains empty spaces- i will impute them with values from EthnicDescription
consumerData$BroadEthnicGroupings <- ifelse(consumerData$BroadEthnicGroupings == "", consumerData$EthnicDescription, consumerData$BroadEthnicGroupings)

#Now i can drop EthnicDescription
consumerData <- select(consumerData,-EthnicDescription)

#We still have 101 values of empty space- i will replace it with "Unknown"
consumerData$BroadEthnicGroupings <- ifelse(consumerData$BroadEthnicGroupings == "", "unknown", consumerData$BroadEthnicGroupings)

#Work with columns- Continuing with "PresenceOfChildrenCode" 
table(consumerData$PresenceOfChildrenCode) #1660 of empty spaces
sum(is.na(consumerData$PresenceOfChildrenCode)) #no null values BUT we have empty spaces

#We can see that this column have 4 categories: Likely to have a child, Modeled Likely to have a child, 
#Modeled Not as Likely to have a child, Not Likely to have a child - i will replace them with binary values of 1 and 0.
consumerData$PresenceOfChildrenCode <- ifelse(consumerData$PresenceOfChildrenCode %in% c("Likely to have a child", "Modeled Likely to have a child"), 1,
                                              ifelse(consumerData$PresenceOfChildrenCode %in% c("Modeled Not as Likely to have a child", "Not Likely to have a child"), 0,
                                                     consumerData$PresenceOfChildrenCode))
table(consumerData$PresenceOfChildrenCode) 
#we have 1660 values of empty spaces, 
#8844  values of 0 (people who are not ikely to have a child), 
#and 4496 values of 1 (people who are not ikely to have a child)

#Now we replace empty spaces 
#The column is of a "character" class- we need to convert it to numeric first
class(consumerData$PresenceOfChildrenCode)
consumerData$PresenceOfChildrenCode <- as.numeric(consumerData$PresenceOfChildrenCode)
table(consumerData$PresenceOfChildrenCode, useNA = "ifany")

#Calculating percentage distribution of "1" and "0"
prop.table(table(consumerData$PresenceOfChildrenCode, useNA = "ifany"))

#Replacing NAN's using the percentage distribution of "1" and "0"
set.seed(123) 
prop <- prop.table(table(consumerData$PresenceOfChildrenCode, useNA = "ifany"))
consumerData$PresenceOfChildrenCode[is.na(consumerData$PresenceOfChildrenCode)] <- sample(c(0, 1), size = sum(is.na(consumerData$PresenceOfChildrenCode)), replace = TRUE, prob = c(prop[1], prop[2]))

#Calculating new percentage distribution of "1" and "0" 
table(consumerData$PresenceOfChildrenCode, useNA = "ifany") / nrow(consumerData) * 100

#Work with columns- Continuing with "ISPSA" and "MosaicZ4"
table(consumerData$ISPSA, useNA = "ifany")
#"ISPSA" stands for Index of Social Position for Small Areas- this column has potential information,
#but unfortunately I did not find any explanation of how to interpret this lifestyle classification system(index).
table(consumerData$MosaicZ4, useNA = "ifany")
#"MosaicZ4"- is a household-based consumer lifestyle segmentation system, that can be very
#useful for  customer segmentation and marketing. Unfortunately, 80% are empty spaces-
#this makes this column useless.

#I will drop these 2 columns
consumerData <- select(consumerData,-ISPSA, -MosaicZ4, -ResidenceHHGenderDescription)

#Work with columns- Continuing with "HomeOwnerRenter"
table(consumerData$HomeOwnerRenter, useNA = "ifany")

#We see that this column have 2 categories: "Likely Homeowner", "Likely Renter" 
#I will replace them with binary values of 1 (for Likely Homeowner) and 0 (for Likely Renter).
consumerData$HomeOwnerRenter <- ifelse(consumerData$HomeOwnerRenter %in% c("Likely Homeowner"), 1,
                                              ifelse(consumerData$HomeOwnerRenter %in% c("Likely Renter"), 0,
                                                     consumerData$HomeOwnerRenter))
table(consumerData$HomeOwnerRenter) #1020- empty spaces, 2358 of 0, and 11622  of 1 

class(consumerData$HomeOwnerRenter) # "character"-we will convert "numeric"
consumerData$HomeOwnerRenter <- as.numeric(consumerData$HomeOwnerRenter)
table(consumerData$HomeOwnerRenter, useNA = "ifany")

#Calculating percentage distribution of "1" and "0"
prop.table(table(consumerData$HomeOwnerRenter, useNA = "ifany"))

#Replacing NAN's using the percentage distribution of "1" and "0"
set.seed(123) 
prop <- prop.table(table(consumerData$HomeOwnerRenter, useNA = "ifany"))
consumerData$HomeOwnerRenter[is.na(consumerData$HomeOwnerRenter)] <- sample(c(0, 1), size = sum(is.na(consumerData$HomeOwnerRenter)), replace = TRUE, prob = c(prop[1], prop[2]))

#Calculating new percentage distribution of "1" and "0" 
table(consumerData$HomeOwnerRenter, useNA = "ifany") / nrow(consumerData) * 100


#Work with columns- Continuing with "MedianEducationYears"
table(consumerData$MedianEducationYears, useNA = "ifany") #484 Nan values
#Replacing nan's with the mean value- calculating the mean firstly and rounding it
mean_educ <- mean(consumerData$MedianEducationYears, na.rm = TRUE)
mean_rounded <- round(mean_educ, digits = 0)
# Replace missing values with the mean value
consumerData$MedianEducationYears[is.na(consumerData$MedianEducationYears)] <- mean_rounded


#Work with columns- Continuing with "OccupationIndustry"
table(consumerData$OccupationIndustry, useNA = "ifany") #9588 "Unknown" values
#I will not impute these values and leave the column- it maybe useful in the future


#Work with columns- Continuing with "Education"
table(consumerData$Education, useNA = "ifany") #2761 "Unknown" values
#We already have column devoted to the education(MedianEducationYears) with no missing values. 
#Based on "MedianEducationYears" column we can assume approximate level of education. 
#This column("Education") has 2761 "Unknown" values that we cannot fill anyhow in a logical way.
#Thus, if "Education" column has missing values and we already have variable about education- i will drop this column.
consumerData <- select(consumerData,-Education)


#Work with columns- Continuing with "Investor"
table(consumerData$Investor, useNA = "ifany") #8701 empty spaces
prop.table(table(consumerData$Investor, useNA = "ifany"))*100 #58% empty spaces
#I will now fill in NAN's and leave this column- maybe useful
consumerData$Investor <- ifelse(consumerData$Investor == "", "unknown", consumerData$Investor)


#Work with columns- Continuing with "BusinessOwner"
table(consumerData$BusinessOwner, useNA = "ifany") #14738 empty spaces
prop.table(table(consumerData$BusinessOwner, useNA = "ifany"))*100 #98% empty spaces
#Almost empty column- dropping it
consumerData <- select(consumerData,-BusinessOwner)


#Work with columns- Continuing with "NetWorth"
table(consumerData$NetWorth, useNA = "ifany") 
prop.table(table(consumerData$NetWorth, useNA = "ifany"))*100 
#We have 8 categories that i want to replace with average value and convert to numeric
#I will also create new column "AverageNetWorth"
consumerData$AverageNetWorth[consumerData$NetWorth == "$1-4999"] <- 2500
consumerData$AverageNetWorth[consumerData$NetWorth == "$5000-9999"] <- 7500
consumerData$AverageNetWorth[consumerData$NetWorth == "$10000-24999"] <- 17500
consumerData$AverageNetWorth[consumerData$NetWorth == "$25000-49999"] <- 37500
consumerData$AverageNetWorth[consumerData$NetWorth == "$50000-99999"] <- 75000
consumerData$AverageNetWorth[consumerData$NetWorth == "$100000-249999"] <- 175000
consumerData$AverageNetWorth[consumerData$NetWorth == "$250000-499999"] <- 375000
consumerData$AverageNetWorth[consumerData$NetWorth == "$500000+"] <- 500000

#Converting to numeric
consumerData$AverageNetWorth <- as.numeric(consumerData$AverageNetWorth)

#Fillin NAN's
mean_AverageNetWorth <- mean(consumerData$AverageNetWorth, na.rm = TRUE)
mean_AverageNetWorth_rounded <- round(mean_AverageNetWorth, digits = 0)

# Replace missing values with the mean value
consumerData$AverageNetWorth[is.na(consumerData$AverageNetWorth)] <- mean_AverageNetWorth_rounded

#Dropping
consumerData <- select(consumerData,-NetWorth)

                                       
#Work with "pets" columns- Continuing with "HorseOwner", "CatOwner", "DogOwner", "OtherPetOwner"
table(consumerData$HorseOwner, useNA = "ifany") #14783 empty spaces
table(consumerData$CatOwner, useNA = "ifany") #13527 empty spaces
table(consumerData$DogOwner, useNA = "ifany") #12888 empty spaces
table(consumerData$OtherPetOwner, useNA = "ifany") #12755 empty spaces

prop.table(table(consumerData$HorseOwner, useNA = "ifany"))*100 #98% empty spaces
prop.table(table(consumerData$CatOwner, useNA = "ifany"))*100 #90% empty spaces
prop.table(table(consumerData$DogOwner, useNA = "ifany"))*100 #85% empty spaces
prop.table(table(consumerData$OtherPetOwner, useNA = "ifany"))*100 #85% empty spaces

#Too much of data is missing- i will drop these columns
consumerData <- select(consumerData,-HorseOwner, -CatOwner, -DogOwner, -OtherPetOwner)


#Column below are almost empty and have 70-99% of empty spaces. I will drop them
prop.table(table(consumerData$HomeOffice, useNA = "ifany"))*100 #82% empty spaces
prop.table(table(consumerData$UpscaleBuyerInHome, useNA = "ifany"))*100 #97% empty spaces
prop.table(table(consumerData$BuyerofAntiquesinHousehold, useNA = "ifany"))*100 #99% empty spaces
prop.table(table(consumerData$BuyerofArtinHousehold, useNA = "ifany"))*100 #79% empty spaces
prop.table(table(consumerData$GeneralCollectorinHousehold, useNA = "ifany"))*100 #70% empty spaces
prop.table(table(consumerData$BooksAudioReadinginHousehold, useNA = "ifany"))*100 #99% empty spaces

consumerData <- select(consumerData,-HomeOffice, -UpscaleBuyerInHome, -BuyerofAntiquesinHousehold, -BuyerofArtinHousehold,
                       -GeneralCollectorinHousehold, -BooksAudioReadinginHousehold, -ComputerOwnerInHome)

#Work with "BookBuyerInHome"
prop.table(table(consumerData$BookBuyerInHome, useNA = "ifany"))*100 #65% empty spaces

#Column "BookBuyerInHome" has 8 categories - i will replace them with binary values of 1 and 0.
consumerData$BookBuyerInHome <- ifelse(consumerData$BookBuyerInHome %in% c(""), 0, 1)


dim(consumerData) #Now we have 11 columns
names(consumerData)
               
       

##################### We are done with ConsumerData file #######################
###################### Moving to donationsData file ############################



dim(donationsData) #15000 rows 15 columns
head(donationsData)
plot_str(donationsData)
introduce(donationsData)

#Checking all columns we have
names(donationsData)

#Let's have a look on "Donates" columns
table(donationsData$DonatesEnvironmentCauseInHome)     
table(donationsData$DonatestoArtsandCulture)    
table(donationsData$DonatestoInternationalAidCauses)   
table(donationsData$DonatestoInternationalAidCauses1)
table(donationsData$ReligiousContributorInHome)
table(donationsData$DonatesToCharityInHome)
table(donationsData$DonatestoChildrensCauses)
table(donationsData$DonatestoVeteransCauses)
table(donationsData$DonatestoWildlifePreservation)
table(donationsData$PoliticalContributerInHome)
table(donationsData$DonatestoAnimalWelfare)
table(donationsData$DonatestoHealthcare)
table(donationsData$DonatestoHealthcare1)             
table(donationsData$DonatestoLocalCommunity)

#Convering values to binary ones
donationsData$DonatesEnvironmentCauseInHome <- ifelse(donationsData$DonatesEnvironmentCauseInHome %in% c("Yes"), 1, 0)
donationsData$DonatestoArtsandCulture <- ifelse(donationsData$DonatestoArtsandCulture %in% c("Yes"), 1, 0)
donationsData$DonatestoInternationalAidCauses <- ifelse(donationsData$DonatestoInternationalAidCauses %in% c("Yes"), 1, 0)
donationsData$DonatestoInternationalAidCauses1 <- ifelse(donationsData$DonatestoInternationalAidCauses1 %in% c("Yes"), 1, 0)
donationsData$ReligiousContributorInHome <- ifelse(donationsData$ReligiousContributorInHome %in% c(""), 0, 1)
donationsData$DonatesToCharityInHome <- ifelse(donationsData$DonatesToCharityInHome %in% c("Yes"), 1, 0)
donationsData$DonatestoChildrensCauses <- ifelse(donationsData$DonatestoChildrensCauses %in% c("Yes"), 1, 0)

donationsData$DonatestoVeteransCauses <- ifelse(donationsData$DonatestoVeteransCauses %in% c("Yes"), 1, 0)
donationsData$DonatestoWildlifePreservation <- ifelse(donationsData$DonatestoWildlifePreservation %in% c("Yes"), 1, 0)
donationsData$PoliticalContributerInHome <- ifelse(donationsData$PoliticalContributerInHome %in% c(""), 0, 1)
donationsData$DonatestoAnimalWelfare <- ifelse(donationsData$DonatestoAnimalWelfare %in% c("Yes"), 1, 0)
donationsData$DonatestoHealthcare <- ifelse(donationsData$DonatestoHealthcare %in% c("Yes"), 1, 0)
donationsData$DonatestoHealthcare1 <- ifelse(donationsData$DonatestoHealthcare1 %in% c("Yes"), 1, 0)
donationsData$DonatestoLocalCommunity <- ifelse(donationsData$DonatestoLocalCommunity %in% c("Yes"), 1, 0)


#Combining all of the columns and creating one column- if person donates or not
donationsData$Donator <- rowSums(donationsData[, c("DonatesEnvironmentCauseInHome",
                                        "DonatestoArtsandCulture",
                                        "DonatestoInternationalAidCauses",
                                        "DonatestoInternationalAidCauses1",
                                        "ReligiousContributorInHome",
                                        "DonatesToCharityInHome",
                                        "DonatestoChildrensCauses",
                                        "DonatestoVeteransCauses",
                                        "DonatestoWildlifePreservation",
                                        "PoliticalContributerInHome",
                                        "DonatestoAnimalWelfare",
                                        "DonatestoHealthcare",
                                        "DonatestoHealthcare1",
                                        "DonatestoLocalCommunity")])

#Now we change values in the "Donator" column to 1 and 0 - if person donates or not
donationsData$Donator <- ifelse(donationsData$Donator %in% c("0"), 0, 1)
table(donationsData$Donator)
#Dropping old columns
donationsData <- select(donationsData,-DonatesEnvironmentCauseInHome, -DonatestoArtsandCulture, -DonatestoInternationalAidCauses,
                        -DonatestoInternationalAidCauses1, -ReligiousContributorInHome, -DonatesToCharityInHome, -DonatestoChildrensCauses,
                        -DonatestoVeteransCauses, -DonatestoWildlifePreservation, -PoliticalContributerInHome, -DonatestoAnimalWelfare,
                        -DonatestoHealthcare, -DonatestoHealthcare1, -DonatestoLocalCommunity)




##################### We are done with donationsData file ######################
####################### Moving to magazineData file ############################


dim(magazineData) #15000 rows 10 columns
head(magazineData)
plot_str(magazineData)
introduce(magazineData)

#Checking all columns we have
names(magazineData)


##Let's have a look on magazine subscription columns
table(magazineData$FamilyMagazineInHome) #11427 empty spaces                                
table(magazineData$FemaleOrientedMagazineInHome) #14123 empty spaces           
table(magazineData$ReligiousMagazineInHome) #14976 empty spaces
table(magazineData$GardeningMagazineInHome) #13953 empty spaces
table(magazineData$CulinaryInterestMagazineInHome) #14180 empty spaces
table(magazineData$HealthFitnessMagazineInHome) #10390 empty spaces
table(magazineData$DoItYourselfMagazineInHome) #13407 empty spaces   
table(magazineData$FinancialMagazineInHome) #13013 empty spaces
table(magazineData$InterestinCurrentAffairsPoliticsInHousehold) #11609 empty spaces


#Most of the data contain empty spaces and I also don't see how this data can benefit my analysis
#I won't use this file



###################### We are done with magazineData file ######################
######################## Moving to politicalData file ##########################



dim(politicalData) #15000 rows 10 columns
head(politicalData)
plot_str(politicalData)
introduce(politicalData)

#Checking all columns we have
names(politicalData)

##Let's have a look on each column
table(politicalData$ReligionsDescription) #4449 empty spaces   
table(politicalData$PartiesDescription)   
table(politicalData$GunOwner) #12005 Unknown values               
table(politicalData$supportsAffordableCareAct) #4963 empty spaces
table(politicalData$supportsGunControl) #14975 empty spaces
table(politicalData$overallsocialviews) #14854 empty spaces
table(politicalData$DonatestoLiberalCauses) #14921 empty spaces
table(politicalData$LikelyUnionMember) #13654 empty spaces
table(politicalData$Veteran) #14162 Unknown values 
table(politicalData$supportsGayMarriage) #14905 empty spaces
table(politicalData$supportsTaxesRaise) #14947 empty spaces
table(politicalData$DonatestoConservativeCauses) #14904 empty spaces
prop.table(table(politicalData$LikelyUnionMember, useNA = "ifany"))*100 #99% empty spaces

#Some columns contain 90-99% of missing values- we drop them
politicalData <- select(politicalData, -supportsGunControl, -overallsocialviews, -DonatestoLiberalCauses, 
                        -LikelyUnionMember, -Veteran, -supportsAffordableCareAct, -GunOwner,
                        -supportsGayMarriage, -supportsTaxesRaise, -DonatestoConservativeCauses)

#Filling missing values for 
politicalData$ReligionsDescription <- ifelse(politicalData$ReligionsDescription == "", "unknown", politicalData$ReligionsDescriptio)



######################## We are done with politicalData file ###################
########################### Moving to inHouse file #############################


dim(inHouse) #10332 rows 20 columns
head(inHouse)
plot_str(inHouse)
introduce(inHouse)
plot_missing(inHouse)

#Checking all columns we have
names(inHouse)

##Let's have a look on each column
table(inHouse$FirstName)#drop
table(inHouse$LastName)#drop
table(inHouse$Gender) 
table(inHouse$Age) 
table(inHouse$TelephonesFullPhone)#drop
table(inHouse$lat)#drop
table(inHouse$lon)#drop
table(inHouse$county)#drop
table(inHouse$city)#drop
table(inHouse$state) 
table(inHouse$fips)#drop
table(inHouse$stateFips)#drop 
table(inHouse$HomePurchasePrice) #remove $ sign
table(inHouse$LandValue) #remove $ sign
table(inHouse$DwellingUnitSize) #drop
table(inHouse$storeVisitFrequency) 
table(inHouse$PropertyType)#nice
table(inHouse$EstHomeValue) #remove $ sign
table(inHouse$y_householdSpend) #round

#Dropping unnecessary columns
inHouse <- select(inHouse, -FirstName, -LastName, -TelephonesFullPhone, 
                  -lat, -lon, -fips, -stateFips, -county, -city, -LandValue, 
                  -DwellingUnitSize)

#Columns with numbers and price- removing $ sign and converting to numeric
inHouse$HomePurchasePrice <- as.numeric(gsub("\\$", "", inHouse$HomePurchasePrice))
inHouse$EstHomeValue <- as.numeric(gsub("\\$", "", inHouse$EstHomeValue))
#Same column- filling missing values
mean_HomePurchasePrice <- mean(inHouse$HomePurchasePrice, na.rm = TRUE)
mean_HomePurchasePrice_rounded <- round(mean_HomePurchasePrice, digits = 0)
# Replace missing values with the mean value
inHouse$HomePurchasePrice[is.na(inHouse$HomePurchasePric)] <- mean_HomePurchasePrice_rounded


#Rounding "y_householdSpend" and "Age" columns
inHouse$y_householdSpend <- round(inHouse$y_householdSpend, digits = 0)
inHouse$Age <- round(inHouse$Age, digits = 0)


######################## We are done with inHouse file #########################
########################### Time to merge all of them ##########################


# Left join dataframes on 'tmpID'
merged_data <- merge(inHouse, consumerData, by = 'tmpID', all.x = TRUE)
merged_data <- merge(merged_data, donationsData, by = 'tmpID', all.x = TRUE)
final_joined <- merge(merged_data, politicalData, by = 'tmpID', all.x = TRUE)


# View the final joined data frame
dim(final_joined) #10332 rows 20 columns
names(final_joined)



#some final touches- converting to proper classes
class(final_joined$HomeOwnerRenter)
final_joined$HomeOwnerRenter <- as.factor(final_joined$HomeOwnerRenter)
final_joined$AverageNetWorth <- as.factor(final_joined$AverageNetWorth)
final_joined$PresenceOfChildrenCode <- as.factor(final_joined$PresenceOfChildrenCode)
final_joined$Donator <- as.factor(final_joined$Donator)
final_joined$BookBuyerInHome <- as.factor(final_joined$BookBuyerInHome)


#I have one missing value in "Gender"- i will fill it manually
table(final_joined$Gender, useNA="ifany")
final_joined$Gender <- ifelse(final_joined$Gender == "", "M", final_joined$Gender)



######################## We are done with joining ##############################
###################### Lets analyse and visualize ##############################

#In vizualization i will use "y_householdSpend" and "storeVisitFrequency" as main indicators

#######################  Section for y_householdSpend  #######################

#distribution of the "y_householdSpend"
ggplot(data = final_joined, aes(x = y_householdSpend)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 40) +
  ggtitle("Distribution of Household Spending")

#mainly most of the customers spend around 350-450$

#Line chart of average spending by age group
final_joined$age_group <- cut(final_joined$Age, breaks = c(20, 30, 40,50, 60, 70,80, 100))
avg_spending_by_age <- aggregate(y_householdSpend ~ age_group, data = final_joined, FUN = mean)
ggplot(data = avg_spending_by_age, aes(x = age_group, y = y_householdSpend, group = 1)) +
  geom_line() +
  ggtitle("Average Household Spending by Age Group")

#customers who spend the most are at the age of 30


#Graphs with binary values

#Household Spending by Home Ownership Status- does not affect anyhow
ggplot(final_joined, aes(x = HomeOwnerRenter, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by Home Ownership Status") +
  xlab("Home Ownership Status") +
  ylab("Average Household Spending")

#Household Spending by Investor- does not affect anyhow
ggplot(final_joined, aes(x = Investor, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by Investor") +
  xlab("Investor") +
  ylab("Average Household Spending")

#Household Spending by Gender- does not affect anyhow
ggplot(final_joined, aes(x = Gender, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by Gender") +
  xlab("Gender") +
  ylab("Average Household Spending")

#Household Spending by PresenceOfChildrenCode- does not affect anyhow
ggplot(final_joined, aes(x = PresenceOfChildrenCode, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by PresenceOfChildrenCode") +
  xlab("PresenceOfChildrenCode") +
  ylab("Average Household Spending")

#Household Spending by Donator- does not affect anyhow
ggplot(final_joined, aes(x = Donator, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by Donator") +
  xlab("Donator") +
  ylab("Average Household Spending")

#Household Spending by BookBuyerInHome- does not affect anyhow
ggplot(final_joined, aes(x = BookBuyerInHome, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by BookBuyerInHome") +
  xlab("BookBuyerInHome") +
  ylab("Average Household Spending")


################################################################################


# Filter data for states with average household spending greater than 300
high_spend_states <- final_joined %>%
  group_by(state) %>%
  summarize(avg_spend = mean(y_householdSpend)) %>%
  filter(avg_spend > 350)

# Create bar chart
ggplot(high_spend_states, aes(x = state, y = avg_spend)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  ggtitle("Average Household Spending by State") +
  xlab("State") +
  ylab("Average Household Spending") +
  theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5))

#We can see that average hh spending is the highest in Alberta state- but we cannot rely on this- because 
#if we look deeper in data- we have only one customer from Alberta

################################################################################
#So we combine all people and their average spending and have a look which state has the highest 
#hh spenditure

# Filter data for states with average household spending greater than 350
high_spend_states <- final_joined %>%
  group_by(state) %>%
  summarize(avg_spend = mean(y_householdSpend)) %>%
  filter(avg_spend > 350)

# Create bar chart
ggplot(final_joined %>% filter(state %in% high_spend_states$state), aes(x = state, y = y_householdSpend)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Average Household Spending by State") +
  xlab("State") +
  ylab("Average Household Spending") +
  theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5))

#Now we see that most of our customers are from California and Texas, and can 
#that these are the most successful states for us either because most of our 
#customers are from there or they just spend much more then customers from  other states

################################################################################


ggplot(final_joined, aes(x = PropertyType, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by Type of Apartment") +
  xlab("Property Type") +
  ylab("Average Household Spending") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#people living in triplex tend to spend a little bit more

################################################################################

ggplot(final_joined, aes(x = factor(MedianEducationYears), y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue", color = "black") +
  ggtitle("Average Household Spending by Median Education Years") +
  xlab("Median Education Years") +
  ylab("Average Household Spending")

#everything is more or less is even

################################################################################

ggplot(final_joined, aes(x = factor(AverageNetWorth), y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue", color = "black") +
  ggtitle("Average Household Spending by AverageNetWorth") +
  xlab("AverageNetWorth") +
  ylab("Average Household Spending")

#we cant rely on this graph- data is not evenly distributed

################################################################################


ggplot(final_joined, aes(x = OccupationIndustry, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by OccupationIndustry") +
  xlab("OccupationIndustry") +
  ylab("Average Household Spending") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#people working in scientific industry tend to spend a more

################################################################################

ggplot(final_joined, aes(x = PartiesDescription, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by PartiesDescription") +
  xlab("PartiesDescription") +
  ylab("Average Household Spending") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#everything is more or less is even

################################################################################

ggplot(final_joined, aes(x = ReligionsDescription, y = y_householdSpend)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue",color = "black") +
  ggtitle("Average Household Spending by ReligionsDescription") +
  xlab("ReligionsDescription") +
  ylab("Average Household Spending") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#we cant rely on this graph- not even distribution of data- we have only 2 people 
#of Sikh religion

################################################################################
####### These basic graphs give good understanding of the dataset ##############
################# but do not provide any insights ##############################
################################################################################

#For the insights i will look on top customers- the ones who have household 
#expenditure higher than 400 and store Visit frequency equals 8.

# Filtering data by "storeVisitFrequency" and "y_householdSpend"
top_storeVisitFrequency <- final_joined %>%
  filter(storeVisitFrequency == 8)

top_y_householdSpend <- final_joined %>%
  filter(y_householdSpend > 400)

#Finding customers who satisfy both conditions
top_customers <- intersect(top_storeVisitFrequency, top_y_householdSpend)

#Now it will be our main dataframe with top customers
names(top_customers)

#Lets have a look on a portrait of a top customer

#Distribution of the "y_householdSpend"
#Most of the top customers spend around 450$
ggplot(data = top_customers, aes(x = y_householdSpend)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 40) +
  ggtitle("Distribution of Household Spending")

#Here we face data limitation- for 61% of the customers we have "unknown" occupation industry
ggplot(data = top_customers, aes(x = OccupationIndustry)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by OccupationIndustry") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$OccupationIndustry, useNA = "ifany"))*100

#80% of top customers live in "Residential" property type
ggplot(data = top_customers, aes(x = PropertyType)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by PropertyType") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$PropertyType, useNA = "ifany"))*100

#42% of our top customers are English/Welsh
ggplot(data = top_customers, aes(x = BroadEthnicGroupings)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by BroadEthnicGroupings") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$BroadEthnicGroupings, useNA = "ifany"))*100

#High majority of our customers are from California and Texas
ggplot(data = top_customers, aes(x = state)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by state") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$state, useNA = "ifany"))*100

#Almost 30% of top customers have 15 years of education
ggplot(data = top_customers, aes(x = MedianEducationYears)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by MedianEducationYears") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$MedianEducationYears, useNA = "ifany"))*100

#More than half of our top customers are Democratic, 40% are Republican
ggplot(data = top_customers, aes(x = PartiesDescription)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by PartiesDescription") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$PartiesDescription, useNA = "ifany"))*100

#Almost 60% of our top customers are Female
ggplot(data = top_customers, aes(x = Gender)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by Gender") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$Gender, useNA = "ifany"))*100

#25% of our top customers are at the age range of 80-100
ggplot(data = top_customers, aes(x = age_group)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by Age group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$age_group, useNA = "ifany"))*100

ggplot(data = top_customers, aes(x = Age)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by BroadEthnicGroupings") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#66% of our top customers do not have children
ggplot(data = top_customers, aes(x = PresenceOfChildrenCode)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by Presence Of Children") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$PresenceOfChildrenCode, useNA = "ifany"))*100

#More than half of our top customers are Protestants
ggplot(data = top_customers, aes(x = ReligionsDescription)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by Religions Description") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$ReligionsDescription, useNA = "ifany"))*100

#82# of our top customers have their own homes and do not rent
ggplot(data = top_customers, aes(x = HomeOwnerRenter)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by Home Owner or Renter") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$HomeOwnerRenter, useNA = "ifany"))*100

#85% of our top customers donate
ggplot(data = top_customers, aes(x = Donator)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  ggtitle("Count of top customers by Donator") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(top_customers$Donator, useNA = "ifany"))*100


#Almost 50% of top customers have average Net Worth in the range of 160.000$-176.000$
prop.table(table(top_customers$AverageNetWorth, useNA = "ifany"))*100




#End

