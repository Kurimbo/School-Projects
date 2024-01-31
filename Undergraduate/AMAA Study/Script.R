## Title: Statistical Analysis AMAA Data
## Author: Elias Ciudad
## Mentor: Dr. Lori Boies
## keep in mind https://bookdown.org/bean_jerry/using_r_for_social_work_research/survey-research.html
#sapply(df, function(x) sum(is.na(x)))
#It also includes Confirmatory Factor Analysis of likert-like data 

# INSTALL ALL OF THESE
library(psych)
library(effectsize)
library(mirt)
library(knitr) 
library(ltm)
library(forcats)
library(PerformanceAnalytics)
library(pwr)
library(gt)
library(ggstatsplot)
library(tidyverse)
library(ggplot2)
library(crosstable)
library(readxl)
library(styler)
library(glmnet)
library(caret)
library(epiDisplay)
library(epitools)
library(FSA)
library(knitr)
library(kableExtra)
library(prismatic)
library(data.table)
library(prismatic)
library(patchwork)
library(magrittr)
library(multcompView)
library(ggpubr)
library(rstatix)
library(DescTools)
library(ggsignif)
library(ggpubr)
library(gplots)
library(FSA)
library(psych)
library(rstatix)
library(likert)
##### NOTES#####
# ANOVA goes first
# anova feeds into regression, What is significant???
# regression at the end as a factor

###### General Data#####
# Data Loading and Cleaning
AMAA <- read_excel("D:\\Academical Things\\Programming\\R\\Workspaces\\AAMA Regression Study\\Clean_Data.xlsx")
AMAA <- readr::type_convert(AMAA)
# new table with binary, factor-like counts
#DATA SUBSET
AMAA<-AMAA %>% filter(Current_Partner == "Yes")
AMAA<-AMAA %>% filter(!is.na(Years_Post_Residency))
AMAA %>% count(Years_Post_Residency)
dim(AMAA)
1686-264 #1422
UseMe <- AMAA
dim(UseMe)
##### Prepping Factor data#####
#Pregnant
{
UseMe$Pregnant<- AMAA$Pregnant
UseMe$Pregnant[is.na(UseMe$Pregnant)] <- 1
UseMe$Pregnant[UseMe$Pregnant == "No"]<-1
UseMe$Pregnant[UseMe$Pregnant == "Yes"]<-2
UseMe %>% count(Pregnant)
#UseMe$Pregnant <- factor(UseMe$Pregnant)
UseMe$Pregnant <- as.numeric(UseMe$Pregnant)
t.test(UseMe$Life_Satisfaction ~ UseMe$Pregnant)

#DATA SUBSET
UseMe<-UseMe %>% filter(Current_Partner == "Yes")
UseMe %>% count(Years_Post_Residency)
dim(UseMe)
1686-264 #1422

#Educational Status
UseMe$Edu_or_Career <- ""
UseMe$Edu_or_Career[UseMe$Education_Current_I %in% c("1st year medical school (MS1)", "2nd year medical school (MS2)", "3rd year medical school (MS3)", "4th year medical school (MS4)")] <- 1 #"Med School"
UseMe$Edu_or_Career[UseMe$Education_Current_I %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater")] <- 2#"Residency"
UseMe$Edu_or_Career[UseMe$Years_Post_Residency == "1-5 years"] <-3# "1-5 yrs post res"
UseMe$Edu_or_Career[UseMe$Years_Post_Residency == "6-10 years"] <- 4#"6-10 yrs post res"
UseMe$Edu_or_Career[UseMe$Years_Post_Residency == "11-15 years"]  <- 5#"11-15 yrs post res"
UseMe$Edu_or_Career[UseMe$Years_Post_Residency == "16-20 years"]  <- 6#"16-20 yrs post res"
UseMe$Edu_or_Career[UseMe$Years_Post_Residency == "21-25 years"]  <- 7#"21-25 yrs post res"
UseMe$Edu_or_Career[UseMe$Years_Post_Residency == "26-30 years"]  <- 8#"26-30 yrs post res"
UseMe$Edu_or_Career[UseMe$Years_Post_Residency == "31+ years"]  <- 9#"31+ yrs post res"
UseMe$Edu_or_Career[UseMe$Edu_or_Career == ""] <- NA
#UseMe$Edu_or_Career <- factor(UseMe$Edu_or_Career, levels = c("Med School","Residency","1-5 yrs post res","6-10 yrs post res","11-15 yrs post res","16-20 yrs post res","21-25 yrs post res","26-30 yrs post res","31+ yrs post res"))
UseMe$Edu_or_Career<- as.numeric(UseMe$Edu_or_Career)
UseMe %>% count(Edu_or_Career)


# Residency length
UseMe$ResLength <- ""
UseMe$ResLength[UseMe$Education_Current_I == "Intern (PGY1)"] <- 1#"PGY1"
UseMe$ResLength[UseMe$Education_Current_I == "2nd year residency (PGY2)"] <-2# "PGY2"
UseMe$ResLength[UseMe$Education_Current_I == "3rd year residency (PGY3)"] <-3# "PGY3"
UseMe$ResLength[UseMe$Education_Current_I == "4th year residency (PGY4)"] <-4# "PGY4"
UseMe$ResLength[UseMe$Education_Current_I == "5th year residency (PGY5)"] <-5# "PGY5"
UseMe$ResLength[UseMe$Education_Current_I == "6th year residency (PGY6)"] <-6# "PGY6"
UseMe$ResLength[UseMe$Education_Current_I == "7th year residency (PGY7) or greater"] <-7# "PGY7 +"
UseMe$ResLength[UseMe$ResLength == ""] <- NA
#UseMe$ResLength <- factor(UseMe$ResLength)
UseMe %>% count(ResLength)
# ranges for age
UseMe$AgeRange <- ""
UseMe$AgeRange[UseMe$Age < 30] <-1# "Under 30 yrs"
UseMe$AgeRange[UseMe$Age %in% c(30:35)] <-2# "30-35 yrs"
UseMe$AgeRange[UseMe$Age %in% c(36:39)] <-3#"36-39 yrs"
UseMe$AgeRange[UseMe$Age %in% c(40:49)] <-4#"40-49 yrs"
UseMe$AgeRange[UseMe$Age %in% c(50:59)] <-5#"50-59 yrs"
UseMe$AgeRange[UseMe$Age %in% c(60:80)] <-6#"60-80 yrs"
UseMe$AgeRange[UseMe$Age %in% c(81:90)] <- NA
UseMe$AgeRange[UseMe$AgeRange == ""] <- NA
#UseMe$AgeRange <- factor(UseMe$AgeRange, levels = c("Under 30 yrs","30-35 yrs","36-39 yrs","40-49 yrs","50-59 yrs","60-80 yrs","81-90 yrs"))
UseMe$AgeRange<- as.numeric(UseMe$AgeRange)
UseMe %>% count(AgeRange)


# Year 1:3 residency, 4:7 residency
UseMe$ResidencyCat <- ""
UseMe$ResidencyCat[UseMe$Education_Current_I %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)")] <- 1 #"PGY1-PGY3"
UseMe$ResidencyCat[UseMe$Education_Current_I %in% c("4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater")] <- 2#"PGY4-PGY7+"
UseMe$ResidencyCat[UseMe$ResidencyCat == ""] <- NA
#UseMe$ResidencyCat <- factor(UseMe$ResidencyCat)
UseMe$ResidencyCat <- as.numeric(UseMe$ResidencyCat)
UseMe %>% count(ResidencyCat)

# burnout
UseMe$Burnout <- ""
UseMe$Burnout[UseMe$Overall_QoL %in% c(0:64)] <- 0
UseMe$Burnout[UseMe$Overall_QoL %in% c(65:100)] <- 1
UseMe$Burnout[is.na(UseMe$Overall_QoL)] <- NA
UseMe$Burnout <- ifelse(UseMe$Burnout == "burnout", 0, ifelse(UseMe$Burnout == "not burnt", 1, NA))
UseMe$Burnout <- factor(UseMe$Burnout)
table(UseMe$Burnout)

# Number of Kids
UseMe$Number_Children<-AMAA$Number_Children
UseMe %>% count(Number_Children)
UseMe$Number_Children[UseMe$Number_Children == "0"] <- 1#"0 kids"
UseMe$Number_Children[UseMe$Number_Children == "1"] <- 2#"1 kid"
UseMe$Number_Children[UseMe$Number_Children == "2"] <- 3#"2 kids"
UseMe$Number_Children[UseMe$Number_Children == "3"] <- 4#"3 kids"
UseMe$Number_Children[UseMe$Number_Children == "4"] <- 5#"4 kids"
UseMe$Number_Children[UseMe$Number_Children == "5+"] <- 6#"5+ kids"
UseMe$Number_Children[is.na(UseMe$Number_Children)] <- NA
#UseMe$Number_Children <- factor(UseMe$Number_Children)
UseMe$Number_Children <- as.numeric(UseMe$Number_Children)
UseMe %>% count(Number_Children)

# child range 0 = 0-3 children 1 =  4-more
UseMe$ChildRange <- ""
UseMe$ChildRange[UseMe$Number_Children %in% c("0 kids", "1 kid", "2 kids", "3 kids")] <-1#"0-3 kids"
UseMe$ChildRange[UseMe$Number_Children %in% c("4 kids", "5+ kids")] <-2#"4-5+ kids"
UseMe$ChildRange[is.na(UseMe$Number_Children)] <- NA
#UseMe$ChildRange <- factor(UseMe$ChildRange)
UseMe %>% count(ChildRange)

#BOTH physician or student
UseMe$Both_Physician_or_Student<-AMAA$Both_Physician_or_Student
UseMe %>% count(Both_Physician_or_Student)
t.test(data = UseMe, Life_Satisfaction ~ Both_Physician_or_Student) # it makes no diff rly. 0.9023
# Gender
UseMe$Gender<- AMAA$Gender
UseMe$Gender[UseMe$Gender == "Male"] <- 1# "Man"
UseMe$Gender[UseMe$Gender == "Female"] <- 2#"Woman"
UseMe$Gender[UseMe$Gender == "Non-binary"] <- NA
UseMe$Gender[is.na(UseMe$Gender)] <- NA
#UseMe$Gender <- factor(UseMe$Gender, levels = c("Man","Woman","Non-Binary"))
UseMe$Gender<- as.numeric(UseMe$Gender)
UseMe %>% count(Gender)
t.test(UseMe$Life_Satisfaction ~ UseMe$Gender) #p = 0.4071
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Gender, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Gender", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Gender"))
dim(UseMe)

Genders <- UseMe %>%
  select(Life_Satisfaction, Gender) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Gender))
gendertable<-Genders %>%
  group_by(Gender) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
gendertable$variable = NULL
gendertable <- gendertable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
gendertable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Residency Length"=4))%>%
  kable_styling()%>%
  add_footnote("t-test:p-val = 0.4071")


# Partner Gender
#UseMe$Partner_Gender<- AMAA$Partner_Gender
UseMe$Partner_Gender[UseMe$Partner_Gender == "Male"]<-1# <-"Man"
UseMe$Partner_Gender[UseMe$Partner_Gender == "Female"] <-2#"Woman"
UseMe$Partner_Gender[UseMe$Partner_Gender == "Non-binary"] <- 3#"Non-binary"
#UseMe$Partner_Gender <- factor(UseMe$Partner_Gender, levels = c("Man","Woman","Non-binary"))
UseMe %>% count(Partner_Gender)
summary(aov(UseMe$Life_Satisfaction ~ UseMe$Partner_Gender))
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Partner_Gender, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Gender", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Gender"))

partnerGenders <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Gender) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Gender))
partnerGenders<-partnerGenders %>%
  group_by(Partner_Gender) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
partnerGenders$variable = NULL
partnerGenders <- partnerGenders %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
partnerGenders %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Residency Length"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 0.882")


# Relationship Status
#UseMe$Relationship_Status<- AMAA$Relationship_Status
UseMe$Relationship_Status[UseMe$Relationship_Status == "Committed/not married"] <-1#"not married/commited"
UseMe$Relationship_Status[UseMe$Relationship_Status == "Divorced"] <- 2#"Divorced"
UseMe$Relationship_Status[UseMe$Relationship_Status == "Engaged"] <- 3#"Engaged"
UseMe$Relationship_Status[UseMe$Relationship_Status == "Married"] <- 4#"Married"
UseMe$Relationship_Status[UseMe$Relationship_Status == "Widow"] <-5# "Widow"
UseMe$Relationship_Status[UseMe$Relationship_Status == "Other (please specify)"] <-6 #"Other"
#UseMe$Relationship_Status <- factor(UseMe$Relationship_Status,levels = c("not married/commited","Engaged","Married","Divorced","Widow","Other"))
UseMe %>% count(Relationship_Status)

# Primary Practice Setting
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == "Hospital/Multi-physician group"] <-1# "Hosp/multi-physician"
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == "Private practice"] <-2# "Private Practice"
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == c("Veteran's hospital", "Veteran's Hospital")] <-3#"Veteran Hosp."
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == c("Veteran's hospital")] <- 4#"Veteran Hosp."
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == "Active Military"] <- 5#"Active Military"
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == "Not in practice, or retired"] <- NA
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == "Not in practice, resident"] <- NA
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == "Not in practice, student"] <- NA
UseMe$Primary_Practice_Setting[UseMe$Primary_Practice_Setting == "Other (please specify)"] <-6# "Other"
#UseMe$Primary_Practice_Setting <- factor(UseMe$Primary_Practice_Setting,levels = c("Hosp/multi-physician","Private Practice","Veteran Hosp.","Active Military","Other"))
UseMe$Primary_Practice_Setting<- as.numeric(UseMe$Primary_Practice_Setting)
UseMe %>% count(Primary_Practice_Setting)

# Age Oldest Child. from age x-y
UseMe$Oldest_Age_Children<- AMAA$Oldest_Age_Children
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children %in% c(0:4, "19mo", "19 months", "2 months", "2 yrs", "2yo", "3yrs", 3.5, 2.5, "32 months", "3i7", "4 yrs", "3 yrs", "6 months", "Four", "21 months")] <-1#"0-4 yrs"
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children %in% c(5:8, 5.5, 6.5, 7.5, 4.5, "4.5 yrs")] <- 2#"5-8 yrs"
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children %in% c(9:13, 9.5)] <- 3#"9-13 yrs"
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children %in% c(14:19, "19 and 19 Twins")] <- 4# "14-19 yrs"
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children %in% c(20:29)] <- 5#"20-29 yrs"
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children %in% c(30:39)] <- 6#"30-39 yrs"
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children %in% c(40:100)] <- 7#"40-100 yrs"
UseMe$Oldest_Age_Children[UseMe$Oldest_Age_Children == "deceased age 18"] <- NA
#UseMe$Oldest_Age_Children<- factor(UseMe$Oldest_Age_Children, levels = c("0-4 yrs","5-8 yrs","9-13 yrs","14-19 yrs","20-29 yrs","30-39 yrs","40-100 yrs"))
UseMe$Oldest_Age_Children<- as.numeric(UseMe$Oldest_Age_Children)
UseMe %>% count(Oldest_Age_Children)

# Med Students with/without children
UseMe %>% count(Number_Children)
UseMe$MedChild <- ""
for (i in 1:length(UseMe$MedChild)) {
  if (UseMe$Education_Current_I[i] %in% c("1st year medical school (MS1)", "2nd year medical school (MS2)", "3rd year medical school (MS3)", "4th year medical school (MS4)") && UseMe$Number_Children[i] == "0 kids") {
    UseMe$MedChild[i] <- 1
  }
}
for (i in 1:length(UseMe$MedChild)) {
  if (UseMe$Education_Current_I[i] %in% c("1st year medical school (MS1)", "2nd year medical school (MS2)", "3rd year medical school (MS3)", "4th year medical school (MS4)") && UseMe$Number_Children[i] == "1 kid") {
    UseMe$MedChild[i] <- 2
  }
}
for (i in 1:length(UseMe$MedChild)) {
  if (UseMe$Education_Current_I[i] %in% c("1st year medical school (MS1)", "2nd year medical school (MS2)", "3rd year medical school (MS3)", "4th year medical school (MS4)") && UseMe$Number_Children[i] == "2 kids") {
    UseMe$MedChild[i] <- 3
  }
}
for (i in 1:length(UseMe$MedChild)) {
  if (UseMe$Education_Current_I[i] %in% c("1st year medical school (MS1)", "2nd year medical school (MS2)", "3rd year medical school (MS3)", "4th year medical school (MS4)") && UseMe$Number_Children[i] == "3 kids") {
    UseMe$MedChild[i] <- 4
  }
}
for (i in 1:length(UseMe$MedChild)) {
  if (UseMe$Education_Current_I[i] %in% c("1st year medical school (MS1)", "2nd year medical school (MS2)", "3rd year medical school (MS3)", "4th year medical school (MS4)") && UseMe$Number_Children[i] == "4 kids") {
    UseMe$MedChild[i] <- 5
  }
}
UseMe$MedChild[UseMe$MedChild == ""] <- NA
#UseMe$MedChild <- factor(UseMe$MedChild)
UseMe$MedChild<-as.numeric(UseMe$MedChild)
UseMe %>% count(MedChild)

# Residents/Fellows without children
UseMe %>% count(Number_Children)
UseMe$ResChild <- ""
for (i in 1:length(UseMe$ResChild)) {
  if (UseMe$Education_Current_I[i] %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater") && UseMe$Number_Children[i] ==6) {
    UseMe$ResChild[i] <- "5+"
  }
} #5+ kid
for (i in 1:length(UseMe$ResChild)) {
  if (UseMe$Education_Current_I[i] %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater") && UseMe$Number_Children[i] == 5) {
    UseMe$ResChild[i] <- 4
  }
} #4 kid
for (i in 1:length(UseMe$ResChild)) {
  if (UseMe$Education_Current_I[i] %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater") && UseMe$Number_Children[i] == 4) {
    UseMe$ResChild[i] <- 3
  }
} #3 kid
for (i in 1:length(UseMe$ResChild)) {
  if (UseMe$Education_Current_I[i] %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater") && UseMe$Number_Children[i] == 3) {
    UseMe$ResChild[i] <- 2
  }
} #2 kid
for (i in 1:length(UseMe$ResChild)) {
  if (UseMe$Education_Current_I[i] %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater") && UseMe$Number_Children[i] == 2) {
    UseMe$ResChild[i] <- 1
  }
} #1 kid
for (i in 1:length(UseMe$ResChild)) {
  if (UseMe$Education_Current_I[i] %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater") && UseMe$Number_Children[i] == 1) {
    UseMe$ResChild[i] <- 0
  }
} #0 kids
UseMe$ResChild <- factor(UseMe$ResChild)
#UseMe$ResChild <- as.numeric(UseMe$ResChild)
UseMe$ResChild[UseMe$ResChild == ""] <- NA
UseMe %>% count(ResChild)

# Therapy participation [broad]
UseMe$Participate_Counseling[UseMe$Participate_Counseling == "I never have."] <- 1#"I never have"
UseMe$Participate_Counseling[UseMe$Participate_Counseling == "Yes, I did in the past."] <-2# "Yes,in the past"
UseMe$Participate_Counseling[UseMe$Participate_Counseling == "Yes, several times a year."] <-3# "several times a year"
UseMe$Participate_Counseling[UseMe$Participate_Counseling == "Yes, 1-2 times each month."] <-4# "1-2 times each month"
UseMe$Participate_Counseling[UseMe$Participate_Counseling == "Yes, weekly or more often"] <-5# "Weekly or more often"
#UseMe$Participate_Counseling <- factor(UseMe$Participate_Counseling,levels = c("I never have","Yes,in the past","several times a year","1-2 times each month","Weekly or more often"))
UseMe$Participate_Counseling <- as.numeric(UseMe$Participate_Counseling)
UseMe %>% count(Participate_Counseling)

# Annual house income
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$0K-$49K"] <-1# "$0K-$49K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$50K-$99K"] <-2# "$50K-$99K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$100K-$199K"] <-3# "$100K-$199K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$200K-$299K"] <-4# "$200K-$299K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$300K-$399K"] <-5# "$300K-$399K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$400K-$499K"] <-6# "$400K-$499K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$500K-$599K"] <-7# "$500K-$599K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$600K-$699K"] <-8# "$600K-$699K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$700K-$799K"] <-9# "$700K-$799K"
UseMe$Household_Annual_Salary[UseMe$Household_Annual_Salary == "$800K+"] <-10# "$800K+"
#UseMe$Household_Annual_Salary <- factor(UseMe$Household_Annual_Salary,levels = c("$0K-$49K","$50K-$99K","$100K-$199K","$200K-$299K","$300K-$399K","$400K-$499K","$500K-$599K","$600K-$699K","$700K-$799K","$800K+"))
UseMe$Household_Annual_Salary <- as.numeric(UseMe$Household_Annual_Salary)
UseMe %>% count(Household_Annual_Salary)
# RICH RICH RICH [wouldn't this be just proportions?]
UseMe$RICH <- ""
UseMe$RICH[UseMe$Household_Annual_Salary == "$800K+"] <- 1
UseMe %>% count(RICH)

# Partner Specialty  [1 = med 2 = surgical]
UseMe$MedOrSur <- ""
UseMe$MedOrSur <- UseMe$Partner_Specialty
UseMe$Partner_Specialty[UseMe$Partner_Specialty == "student"] <- NA
UseMe$MedOrSur[UseMe$Partner_Specialty == "surgical specialty"] <- 2#"surgical"
UseMe$MedOrSur[UseMe$Partner_Specialty != "student"] <-1#"medical"
UseMe$MedOrSur[UseMe$MedOrSur == ""] <- NA
#UseMe$MedOrSur <- factor(UseMe$MedOrSur)
UseMe$MedOrSur <- as.numeric(UseMe$MedOrSur)
UseMe %>% count(MedOrSur)

# Surgical Specialty
UseMe$Partner_Surgical_Specialty<- AMAA$Partner_Surgical_Specialty
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "Colon and Rectal"] <- 1#"Colorectal"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty %in% c("Oral and Maxillofacial", "Ophthalmologic", "ENT")] <- 2#"ENT"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "General"] <- 3#"General"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "Neurologic"] <-4# "Neuro"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "Orthopaedic"] <-5# "Orthopedics"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "Plastic"] <- 6#"Plastics"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "Thoracic and Cardiovascular"] <-7# "Cardiothoracic"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "Vascular"] <- 8#"Vascular"
UseMe$Partner_Surgical_Specialty[UseMe$Partner_Surgical_Specialty == "Other (please specify)"] <-9# "Other"
#UseMe$Partner_Surgical_Specialty <- factor(UseMe$Partner_Surgical_Specialty,levels = c("Colorectal","ENT","General","Neuro","Orthopedics","Plastics","Cardiothoracic","Vascular","Other"))
UseMe$Partner_Surgical_Specialty <- as.numeric(UseMe$Partner_Surgical_Specialty)
UseMe %>% count(Partner_Surgical_Specialty)

# Student Loan Balance
UseMe$Household_Loan_Current<- AMAA$Household_Loan_Current
UseMe$Household_Loan_Current[UseMe$Household_Loan_Current == "0"] <-1# "$0"
UseMe$Household_Loan_Current[UseMe$Household_Loan_Current == "$1-$99K"] <-2# "$1-$99K"
UseMe$Household_Loan_Current[UseMe$Household_Loan_Current == "$100K-$199K"] <-3# "$100K-$199K"
UseMe$Household_Loan_Current[UseMe$Household_Loan_Current == "$200K-$299K"] <-4# "$200K-$299K"
UseMe$Household_Loan_Current[UseMe$Household_Loan_Current == "$300K-$399K"] <-5# "$300K-$399K"
UseMe$Household_Loan_Current[UseMe$Household_Loan_Current == "$400K-$499K"] <-6# "$400K-$499K"
UseMe$Household_Loan_Current[UseMe$Household_Loan_Current == "$500K+"] <-7# "$500K+"
#UseMe$Household_Loan_Current<-factor(UseMe$Household_Loan_Current)
UseMe$Household_Loan_Current<-as.numeric(UseMe$Household_Loan_Current)
UseMe %>% count(Household_Loan_Current)

# AMAA Membership
UseMe$AMAA_Member<- AMAA$AMAA_Member
colnames(UseMe)[which(names(UseMe) == "AMA_Member")] <- "AMAA_Member"
UseMe$AMAA_Member[UseMe$AMAA_Member %in% c("No, I'm good.", "No, but I'd like to know more.")] <-1# "No"
UseMe$AMAA_Member[UseMe$AMAA_Member == "Yes."] <- 2#"Yes"
#UseMe$AMAA_Member <- factor(UseMe$AMAA_Member)
UseMe$AMAA_Member <- as.numeric(UseMe$AMAA_Member)
UseMe %>% count(AMAA_Member)

# Sleep Refresh
UseMe$seven_Days_Sleep_Refresh <- AMAA$seven_Days_Sleep_Refresh
UseMe$seven_Days_Sleep_Refresh[UseMe$seven_Days_Sleep_Refresh == "not at all"] <- 1
UseMe$seven_Days_Sleep_Refresh[UseMe$seven_Days_Sleep_Refresh == "a little bit"] <- 2
UseMe$seven_Days_Sleep_Refresh[UseMe$seven_Days_Sleep_Refresh == "somewhat"] <- 3
UseMe$seven_Days_Sleep_Refresh[UseMe$seven_Days_Sleep_Refresh == "quite a bit"] <- 4
UseMe$seven_Days_Sleep_Refresh[UseMe$seven_Days_Sleep_Refresh == "very much"] <- 5
#UseMe$seven_Days_Sleep_Refresh <- factor(UseMe$seven_Days_Sleep_Refresh,levels = c("not at all","a little bit","somewhat","quite a bit","very much"))
UseMe$seven_Days_Sleep_Refresh <- as.numeric(UseMe$seven_Days_Sleep_Refresh)
UseMe %>% count(seven_Days_Sleep_Refresh)

# Problems in the sleep
UseMe$seven_Day_Sleep_Problem <- AMAA$seven_Day_Sleep_Problem
UseMe$seven_Day_Sleep_Problem[UseMe$seven_Day_Sleep_Problem == "not at all"] <- 1
UseMe$seven_Day_Sleep_Problem[UseMe$seven_Day_Sleep_Problem == "a little bit"] <- 2
UseMe$seven_Day_Sleep_Problem[UseMe$seven_Day_Sleep_Problem == "somewhat"] <- 3
UseMe$seven_Day_Sleep_Problem[UseMe$seven_Day_Sleep_Problem == "quite a bit"] <- 4
UseMe$seven_Day_Sleep_Problem[UseMe$seven_Day_Sleep_Problem == "very much"] <- 5
#UseMe$seven_Day_Sleep_Problem <- factor(UseMe$seven_Day_Sleep_Problem,levels = c("not at all","a little bit","somewhat","quite a bit","very much"))
UseMe$seven_Day_Sleep_Problem <- as.numeric(UseMe$seven_Day_Sleep_Problem)
UseMe %>% count(seven_Day_Sleep_Problem)

# Difficulty fall asleep
UseMe$seven_Day_Problem_Fall_Asleep <- AMAA$seven_Day_Problem_Fall_Asleep
UseMe$seven_Day_Problem_Fall_Asleep[UseMe$seven_Day_Problem_Fall_Asleep == "not at all"] <- 1
UseMe$seven_Day_Problem_Fall_Asleep[UseMe$seven_Day_Problem_Fall_Asleep == "a little bit"] <- 2
UseMe$seven_Day_Problem_Fall_Asleep[UseMe$seven_Day_Problem_Fall_Asleep == "somewhat"] <- 3
UseMe$seven_Day_Problem_Fall_Asleep[UseMe$seven_Day_Problem_Fall_Asleep == "quite a bit"] <- 4
UseMe$seven_Day_Problem_Fall_Asleep[UseMe$seven_Day_Problem_Fall_Asleep == "very much"] <- 5
#UseMe$seven_Day_Problem_Fall_Asleep <- factor(UseMe$seven_Day_Problem_Fall_Asleep,levels = c("not at all","a little bit","somewhat","quite a bit","very much"))
UseMe$seven_Day_Problem_Fall_Asleep <- as.numeric(UseMe$seven_Day_Problem_Fall_Asleep)
UseMe %>% count(seven_Day_Problem_Fall_Asleep)

#EDUCATION DEGREE -TBD-
UseMe$Highest_Education_Completed<- AMAA$Highest_Education_Completed
UseMe$Highest_Education_Completed[UseMe$Highest_Education_Completed == "High school degree or equivalent (e.g. GED)"] <-1# "HS"
UseMe$Highest_Education_Completed[UseMe$Highest_Education_Completed == "Some college, no degree"] <-2# "Some college,no degree"
UseMe$Highest_Education_Completed[UseMe$Highest_Education_Completed == "Associate degree (e.g. AA, AS)"] <-3# "Associate's"
UseMe$Highest_Education_Completed[UseMe$Highest_Education_Completed == "Bachelor's degree (e.g. BA, BS)"] <-4# "Bachelor's"
UseMe$Highest_Education_Completed[UseMe$Highest_Education_Completed == "Master's degree (e.g. MA, MS, Med)"] <-5# "Master's"
UseMe$Highest_Education_Completed[UseMe$Highest_Education_Completed == "Professional degree (e.g. JD, MD, DO, MBBS)"] <-6# "Professional deg."
UseMe$Highest_Education_Completed[UseMe$Highest_Education_Completed == "Doctorate degree (e.g. PhD, EdD)"] <-7# "Doctorate"
#UseMe$Highest_Education_Completed<- factor(UseMe$Highest_Education_Completed, levels = c("HS","Some college,no degree","Associate's","Bachelor's","Master's","Professional deg.","Doctorate"))
UseMe$Highest_Education_Completed<- as.numeric(UseMe$Highest_Education_Completed)
UseMe %>% count(Highest_Education_Completed)

#TIME ALONE PARTNER
UseMe$Time_Alone_Partner<- AMAA$Time_Alone_Partner
UseMe$Time_Alone_Partner[UseMe$Time_Alone_Partner == "<20 minutes"] <-1
UseMe$Time_Alone_Partner[UseMe$Time_Alone_Partner == "21-45 minutes"] <-2
UseMe$Time_Alone_Partner[UseMe$Time_Alone_Partner == "46-60 minutes"] <-3
UseMe$Time_Alone_Partner[UseMe$Time_Alone_Partner == "61-90 minutes"] <-4
UseMe$Time_Alone_Partner[UseMe$Time_Alone_Partner == "91-120 minutes"] <-5
UseMe$Time_Alone_Partner[UseMe$Time_Alone_Partner == ">120 minutes"] <-6
UseMe$Time_Alone_Partner<- as.numeric(UseMe$Time_Alone_Partner)
UseMe %>% count(Time_Alone_Partner)

#QUALITY TIME PARTNER
#UseMe$Quality_Time_Partner <- AMAA$Quality_Time_Partner
UseMe$Quality_Time_Partner[UseMe$Quality_Time_Partner == "<20 minutes"] <-1
UseMe$Quality_Time_Partner[UseMe$Quality_Time_Partner == "21-45 minutes"] <-2
UseMe$Quality_Time_Partner[UseMe$Quality_Time_Partner == "46-60 minutes"] <-3
UseMe$Quality_Time_Partner[UseMe$Quality_Time_Partner == "61-90 minutes"] <-4
UseMe$Quality_Time_Partner[UseMe$Quality_Time_Partner == "91-120 minutes"] <-5
UseMe$Quality_Time_Partner[UseMe$Quality_Time_Partner == ">120 minutes"] <-6
UseMe$Quality_Time_Partner<- as.numeric(UseMe$Quality_Time_Partner)
#UseMe$Quality_Time_Partner<- factor(UseMe$Quality_Time_Partner,levels = c("<20 minutes","21-45 minutes","46-60 minutes","61-90 minutes","91-120 minutes",">120 minutes"))
UseMe %>% count(Quality_Time_Partner)

#Years with Partner
UseMe$Years_With_Partner<- AMAA$Years_With_Partner
UseMe$Years_With_Partner[UseMe$Years_With_Partner == "1-5 years"] <- 1
UseMe$Years_With_Partner[UseMe$Years_With_Partner == "6-10 years"]<- 2
UseMe$Years_With_Partner[UseMe$Years_With_Partner == "11-20 years"]<- 3
UseMe$Years_With_Partner[UseMe$Years_With_Partner == ">20 years"]<-4
UseMe$Years_With_Partner <- as.numeric(UseMe$Years_With_Partner)
UseMe$Years_With_Partner <- factor(UseMe$Years_With_Partner, levels = c("1-5 years","6-10 years","11-20 years",">20 years"))
UseMe %>% count(Years_With_Partner)

#Pride Medical Family
UseMe$Pride_Medical_Family<- AMAA$Pride_Medical_Family
UseMe$Pride_Medical_Family[UseMe$Pride_Medical_Family == "Never"] <-1
UseMe$Pride_Medical_Family[UseMe$Pride_Medical_Family == "Rarely"] <-2
UseMe$Pride_Medical_Family[UseMe$Pride_Medical_Family == "Sometimes"]<-3
UseMe$Pride_Medical_Family[UseMe$Pride_Medical_Family == "Frequently"]<-4
UseMe$Pride_Medical_Family[UseMe$Pride_Medical_Family == "Always"]<-5
#UseMe$Pride_Medical_Family<- factor(UseMe$Pride_Medical_Family, levels = c("Never","Rarely","Sometimes","Frequently","Always"))
UseMe$Pride_Medical_Family<- as.numeric(UseMe$Pride_Medical_Family)
UseMe %>% count(Pride_Medical_Family)

#GEOGRAPHIC LOCATION
UseMe$Region_Country <- as.numeric(UseMe$Region_Country)

#MODERATE EXERCISE
UseMe$Time_Moderate_Exercise<- AMAA$Time_Moderate_Exercise
UseMe %>% count(Time_Moderate_Exercise)
UseMe$Time_Moderate_Exercise[UseMe$Time_Moderate_Exercise == ">150 minutes per week (e.g. 2 1/2 hrs)"] <- ">150 minutes per week"
UseMe$Time_Moderate_Exercise<- factor(UseMe$Time_Moderate_Exercise, levels = c("<30 minutes per week","30-60 minutes per week","61-120 minutes per week","121-150 minutes per week",">150 minutes per week"))
UseMe$Time_Moderate_Exercise<- as.numeric(UseMe$Time_Moderate_Exercise)
UseMe %>% count(Time_Moderate_Exercise)

#STRENGTH TRAINING
UseMe$Time_Strength_Training<-factor(UseMe$Time_Strength_Training, levels = c("none","once per week","twice per week","three times per week","four times per week or more"))
UseMe$Time_Strength_Training<- as.numeric(UseMe$Time_Strength_Training)
UseMe %>% count(Time_Strength_Training)

#VIGOROUS EXERCISE
UseMe %>% count(Time_Vigorous_Exercise)
UseMe$Time_Vigorous_Exercise[UseMe$Time_Vigorous_Exercise == ">150 minutes per week (e.g. 2 1/2 hrs)"] <- ">150 minutes per week"
UseMe$Time_Vigorous_Exercise<- factor(UseMe$Time_Vigorous_Exercise, levels = c("<30 minutes per week","30-60 minutes per week","61-120 minutes per week","121-150 minutes per week",">150 minutes per week"))
UseMe$Time_Vigorous_Exercise<- as.numeric(UseMe$Time_Vigorous_Exercise)
UseMe %>% count(Time_Vigorous_Exercise)

#----------------------------------------------------#
UseMe$Stress_Factor_Lack_Time<- AMAA$Stress_Factor_Lack_Time #Spouses lack of time for activities due to work
UseMe %>% count(Stress_Factor_Lack_Time)
UseMe$Stress_Factor_Lack_Time<- factor(UseMe$Stress_Factor_Lack_Time,levels = c("Not stressful at all","Somewhat not stressful","Neither stressful or not stressful","Somewhat stressful","Very stressful"))
UseMe$Stress_Factor_Lack_Time<- as.numeric(UseMe$Stress_Factor_Lack_Time)

#---------------------------------------#
UseMe %>% count(Stress_Factor_Business) # Business Aspect of spouse's partner medical practice
UseMe$Stress_Factor_Business<- factor(UseMe$Stress_Factor_Business,levels = c("Not stressful at all","Somewhat not stressful","Neither stressful or not stressful","Somewhat stressful","Very stressful"))
UseMe$Stress_Factor_Business<- as.numeric(UseMe$Stress_Factor_Business)

#----------------------------------------#
UseMe %>% count(Stress_Factor_Work_Overload)#partner's fatigue from work overload
UseMe$Stress_Factor_Work_Overload<- factor(UseMe$Stress_Factor_Work_Overload,levels = c("Not stressful at all","Somewhat not stressful","Neither stressful or not stressful","Somewhat stressful","Very stressful"))
UseMe$Stress_Factor_Work_Overload<- as.numeric(UseMe$Stress_Factor_Work_Overload)

#----------------------------------------#
UseMe %>% count(Stress_Factor_Work_Issues_Home)#Partner worries work when home
UseMe$Stress_Factor_Work_Issues_Home<- factor(UseMe$Stress_Factor_Work_Issues_Home,levels = c("Not stressful at all","Somewhat not stressful","Neither stressful or not stressful","Somewhat stressful","Very stressful"))
UseMe$Stress_Factor_Work_Issues_Home<- as.numeric(UseMe$Stress_Factor_Work_Issues_Home)

#----------------------------------------#
UseMe %>% count(Primary_Cause_Stress)
forcats::fct_relevel(UseMe$Primary_Cause_Stress,"Other (please specify)", after = Inf)
UseMe$Primary_Cause_Stress<-as.numeric(UseMe$Primary_Cause_Stress)
#----------------------------------------#
UseMe %>% count(Fullfilment_Individual)
UseMe$Fullfilment_Individual<- factor(UseMe$Fullfilment_Individual,levels = c("Not fulfilled at all","Somewhat unfulfilled","Neither unfulfilled or fulfilled","Somewhat fulfilled","Very fulfilled"))
UseMe$Fullfilment_Individual<-as.numeric(UseMe$Fullfilment_Individual)
#----------------------------------------#
UseMe %>% count(Fulfillment_as_Parent)
UseMe$Fulfillment_as_Parent<- factor(UseMe$Fulfillment_as_Parent,levels = c("Not fulfilled at all","Somewhat unfulfilled","Neither unfulfilled or fulfilled","Somewhat fulfilled","Very fulfilled"))
UseMe$Fulfillment_as_Parent<-as.numeric(UseMe$Fulfillment_as_Parent)
#----------------------------------------#
UseMe %>% count(Fulfillment_as_Partner)
UseMe$Fulfillment_as_Partner<- factor(UseMe$Fulfillment_as_Partner,levels = c("Not fulfilled at all","Somewhat unfulfilled","Neither unfulfilled or fulfilled","Somewhat fulfilled","Very fulfilled"))
UseMe$Fulfillment_as_Partner<-as.numeric(UseMe$Fulfillment_as_Partner)
#----------------------------------------#


########################################COMMUNITY###############################
#---------------ISOLATED LOCAL COMMUNITY-------------#
#Isolation local community  
UseMe$Community_Isolated<- AMAA$Community_Isolated
UseMe$Community_Isolated[UseMe$Community_Isolated == "Never"] <-1
UseMe$Community_Isolated[UseMe$Community_Isolated == "Rarely"] <-2
UseMe$Community_Isolated[UseMe$Community_Isolated == "Sometimes"]<-3
UseMe$Community_Isolated[UseMe$Community_Isolated == "Usually"]<-4
UseMe$Community_Isolated[UseMe$Community_Isolated == "Always"]<-5
UseMe$Community_Isolated<- as.numeric(UseMe$Community_Isolated)
#UseMe$Community_Isolated<- factor(UseMe$Community_Isolated, levels = c("Never","Rarely","Sometimes","Usually","Always"))
UseMe %>% count(Community_Isolated)

#LEFT OUT LOCAL COMMUNITY-----#
UseMe$Community_Left_Out<- AMAA$Community_Left_Out
UseMe$Community_Left_Out[UseMe$Community_Left_Out == "Never"] <-1
UseMe$Community_Left_Out[UseMe$Community_Left_Out == "Rarely"] <-2
UseMe$Community_Left_Out[UseMe$Community_Left_Out == "Sometimes"]<-3
UseMe$Community_Left_Out[UseMe$Community_Left_Out == "Usually"]<-4
UseMe$Community_Left_Out[UseMe$Community_Left_Out == "Always"]<-5
UseMe$Community_Left_Out<- as.numeric(UseMe$Community_Left_Out)
#UseMe$Community_Left_Out<- factor(UseMe$Community_Left_Out, levels = c("Never","Rarely","Sometimes","Usually","Always"))
UseMe %>% count(Community_Left_Out)

#Nobody knows me, local community 
UseMe$Community_Unknwon<- AMAA$Community_Unknwon
UseMe$Community_Unknwon[UseMe$Community_Unknwon == "Never"] <-1
UseMe$Community_Unknwon[UseMe$Community_Unknwon == "Rarely"] <-2
UseMe$Community_Unknwon[UseMe$Community_Unknwon == "Sometimes"]<-3
UseMe$Community_Unknwon[UseMe$Community_Unknwon == "Usually"]<-4
UseMe$Community_Unknwon[UseMe$Community_Unknwon == "Always"]<-5
UseMe$Community_Unknwon<- as.numeric(UseMe$Community_Unknwon)
#UseMe$Community_Unknwon<- factor(UseMe$Community_Unknwon, levels = c("Never","Rarely","Sometimes","Usually","Always"))
UseMe %>% count(Community_Unknwon)

#Detached, Local Community
UseMe$Community_Detached<- AMAA$Community_Detached
UseMe$Community_Detached[UseMe$Community_Detached == "Never"] <-1
UseMe$Community_Detached[UseMe$Community_Detached == "Rarely"] <-2
UseMe$Community_Detached[UseMe$Community_Detached == "Sometimes"]<-3
UseMe$Community_Detached[UseMe$Community_Detached == "Usually"]<-4
UseMe$Community_Detached[UseMe$Community_Detached == "Always"]<-5
UseMe$Community_Detached<- as.numeric(UseMe$Community_Detached)
#UseMe$Community_Detached<- factor(UseMe$Community_Detached, levels = c("Never","Rarely","Sometimes","Usually","Always"))

#####################################PARTNER AFTER WORK#######################-#
#PARTNER TIRED AFTER
UseMe$Partner_Tired_After<- AMAA$Partner_Tired_After
UseMe%>% count(Partner_Tired_After)
UseMe$Partner_Tired_After<- factor(UseMe$Partner_Tired_After,levels = c("Never","A few times a year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day"))
#UseMe$Partner_Tired_After<- as.numeric(UseMe$Partner_Tired_After)
UseMe%>% count(Partner_Tired_After)
summary(aov(data = UseMe, Life_Satisfaction~Partner_Tired_After))

tired <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Tired_After) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Tired_After))
tiredtable<-tired %>%
  group_by(Partner_Tired_After) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
tiredtable$variable = NULL
tiredtable <- tiredtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
tiredtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding tired after work"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 8.24e-41")
#----------------------------------------------------#
tiredaov <- tired %>% anova_test(Life_Satisfaction ~ Partner_Tired_After)
tiredaov
summary(aov(tired$Life_Satisfaction ~ tired$Partner_Tired_After))
plotmeans(tired$Life_Satisfaction ~ tired$Partner_Tired_After, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Being Tired After Work", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Being Tired After work"))
#---------------------------------------------------------------------------#
tiredTuk <- tired %>% tukey_hsd(Life_Satisfaction ~ Partner_Tired_After)
tiredTuk <- tiredTuk %>% add_xy_position(x = "Partner_Tired_After")
tiredTuk

ggboxplot(tired, x = "Partner_Tired_After", y = "Life_Satisfaction") +
  stat_pvalue_manual(tiredTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Tired after work  vs Life Satisfaction",
    subtitle = get_test_label(tiredaov, detailed = TRUE),
    caption = get_pwc_label(tiredTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Partner Tired After Work") +
  ylab(label = "Life Satisfaction")



#PARTER IRRITABLE AFTER
UseMe$Partner_Irritable_After<- AMAA$Partner_Irritable_After
UseMe%>% count(Partner_Irritable_After)
UseMe$Partner_Irritable_After<- factor(UseMe$Partner_Irritable_After,levels = c("Never","A few times a year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day"))
#UseMe$Partner_Irritable_After<- as.numeric(UseMe$Partner_Irritable_After)
UseMe%>% count(Partner_Irritable_After)
summary(aov(data = UseMe, Life_Satisfaction~Partner_Irritable_After))

irritated <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Irritable_After) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Irritable_After))
irritatedtable<-irritated %>%
  group_by(Partner_Irritable_After) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
irritatedtable$variable = NULL
irritatedtable <- irritatedtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
irritatedtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Partner Being Irritated After Work"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 2.92e-37")
#----------------------------------------------------#
irritatedaov <- irritated %>% anova_test(Life_Satisfaction ~ Partner_Irritable_After)
irritatedaov
plotmeans(irritated$Life_Satisfaction ~ irritated$Partner_Irritable_After, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Being irritated After Work", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Being irritated After work"))
#---------------------------------------------------------------------------#
irritatedTuk <- irritated %>% tukey_hsd(Life_Satisfaction ~ Partner_Irritable_After)
irritatedTuk <- irritatedTuk %>% add_xy_position(x = "Partner_Irritable_After")
irritatedTuk

ggboxplot(irritated, x = "Partner_Irritable_After", y = "Life_Satisfaction") +
  stat_pvalue_manual(irritatedTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = " Partner Irritated After Work vs Life Satisfaction",
    subtitle = get_test_label(irritatedaov, detailed = TRUE),
    caption = get_pwc_label(irritatedTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Partner Irritated After Work") +
  ylab(label = "Life Satisfaction")




#PARTNER ARGUING ABOUT WORK TIME
UseMe$Partner_Argue_Work_Time<- AMAA$Partner_Argue_Work_Time
UseMe$Partner_Argue_Work_Time
UseMe%>% count(Partner_Argue_Work_Time)
UseMe$Partner_Argue_Work_Time<- factor(UseMe$Partner_Argue_Work_Time,levels = c("Never","Rarely","Sometimes","Frequently","Always"))
#UseMe$Partner_Argue_Work_Time<- as.numeric(UseMe$Partner_Argue_Work_Time)
UseMe%>% count(Partner_Argue_Work_Time)

summary(aov(data = UseMe, Life_Satisfaction~Partner_Argue_Work_Time))

argueAFwork <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Argue_Work_Time) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Argue_Work_Time))
argueAFworktable<-argueAFwork %>%
  group_by(Partner_Argue_Work_Time) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
argueAFworktable$variable = NULL
argueAFworktable <- argueAFworktable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
argueAFworktable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Arguing w/Partner About Work Time"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 1.78e-33")
#----------------------------------------------------#
argueAFworkaov <- argueAFwork %>% anova_test(Life_Satisfaction ~ Partner_Argue_Work_Time)
argueAFworkaov
plotmeans(argueAFwork$Life_Satisfaction ~ argueAFwork$Partner_Argue_Work_Time, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Arguing w/Partner about Work Time", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Arguing w/Partner about Work Time"))
#---------------------------------------------------------------------------#
argueAFworkTuk <- argueAFwork %>% tukey_hsd(Life_Satisfaction ~ Partner_Argue_Work_Time)
argueAFworkTuk <- argueAFworkTuk %>% add_xy_position(x = "Partner_Argue_Work_Time")
argueAFworkTuk

ggboxplot(argueAFwork, x = "Partner_Argue_Work_Time", y = "Life_Satisfaction") +
  stat_pvalue_manual(argueAFworkTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = " Arguing w/Partner About Work Time vs Life Satisfaction",
    subtitle = get_test_label(argueAFworkaov, detailed = TRUE),
    caption = get_pwc_label(argueAFworkTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Arguing w/Partner About Work Time") +
  ylab(label = "Life Satisfaction")


#PARTNER BUSY AFTER WORK, TAKING WORK HOME
UseMe$Partner_Busy_Work_After<-AMAA$Partner_Busy_Work_After
UseMe%>% count(Partner_Busy_Work_After)
UseMe$Partner_Busy_Work_After<- factor(UseMe$Partner_Busy_Work_After,levels = c("Never","A few times a year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day"))
#UseMe$Partner_Busy_Work_After<- as.numeric(UseMe$Partner_Busy_Work_After)
UseMe%>% count(Partner_Busy_Work_After)

summary(aov(data = UseMe, Life_Satisfaction~Partner_Busy_Work_After))

wrktohome <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Busy_Work_After) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Busy_Work_After))
wrktohometable<-wrktohome %>%
  group_by(Partner_Busy_Work_After) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
wrktohometable
wrktohometable$variable = NULL
wrktohometable <- wrktohometable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
wrktohometable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Partner Taking Work Home"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 3.79e-22")
#----------------------------------------------------#
wrktohomeaov <- wrktohome %>% anova_test(Life_Satisfaction ~ Partner_Busy_Work_After)
wrktohomeaov
plotmeans(wrktohome$Life_Satisfaction ~ wrktohome$Partner_Busy_Work_After, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Taking Work Home", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Taking Work Home"))
#---------------------------------------------------------------------------#
wrktohomeTuk <- wrktohome %>% tukey_hsd(Life_Satisfaction ~ Partner_Busy_Work_After)
wrktohomeTuk <- wrktohomeTuk %>% add_xy_position(x = "Partner_Busy_Work_After")
wrktohomeTuk

ggboxplot(wrktohome, x = "Partner_Busy_Work_After", y = "Life_Satisfaction") +
  stat_pvalue_manual(wrktohomeTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = " Partner Taking Work Home vs Life Satisfaction",
    subtitle = get_test_label(wrktohomeaov, detailed = TRUE),
    caption = get_pwc_label(wrktohomeTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Partner Taking Work Home") +
  ylab(label = "Life Satisfaction")


#PARTNER SHOWS ME APPRECIATION
UseMe$Partner_Show_Appreciation<- AMAA$Partner_Show_Appreciation
UseMe%>% count(Partner_Show_Appreciation)
UseMe$Partner_Show_Appreciation<- factor(UseMe$Partner_Show_Appreciation,levels = c("Never","Rarely","Sometimes","Frequently","Always"))
#UseMe$Partner_Show_Appreciation<- as.numeric(UseMe$Partner_Show_Appreciation)
UseMe%>% count(Partner_Show_Appreciation)
summary(aov(data = UseMe, Life_Satisfaction~Partner_Show_Appreciation))

prtnrShAprctn <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Show_Appreciation) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Show_Appreciation))
prtnrShAprctntable<-prtnrShAprctn %>%
  group_by(Partner_Show_Appreciation) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
prtnrShAprctntable$variable = NULL
prtnrShAprctntable <- prtnrShAprctntable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
prtnrShAprctntable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Partner Showing Appreciation"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 2.63e-28")
#----------------------------------------------------#
prtnrShAprctnaov <- prtnrShAprctn %>% anova_test(Life_Satisfaction ~ Partner_Show_Appreciation)
prtnrShAprctnaov
plotmeans(prtnrShAprctn$Life_Satisfaction ~ prtnrShAprctn$Partner_Show_Appreciation, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Shows Me Appreciation", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Shows Me Appreciation"))
#---------------------------------------------------------------------------#
prtnrShAprctnTuk <- prtnrShAprctn %>% tukey_hsd(Life_Satisfaction ~ Partner_Show_Appreciation)
prtnrShAprctnTuk <- prtnrShAprctnTuk %>% add_xy_position(x = "Partner_Show_Appreciation")
prtnrShAprctnTuk

ggboxplot(prtnrShAprctn, x = "Partner_Show_Appreciation", y = "Life_Satisfaction") +
  stat_pvalue_manual(prtnrShAprctnTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = " Partner Shows Me Appreciation vs Life Satisfaction",
    subtitle = get_test_label(prtnrShAprctnaov, detailed = TRUE),
    caption = get_pwc_label(prtnrShAprctnTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Partner Shows Me Appreciation") +
  ylab(label = "Life Satisfaction")

#PARTNER PULLS WEIGHT AROUND HOUSE
UseMe$Partner_Participant_Household_Activities<- AMAA$Partner_Participant_Household_Activities
UseMe%>% count(Partner_Participant_Household_Activities)
UseMe$Partner_Participant_Household_Activities<- factor(UseMe$Partner_Participant_Household_Activities,levels = c("Never","Rarely","Sometimes","Frequently","Always"))
#UseMe$Partner_Participant_Household_Activities<- as.numeric(UseMe$Partner_Participant_Household_Activities)
UseMe%>% count(Partner_Participant_Household_Activities)
summary(aov(data = UseMe, Life_Satisfaction~Partner_Participant_Household_Activities))

Phouseactiv <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Participant_Household_Activities) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Participant_Household_Activities))
Phouseactivtable<-Phouseactiv %>%
  group_by(Partner_Participant_Household_Activities) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
Phouseactivtable$variable = NULL
Phouseactivtable <- Phouseactivtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
Phouseactivtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Partner Participating in Household Activities"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 3.08e-20")
#----------------------------------------------------#
Phouseactivaov <- Phouseactiv %>% anova_test(Life_Satisfaction ~ Partner_Participant_Household_Activities)
Phouseactivaov
plotmeans(Phouseactiv$Life_Satisfaction ~ Phouseactiv$Partner_Participant_Household_Activities, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Participating in Household Activities", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Participating in Household Activities"))
#---------------------------------------------------------------------------#
PhouseactivTuk <- Phouseactiv %>% tukey_hsd(Life_Satisfaction ~ Partner_Participant_Household_Activities)
PhouseactivTuk <- PhouseactivTuk %>% add_xy_position(x = "Partner_Participant_Household_Activities")
PhouseactivTuk

ggboxplot(Phouseactiv, x = "Partner_Participant_Household_Activities", y = "Life_Satisfaction") +
  stat_pvalue_manual(PhouseactivTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = " Partner Participating in Household Activities vs Life Satisfaction",
    subtitle = get_test_label(Phouseactivaov, detailed = TRUE),
    caption = get_pwc_label(PhouseactivTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Partner Participating in Household Activities") +
  ylab(label = "Life Satisfaction")

#PARTNER SPENDING TIME WITH KIDS
UseMe$Partner_Time_Children<-AMAA$Partner_Time_Children
UseMe%>% count(Partner_Time_Children)
UseMe$Partner_Time_Children<- factor(UseMe$Partner_Time_Children,levels = c("Not applicable","0-19 minutes","20-44 minutes","45-59 minutes","60-89 minutes","90+ minutes"))
#UseMe$Partner_Time_Children<- as.numeric(UseMe$Partner_Time_Children)
UseMe%>% count(Partner_Time_Children)
summary(aov(data = UseMe, Life_Satisfaction~Partner_Time_Children)) #sig

PTimeKid <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Time_Children) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Time_Children))
PTimeKidtable<-PTimeKid %>%
  group_by(Partner_Time_Children) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
PTimeKidtable$variable = NULL
PTimeKidtable <- PTimeKidtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
PTimeKidtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Partner Spending Time with Kids"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 4.07e-15")
#----------------------------------------------------#
PTimeKidaov <- PTimeKid %>% anova_test(Life_Satisfaction ~ Partner_Time_Children)
PTimeKidaov
plotmeans(PTimeKid$Life_Satisfaction ~ PTimeKid$Partner_Time_Children, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Spending Time with Kids", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Participating in Household Activities"))
#---------------------------------------------------------------------------#
PTimeKidTuk <- PTimeKid %>% tukey_hsd(Life_Satisfaction ~ Partner_Time_Children)
PTimeKidTuk <- PTimeKidTuk %>% add_xy_position(x = "Partner_Time_Children")
PTimeKidTuk

ggboxplot(PTimeKid, x = "Partner_Time_Children", y = "Life_Satisfaction") +
  stat_pvalue_manual(PTimeKidTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Partner Spending Time with Kids vs Life Satisfaction",
    subtitle = get_test_label(PTimeKidaov, detailed = TRUE),
    caption = get_pwc_label(PTimeKidTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Partner Spending Time with Kids") +
  ylab(label = "Life Satisfaction")


#---------FINANCIAL THINGS------------#
#FREQUENCY OF FINANCIAL CONFLICT
UseMe$Frequency_Finance_Conflict<- AMAA$Frequency_Finance_Conflict
UseMe%>% count(Frequency_Finance_Conflict)
UseMe$Frequency_Finance_Conflict<- factor(UseMe$Frequency_Finance_Conflict,levels = c("Never","Rarely","Sometimes","Usually","Always"))
UseMe$Frequency_Finance_Conflict<- as.numeric(UseMe$Frequency_Finance_Conflict)
UseMe%>% count(Frequency_Finance_Conflict)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Finance_Conflict))

FreqFinCon <- UseMe %>%
  dplyr::select(Life_Satisfaction, Frequency_Finance_Conflict) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Frequency_Finance_Conflict))
FreqFinContable<-FreqFinCon %>%
  group_by(Frequency_Finance_Conflict) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
FreqFinContable$variable = NULL
FreqFinContable <- FreqFinContable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
FreqFinContable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Partner Spending Time with Kids"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 4.07e-15")
#----------------------------------------------------#
FreqFinConaov <- FreqFinCon %>% anova_test(Life_Satisfaction ~ Frequency_Finance_Conflict)
FreqFinConaov
plotmeans(FreqFinCon$Life_Satisfaction ~ FreqFinCon$Frequency_Finance_Conflict, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Partner Spending Time with Kids", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Partner Participating in Household Activities"))
#---------------------------------------------------------------------------#
FreqFinConTuk <- FreqFinCon %>% tukey_hsd(Life_Satisfaction ~ Frequency_Finance_Conflict)
FreqFinConTuk <- FreqFinConTuk %>% add_xy_position(x = "Frequency_Finance_Conflict")
FreqFinConTuk

ggboxplot(FreqFinCon, x = "Frequency_Finance_Conflict", y = "Life_Satisfaction") +
  stat_pvalue_manual(FreqFinConTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Partner Spending Time with Kids vs Life Satisfaction",
    subtitle = get_test_label(FreqFinConaov, detailed = TRUE),
    caption = get_pwc_label(FreqFinConTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Partner Spending Time with Kids") +
  ylab(label = "Life Satisfaction")



#FREQUENCY OF PARTNER MAKING FINANCIAL DECISONS
UseMe$Frequency_Financial_Decision
UseMe%>% count(Frequency_Financial_Decision)
UseMe$Frequency_Financial_Decision<- factor(UseMe$Frequency_Financial_Decision,levels = c("Never","Rarely","Sometimes","Usually","Always"))
UseMe$Frequency_Financial_Decision<- as.numeric(UseMe$Frequency_Financial_Decision)
UseMe%>% count(Frequency_Financial_Decision)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Financial_Decision))


#FREQUENCY OF PARTNER MAKING INVESTMENT DECISIONS
UseMe$Frequency_Investment_Decision
UseMe%>% count(Frequency_Investment_Decision)
UseMe$Frequency_Investment_Decision<- factor(UseMe$Frequency_Investment_Decision,levels = c("I do.","My spouse/partner does."))
UseMe$Frequency_Investment_Decision<- as.numeric(UseMe$Frequency_Investment_Decision)
UseMe%>% count(Frequency_Investment_Decision)
t.test(data = UseMe, Life_Satisfaction~Frequency_Investment_Decision) #ns


#FREQUENCY OF PARTNER MAKING DECISIONS OF SAVING
UseMe$Frequency_Savings_Decision<- AMAA$Frequency_Savings_Decision
UseMe%>% count(Frequency_Savings_Decision)
UseMe$Frequency_Savings_Decision<- factor(UseMe$Frequency_Savings_Decision,levels = c("I do.","My spouse/partner does."))
UseMe$Frequency_Savings_Decision<- as.numeric(UseMe$Frequency_Savings_Decision)
UseMe%>% count(Frequency_Savings_Decision)
t.test(data = UseMe, Life_Satisfaction~Frequency_Savings_Decision) #ns

#----------------------------#
#HOURS OF PARTNER AT WORK
UseMe$Hours_Partner_Work<-AMAA$Hours_Partner_Work
UseMe%>% count(Hours_Partner_Work)
UseMe$Hours_Partner_Work<- factor(UseMe$Hours_Partner_Work)
UseMe$Hours_Partner_Work<- as.numeric(UseMe$Hours_Partner_Work)
UseMe%>% count(Hours_Partner_Work)
summary(aov(data = UseMe, Life_Satisfaction~Hours_Partner_Work)) #ns

#AT CALL DAYS PER MONTH
UseMe$At_Call_Days_Per_Month
UseMe%>% count(At_Call_Days_Per_Month)
#UseMe$At_Call_Days_Per_Month<- factor(UseMe$At_Call_Days_Per_Month,levels = c("0","1","2","3","4","5","6","7+"))
UseMe$At_Call_Days_Per_Month<- as.numeric(UseMe$At_Call_Days_Per_Month)
UseMe%>% count(At_Call_Days_Per_Month)
summary(aov(data = UseMe, Life_Satisfaction~At_Call_Days_Per_Month)) #ns


#HOW MANY HOSPITALS PARTNER HIRED AT OVERALL
UseMe$Number_Hospitals_Hired_Cumulative
UseMe%>% count(Number_Hospitals_Hired_Cumulative)
#UseMe$Number_Hospitals_Hired_Cumulative<- factor(UseMe$Number_Hospitals_Hired_Cumulative,levels = c("0","1","2","3","4+"))
UseMe$Number_Hospitals_Hired_Cumulative<- as.numeric(UseMe$Number_Hospitals_Hired_Cumulative)
UseMe%>% count(Number_Hospitals_Hired_Cumulative)
summary(aov(data = UseMe, Life_Satisfaction~Number_Hospitals_Hired_Cumulative))

#VACATION WEEKS PER YEAR
UseMe$Week_Vacation_Year<-AMAA$Week_Vacation_Year
UseMe%>% count(Week_Vacation_Year)
#UseMe$Week_Vacation_Year<- factor(UseMe$Week_Vacation_Year)
UseMe$Week_Vacation_Year<- as.numeric(UseMe$Week_Vacation_Year)
UseMe%>% count(Week_Vacation_Year)
summary(aov(data = UseMe, Life_Satisfaction~Week_Vacation_Year))#ns

#HOW MANY MEDICAL CONFERENCES WE GO TOGETHER
UseMe$Frequency_Medical_Conference_Together
UseMe%>% count(Frequency_Medical_Conference_Together)
UseMe$Frequency_Medical_Conference_Together<- factor(UseMe$Frequency_Medical_Conference_Together,levels = c("Never","Rarely","Sometimes","Usually","Always"))
UseMe$Frequency_Medical_Conference_Together<- as.numeric(UseMe$Frequency_Medical_Conference_Together)
UseMe%>% count(Frequency_Medical_Conference_Together)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Medical_Conference_Together))


#HOW MANY TIMES DO WE TRAVEL TOGETHER
UseMe$Frequency_Alone_Travel
UseMe%>% count(Frequency_Alone_Travel)
UseMe$Frequency_Alone_Travel<- factor(UseMe$Frequency_Alone_Travel,levels = c("Never","Every few years","Every other year","Every year","Twice a year","Few times a year","Several times a year"))
UseMe$Frequency_Alone_Travel<- as.numeric(UseMe$Frequency_Alone_Travel)
UseMe%>% count(Frequency_Alone_Travel)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Alone_Travel))


#PARTNER WORKPLACE SHOULD DO MORE
UseMe$Opinion_Partner_More_Effort
UseMe%>% count(Opinion_Partner_More_Effort)
UseMe$Opinion_Partner_More_Effort<- factor(UseMe$Opinion_Partner_More_Effort,levels = c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree"))
UseMe$Opinion_Partner_More_Effort<- as.numeric(UseMe$Opinion_Partner_More_Effort)
UseMe%>% count(Opinion_Partner_More_Effort)
summary(aov(data = UseMe, Life_Satisfaction~Opinion_Partner_More_Effort))


#PARTNER DOESN'T DO MUCH AFTER WORK
UseMe$Partner_Work_No_Participation
UseMe%>% count(Partner_Work_No_Participation)
UseMe$Partner_Work_No_Participation<- factor(UseMe$Partner_Work_No_Participation,levels = c("Never","Rarely","Sometimes","Frequently","Always"))
UseMe$Partner_Work_No_Participation<- as.numeric(UseMe$Partner_Work_No_Participation)
UseMe%>% count(Partner_Work_No_Participation)
summary(aov(data = UseMe, Life_Satisfaction~Partner_Work_No_Participation))


#WORKPLACE IS SUPPORTIVE OF FAMILY
UseMe%>% count(Opinion_Workplace_Family_Supportive)
UseMe$Opinion_Workplace_Family_Supportive<- factor(UseMe$Opinion_Workplace_Family_Supportive,levels = c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree"))
UseMe$Opinion_Workplace_Family_Supportive<- as.numeric(UseMe$Opinion_Workplace_Family_Supportive)
UseMe%>% count(Opinion_Workplace_Family_Supportive)
summary(aov(data = UseMe, Life_Satisfaction~Opinion_Workplace_Family_Supportive))


#------------------------------ME RELAXING-------------------------#
#HOW MUCH TIME AT HOBBIES
UseMe$Frequency_Engage_Hobbies
UseMe%>% count(Frequency_Engage_Hobbies)
UseMe$Frequency_Engage_Hobbies<- factor(UseMe$Frequency_Engage_Hobbies,levels = c("Never","Seldom","Monthly","2-3 times per month","Weekly","3-5 times per week","Daily"))
UseMe$Frequency_Engage_Hobbies<- as.numeric(UseMe$Frequency_Engage_Hobbies)
UseMe%>% count(Frequency_Engage_Hobbies)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Engage_Hobbies))

#HOW MUCH TIME USING RELAX TECHNIQUES
UseMe$Frequency_Relaxation_Techniques
UseMe%>% count(Frequency_Relaxation_Techniques)
UseMe$Frequency_Relaxation_Techniques<- factor(UseMe$Frequency_Relaxation_Techniques,levels = c("Never","Seldom","Monthly","2-3 times per month","Weekly","3-5 times per week","Daily"))
UseMe$Frequency_Relaxation_Techniques<- as.numeric(UseMe$Frequency_Relaxation_Techniques)
UseMe%>% count(Frequency_Relaxation_Techniques)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Relaxation_Techniques)) #ns

#HOW MUCH TIME RELIGION
UseMe$Frequency_Draw_Religion
UseMe%>% count(Frequency_Draw_Religion)
UseMe$Frequency_Draw_Religion<- factor(UseMe$Frequency_Draw_Religion,levels = c("Never","Seldom","Monthly","2-3 times per month","Weekly","3-5 times per week","Daily"))
UseMe$Frequency_Draw_Religion<- as.numeric(UseMe$Frequency_Draw_Religion)
UseMe%>% count(Frequency_Draw_Religion)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Draw_Religion))

#HOW MUCH TIME AT WORSHIP SERVICES
UseMe$Frequency_Worship_Services
UseMe%>% count(Frequency_Worship_Services)
UseMe$Frequency_Worship_Services<- factor(UseMe$Frequency_Worship_Services,levels = c("Never","Seldom","Monthly","2-3 times per month","Weekly","3-5 times per week","Daily"))
UseMe$Frequency_Worship_Services<- as.numeric(UseMe$Frequency_Worship_Services)
UseMe%>% count(Frequency_Worship_Services)
summary(aov(data = UseMe, Life_Satisfaction~Frequency_Worship_Services))



#----------------------------------------------------------#
}

#### create new column with medical school/residency in it #coalesce is GOAT#
# PGY = residency, MSY = Medical School
Q1 <- UseMe %>% bplyr::select(Sexual_QoL, Emotional_QoL, Physical_QoL, Family_Quality_Life, Mental_QoL, Life_Satisfaction, Overall_QoL, Overall_QoL_Range, Relationship_Status, Age, AgeRange, Education_Current_I, Years_Post_Residency, Primary_Practice_Setting, Years_With_Partner, Partner_Gender, Gender)
# this ^^^^
Edu_or_relastats <- Q1 %>% dplyr::select(Years_Post_Residency, Education_Current_I)
Edu_or_relastats$Years_Post_Residency[Edu_or_relastats$Years_Post_Residency == 0] <- NA
Edu_or_relastats <- Edu_or_relastats %>%
  mutate(both = coalesce(Years_Post_Residency, Education_Current_I))
Edu_or_relastats <- Edu_or_relastats %>% dplyr::select(both)
Q1["Edu_or_relastats"] <- Edu_or_relastats


# making labels
{
  Q1 <- Q1 %>%
    mutate(Edu_or_relastats = ifelse(
      Edu_or_relastats %in% c("1st year medical school (MS1)", "2nd year medical school (MS2)", "3rd year medical school (MS3)", "4th year medical school (MS4)"),
      "medschool",
      Edu_or_relastats
    ))
  Q1 <- Q1 %>%
    mutate(Edu_or_relastats = ifelse(
      Edu_or_relastats %in% c("Intern (PGY1)", "2nd year residency (PGY2)", "3rd year residency (PGY3)", "4th year residency (PGY4)", "5th year residency (PGY5)", "6th year residency (PGY6)", "7th year residency (PGY7) or greater"),
      "residency",
      Edu_or_relastats
    ))
}

##### Analysis####
################################### .##################.###################################.
################################### .#SPECIFIC INQUIRY#.###################################.
################################### .##################.###################################.

#Age


# ANOVA Age - 7 levels
UseMe %>% count(AgeRange)
by(UseMe$Life_Satisfaction, UseMe$AgeRange, shapiro.test) # not normal but big data, so ignorable
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$AgeRange) # unequal variance p= 5.109 e-05
agesatf <- aov(UseMe$Life_Satisfaction ~ UseMe$AgeRange)
summary(agesatf)
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$AgeRange, p.adjust.method = "bonferroni")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$AgeRange, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Age Range", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Age Range"))
#-------------------------------------------------#
Age <- UseMe %>%
  dplyr::select(Life_Satisfaction, AgeRange) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(AgeRange))
AgeTable <- Age %>%
  group_by(AgeRange) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
AgeTable
AgeTable$variable = NULL
AgeTable %>% 
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Age"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 6.92e-21")

ageaov <- Age %>% anova_test(Life_Satisfaction ~ AgeRange)
ageaov
AgeTuk <- Age %>% tukey_hsd(Life_Satisfaction ~ AgeRange)
AgeTuk
AgeTuk <- AgeTuk %>% add_xy_position(x = "AgeRange")
AgeTuk

ggboxplot(Age, x = "AgeRange", y = "Life_Satisfaction") +
  stat_pvalue_manual(AgeTuk, hide.ns = TRUE, step.increase = 0.039) +
  labs(
    title = "Life Satisfaction vs Range of Age",
    subtitle = get_test_label(ageaov, detailed = TRUE),
    caption = get_pwc_label(AgeTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Range of Age") +
  ylab(label = "Life Satisfaction")

#ANOVA Relationship status
by(UseMe$Life_Satisfaction, UseMe$Relationship_Status, shapiro.test) # not normal but big data, so ignorable
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$Relationship_Status) # unequal variance
relastatuss <- aov(UseMe$Life_Satisfaction ~ UseMe$Relationship_Status)
summary(relastatuss) # very significant <0.000168
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Relationship_Status, p.adjust.method = "bonferroni")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Relationship_Status, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Relationship", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Relationship"))
#-----------------------------------------------------#
relastats <- UseMe %>%
  dplyr::select(Life_Satisfaction, Relationship_Status) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Relationship_Status))
ggqqplot(relastats, "Life_Satisfaction", facet.by = "Relationship_Status")
relatable<-relastats %>%
  group_by(Relationship_Status) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
relatable
relatable <- relatable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
relatable
relatable$variable = NULL
relatable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Relation Status"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 0.196")

# ANOVA residency length
by(UseMe$Life_Satisfaction, UseMe$ResLength, shapiro.test)
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$ResLength) # equal variance
reslength <- aov(UseMe$Life_Satisfaction ~ UseMe$ResLength) # ns p = 0.628
summary(reslength)
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$ResLength, p.adjust.method = "bonferroni")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$ResLength, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Residency Length", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Residency Length"))
#------------------------------------#
ggplot(UseMe, aes(x = ResLength, y = Life_Satisfaction)) +
  geom_errorbar(stat = "summary", fun.y = "mean", width = 0.5, position = position_dodge(0.8), na.rm = TRUE) +
  geom_point(stat = "summary", fun.y = "mean", shape = 18, size = 3, position = position_dodge(0.8), na.rm = TRUE) +
  geom_line(stat = "summary", fun.y = "mean", group = 1, linetype = "solid", size = 1, position = position_dodge(0.8), na.rm = TRUE, colour = "red") +
  geom_text(stat = "summary", fun.y = "mean", aes(label = sprintf("%.2f", after_stat(y))), position = position_dodge(0.3), vjust = 2, hjust = 0.2, na.rm = TRUE) +
  labs(x = "Residency Length", y = "Life Satisfaction", title = "Life Satisfaction vs Residency Length") +
  theme_minimal()

#------------------------------------#
Residency <- UseMe %>%
  select(Life_Satisfaction, ResLength) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(ResLength))
restable<-Residency %>%
  group_by(ResLength) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
restable$variable = NULL
restable <- restable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

restable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Residency Length"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 0.654")

ggqqplot(Residency, "Life_Satisfaction", facet.by = "ResLength")
#---------------------------#

# t.test residency 1-3, 4-7 year of residency
by(UseMe$Life_Satisfaction, UseMe$ResidencyCat, shapiro.test) # not normal... but data big so..
t.test(UseMe$Life_Satisfaction ~ UseMe$ResidencyCat) # ns p=0.5885
wilcox.test(UseMe$Life_Satisfaction ~ UseMe$ResidencyCat)
plotmeans(UseMe$Life_Satisfaction ~ UseMe$ResidencyCat, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",barcol = "black",xlab = "Residency Category", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Residency Category"))
#-----------------------------------------#
ResCat <- UseMe %>%
  select(Life_Satisfaction, ResidencyCat) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(ResidencyCat))
rescatable<-ResCat %>%
  group_by(ResidencyCat) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
rescatable$variable = NULL
rescatable <- rescatable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

rescatable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Residency Category"=4))%>%
  kable_styling()%>%
  add_footnote("twosample.t.test:p-val = 0.722")


ggqqplot(ResCat, "Life_Satisfaction", facet.by = "ResidencyCat")
hist(UseMe$Life_Satisfaction ~ UseMe$Number_Children)


#-------------------------------------------#
ggplot(UseMe, aes(x = ResidencyCat, y = Life_Satisfaction)) +
  geom_errorbar(stat = "summary", fun.y = "mean", width = 0.5, position = position_dodge(0.8), na.rm = TRUE) +
  geom_point(stat = "summary", fun.y = "mean", shape = 18, size = 3, position = position_dodge(0.8), na.rm = TRUE) +
  geom_line(stat = "summary", fun.y = "mean", group = 1, linetype = "solid", size = 1, position = position_dodge(0.8), na.rm = TRUE, colour = "red") +
  geom_text(stat = "summary", fun.y = "mean", aes(label = sprintf("%.2f", after_stat(y))), position = position_dodge(0.3), vjust = 2, hjust = 0.2, na.rm = TRUE) +
  labs(x = "Residency Category", y = "Life Satisfaction", title = "Life Satisfaction vs Residency Category") +
  theme_minimal()


# ANOVA/t.test for general pop. satisfaction (get from where???)



# ANOVA Relationship between number of children and life satisfaction
by(UseMe$Life_Satisfaction, UseMe$Number_Children, shapiro.test)
hist(UseMe$Life_Satisfaction ~ UseMe$Number_Children)
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$Number_Children) # equal variance
childaov <- aov(UseMe$Life_Satisfaction ~ UseMe$Number_Children) 
summary(childaov)
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Number_Children, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Number of Children", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Number of Children"))
?plotmeans
#-----------------------------------------------------------#
Child <- UseMe %>%
  dplyr::select(Life_Satisfaction, Number_Children) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Number_Children))

chldtable<-Child %>%
  group_by(Number_Children) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
chldtable$variable = NULL
chldtable <- chldtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
chldtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Number of Children"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA test:p-val = 0.111")



ggqqplot(Child, "Life_Satisfaction", facet.by = "Number_Children")
Childaov <- Child %>% anova_test(Life_Satisfaction ~ Number_Children)
Childaov
ChildTuk <- Child %>% tukey_hsd(Life_Satisfaction ~ Number_Children)
ChildTuk <- ChildTuk %>% add_xy_position(x = "Number_Children")
ChildTuk
#----------ANOVA Boxplot------------------------------#
ggboxplot(Child, x = "Number_Children", y = "Life_Satisfaction") +
  stat_pvalue_manual(ChildTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Number of Children",
    subtitle = get_test_label(Childaov, detailed = TRUE),
    caption = get_pwc_label(ChildTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Number of Children") +
  ylab(label = "Life Satisfaction")
#--------plot means----------------------------#
ggplot(UseMe, aes(x = Number_Children, y = Life_Satisfaction)) +
  geom_errorbar(stat = "summary", fun.y = "mean", width = 0.5, position = position_dodge(0.8), na.rm = TRUE) +
  geom_point(stat = "summary", fun.y = "mean", shape = 18, size = 3, position = position_dodge(0.8), na.rm = TRUE) +
  geom_line(stat = "summary", fun.y = "mean", group = 1, linetype = "solid", size = 1, position = position_dodge(0.8), na.rm = TRUE, colour = "red") +
  geom_text(stat = "summary", fun.y = "mean", aes(label = sprintf("%.2f", after_stat(y))), position = position_dodge(0.3), vjust = 2, hjust = 0.2, na.rm = TRUE) +
  labs(x = "Number of Children", y = "Life Satisfaction", title = "Life Satisfaction vs Number of Children") +
  theme_minimal()



#T.test for 0-3 kids or 4+kids CHILD RANGE
UseMe %>% count(ChildRange)
by(UseMe$Life_Satisfaction, UseMe$ChildRange, shapiro.test)
t.test(UseMe$Life_Satisfaction ~ UseMe$ChildRange)
wilcox.test(UseMe$Life_Satisfaction~ UseMe$ChildRange)
plotmeans(UseMe$Life_Satisfaction ~ UseMe$ChildRange, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Children Range", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Children Range"))
numkids <- UseMe %>% select(ChildRange, Life_Satisfaction)
numkids %>%
  group_by(ChildRange) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
#----------------------------------------------#
numkiddo <- UseMe %>%
  dplyr::select(Life_Satisfaction, ChildRange) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(ChildRange))
numkidtable<-numkiddo %>%
  group_by(ChildRange) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
numkidtable$variable = NULL
numkidtable <- numkidtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

numkidtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Range of Children"=4))%>%
  kable_styling()%>%
  add_footnote("twosample-t-test:p-val = 0.005124")

numkiddo %>% t_test(Life_Satisfaction ~ ChildRange)
numkiddo.ttest <- numkiddo %>%
  t_test(Life_Satisfaction ~ ChildRange) %>%
  add_significance() 
numkiddo.ttest
numkiddo.ttest <- member.test %>% add_xy_position(x = "AMA_Member")

numkiddobxp <- ggboxplot(
  numkiddo,
  x = "ChildRange", y = "Life_Satisfaction",
  ylab = "Life_Satisfaction", xlab = "ChildRange", add = "boxplot"
)

numkiddobxp +
  stat_pvalue_manual(numkiddo.ttest, tip.length = 0, step.increase = 0.4) +
  labs(title = "Life Satisfaction vs Range of Number of Children", subtitle = get_test_label(numkiddo.ttest, detailed = TRUE)) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Range of Number of Children") +
  ylab(label = "Life Satisfaction")


# ANOVA Relationship between oldest children age [0-4,5-8,9-13,14-19,20-29,30-39,40+]
by(UseMe$Life_Satisfaction, UseMe$Oldest_Age_Children, shapiro.test)
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$Oldest_Age_Children) # variance not normal so... no anova?
oldchild <- aov(UseMe$Life_Satisfaction ~ UseMe$Oldest_Age_Children)
summary(oldchild) # p <2e-16
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Oldest_Age_Children, p.adjust.method = "bonferroni")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Oldest_Age_Children, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Oldest Age Children", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Oldest Age Children"))

#------------------------------#
OldChild <- UseMe %>%
  dplyr::select(Life_Satisfaction, Oldest_Age_Children) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Oldest_Age_Children))
oldtable<-OldChild %>%
  group_by(Oldest_Age_Children) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")

oldtable$variable = NULL
oldtable <- oldtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

oldtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Age of Oldest Children"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 5.04e-19")


OldChildaov <- OldChild %>% anova_test(Life_Satisfaction ~ Oldest_Age_Children)
OldChildaov
OldChildTuk <- OldChild %>% tukey_hsd(Life_Satisfaction ~ Oldest_Age_Children)
OldChildTuk <- OldChildTuk %>% add_xy_position(x = "Oldest_Age_Children")
OldChildTuk

ggboxplot(OldChild, x = "Oldest_Age_Children", y = "Life_Satisfaction") +
  stat_pvalue_manual(OldChildTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Oldest Age of Children",
    subtitle = get_test_label(OldChildaov, detailed = TRUE),
    caption = get_pwc_label(OldChildTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Oldest Age of Children") +
  ylab(label = "Life Satisfaction")


# ANOVA medical students w or w/o children.
UseMe %>% count(MedChild)
by(UseMe$Life_Satisfaction, UseMe$MedChild, shapiro.test)
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$MedChild) # variance not normal so... no anova?
medchildaov <- aov(UseMe$Life_Satisfaction ~ UseMe$MedChild)
summary(medchildaov) # p = 0.731
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$MedChild, p.adjust.method = "bonferroni")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$MedChild, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Med Student with w/o children", ylab = "Life Satisfaction", main = c("Life Satisfaction vs MedStudent With/Without Kid"))
#-----------------------------------------------------------#
medchild <- UseMe %>%
  dplyr::select(Life_Satisfaction, MedChild) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(MedChild))
medchildtable<-medchild %>%
  group_by(MedChild) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
medchildtable$variable = NULL
medchildtable <- medchildtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

medchildtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Childrens of Med Students"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 0.861")

# ANOVA Resident/Fellow w/o children
by(UseMe$Life_Satisfaction, UseMe$ResChild, shapiro.test)
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$ResChild) # variance not normal so... no anova?
reschildaov <- aov(UseMe$Life_Satisfaction ~ UseMe$ResChild)
summary(reschildaov) # p = 0.731
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$ResChild, p.adjust.method = "bonferroni")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$ResChild, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Resident with w/o children", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Resident With/Without Kid"))
#-----------------------------------------------------------#
reschild <- UseMe %>%
  dplyr::select(Life_Satisfaction, ResChild) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(ResChild))

reschildtable<-reschild %>%
  group_by(ResChild) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")

reschildtable$variable = NULL
reschildtable <- reschildtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

reschildtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Childrens of Resident Fellow "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 0.888")


# ANOVA therapy [broad]
by(UseMe$Life_Satisfaction, UseMe$Participate_Counseling, shapiro.test) # no normal ?
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$Participate_Counseling) # equal variance, do anova
therapygud <- aov(UseMe$Life_Satisfaction ~ UseMe$Participate_Counseling)
summary(therapygud) # sig p = 4,55 e-09
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Participate_Counseling, p.adjust.method = "bonferroni")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Participate_Counseling, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Counseling Participation", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Counseling Participation"))
ggqqplot(Therapy, "Life_Satisfaction", facet.by = "Participate_Counseling")
#------------------------------#
Therapy <- UseMe %>%
  dplyr::select(Life_Satisfaction, Participate_Counseling) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Participate_Counseling))
therapytable<-Therapy %>%
  group_by(Participate_Counseling) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
therapytable$variable = NULL
therapytable <- therapytable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

therapytable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Counseling Participation "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 2.91e-07")
#---------------------------#
Therapyaov <- Therapy %>% anova_test(Life_Satisfaction ~ Participate_Counseling)
Therapyaov
TherapyTuk <- Therapy %>% tukey_hsd(Life_Satisfaction ~ Participate_Counseling)
TherapyTuk <- TherapyTuk %>% add_xy_position(x = "Participate_Counseling")
TherapyTuk

ggboxplot(Therapy, x = "Participate_Counseling", y = "Life_Satisfaction") +
  stat_pvalue_manual(TherapyTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Counseling Participation",
    subtitle = get_test_label(Therapyaov, detailed = TRUE),
    caption = get_pwc_label(TherapyTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Counseling Participation") +
  ylab(label = "Life Satisfaction")

# ANOVA therapy [specific]


# high income earners
phyrich <- UseMe %>%
  select(Life_Satisfaction, Household_Annual_Salary, Overall_QoL, Sexual_QoL, Emotional_QoL, Physical_QoL, Family_Quality_Life, Mental_QoL, ) %>%
  filter(Household_Annual_Salary == 10) %>%
  get_summary_stats(Physical_QoL, type = "mean_sd")
famrich <- UseMe %>%
  select(Life_Satisfaction, Household_Annual_Salary, Overall_QoL, Sexual_QoL, Emotional_QoL, Physical_QoL, Family_Quality_Life, Mental_QoL, ) %>%
  filter(Household_Annual_Salary == 10) %>%
  get_summary_stats(Family_Quality_Life, type = "mean_sd")
mntrich <- UseMe %>%
  select(Life_Satisfaction, Household_Annual_Salary, Overall_QoL, Sexual_QoL, Emotional_QoL, Physical_QoL, Family_Quality_Life, Mental_QoL, ) %>%
  filter(Household_Annual_Salary == 10) %>%
  get_summary_stats(Mental_QoL, type = "mean_sd")
emorich <- UseMe %>%
  select(Life_Satisfaction, Household_Annual_Salary, Overall_QoL, Sexual_QoL, Emotional_QoL, Physical_QoL, Family_Quality_Life, Mental_QoL, ) %>%
  filter(Household_Annual_Salary == 10) %>%
  get_summary_stats(Emotional_QoL, type = "mean_sd")
sexrich <- UseMe %>%
  select(Life_Satisfaction, Household_Annual_Salary, Overall_QoL, Sexual_QoL, Emotional_QoL, Physical_QoL, Family_Quality_Life, Mental_QoL, ) %>%
  filter(Household_Annual_Salary == 10) %>%
  get_summary_stats(Sexual_QoL, type = "mean_sd")
ovallrich <- UseMe %>%
  select(Life_Satisfaction, Household_Annual_Salary, Overall_QoL, Sexual_QoL, Emotional_QoL, Physical_QoL, Family_Quality_Life, Mental_QoL, ) %>%
  filter(Household_Annual_Salary == 10) %>%
  get_summary_stats(Overall_QoL, type = "mean_sd")
richtable <- as.data.frame(c(phyrich, famrich, mntrich, emorich, sexrich, ovallrich))
richtable

# high income but no medschool/residency

# ANOVA annual house income
by(UseMe$Life_Satisfaction, UseMe$Household_Annual_Salary, shapiro.test) # no normal ?
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$Household_Annual_Salary) # barely equal var
annualcash <- aov(UseMe$Life_Satisfaction ~ UseMe$Household_Annual_Salary)
summary(annualcash) # sig p = 1.17 e-09
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Household_Annual_Salary, p.adjust.method = "bonferroni")
#------------------------------#
YrCash <- UseMe %>%
  dplyr::select(Life_Satisfaction, Household_Annual_Salary) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Household_Annual_Salary))

yrcashtable<-YrCash %>%
  group_by(Household_Annual_Salary) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
#-#
yrcashtable$variable = NULL
yrcashtable <- yrcashtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))

yrcashtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Annual House Salary"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 8.43e-10")



YrCash
YrCashaov <- YrCash %>% anova_test(Life_Satisfaction ~ Household_Annual_Salary)
YrCashaov
YrCashTuk <- YrCash %>% tukey_hsd(Life_Satisfaction ~ Household_Annual_Salary)
YrCashTuk <- YrCashTuk %>% add_xy_position(x = "Household_Annual_Salary")
YrCashTuk

ggboxplot(YrCash, x = "Household_Annual_Salary", y = "Life_Satisfaction") +
  stat_pvalue_manual(YrCashTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Annual Salary per Household",
    subtitle = get_test_label(YrCashaov, detailed = TRUE),
    caption = get_pwc_label(YrCashTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Annual Salary per Household") +
  ylab(label = "Life Satisfaction")


# ANOVA student loans - LifeSatisfaction
by(UseMe$Life_Satisfaction, UseMe$Household_Loan_Current, shapiro.test)
bartlett.test(UseMe$Life_Satisfaction ~ UseMe$Household_Loan_Current) # barely equal var
stdntloanLSnono <- aov(UseMe$Life_Satisfaction ~ UseMe$Household_Loan_Current)
summary(stdntloanLSnono) # sig p = <2e-16
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Household_Loan_Current, p.adjust.method = "bonferroni")
#------------------------------#
stdntloanLS <- UseMe %>%
  dplyr::select(Life_Satisfaction, Household_Loan_Current) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Household_Loan_Current))
stdntloantable<-stdntloanLS %>%
  group_by(Household_Loan_Current) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
#-#
stdntloantable$variable = NULL
stdntloantable <- stdntloantable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
stdntloantable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Current Student Loans"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA:p-val = 1.69e-19")

stdntloanLS
stdntloanLSaov <- stdntloanLS %>% anova_test(Life_Satisfaction ~ Household_Loan_Current)
stdntloanLSaov
stdntloanLSTuk <- stdntloanLS %>% tukey_hsd(Life_Satisfaction ~ Household_Loan_Current)
stdntloanLSTuk <- stdntloanLSTuk %>% add_xy_position(x = "Household_Loan_Current")
stdntloanLSTuk

ggboxplot(stdntloanLS, x = "Household_Loan_Current", y = "Life_Satisfaction") +
  stat_pvalue_manual(stdntloanLSTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Current Student Loans",
    subtitle = get_test_label(stdntloanLSaov, detailed = TRUE),
    caption = get_pwc_label(stdntloanLSTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Current Student Loans") +
  ylab(label = "Life Satisfaction")



# ANOVA student loans - Familiy QoL
bartlett.test(UseMe$Family_Quality_Life ~ UseMe$Household_Loan_Current) # barely equal var
stdntloanFAM <- aov(UseMe$Family_Quality_Life ~ UseMe$Household_Loan_Current)
summary(stdntloanFAM) # p = <2e-16
pairwise.t.test(UseMe$Family_Quality_Life, UseMe$Household_Loan_Current, p.adjust.method = "bonferroni")


# ANOVA student loans - Mental QoL
bartlett.test(UseMe$Mental_QoL ~ UseMe$Household_Loan_Current) # barely equal var
stdntloanMT <- aov(UseMe$Mental_QoL ~ UseMe$Household_Loan_Current)
summary(stdntloanMT) # p = <2e-16
pairwise.t.test(UseMe$Mental_QoL, UseMe$Household_Loan_Current, p.adjust.method = "bonferroni")

# ANOVA student loans - Physical QoL
bartlett.test(UseMe$Physical_QoL ~ UseMe$Household_Loan_Current) # barely equal var
stdntloanPHY <- aov(UseMe$Physical_QoL ~ UseMe$Household_Loan_Current)
summary(stdntloanPHY) # p = 2e-16
pairwise.t.test(UseMe$Physical_QoL, UseMe$Household_Loan_Current, p.adjust.method = "bonferroni")


# ANOVA student loans - Emotional QoL
bartlett.test(UseMe$Emotional_QoL ~ UseMe$Household_Loan_Current) # barely equal var
stdntloanEMO <- aov(UseMe$Emotional_QoL ~ UseMe$Household_Loan_Current)
summary(stdntloanEMO) # p = 2e-16
pairwise.t.test(UseMe$Emotional_QoL, UseMe$Household_Loan_Current)

# ANOVA student loans - Sexual QoL
bartlett.test(UseMe$Sexual_QoL ~ UseMe$Household_Loan_Current) # barely equal var
stdntloanSX <- aov(UseMe$Sexual_QoL ~ UseMe$Household_Loan_Current)
summary(stdntloanSX) # p = 7.93e-06
pairwise.t.test(UseMe$Sexual_QoL, UseMe$Household_Loan_Current)

# ANOVA student loans - Overall_QoL
bartlett.test(UseMe$Overall_QoL ~ UseMe$Household_Loan_Current) # barely equal var
stdntloanALL <- aov(UseMe$Overall_QoL ~ UseMe$Household_Loan_Current)
summary(stdntloanALL) # p = <2e-16
pairwise.t.test(UseMe$Overall_QoL, UseMe$Household_Loan_Current)


# t.test med/surg
by(UseMe$Life_Satisfaction, UseMe$MedOrSur, shapiro.test)
t.test(UseMe$Life_Satisfaction ~ UseMe$MedOrSur) # p-value: 0.03946
wilcox.test(UseMe$Life_Satisfaction ~ UseMe$MedOrSur) # p-value:
medOrSurg <- UseMe %>%
  dplyr::select(Life_Satisfaction, MedOrSur) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(MedOrSur))
medOrsurgtable<-medOrSurg %>%
  group_by(MedOrSur) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
medOrsurgtable$variable = NULL
medOrsurgtable <- medOrsurgtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
medOrsurgtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Current Student Loans"=4))%>%
  kable_styling()%>%
  add_footnote("two-sample.t.test = 0.03946")
#------------------------------#
medsurg.test <- medOrSurg %>%
  t_test(Life_Satisfaction ~ MedOrSur) %>%
  add_significance()
medsurg.test
medsurg.test <- medsurg.test %>% add_xy_position(x = "MedOrSur")

medsurgbxp <- ggboxplot(
  medOrSurg,
  x = "MedOrSur", y = "Life_Satisfaction",
  ylab = "Life_Satisfaction", xlab = "MedOrSur", add = "boxplot"
)
medsurgbxp +
  stat_pvalue_manual(member.test, tip.length = 0, step.increase = 0.4) +
  labs(title = "Life Satisfaction vs Physician Category ", subtitle = get_test_label(medsurg.test, detailed = TRUE)) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Physician Category") +
  ylab(label = "Life Satisfaction")


# ANOVA Surgical Specialty
by(UseMe$Life_Satisfaction, UseMe$Partner_Surgical_Specialty, shapiro.test)
surgspecial <- aov(UseMe$Life_Satisfaction ~ UseMe$Partner_Surgical_Specialty)
summary(surgspecial) # ns
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Partner_Surgical_Specialty, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Surgical Specialty", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Surgical Specialty"))
surgical <- UseMe %>%
  dplyr::select(Life_Satisfaction, Partner_Surgical_Specialty) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Partner_Surgical_Specialty))
surgicaltable<-surgical %>%
  group_by(Partner_Surgical_Specialty) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
colnames(surgicaltable)<- c("Surgical Specialty Classification","N","Mean Satisfaction","Standard Deviation")
surgicaltable$variable = NULL
surgicaltable <- surgicaltable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
surgicaltable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Surgical Specialty"=4))%>%
  kable_styling()%>%
  add_footnote("two-sample.t.test = 0.197")
#---------------------------#

# ANOVA primary practice setting

by(UseMe$Life_Satisfaction, UseMe$Primary_Practice_Setting, shapiro.test)
kruskal.test(UseMe$Life_Satisfaction ~ UseMe$Primary_Practice_Setting)
primpractice <- aov(UseMe$Life_Satisfaction ~ UseMe$Primary_Practice_Setting)
ggqqplot(UseMe$Life_Satisfaction, UseMe$Primary_Practice_Setting)
summary(primpractice) # barely sig
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Primary_Practice_Setting, p.adjust.method = "bonferroni") # difference but by hwo much
#------------------------------#
ppset <- UseMe %>%
  dplyr::select(Life_Satisfaction, Primary_Practice_Setting) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Primary_Practice_Setting))
ppsettable<-ppset %>%
  group_by(Primary_Practice_Setting) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")

ppsettable$variable = NULL
ppsettable <- ppsettable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
ppsettable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Primary Practice Setting"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 0.0634")

ppset %>% kruskal_test(Life_Satisfaction ~ Primary_Practice_Setting)
ppsetaov <- ppset %>% anova_test(Life_Satisfaction ~ Primary_Practice_Setting)
ppsetaov
ppsetTuk <- ppset %>% tukey_hsd(Life_Satisfaction ~ Primary_Practice_Setting)
ppsetTuk <- ppsetTuk %>% add_xy_position(x = "Primary_Practice_Setting")
ppsetTuk

ggboxplot(ppset, x = "Primary_Practice_Setting", y = "Life_Satisfaction") +
  stat_pvalue_manual(ppsetTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Setting of Practice",
    subtitle = get_test_label(ppsetaov, detailed = TRUE),
    caption = get_pwc_label(ppsetTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Setting of Practice") +
  ylab(label = "Life Satisfaction")

# T.test AMA member
by(UseMe$Life_Satisfaction, UseMe$AMAA_Member, shapiro.test)
t.test(UseMe$Life_Satisfaction~ UseMe$AMAA_Member) # p< 2.2e-16... interesting
wilcox.test(UseMe$Life_Satisfaction~ UseMe$AMAA_Member)
plotmeans(UseMe$Life_Satisfaction ~ UseMe$AMAA_Member, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "AMAA Member", ylab = "Life Satisfaction", main = c("Life Satisfaction vs AMAA Membership"))
#-----------------------#
Member <- UseMe %>%
  dplyr::select(Life_Satisfaction, AMAA_Member) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(AMAA_Member))
membertable<-Member %>%
  group_by(AMAA_Member) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
#---------------------------------#
membertable$variable = NULL
membertable <- membertable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
membertable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding AMAA Membership"=4))%>%
  kable_styling()%>%
  add_footnote("two-sample.t.test = 1.08e-18")


Member %>% t_test(Life_Satisfaction ~ AMAA_Member)
member.test <- Member %>%
  t_test(Life_Satisfaction ~ AMAA_Member) %>%
  add_significance()
member.test
member.test <- member.test %>% add_xy_position(x = "AMAA_Member")

memberbxp <- ggboxplot(
  Member,
  x = "AMAA_Member", y = "Life_Satisfaction",
  ylab = "Life Satisfaction", xlab = "AMAA Membership", add = "boxplot"
)


memberbxp +
  stat_pvalue_manual(member.test, tip.length = 0, step.increase = 0.4) +
  labs(title = "Life Satisfaction vs AMAA Membership", subtitle = get_test_label(member.test, detailed = TRUE)) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "AMAA Membership") +
  ylab(label = "Life Satisfaction")

# Sleep refreshing
UseMe %>% count(seven_Days_Sleep_Refresh)
summary(aov(refresh$Life_Satisfaction ~ refresh$seven_Days_Sleep_Refresh))
refresh <- UseMe %>%
  dplyr::select(Life_Satisfaction, seven_Days_Sleep_Refresh) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(seven_Days_Sleep_Refresh))
refreshtable<-refresh %>%
  group_by(seven_Days_Sleep_Refresh) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
refreshtable$variable = NULL
refreshtable <- refreshtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
refreshtable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Refreshing Sleep"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 8.24e-19")
#----------------------------------------------------#
refreshaov <- refresh %>% anova_test(Life_Satisfaction ~ seven_Days_Sleep_Refresh)
refreshaov
summary(aov(refresh$Life_Satisfaction ~ refresh$seven_Days_Sleep_Refresh))
#---------------------------------------------------------------------------#
refreshTuk <- refresh %>% tukey_hsd(Life_Satisfaction ~ seven_Days_Sleep_Refresh)
refreshTuk <- refreshTuk %>% add_xy_position(x = "seven_Days_Sleep_Refresh")
refreshTuk

ggboxplot(refresh, x = "seven_Days_Sleep_Refresh", y = "Life_Satisfaction") +
  stat_pvalue_manual(refreshTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Over Last 7 days: Refreshing Sleep vs Life Satisfaction",
    subtitle = get_test_label(refreshaov, detailed = TRUE),
    caption = get_pwc_label(refreshTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Refreshing Sleep") +
  ylab(label = "Life Satisfaction")

# Problems in SLEEP
zzzproblem <- UseMe %>%
  dplyr::select(Life_Satisfaction, seven_Day_Sleep_Problem) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(seven_Day_Sleep_Problem))
zzztable<-zzzproblem %>%
  group_by(seven_Day_Sleep_Problem) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
zzztable$variable = NULL
zzztable <- zzztable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
zzztable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Sleep Problems"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 5.65e-10")

zzzproblemaov <- zzzproblem %>% anova_test(Life_Satisfaction ~ seven_Day_Sleep_Problem)
summary(aov(zzzproblem$Life_Satisfaction ~ zzzproblem$seven_Day_Sleep_Problem))
#---------------------------------------------------------------------------#
zzzproblem %>% anova_test(Life_Satisfaction ~ seven_Day_Sleep_Problem)
zzzproblemTuk <- zzzproblem %>% tukey_hsd(Life_Satisfaction ~ seven_Day_Sleep_Problem)
zzzproblemTuk <- zzzproblemTuk %>% add_xy_position(x = "seven_Day_Sleep_Problem")
zzzproblemTuk

ggboxplot(zzzproblem, x = "seven_Day_Sleep_Problem", y = "Life_Satisfaction") +
  stat_pvalue_manual(zzzproblemTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Over Last 7 days:Sleeping Problems vs Life Satisfaction",
    subtitle = get_test_label(zzzproblemaov, detailed = TRUE), # ANOVA summary statistics on the top
    caption = get_pwc_label(zzzproblemTuk) # Place we got p-values from
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Sleeping Problems") +
  ylab(label = "Life Satisfaction")

# Difficult fall asleep
zzzcant <- UseMe %>%
  dplyr::select(Life_Satisfaction, seven_Day_Problem_Fall_Asleep) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(seven_Day_Problem_Fall_Asleep))
zzzcantable<-zzzcant %>%
  group_by(seven_Day_Problem_Fall_Asleep) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
zzzcantable$variable = NULL
zzzcantable <- zzzcantable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
zzzcantable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Problem Fall Asleep"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 2.06e-5")



zzzcant %>% anova_test(Life_Satisfaction ~ seven_Day_Problem_Fall_Asleep)
zzzcantaov <- zzzcant %>% anova_test(Life_Satisfaction ~ seven_Day_Problem_Fall_Asleep)

summary(aov(zzzcant$Life_Satisfaction ~ zzzcant$seven_Day_Problem_Fall_Asleep))
#---------------------------------------------------------------------------#
zzzcantTuk <- zzzcant %>% tukey_hsd(Life_Satisfaction ~ seven_Day_Problem_Fall_Asleep)
zzzcantTuk <- zzzcantTuk %>% add_xy_position(x = "seven_Day_Problem_Fall_Asleep")
zzzcantTuk

ggboxplot(zzzcant, x = "seven_Day_Problem_Fall_Asleep", y = "Life_Satisfaction") +
  stat_pvalue_manual(zzzcantTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Over Last 7 Days:Problem Falling Asleep vs Life Satisfaction",
    subtitle = get_test_label(zzzcantaov, detailed = TRUE),
    caption = get_pwc_label(zzzcantTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Problem Falling Asleep") +
  ylab(label = "Life Satisfaction")

#EDUCATIONAL STATUS
edustat<- UseMe %>% dplyr::select(Highest_Education_Completed,Life_Satisfaction) 
edustat <- UseMe %>%
  dplyr::select(Life_Satisfaction, Highest_Education_Completed) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Highest_Education_Completed))
plotmeans(edustat$Life_Satisfaction ~ edustat$Highest_Education_Completed, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Highest Education Completed", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Highest Education Completed"))

summary(aov(UseMe$Life_Satisfaction~UseMe$Highest_Education_Completed))

edustatable<-edustat %>%
  group_by(Highest_Education_Completed) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
edustatable$variable = NULL
edustatable <- edustatable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
edustatable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Highest Degree Achieved"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 0.171")

# SLEEP chi-square
slpchi <- matrix(c(64.4, 71.2, 73.9, 81.4, 82.2, 78.1, 76, 75, 72.9, 63.4, 76, 75.5, 73.8, 71.3, 65.5), ncol = 3, byrow = FALSE)
colnames(slpchi) <- c("sleep refreshing", "problems IN sleep", "problems falling asleep")
rownames(slpchi) <- c("not at all", "a little bit", "somewhat", "quite a bit", "very much")
slpchi <- as.table(slpchi)
slpchi
chisq.test(slpchi)

#TIME spent together
summary(aov(UseMe$Life_Satisfaction~UseMe$Time_Alone_Partner))
summary(glm(UseMe$Life_Satisfaction~UseMe$Time_Alone_Partner + UseMe$Edu_or_Career))
#--------------------------------------3
timespent <- UseMe %>%
  dplyr::select(Life_Satisfaction, Time_Alone_Partner) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Time_Alone_Partner))
timespentable<-timespent %>%
  group_by(Time_Alone_Partner) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
plotmeans(timespent$Life_Satisfaction ~ timespent$Time_Alone_Partner, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Time Spent Together", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Time Spent Together"))
#------------#
timespentable$variable = NULL
timespentable <- timespentable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
timespentable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Time Alone Together"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 1.59e-22")

timespent
timespentaov <- timespent %>% anova_test(Life_Satisfaction ~ Time_Alone_Partner)
timespentaov
timespentTuk <- timespent %>% tukey_hsd(Life_Satisfaction ~ Time_Alone_Partner)
timespentTuk <- timespentTuk %>% add_xy_position(x = "Time_Alone_Partner")
timespentTuk

timespent$Time_Alone_Partner<- factor(timespent$Time_Alone_Partner,levels = c("<20 minutes","21-45 minutes","46-60 minutes","61-90 minutes","91-120 minutes",">120 minutes"))
ggboxplot(timespent, x = "Time_Alone_Partner", y = "Life_Satisfaction") +
  stat_pvalue_manual(timespentTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Time Spent Together",
    subtitle = get_test_label(timespentaov, detailed = TRUE),
    caption = get_pwc_label(timespentTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Time Spent Together") +
  ylab(label = "Life Satisfaction")

#TIME quality together
summary(aov(data = UseMe,Life_Satisfaction~ Quality_Time_Partner ))
#summary(glm(data = UseMe,Life_Satisfaction~ Quality_Time_Partner ))
#----------------------------------------------#
timequality <- UseMe %>%
  dplyr::select(Life_Satisfaction, Quality_Time_Partner) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Quality_Time_Partner))
timequalitytable<-timequality %>%
  group_by(Quality_Time_Partner) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
plotmeans(timequality$Life_Satisfaction ~ timequality$Quality_Time_Partner, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Quality Time Spent Together", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Quality Time Spent Together"))


timequalitytable$variable = NULL
timequalitytable <- timequalitytable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
timequalitytable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Quality Time Together"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA-p = 1.49e-25")



timequalityaov <- timequality %>% anova_test(Life_Satisfaction ~ Quality_Time_Partner)
timequalityaov
timequalityTuk <- timequality %>% tukey_hsd(Life_Satisfaction ~ Quality_Time_Partner)
timequalityTuk <- timequalityTuk %>% add_xy_position(x = "Quality_Time_Partner")
timequalityTuk

ggboxplot(timequality, x = "Quality_Time_Partner", y = "Life_Satisfaction") +
  stat_pvalue_manual(timequalityTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Quality Time Partner",
    subtitle = get_test_label(timequalityaov, detailed = TRUE),
    caption = get_pwc_label(timequalityTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Quality Time Partner") +
  ylab(label = "Life Satisfaction")

View(data)
#PREGNANT
t.test(UseMe$Life_Satisfaction ~ UseMe$Pregnant)
wilcox.test(UseMe$Life_Satisfaction ~ UseMe$Pregnant)
ggqqplot(UseMe$Life_Satisfaction ~ UseMe$Pregnant)
UseMe %>% count(Pregnant)
plotmeans(Pregnant$Life_Satisfaction ~Pregnant$Pregnant, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Pregnant", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Pregnancy"))
#----------------------------------------#
Pregnant <- UseMe %>%
  dplyr::select(Life_Satisfaction, Pregnant) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Pregnant))
pregnantable<-Pregnant %>%
  group_by(Pregnant) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
pregnantable$variable = NULL
pregnantable <- pregnantable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
pregnantable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Quality Time Together"=4))%>%
  kable_styling()%>%
  add_footnote("two-sample.t.test = 0.3246")

#Years with PARTNER
summary(aov(data = UseMe, Life_Satisfaction ~ Years_With_Partner))
#----------------------------------------------#
YrsWpart <- UseMe %>%
  dplyr::select(Life_Satisfaction, Years_With_Partner) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Years_With_Partner))
UseMe%>% count(Years_With_Partner)
yrpartnertable<-YrsWpart %>%
  group_by(Years_With_Partner) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
yrpartnertable$variable = NULL
yrpartnertable <- yrpartnertable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
yrpartnertable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction regarding Years with Partner"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 1.14e-18")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Years_With_Partner, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Years with partner", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Years with Partner"))


YrsWpartaov <- YrsWpart %>% anova_test(Life_Satisfaction ~ Years_With_Partner)
YrsWpartaov
YrsWpartTuk <- YrsWpart %>% tukey_hsd(Life_Satisfaction ~ Years_With_Partner)
YrsWpartTuk <- YrsWpartTuk %>% add_xy_position(x = "Years_With_Partner")
YrsWpartTuk 

ggboxplot(YrsWpart, x = "Years_With_Partner", y = "Life_Satisfaction") +
  stat_pvalue_manual(YrsWpartTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Years with Partner",
    subtitle = get_test_label(YrsWpartaov, detailed = TRUE),
    caption = get_pwc_label(YrsWpartTuk),
    x = "Years with Partner",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )

pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Years_With_Partner)
#Pride medical family
UseMe %>% count(Pride_Medical_Family)
summary(aov(UseMe$Life_Satisfaction~UseMe$Pride_Medical_Family))
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Pride_Medical_Family, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Pride in Medical Family", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Pride in Medical Family"))
#-----------------------------------------------------------#
pridemed <- UseMe %>%
  dplyr::select(Life_Satisfaction, Pride_Medical_Family) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Pride_Medical_Family))
jk<-pridemed %>%
  group_by(Pride_Medical_Family) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
jktable <- jk %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
jktable$variable <- NULL
jktable %>% 
  kbl() %>%
  kable_styling() %>%
  add_header_above(c("Life Satisfaction regarding Pride in Medical Family"=4)) %>%
  add_footnote("ANOVA p = 1.92e-27")


pridemedaov <- pridemed %>% anova_test(Life_Satisfaction ~ Pride_Medical_Family)
pridemedaov
pridemedTuk <- pridemed %>% tukey_hsd(Life_Satisfaction ~ Pride_Medical_Family)
pridemedTuk <- pridemedTuk %>% add_xy_position(x = "Pride_Medical_Family")
pridemedTuk

ggboxplot(pridemed, x = "Pride_Medical_Family", y = "Life_Satisfaction") +
  stat_pvalue_manual(pridemedTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Pride Medical Family",
    subtitle = get_test_label(pridemedaov, detailed = TRUE),
    caption = get_pwc_label(pridemedTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Pride Medical") +
  ylab(label = "Life Satisfaction")

#-------------------------------COMMUNITY-----------------------------------------#
#Community Left out
summary(aov(data = UseMe, Life_Satisfaction ~ Community_Left_Out))
#----------------------------------------------#
ComOut <- UseMe %>%
  dplyr::select(Life_Satisfaction, Community_Left_Out) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Left_Out))
UseMe%>% count(Community_Left_Out)
ComOutTable<-ComOut %>%
  group_by(Community_Left_Out) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
ComOutTable$variable = NULL
ComOutTable <- ComOutTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(ComOutTable)<- c("Feeling left out of Comunity","N","Mean Satisfaction","Standard Deviation")

ComOutTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and feeling left out in their community"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 1.18e-42")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Community_Left_Out, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Feel left out community", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Feeling Left Out (Community)"))
colnames(ComOutTable)<- c("Feeling left out of Comunity","N","Mean Satisfaction","Standard Deviation")
UseMe%>% count(Community_Left_Out)

ComOutaov <- ComOut %>% anova_test(Life_Satisfaction ~ Community_Left_Out)
ComOutaov
ComOutTuk <- ComOut %>% tukey_hsd(Life_Satisfaction ~ Community_Left_Out)
ComOutTuk <- ComOutTuk %>% add_xy_position(x = "Community_Left_Out")
ComOutTuk 

ggboxplot(ComOut, x = "Community_Left_Out", y = "Life_Satisfaction") +
  stat_pvalue_manual(ComOutTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Feel Left out of Community",
    subtitle = get_test_label(ComOutaov, detailed = TRUE),
    caption = get_pwc_label(ComOutTuk),
    x = "Feel left out of Community",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )

#Community Isolated
summary(aov(data = UseMe, Life_Satisfaction ~ Community_Isolated))
#----------------------------------------------#
ComIso <- UseMe %>%
  dplyr::select(Life_Satisfaction, Community_Isolated) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Isolated))
UseMe%>% count(Community_Isolated)
ComIsoTable<-ComIso %>%
  group_by(Community_Isolated) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
ComIsoTable$variable = NULL
ComIsoTable <- ComIsoTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(ComIsoTable)<- c("Feeling isolated from Comunity","N","Mean Satisfaction","Standard Deviation")

ComIsoTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and feeling isolated from community"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 9.15e-59")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Community_Isolated, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Feel isolated from community", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Feeling Isolated (Community)"))
colnames(ComIsoTable)<- c("Feeling left out of Comunity","N","Mean Satisfaction","Standard Deviation")

ComIsoaov <- ComIso %>% anova_test(Life_Satisfaction ~ Community_Isolated)
ComIsoaov
ComIsoTuk <- ComIso %>% tukey_hsd(Life_Satisfaction ~ Community_Isolated)
ComIsoTuk <- ComIsoTuk %>% add_xy_position(x = "Community_Isolated")
ComIsoTuk 

ggboxplot(ComIso, x = "Community_Isolated", y = "Life_Satisfaction") +
  stat_pvalue_manual(ComIsoTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Feel Isolated from Community",
    subtitle = get_test_label(ComIsoaov, detailed = TRUE),
    caption = get_pwc_label(ComIsoTuk),
    x = "Feel isolated from community",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )

#Community Detached
summary(aov(data = UseMe, Life_Satisfaction ~ Community_Detached))
#----------------------------------------------#
ComDetach <- UseMe %>%
  dplyr::select(Life_Satisfaction, Community_Detached) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Detached))
UseMe%>% count(Community_Detached)
ComDetachTable<-ComDetach %>%
  group_by(Community_Detached) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
ComDetachTable$variable = NULL
ComDetachTable <- ComDetachTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(ComDetachTable)<- c("Feeling detached from Comunity","N","Mean Satisfaction","Standard Deviation")

ComDetachTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and feeling detached from community"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 6.09e-45")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Community_Detached, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Feel detached from community", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Feeling detached (Community)"))
colnames(ComDetachTable)<- c("Feeling detached from Comunity","N","Mean Satisfaction","Standard Deviation")

ComDetachaov <- ComDetach %>% anova_test(Life_Satisfaction ~ Community_Detached)
ComDetachaov
ComDetachTuk <- ComDetach %>% tukey_hsd(Life_Satisfaction ~ Community_Detached)
ComDetachTuk <- ComDetachTuk %>% add_xy_position(x = "Community_Detached")
ComDetachTuk 

ggboxplot(ComDetach, x = "Community_Detached", y = "Life_Satisfaction") +
  stat_pvalue_manual(ComDetachTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Feel Detached from Community",
    subtitle = get_test_label(ComDetachaov, detailed = TRUE),
    caption = get_pwc_label(ComDetachTuk),
    x = "Feel detached from community",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )


UseMe %>% count(Community_Left_Out)
UseMe %>% count(Community_Unknwon)
UseMe %>% count(Community_Isolated)
UseMe %>% count(Community_Detached)

#Community Unknown 
summary(aov(data = UseMe, Life_Satisfaction ~ Community_Unknwon))
#----------------------------------------------#
ComUnk <- UseMe %>%
  dplyr::select(Life_Satisfaction, Community_Unknwon) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Unknwon))
UseMe%>% count(Community_Unknwon)
ComUnkTable<-ComUnk %>%
  group_by(Community_Unknwon) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
ComUnkTable$variable = NULL
ComUnkTable <- ComUnkTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(ComUnkTable)<- c("Feeling no one knows me in Comunity","N","Mean Satisfaction","Standard Deviation")

ComUnkTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and feeling unknown community"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 2.2e-38")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Community_Unknwon, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Feel unknown in community", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Feeling Unknown (Community)"))
colnames(ComUnkTable)<- c("Feeling Unknwon from Comunity","N","Mean Satisfaction","Standard Deviation")

ComUnkaov <- ComUnk %>% anova_test(Life_Satisfaction ~ Community_Unknwon)
ComUnkaov
ComUnkTuk <- ComUnk %>% tukey_hsd(Life_Satisfaction ~ Community_Unknwon)
ComUnkTuk <- ComUnkTuk %>% add_xy_position(x = "Community_Unknwon")
ComUnkTuk 

ggboxplot(ComUnk, x = "Community_Unknwon", y = "Life_Satisfaction") +
  stat_pvalue_manual(ComUnkTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Feel unknown in Community",
    subtitle = get_test_label(ComUnkaov, detailed = TRUE),
    caption = get_pwc_label(ComUnkTuk),
    x = "Feel unknown in community",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis http://127.0.0.1:19997/graphics/5bd2ad89-aa22-4c65-a92c-d83990d74e80.pnglabel size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )

#REGION OR COUNTRY
summary(aov(data = UseMe, Life_Satisfaction ~ Region_Country))
#----------------------------------------------#
Geograph <- UseMe %>%
  dplyr::select(Life_Satisfaction, Region_Country) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Region_Country))
UseMe%>% count(Region_Country)
GeographTable<-Geograph %>%
  group_by(Region_Country) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
GeographTable$variable = NULL
GeographTable <- GeographTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(GeographTable)<- c("Country/Region","N","Mean Satisfaction","Standard Deviation")

GeographTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and Country/Region of living"=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 0.0107")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Region_Country, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Region/Country of living", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Region/Country of living"))
colnames(GeographTable)<- c("Region/Country of Living","N","Mean Satisfaction","Standard Deviation")
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Region_Country)

Geographaov <- Geograph %>% anova_test(Life_Satisfaction ~ Region_Country)
Geographaov
GeographTuk <- Geograph %>% tukey_hsd(Life_Satisfaction ~ Region_Country)
GeographTuk <- GeographTuk %>% add_xy_position(x = "Region_Country")
GeographTuk 

ggboxplot(Geograph, x = "Region_Country", y = "Life_Satisfaction") +
  stat_pvalue_manual(GeographTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Region/Country of living",
    subtitle = get_test_label(Geographaov, detailed = TRUE),
    caption = get_pwc_label(GeographTuk),
    x = "Region/Country of living",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis http://127.0.0.1:19997/graphics/5bd2ad89-aa22-4c65-a92c-d83990d74e80.pnglabel size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )
##----------------------------------EXERCISE---------------------------##
#MODERATE
summary(aov(data = UseMe, Life_Satisfaction ~ Time_Moderate_Exercise))
#----------------------------------------------#
ModEx <- UseMe %>%
  dplyr::select(Life_Satisfaction, Time_Moderate_Exercise) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Time_Moderate_Exercise))
UseMe%>% count(Time_Moderate_Exercise)
ModExTable<-ModEx %>%
  group_by(Time_Moderate_Exercise) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
ModExTable$variable = NULL
ModExTable <- ModExTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(ModExTable)<- c("Frequency of Moderate Exercise","N","Mean Satisfaction","Standard Deviation")

ModExTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and Frequency of Moderate Exercise "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 0.00475")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Time_Moderate_Exercise, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Frequency of Moderate Exercise", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Frequency of Moderate Exercise"))
colnames(ModExTable)<- c("Frequency of Moderate Exercise","N","Mean Satisfaction","Standard Deviation")
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Time_Moderate_Exercise)

ModExaov <- ModEx %>% anova_test(Life_Satisfaction ~ Time_Moderate_Exercise)
ModExaov
ModExTuk <- ModEx %>% tukey_hsd(Life_Satisfaction ~ Time_Moderate_Exercise)
ModExTuk <- ModExTuk %>% add_xy_position(x = "Time_Moderate_Exercise")
ModExTuk 

ggboxplot(ModEx, x = "Time_Moderate_Exercise", y = "Life_Satisfaction") +
  stat_pvalue_manual(ModExTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Frequency of Moderate Exercise",
    subtitle = get_test_label(ModExaov, detailed = TRUE),
    caption = get_pwc_label(ModExTuk),
    x = "Frequency of Moderate Exercise",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis http://127.0.0.1:19997/graphics/5bd2ad89-aa22-4c65-a92c-d83990d74e80.pnglabel size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )

#VIGOROUS
summary(aov(data = UseMe, Life_Satisfaction ~ Time_Vigorous_Exercise))
#----------------------------------------------#
ModVig <- UseMe %>%
  dplyr::select(Life_Satisfaction, Time_Vigorous_Exercise) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Time_Vigorous_Exercise))
UseMe%>% count(Time_Vigorous_Exercise)
ModVigTable<-ModVig %>%
  group_by(Time_Vigorous_Exercise) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
ModVigTable$variable = NULL
ModVigTable <- ModVigTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(ModVigTable)<- c("Frequency of Vigorous Exercise","N","Mean Satisfaction","Standard Deviation")

ModVigTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and Frequency of Vigorous Exercise "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 0.0147")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Time_Vigorous_Exercise, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Frequency of Vigorous Exercise", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Frequency of Vigorous Exercise"))
colnames(ModVigTable)<- c("Frequency of Vigorous Exercise","N","Mean Satisfaction","Standard Deviation")
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Time_Vigorous_Exercise)

ModVigaov <- ModVig %>% anova_test(Life_Satisfaction ~ Time_Vigorous_Exercise)
ModVigaov
ModVigTuk <- ModVig %>% tukey_hsd(Life_Satisfaction ~ Time_Vigorous_Exercise)
ModVigTuk <- ModVigTuk %>% add_xy_position(x = "Time_Vigorous_Exercise")
ModVigTuk 

y <- ggboxplot(ModVig, x = "Time_Vigorous_Exercise", y = "Life_Satisfaction") +
  stat_pvalue_manual(ModVigTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Frequency of Vigorous Exercise",
    subtitle = get_test_label(ModVigaov, detailed = TRUE),
    caption = get_pwc_label(ModVigTuk),
    x = "Frequency of Vigorous Exercise",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis http://127.0.0.1:19997/graphics/5bd2ad89-aa22-4c65-a92c-d83990d74e80.pnglabel size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )


#STRENGTH TRAINING
summary(aov(data = UseMe, Life_Satisfaction ~ Time_Strength_Training))
#----------------------------------------------#
ModStrength <- UseMe %>%
  dplyr::select(Life_Satisfaction, Time_Strength_Training) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Time_Strength_Training))
UseMe%>% count(Time_Strength_Training)
ModStrengthTable<-ModStrength %>%
  group_by(Time_Strength_Training) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
ModStrengthTable$variable = NULL
ModStrengthTable <- ModStrengthTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(ModStrengthTable)<- c("Frequency of Strength Training","N","Mean Satisfaction","Standard Deviation")

ModStrengthTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and Frequency of Strength Training "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 0.000293")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Time_Strength_Training, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Frequency of Strength Training", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Frequency of Strength Training"))
colnames(ModStrengthTable)<- c("Frequency of Strength Training","N","Mean Satisfaction","Standard Deviation")
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Time_Strength_Training)

ModStrengthaov <- ModStrength %>% anova_test(Life_Satisfaction ~ Time_Strength_Training)
ModStrengthaov
ModStrengthTuk <- ModStrength %>% tukey_hsd(Life_Satisfaction ~ Time_Strength_Training)
ModStrengthTuk <- ModStrengthTuk %>% add_xy_position(x = "Time_Strength_Training")
ModStrengthTuk 

x <-ggboxplot(ModStrength, x = "Time_Strength_Training", y = "Life_Satisfaction") +
  stat_pvalue_manual(ModStrengthTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Frequency of Strength Training",
    subtitle = get_test_label(ModStrengthaov, detailed = TRUE),
    caption = get_pwc_label(ModStrengthTuk),
    x = "Frequency of Strength Training",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis http://127.0.0.1:19997/graphics/5bd2ad89-aa22-4c65-a92c-d83990d74e80.pnglabel size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )
x + y
#--------------------------STRESS FACTOR------------------------#
UseMe$Primary_Cause_Stress<-AMAA$Primary_Cause_Stress
UseMe %>% count(Primary_Cause_Stress)
summary(aov(data = UseMe, Life_Satisfaction ~ Primary_Cause_Stress))  #Show this on a graph


#--------------------------------------#
UseMe %>% count(Stress_Factor_Lack_Time)
summary(aov(data = UseMe, Life_Satisfaction ~ Stress_Factor_Lack_Time)) # how much of partner's lack of time freaks me out
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Stress_Factor_Lack_Time, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Stress_Factor_Lack_Time", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Stress_Factor_Lack_Time"))

#---------------------------------------#
UseMe %>% count(Stress_Factor_Business) # Business Aspect of spouse's partner medical practice
summary(aov(data = UseMe, Life_Satisfaction ~ Stress_Factor_Business))
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Stress_Factor_Business, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Stress_Factor_Business", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Stress_Factor_Business"))

#----------------------------------------#
UseMe %>% count(Stress_Factor_Work_Overload)#partner's fatigue from work overload
summary(aov(data = UseMe, Life_Satisfaction ~ Stress_Factor_Work_Overload))
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Stress_Factor_Work_Overload, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Stress_Factor_Work_Overload", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Stress_Factor_Work_Overload"))

#----------------------------------------#
UseMe %>% count(Stress_Factor_Work_Issues_Home)#Partner worries work when home
summary(aov(data = UseMe, Life_Satisfaction ~ Stress_Factor_Work_Issues_Home))
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Stress_Factor_Work_Issues_Home, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Stress_Factor_Work_Issues_Home", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Stress_Factor_Work_Issues_Home"))


#-------------STH FULFILLMENT------------#
UseMe %>% count(Fullfilment_Individual)
summary(aov(data = UseMe, Life_Satisfaction ~ Fullfilment_Individual))

FullMe <- UseMe %>%
  dplyr::select(Life_Satisfaction, Fullfilment_Individual) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Fullfilment_Individual))
UseMe%>% count(Fullfilment_Individual)
FullMeTable<-FullMe %>%
  group_by(Fullfilment_Individual) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
FullMeTable$variable = NULL
FullMeTable <- FullMeTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(FullMeTable)<- c("Sense of Personal Fulfillment","N","Mean Satisfaction","Standard Deviation")

FullMeTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and Sense of Personal Fulfillment "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 5.68e-112")
a <-plotmeans(UseMe$Life_Satisfaction ~ UseMe$Fullfilment_Individual, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Sense of Personal Fulfillment", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Sense of Personal Fulfillment"))
colnames(FullMeTable)<- c("Sense of Personal Fulfillment","N","Mean Satisfaction","Standard Deviation")
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Fullfilment_Individual)

FullMeaov <- FullMe %>% anova_test(Life_Satisfaction ~ Fullfilment_Individual)
FullMeaov
FullMeTuk <- FullMe %>% tukey_hsd(Life_Satisfaction ~ Fullfilment_Individual)
FullMeTuk <- FullMeTuk %>% add_xy_position(x = "Fullfilment_Individual")
FullMeTuk 

ggboxplot(FullMe, x = "Fullfilment_Individual", y = "Life_Satisfaction") +
  stat_pvalue_manual(FullMeTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Life Satisfaction vs Sense of Personal Fulfillment",
    subtitle = get_test_label(FullMeaov, detailed = TRUE),
    caption = get_pwc_label(FullMeTuk),
    x = "Sense of Personal Fulfillment",
    y = "Life Satisfaction"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis http://127.0.0.1:19997/graphics/5bd2ad89-aa22-4c65-a92c-d83990d74e80.pnglabel size
    axis.text = element_text(size = 10), # Adjust axis text size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )


#----------------------------------------#
UseMe %>% count(Fulfillment_as_Parent)
summary(aov(data = UseMe, Life_Satisfaction ~ Fulfillment_as_Parent))
FullAsParent <- UseMe %>%
  dplyr::select(Life_Satisfaction, Fulfillment_as_Parent) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Fulfillment_as_Parent))
UseMe%>% count(Fulfillment_as_Parent)
FullAsParentTable<-FullAsParent %>%
  group_by(Fulfillment_as_Parent) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
FullAsParentTable$variable = NULL
FullAsParentTable <- FullAsParentTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(FullAsParentTable)<- c("Sense of Fulfillment as a Parent","N","Mean Satisfaction","Standard Deviation")

FullAsParentTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and Sense of Fulfillment as a Parent "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 1.81e-33")
plotmeans(UseMe$Life_Satisfaction ~ UseMe$Fulfillment_as_Parent, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Sense of Fulfillment as a Parent", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Sense of Fulfillment as a Parent"))
colnames(FullAsParentTable)<- c("Sense of Fulfillment as a Parent","N","Mean Satisfaction","Standard Deviation")
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Fulfillment_as_Parent)

FullAsParentaov <- FullAsParent %>% anova_test(Life_Satisfaction ~ Fulfillment_as_Parent)
FullAsParentaov



#----------------------------------------#
UseMe %>% count(Fulfillment_as_Partner)
summary(aov(data = UseMe, Life_Satisfaction ~ Fulfillment_as_Partner))

FullAsPartner <- UseMe %>%
  dplyr::select(Life_Satisfaction, Fulfillment_as_Partner) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Fulfillment_as_Partner))
UseMe%>% count(Fulfillment_as_Partner)
FullAsPartnerTable<-FullAsPartner %>%
  group_by(Fulfillment_as_Partner) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_sd")
FullAsPartnerTable$variable = NULL
FullAsPartnerTable <- FullAsPartnerTable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
colnames(FullAsPartnerTable)<- c("Sense of Fulfillment as a Partner","N","Mean Satisfaction","Standard Deviation")

FullAsPartnerTable %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and Sense of Fulfillment as a Partner "=4))%>%
  kable_styling()%>%
  add_footnote("ANOVA = 2.97e-117")
b <-plotmeans(UseMe$Life_Satisfaction ~ UseMe$Fulfillment_as_Partner, mean.labels = TRUE, digits = 2,col = "red",ccol = "black",xlab = "Sense of Fulfillment as a Partner", ylab = "Life Satisfaction", main = c("Life Satisfaction vs Sense of Fulfillment as a Partner"))
colnames(FullAsPartnerTable)<- c("Sense of Fulfillment as a Partner","N","Mean Satisfaction","Standard Deviation")
pairwise.t.test(UseMe$Life_Satisfaction, UseMe$Fulfillment_as_Partner)

FullAsPartneraov <- FullAsPartner %>% anova_test(Life_Satisfaction ~ Fulfillment_as_Partner)
FullAsPartneraov

######Multiple Regression#####
UseMe$MedOrSur <- as.numeric(UseMe$MedOrSur) #stray line
#PERFORMANCE METRICS
#SeeRela <- UseMe %>% dplyr::select(Life_Satisfaction,Edu_or_Career , Number_Children ,Oldest_Age_Children ,  Gender , Primary_Practice_Setting , AgeRange , Years_With_Partner , Participate_Counseling , Household_Annual_Salary , Household_Loan_Current , Highest_Education_Completed , AMAA_Member , MedOrSur , seven_Day_Sleep_Problem , seven_Days_Sleep_Refresh, seven_Day_Problem_Fall_Asleep , Community_Detached , Community_Isolated , Community_Left_Out , Community_Unknwon , Time_Alone_Partner , Quality_Time_Partner)
chart.Correlation(SeeRela, method = "spearm", histogram = TRUE, pch = 16)
?chart.Correlation

#ARE MY SACRIFICES WORTHWHILE? BC NO TIME WITH PARTHNER
#IF YES, HOW SUBSET
#SUBSET SACRIFICES WORTWHILE, WHY T.TEST/CHI-SQUARE
#SUBSET THE BIGGER DATASET INTO SACRIFICES MEASURES    -------DRIVERS OF SATISFACTION ACROSS YES/NO IS EVERYTHING VALUABLE-------
#ALWAYS & FREQ VS RARELY AND NEVER vs Sometimes

#_-------------------------#
#USE THIS ONE
z<-lm(data = UseMe,Life_Satisfaction~ Pride_Medical_Family + AgeRange + Gender + Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Time_Moderate_Exercise + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation + Stress_Factor_Lack_Time + Fullfilment_Individual )
summary(z)
VIF(z)
tidy(z)
glance(z)
plot(z)

xd<-glm(data = UseMe,Life_Satisfaction~ Pride_Medical_Family + AgeRange + Gender + Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Time_Moderate_Exercise + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation + Stress_Factor_Lack_Time + Fullfilment_Individual )
summary(xd)
tidy(xd)


#standardize_parameters(z)

class(UseMe$Life_Satisfaction)
#---------------------------required model (small)-----------------# #DO YOU FEEEL LIKE A PERSON (VALUED)
reqmodel<-lm(data = UseMe,Life_Satisfaction~Edu_or_Career  + AgeRange +  Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Time_Moderate_Exercise + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation )
summary(reqmodel)
x<-lm(data = UseMe,Life_Satisfaction~Edu_or_Career  + AgeRange +  Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Time_Moderate_Exercise + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation + Stress_Factor_Lack_Time + Fullfilment_Individual )
summary(x)
VIF(x) #colinearity// inflation
y<-lm(data = UseMe,Life_Satisfaction~  AgeRange +  Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Time_Moderate_Exercise + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation + Stress_Factor_Lack_Time + Fullfilment_Individual )
summary(y)
VIF(y)


f<-lm(data = UseMe,Life_Satisfaction~ Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation + Stress_Factor_Lack_Time + Fullfilment_Individual )
summary(f)


#without reporting fulfillment
nah<-lm(data = UseMe,Life_Satisfaction~ Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation )
summary(nah)
plot(nah)

#keep the stress factor
ye<-lm(data = UseMe,Life_Satisfaction~ Stress_Factor_Lack_Time  + Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation )
summary(ye)
plot(ye)
glance(ye)

#Boies
boies<- lm(data = UseMe, Life_Satisfaction ~ Edu_or_Career + AgeRange + Time_Alone_Partner + Quality_Time_Partner + Participate_Counseling + Pride_Medical_Family + seven_Days_Sleep_Refresh + seven_Day_Problem_Fall_Asleep + Household_Annual_Salary + Household_Loan_Current + AMAA_Member)
summary(boies)
VIF(boies)


#-----------------------GENERAL/OVERALL MODEL--------------#
genmodel<-lm(data = UseMe,Life_Satisfaction~Edu_or_Career + Region_Country + Number_Children +Oldest_Age_Children +  Gender + Primary_Practice_Setting + AgeRange + Years_With_Partner  + Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + Highest_Education_Completed + AMAA_Member + MedOrSur + seven_Day_Sleep_Problem + seven_Days_Sleep_Refresh + seven_Day_Problem_Fall_Asleep + Community_Detached + Community_Isolated + Community_Left_Out + Community_Unknwon + Time_Alone_Partner + Quality_Time_Partner + Time_Moderate_Exercise + Time_Vigorous_Exercise + Time_Strength_Training + Frequency_Engage_Hobbies + Frequency_Draw_Religion + Frequency_Worship_Services + Partner_Tired_After + Partner_Irritable_After + Partner_Busy_Work_After + Partner_Participant_Household_Activities + Partner_Work_No_Participation + Partner_Argue_Work_Time + Pride_Medical_Family + Partner_Show_Appreciation + Frequency_Medical_Conference_Together + Frequency_Alone_Travel + Frequency_Financial_Decision + Frequency_Finance_Conflict)
summary(genmodel)
plot(genmodel) #has standardized pearson residuals

VIF(genmodel)
#---------------------STRESS FACTOR MODEL------------------#
stressmodel<-lm(data = UseMe,Life_Satisfaction ~ Stress_Factor_Business + Stress_Factor_Lack_Time + Stress_Factor_Work_Overload + Stress_Factor_Work_Issues_Home)
summary(stressmodel)

VIF(stressmodel)

#------------------FULFILLMENT------------#
fulfillmodel<- lm(data = UseMe, Life_Satisfaction ~ Fullfilment_Individual +Fulfillment_as_Partner + Fulfillment_as_Parent)
summary(fulfillmodel)
UseMe %>% count(Fullfilment_Individual)
UseMe %>% count(Fulfillment_as_Partner)
VIF(fulfillmodel)

#------------------WORK RELATED MODEL------------# #-get the most significant and plug in#
workmodel<-lm(data = UseMe,Life_Satisfaction~Edu_or_Career + Primary_Practice_Setting  + Household_Annual_Salary + Household_Loan_Current + Highest_Education_Completed  + MedOrSur + Partner_Surgical_Specialty + Hours_Partner_Work + At_Call_Days_Per_Month+ Number_Hospitals_Hired_Cumulative) 
summary(workmodel)
UseMe %>% count(MedOrSur)
UseMe %>% count(Gender)
UseMe %>% count(Partner_Surgical_Specialty)
VIF(workmodel)
#----------SLEEP------#
slpmodel<-lm(data = UseMe, Life_Satisfaction~ seven_Day_Sleep_Problem + seven_Days_Sleep_Refresh+ seven_Day_Problem_Fall_Asleep)
plot(slpmodel)
summary(slpmodel)
VIF(slpmodel)
#-------------COMMUNITY---------#
comumodel<- lm(data = UseMe, Life_Satisfaction ~ Community_Detached + Community_Isolated + Community_Left_Out + Community_Unknwon)
plot(comumodel)
summary(comumodel)
VIF(comumodel)
#---------------TIME WITH PARTNER---------#
timemodel<- lm(data = UseMe, Life_Satisfaction ~ Time_Alone_Partner + Quality_Time_Partner)
summary(timemodel)
plot(timemodel)
VIF(timemodel)

#-------------------------EXERCISE----------------#
exermodel<-lm(data = UseMe, Life_Satisfaction ~ Time_Moderate_Exercise + Time_Vigorous_Exercise + Time_Strength_Training)
plot(exermodel) 
summary(exermodel)

#-----------------------HOBBIES AND SIMILAR-----------------------------#
summary(lm(data = UseMe,Life_Satisfaction ~ Frequency_Engage_Hobbies + Frequency_Relaxation_Techniques + Frequency_Draw_Religion + Frequency_Worship_Services)) # hobbies are best at predicting

#---------------------------PARTNER AFTER WORK------------------------#
summary(lm(data = UseMe, Life_Satisfaction ~ Partner_Tired_After + Partner_Irritable_After + Partner_Busy_Work_After))

#--------------------SOMETHING ABOUT PARTNER-PAGE 14--------------------------------#
summary(lm(data = UseMe, Life_Satisfaction ~ Partner_Participant_Household_Activities + Partner_Work_No_Participation + Partner_Argue_Work_Time + Partner_Show_Appreciation))

#--------------------------FINANCIAL MODEL---------------------------------------#
summary(lm(data = UseMe, Life_Satisfaction ~ Frequency_Financial_Decision + Frequency_Savings_Decision + Frequency_Finance_Conflict + Frequency_Investment_Decision))

#####MODEL FOR FELLOWS####

Fellows<- UseMe 
Fellows$Edu_or_Career[Fellows$Edu_or_Career == 2]<-"Resident Fellow"
Fellows<-Fellows %>% filter(Edu_or_Career == "Resident Fellow")

fel_model <-lm(data = Fellows,Life_Satisfaction~ Pride_Medical_Family + AgeRange + Gender + Participate_Counseling + Household_Annual_Salary + Household_Loan_Current + AMAA_Member + seven_Days_Sleep_Refresh + Community_Detached + Time_Alone_Partner + Quality_Time_Partner + Time_Moderate_Exercise + Frequency_Engage_Hobbies + Partner_Argue_Work_Time  + Frequency_Finance_Conflict + Partner_Show_Appreciation + Stress_Factor_Lack_Time + Fullfilment_Individual )
summary(fel_model)
VIF(fel_model)
tidy(fel_model)
glance(fel_model)
plot(fel_model)

#########PROMIS SCORE FOR LONELINESS AND DEPRESSION######
#using item response theory
#https://bookdown.org/bean_jerry/using_r_for_social_work_research/item-response-theory.html

#----------------------------Item response theory-------------------------------------------#
depre<- UseMe %>% dplyr::select(Community_Left_Out,Community_Unknwon,Community_Isolated,Community_Detached) %>% drop_na()
depre1 <- (mirt(depre, 1, verbose = FALSE, itemtype = 'graded', SE = TRUE))
M2(depre1, type = "C2", calcNULL = FALSE)
itemfit(depre1)
IRT_parms <- coef(depre1, IRTpars = TRUE, simplify = TRUE)
IRT_parms$items
summary(depre1)
#4 items explaining stuff
plot(depre1, type='trace', which.item = c(1,2,3,4), facet_items=T, 
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
     theta_lim = c(-3, 3), 
     main = "")
#per item 
plot(depre1, type='infotrace', which.item = c(1,2,3,4), facet_items=T, 
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     theta_lim = c(-3, 3), 
     main="")
#scale information and conditional standard errors
plot(depre1, type = 'infoSE', theta_lim = c(-3, 3), 
     main="")
#conditional reliability
plot(depre1, type = 'rxx', theta_lim = c(-3, 3), 
     main="" )
    #marginal reliability??
marginal_rxx(depre1)
#scale characteristic curve
plot(depre1, type = 'score', theta_lim = c(-3, 3), main = "")

#do one for each of the criteria
AMAA$Community_Left_Out<- factor(AMAA$Community_Left_Out,levels = c("Never","Rarely","Sometimes","Usually","Always"))

promis1 <- AMAA %>%
  dplyr::select(Life_Satisfaction, Community_Left_Out) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Left_Out))
AMAA%>% count(Community_Left_Out)
promis1Table<-promis1 %>%
  group_by(Community_Left_Out) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_se")
promis1Table$variable = NULL
promis1Table <- promis1Table %>%
  mutate(across(c(mean, se), ~ signif(round(., 3), digits = 4)))
colnames(promis1Table)<- c("Community Left Out","N","Mean Satisfaction","Standard Error")

promis1Table %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and left out in community "=4))%>%
  kable_styling()

#----#
AMAA$Community_Unknwon <- factor(AMAA$Community_Unknwon,levels = c("Never","Rarely","Sometimes","Usually","Always"))

promis2 <- AMAA %>%
  dplyr::select(Life_Satisfaction, Community_Unknwon) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Unknwon))
AMAA%>% count(Community_Unknwon)
promis2Table<-promis2 %>%
  group_by(Community_Unknwon) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_se")
promis2Table$variable = NULL
promis2Table <- promis2Table %>%
  mutate(across(c(mean, se), ~ signif(round(., 3), digits = 4)))
colnames(promis2Table)<- c("Community Feel Unknown","N","Mean Satisfaction","Standard Error")

promis2Table %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and feel unknown in the community "=4))%>%
  kable_styling()
#----------#
AMAA$Community_Isolated <- factor(AMAA$Community_Isolated,levels = c("Never","Rarely","Sometimes","Usually","Always"))
##########-#
promis3 <- AMAA %>%
  dplyr::select(Life_Satisfaction, Community_Isolated) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Isolated))
AMAA%>% count(Community_Isolated)
promis3Table<-promis3 %>%
  group_by(Community_Isolated) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_se")
promis3Table$variable = NULL
promis3Table <- promis3Table %>%
  mutate(across(c(mean, se), ~ signif(round(., 3), digits = 4)))
colnames(promis3Table)<- c("Community Feel Isolated","N","Mean Satisfaction","Standard Error")

promis3Table %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and feeling isolated from community "=4))%>%
  kable_styling()

#84.795 mid cut-off point from Never-Rarely felt isolated in community
#78.325 mid cut-off point from Rarely-Sometimes felt isolated in community
#71.045 mid cut-off point from Sometimes-Usually felt isolated in community
#60.805 mid cut-off point from Usually-Always felt isolated in community

#---------#
AMAA$Community_Detached <- factor(AMAA$Community_Detached,levels = c("Never","Rarely","Sometimes","Usually","Always"))
##########-#
promis4 <- AMAA %>%
  dplyr::select(Life_Satisfaction, Community_Detached) %>%
  filter(!is.na(Life_Satisfaction)) %>%
  filter(!is.na(Community_Detached))
AMAA%>% count(Community_Detached)
promis4Table<-promis4 %>%
  group_by(Community_Detached) %>%
  get_summary_stats(Life_Satisfaction, type = "mean_se")
promis4Table$variable = NULL
promis4Table <- promis4Table %>%
  mutate(across(c(mean, se), ~ signif(round(., 3), digits = 4)))
colnames(promis4Table)<- c("Community Feel Detached","N","Mean Satisfaction","Standard Error")

promis4Table %>%
  kbl() %>%
  add_header_above(c("Life Satisfaction and feeling detached from community "=4))%>%
  kable_styling()

################# FUTURE INQUIRIES#############
##Given that AMAA members seem to be more satisfied than non-member
##Evaluate for sense of community (left out and such, in a chi-square see if they are diff)
member_stuff<-UseMe   #Make object for AMAA stuff
member_stuff$AMAA_Member[member_stuff$AMAA_Member == 2]<- "Yes"
member_stuff<-member_stuff %>% select(AMAA_Member,Community_Left_Out) %>% filter(!is.na(Community_Left_Out)) %>% filter(!is.na(AMAA_Member)) %>% filter(AMAA_Member == "Yes")
member_stuff<-member_stuff %>% arrange(Community_Left_Out) %>% group_by(Community_Left_Out)
member_stuff
member_stuff<-member_stuff %>% group_by(Community_Left_Out,AMAA_Member) %>% summarise(count = n()) %>% mutate(percentage = count/sum(count)) 
member_stuff


non_member_stuff <- UseMe
non_member_stuff$AMAA_Member[non_member_stuff$AMAA_Member == 1]<- "NO"
non_member_stuff<-non_member_stuff %>% select(AMAA_Member,Community_Left_Out) %>% filter(!is.na(Community_Left_Out)) %>% filter(!is.na(AMAA_Member)) %>% filter(AMAA_Member == "NO")
non_member_stuff<-non_member_stuff %>% arrange(Community_Left_Out) %>% group_by(Community_Left_Out)
non_member_stuff<-non_member_stuff %>% group_by(Community_Left_Out) %>% summarise(count = n()) %>% mutate(percentage = count/sum(count)) 
non_member_stuff





#This is a matrix between membership of AMA compared to response to: "Do you feel left out of community")
Community_Compare<-matrix(nrow = 5, ncol = 2)

colnames(Community_Compare) <- c("AMAA Member","Non AMAA Member")
rownames(Community_Compare)<- c("Never","Rarely","Sometimes","Usually","Always")




#For Non-Members
Community_Compare[1,1] <- 19.4
Community_Compare[2,1] <- 30.4
Community_Compare[3,1] <- 37.2
Community_Compare[4,1] <- 10.3
Community_Compare[5,1] <- 2.77
Community_Compare[1,2] <- 6.29
Community_Compare[2,2] <- 21.3
Community_Compare[3,2] <- 45.4
Community_Compare[4,2] <- 21.9
Community_Compare[5,2] <- 5.05
Community_Compare %>% 
  kable() %>% 
  kable_styling() %>% 
  add_footnote("This is the percentage of people experiencing feeling left out of community")%>%
  add_header_above(c("Do you feel left out?","",""))%>%
  add_footnote("fisher-test result:p-value = 0.005856")

fisher.test(Community_Compare)

#########VISUALIZATION, NO STATISTICS, JUST SHOW THINGS FOR RESIDENCE FELLOWS######
#Need to turn data into factors again..... which seems to be the biggest stressor
UseMe %>% dplyr::select(Life_Satisfaction,AgeRange,Stress_Factor_Lack_Time)
ggplot(UseMe, aes(x = Stress_Factor_Lack_Time, y = Life_Satisfaction, color = factor(AgeRange))) +
  geom_point() +
  labs(title = "Scatter Plot of Life Satisfaction vs. Stress Factor (Lack of Time)",
       x = "Stress Factor (Lack of Time)",
       y = "Life Satisfaction")

ggplot(x, aes(x = factor(Stress_Factor_Lack_Time), fill = factor(AgeRange))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Bar Graph of Life Satisfaction vs. Stress Factor (Lack of Time) by Age Range",
       x = "Stress Factor (Lack of Time)",
       y = "Count") +
  scale_x_discrete(drop = FALSE) +  # Show all x-axis categories
  scale_fill_discrete(name = "Age Range")  # Customize legend title

ggplot(UseMe, aes(x = factor(Primary_Cause_Stress), fill = factor(AgeRange))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Bar Graph of Life Satisfaction vs. Stress Factor (Lack of Time) by Age Range",
       x = "Stress Factor (Lack of Time)",
       y = "Count") +
  scale_x_discrete(drop = FALSE) +  # Show all x-axis categories
  scale_fill_discrete(name = "Age Range")  # Customize legend title

#############################GRAPHS FOR MODELS#####################
#####GRAPH FOR GENERALIZED MODEL###############-#
# Extract coefficients from the linear regression model
coefficients <- coef(z)[-1]  # Exclude intercept

# Create a data frame with predictor names and absolute coefficients
plot_data <- data.frame(predictor = names(coefficients), coefficient = abs(coefficients))

# Create a horizontal bar plot using ggplot2
b<-ggplot(plot_data, aes(x = coefficient, y = fct_reorder(predictor, coefficient))) +
  geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
  labs(title = "General Predictor Importance", x = "Absolute Coefficient Value", y = "Predictor") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(hjust = 1, angle = 0),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )
b
#####GRAPH FOR RESIDENT FELLOW MODEL###############-#
coef_res <- coef(fel_model)[-1]  # Exclude intercept

# Create a data frame with predictor names and absolute coefficients
plot_data_res <- data.frame(predictor = names(coef_res), coefficient = abs(coef_res))

# Create a horizontal bar plot using ggplot2
a<-ggplot(plot_data_res, aes(x = coefficient, y = fct_reorder(predictor, coefficient))) +
  geom_bar(stat = "identity", fill = "maroon", alpha = 0.8) +
  labs(title = "Resident Predictor Importance", x = "Absolute Coefficient Value", y = "Predictor") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(hjust = 1, angle = 0),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )
c<-a+b 
c 
