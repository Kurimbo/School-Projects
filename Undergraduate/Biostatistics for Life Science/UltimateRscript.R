# Name: Elias Ciudad 04/17/2023
# Instructor:
# This script contains the analysis of Mayo Clinic Primary Biliary Cirrhosis Data
# This was the final project of my "Biostatistics for Life Sciences" class

# Packages used for Project
install.packages("tidyverse")
library(tidyverse)
install.packages("sjlabelled")
library("sjlabelled")
install.packages("report")
library(report)
install.packages("styler")
library(styler)
# Obtained sample (randomized) data set
Elaias <- sample_n(Mayo_Hepato, 148)
Elaias
attach(Elaias)
View(Elaias)

############  TABLE 1 & 2  ##################
# Add Column with Alive(0:1) or Dead (2) variable
# Not to be used for numerical tests
Deceased_NonDeceased <- Elaias %>%
  group_by(status) %>%
  count() %>%
  mutate(
    Diagnosis = ifelse(status <= 1, "Alive", "Dead"), # if status <=1, label it as "Alive" in new column. Otherwise, label as "Dead"
    status = ifelse(status <= 1, "0 & 1", as.character(status)) # if status <=1, label as "0&1" in the status colum. #homogenizes
  ) %>%
  group_by(status, Diagnosis) %>%
  summarise(count = sum(n))
Deceased_NonDeceased # Total number of Deceased people is 60 , alive is 88
d<- numeric()
d
# d will used for numerical tests (chi-square)
d <- numeric() #empty numeric vector hold/store variables
d[status <= 1] <- 0 #If 'status is <=1, assign it the num value of 0 for every true condition
d[status >= 2] <- 1 #If 'status is >=2, assign it the num value of 1 for every true condition
DeadOrNot_Elaias["deceased"] <- d #Creates a column that passes in the parameters in D
DeadOrNot_Elaias <- mutate(DeadOrNot_Elaias, Patient_Is = ifelse(status < 2, "Alive (0 and 1)", "Dead(2)")) #Where status <2, assign arg1 if T, arg 2 if F
detach(Elaias)
attach(DeadOrNot_Elaias)

# Age
summary(age)
hist(age, xlab = "Age", ylab = "Frequency", main = "Age Histogram", col = "red", border = "black")
boxplot(age, main = "Boxplot of Age", col = "maroon", border = "black")
shapiro.test(age) # p-value >0.05, data is normally distributed

mean(DeadOrNot_Elaias$age)
sd(DeadOrNot_Elaias$age)

Mean_Age_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(age)) %>% #removes NA values
  group_by(Patient_Is) %>% #groups by status
  summarise(mean_age = mean(age, na.rm = TRUE)) #table-like count summarised by mean age
Mean_Age_By_Status # 48.4 alive and Dead 54.3

t.test(DeadOrNot_Elaias$age ~ DeadOrNot_Elaias$Patient_Is)

# Sex
Sex_Per_Status <- DeadOrNot_Elaias %>%
  group_by(sex, Patient_Is) %>%
  count()
Sex_Per_Status # We are choosing females so we can get a bigger sample size

SexForChisqTest <- matrix(c(85, 50), nrow = 2)
chisq.test(SexForChisqTest)

# Albumin Levels

Clean_Albumin <- DeadOrNot_Elaias %>% filter(!is.na(albumin))
summary(Clean_Albumin$albumin)
hist(Clean_Albumin$albumin)
boxplot(Clean_Albumin$albumin)
hist(Clean_Albumin$albumin, xlab = "Albumin levels (g/dl)", ylab = "frequency", main = "Histagram of Albumin levels(g/dl)", col = "blue", border = "black")
boxplot(Clean_Albumin$albumin, main = "Boxplot of Albumin levels (g/dl)", col = "green", border = "black")
shapiro.test(Clean_Albumin$albumin)
sd(DeadOrNot_Elaias$albumin)

Mean_Albumin_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(albumin)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_bilirubin = mean(albumin, na.rm = TRUE))
Mean_Albumin_By_Status
t.test(DeadOrNot_Elaias$albumin ~ DeadOrNot_Elaias$Patient_Is)

# Alkaline Phosphatase Levels

summary(alk.phos, na.rm = TRUE)
hist(DeadOrNot_Elaias$alk.phos)
boxplot(DeadOrNot_Elaias$alk.phos)
hist(DeadOrNot_Elaias$alk.phos, xlab = "Alkaline phosphotase levels (units/liter)", ylab = "Frequency", main = "Alkaline phosphotase levels units/liter)", col = "red", border = "black")
boxplot(DeadOrNot_Elaias$alk.phos, main = "Alkaline phosphotase levels (units/liter)", col = "red", border = "black")
shapiro.test(DeadOrNot_Elaias$alk.phos)
wilcox.test(alk.phos)
summary(DeadOrNot_Elaias$alk.phos)
IQR(DeadOrNot_Elaias$alk.phos, na.rm = TRUE)

Mean_Alk.Phos_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(alk.phos)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_AlkPhos = mean(alk.phos, na.rm = TRUE))
Mean_Alk.Phos_By_Status

# Aspartate Aminotransferase Levels

summary(ast, na.rm = TRUE)
hist(DeadOrNot_Elaias$ast, xlab = " Aspartate Aminotransferase", ylab = "frequency", main = "Aspartate aminotransferase", col = "yellow", border = "black")
boxplot(DeadOrNot_Elaias$ast, main = "Aspartate Aminotransferase", col = "green", border = "blue")
shapiro.test(ast)
wilcox.test(ast)
IQR(ast, na.rm = TRUE)

Mean_Asp.Amino_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(ast)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_AspAmino = mean(ast, na.rm = TRUE))
Mean_Asp.Amino_By_Status

# Serum Bilirubin Levels

summary(bili, na.rm = TRUE)
hist(DeadOrNot_Elaias$ast, xlab = " Serum Bilirubin Levels", ylab = "frequency", main = "Histagram of Serum Bilirubin Levels", col = "yellow", border = "red")
boxplot(DeadOrNot_Elaias$ast, main = "Boxplot of Serum Bilirubin Levels ", col = "yellow", border = "red")
shapiro.test(bili)
wilcox.test(bili)
IQR(DeadOrNot_Elaias$bili, na.rm = TRUE)

Mean_SerumBilirubin_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(bili)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_SerumBilirubin = mean(bili, na.rm = TRUE))
Mean_SerumBilirubin_By_Status


# Serum Cholesterol Levels

summary(chol)
hist(DeadOrNot_Elaias$chol, xlab = "Serum cholesterol levels (mg/dl)", ylab = "Frequency", main = "feetHistagram of Serum Cholesterol (mg/dl) Levels", col = "purple", border = "green")
boxplot(DeadOrNot_Elaias$chol, main = "Boxplot of  serum cholesterol levels (mg/dl) ", col = black, border = "green")
shapiro.test(DeadOrNot_Elaias$chol)
wilcox.test(chol)
IQR(DeadOrNot_Elaias$chol, na.rm = TRUE)

Mean_Cholesterol_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(chol)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_Cholesterol = mean(chol, na.rm = TRUE))
Mean_Cholesterol_By_Status


# Triglycerides Levels

summary(trig)
hist(DeadOrNot_Elaias$trig, xlab = "Triglycerides (mg/dl) levels", ylab = "frequency", main = "Histagram of triglycerides (mg/dl) Levels", col = "orange", border = "black")
boxplot(DeadOrNot_Elaias$trig, main = "Ttriglycerides (mg/dl) levels ", col = "orange", border = "black")
shapiro.test(DeadOrNot_Elaias$trig)
wilcox.test(trig)
IQR(DeadOrNot_Elaias$trig, na.rm = TRUE)

Mean_Triglyceride_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(trig)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_Trig = mean(trig, na.rm = TRUE))
Mean_Triglyceride_By_Status


# Urine Copper Levels

summary(copper)
hist(DeadOrNot_Elaias$copper, xlab = "urine copper (ug/day)levels", ylab = "frequency", main = "Histagram of urine copper (ug/day) Levels", col = "gray", border = "blue")
boxplot(DeadOrNot_Elaias$copper, main = "urine copper (ug/day)levels ", col = "gray", border = "blue")
shapiro.test(DeadOrNot_Elaias$copper)
wilcox.test(copper)
IQR(DeadOrNot_Elaias$copper, na.rm = TRUE)

Mean_UrineCopper_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(copper)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_Copper = mean(copper, na.rm = TRUE))
Mean_UrineCopper_By_Status

# Platelet Count

summary(platelet, na.rm = TRUE)
hist(platelet, xlab = "Platelet Count", ylab = "frequency", main = "Histagram of platelet Count", col = "turquoise", border = "gray")
boxplot(DeadOrNot_Elaias$platelet, main = " Boxplot of Platelet Count", col = "lavender", border = "black")
shapiro.test(DeadOrNot_Elaias$platelet)
wilcox.test(platelet)
IQR(DeadOrNot_Elaias$platelet, na.rm = TRUE)

Mean_Plat_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(platelet)) %>%
  group_by(Patient_Is) %>%
  summarise(mean_Plat = mean(platelet, na.rm = TRUE))
Mean_Plat_By_Status

# Edema

DeadOrNot_Elaias %>%
  filter(!is.na(edema)) %>%
  group_by(edema, Patient_Is) %>%
  count() %>%
  summarise(count = (n))

# Deceased Percents

46 / 60
6 / 60
8 / 60

# NonDeceased Percents

81 / 88
7 / 88

Edema_Counts <- matrix(c(81, 46, 7, 6, 0, 8), nrow = 2)
Edema_Counts
chisq.test(Edema_Counts)$expected
fisher.test(Edema_Counts)
summary(Edema_Counts)
IQR(edema)
# Presence of Hepatomegaly

Clean_Hepato <- DeadOrNot_Elaias %>% filter(!is.na(hepato))
Clean_Hepato
shapiro.test(Clean_Hepato$hepato)
wilcox.test(Clean_Hepato$hepato)
summary(Clean_Hepato$hepato)
IQR(Clean_Hepato$hepato)
Mean_Hepato_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(hepato)) %>% #removes all NA-value-containing rows from hepato variables
  group_by(Patient_Is, hepato) %>% () #group in a basis of patient status and state/presence of hepatomegaly
  summarise(count = n()) %>%   #count all rows (grouped)
  mutate(percent = count / sum(count) * 100)
Mean_Hepato_By_Status
summary(Clean_Hepato$hepato) #give descriptive statistics of hepatomegaly variable
# ASCITES
Clean_Ascites <- DeadOrNot_Elaias %>%
  filter(!is.na(ascites)) %>% #remove all NA-Value-having rows
  filter(ascites == "1") #retain rows where ascites == 1 
Clean_Ascites
summary(ascites)
wilcox.test(Clean_Ascites$ascites)
Mean_Ascites_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(ascites)) %>%
  group_by(Patient_Is, ascites) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
Mean_Ascites_By_Status


# BLOOD VESSEL MALFORMATION
# Presence of Blood Vessel Malformations in the Skin, if Alive and % per category

Clean_Spiders <- DeadOrNot_Elaias %>%
  filter(!is.na(spiders)) %>%
  filter(spiders == "1")
Clean_Spiders
summary(DeadOrNot_Elaias$spiders)
Summary_CSpiders <- Clean_Spiders %>%
  group_by(Patient_Is, spiders) %>%
  summarize(count = n())
Summary_CSpiders
chisq.test(Stats_Spiders)

# HISTOLOGIC STAGE OF DISEASE
summary(DeadOrNot_Elaias$hepato)
Histology_By_Status_Real <- DeadOrNot_Elaias %>%
  filter(!is.na(stage)) %>%
  group_by(Patient_Is, stage) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
summary(Histology_By_Status_Real)
Histology_By_Status_Real

x <- c(5, 29, 37, 17)
y <- c(0, 10, 16, 33)
wilcox.test(x, y)

# Histologic Stage of Disease

summary(stage)
Mean_Histology_By_Status <- DeadOrNot_Elaias %>%
  filter(!is.na(stage)) %>%
  group_by(Patient_Is, stage) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
Mean_Histology_By_Status
HistologyCount <- matrix(c(5, 0, 29, 10, 37, 16, 17, 33), nrow = 2)
HistologyCount
chisq.test(HistologyCount)$expected
fisher.test(HistologyCount)

######### Part 2- comparing with mean triglyceride scores ANOVA #####

# Variable for statistical tests
T <- numeric()  #empty vector hold numeric conditions. has passed down dimensions of data set
T[DeadOrNot_Elaias$trig <= 150] <- 0  #where trig <=150, label as 0 in the numeric vector
T[DeadOrNot_Elaias$trig >= 150 & DeadOrNot_Elaias$trig <= 199] <- 1 #where trig levels >=150 and <=199, label as 1 in the numeric vector
T[DeadOrNot_Elaias$trig >= 200 & DeadOrNot_Elaias$trig <= 499] <- 2 #where trig levels >=200 and <=499, label as 2 in the numeric vector
T[DeadOrNot_Elaias$trig >= 500] <- 3 #where trig levels >= 500, label as 3 in the numeric vector
DeadOrNot_Elaias["triglyceride levels"] <- T  #create a column passing down the values to each pertaining row
# Variable to look good in table
DeadOrNot_Elaias$triglyceride_categories <- cut(DeadOrNot_Elaias$trig,
  breaks = c(-Inf, 149, 199, 499, Inf),   #intervals for labeling--- based off trigliceride levels
  labels = c("Normal", "Mildly High", "High", "Very High") # to label if value between intervals i, i+1. progresses
)


# triglyceride by bilirubin
shapiro.test(DeadOrNot_Elaias$"triglyceride levels")
shapiro.test(DeadOrNot_Elaias$bili)
bartlett.test(list(DeadOrNot_Elaias$"triglyceride levels", DeadOrNot_Elaias$bili))
bil_over_trig <- aov(DeadOrNot_Elaias$bili ~ DeadOrNot_Elaias$"triglyceride levels")
summary(bil_over_trig)
report(bil_over_trig)
bartlett.test(list(DeadOrNot_Elaias$"triglyceride levels", DeadOrNot_Elaias$bili))
GoodTrigXbili <- na.omit(DeadOrNot_Elaias[c("triglyceride levels", "bili")])
GoodTrigXbili
pairwise.t.test(GoodTrigXbili$"triglyceride levels", GoodTrigXbili$bili, p.adjust.method = "bonferroni")
# triglyceride by albumin
bartlett.test(list(DeadOrNot_Elaias$"triglyceride levels", DeadOrNot_Elaias$albumin))
AlbXTrig <- aov(DeadOrNot_Elaias$albumin ~ DeadOrNot_Elaias$"triglyceride levels")
summary(AlbXTrig)
report(AlbXTrig)
pairwise.t.test(DeadOrNot_Elaias$"triglyceride levels", DeadOrNot_Elaias$albumin, p.adjust.method = "bonferroni")

# triglyceride by alkaline phosphate
bartlett.test(list(DeadOrNot_Elaias$"triglyceride levels", DeadOrNot_Elaias$alk.phos))
AkPhXTrig <- aov(DeadOrNot_Elaias$alk.phos ~ DeadOrNot_Elaias$"triglyceride levels")
AkPhXTrig
summary(AkPhXTrig)
report(AkPhXTrig)
chisq.test(DeadOrNot_Elaias$sex, DeadOrNot_Elaias$deceased)$expected
chisq.test(DeadOrNot_Elaias$deceased, DeadOrNot_Elaias$sex)

########## TABLE 3######
# Diff b/t trt and variables

by(status, trt, shapiro.test)
wilcox.test(status ~ trt)

by(time, trt, shapiro.test)
t.test(time ~ trt)

by(age, trt, shapiro.test)
t.test(age ~ trt)

by(status, trt, shapiro.test)
wilcox.test(status ~ trt)

by(albumin, trt, shapiro.test)
wilcox.test(albumin ~ trt)

by(alk.phos, trt, shapiro.test)
wilcox.test(alk.phos ~ trt)

by(ast, trt, shapiro.test)
wilcox.test(ast ~ trt)

by(bili, trt, shapiro.test)
wilcox.test(bili ~ trt)

by(chol, trt, shapiro.test)
wilcox.test(chol ~ trt)

by(trig, trt, shapiro.test)
wilcox.test(trig ~ trt)

by(copper, trt, shapiro.test)
wilcox.test(copper ~ trt)

by(platelet, trt, shapiro.test)
wilcox.test(platelet ~ trt)

chisq.test(stage, Elaias$trt)$expected
fisher.test(stage, Elaias$trt)

chisq.test(edema, Elaias$trt)$expected
fisher.test(edema, Elaias$trt)

wilcox.test(ascites ~ trt, data = DeadOrNot_Elaias)

by(hepato, trt, shapiro.test)
wilcox.test(hepato, trt)

by(spiders, trt, shapiro.test)
wilcox.test(spiders, trt)
