# Author: Elias Ciudad
# Research advisor/PI: Dr. Benjamin Enslow
#
# This is the script of the analysis (IRT,CFA,descriptive stats) of pre-survey data of the U-12
# study for tolerance/intolerance to uncertainty
#
#-#Asses baseline uncertainty tolerance have difference other salce
# Cluster based off demo data

##### INSTALL/LOAD THESE PACKAGES####
library(report)
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
##### LOADING DATA#####
# turning the trinity ID to character
demo_data <- read_excel("D:\\Academical Things\\Programming\\R\\Workspaces\\SOTL Tolerance to Uncertainty\\Pre_Survey_Data_Only.xlsx", sheet = 1)
demo_data$Trinity_ID <- as.character(demo_data$Trinity_ID)
#
sci_attitude_score <- read_excel("D:\\Academical Things\\Programming\\R\\Workspaces\\SOTL Tolerance to Uncertainty\\Pre_Survey_Data_Only.xlsx", sheet = 2)
sci_attitude_score$Trinity_ID <- as.character(sci_attitude_score$Trinity_ID)
#
IUS_12_scores <- read_excel("D:\\Academical Things\\Programming\\R\\Workspaces\\SOTL Tolerance to Uncertainty\\Pre_Survey_Data_Only.xlsx", sheet = 3)
IUS_12_scores$Trinity_ID <- as.character(IUS_12_scores$Trinity_ID)
#
Tolerate_Ambiguity <- read_excel("D:\\Academical Things\\Programming\\R\\Workspaces\\SOTL Tolerance to Uncertainty\\Pre_Survey_Data_Only.xlsx", sheet = 4)
Tolerate_Ambiguity$Trinity_ID <- as.character(Tolerate_Ambiguity$Trinity_ID)
#
LCAS_Survey <- read_excel("D:\\Academical Things\\Programming\\R\\Workspaces\\SOTL Tolerance to Uncertainty\\Pre_Survey_Data_Only.xlsx", sheet = 5)
LCAS_Survey$Trinity_ID <- as.character(LCAS_Survey$Trinity_ID)
#
Project_Own_Survey <- read_excel("D:\\Academical Things\\Programming\\R\\Workspaces\\SOTL Tolerance to Uncertainty\\Pre_Survey_Data_Only.xlsx", sheet = 6)
Project_Own_Survey$Trinity_ID <- as.character(Project_Own_Survey$Trinity_ID)
#-
UseMe <- list(demo_data, sci_attitude_score, IUS_12_scores, Tolerate_Ambiguity, LCAS_Survey, Project_Own_Survey)
UseMe <- reduce(UseMe, full_join, by = "Trinity_ID")
############################### CLEANING/TRANSFORMING DATA##############################
# hist(subset(x, variable = logic))
# Ordering them by time duration
UseMe$Prior_Research_exp_months[UseMe$Prior_Research_exp_months == "I have never spent time doing scientific research before"] <- "Never"

UseMe$Prior_Research_exp_months <- factor(UseMe$Prior_Research_exp_months, levels = c("Never", "Less than 1 month", "1-3 months", "4-6 months", "7-12 months", "More than 12 months"))
table(UseMe$Prior_Research_exp_months)

################################ ANALYSIS OF INTOLERANCE#############################
IUS_12_scores$IUS_total_score # baseline scores for IUS
attach(UseMe)

# by univ_level
table(UseMe$University_Level)
summary(aov(UseMe$IUS_total_score ~ UseMe$University_Level))
hist(IUS_total_score ~ University_Level)
# by gender
table(UseMe$Gender)
summary(aov(UseMe$IUS_total_score ~ UseMe$Gender))
hist(IUS_total_score ~ Gender, ncol = 3)
report(aov(UseMe$IUS_total_score ~ UseMe$Gender)) #report over summary? for AOV?
# by sex orientation
table(UseMe$Sexual_Orientation)
summary(aov(UseMe$IUS_total_score ~ UseMe$Sexual_Orientation))
hist(IUS_total_score ~ Sexual_Orientation)

# by College Major---Recategorized
table(UseMe$Major_recategorized)
summary(aov(UseMe$IUS_total_score ~ UseMe$Major_recategorized))
hist(IUS_total_score ~ Major_recategorized)

# by Research Discipline---Recategorized
table(UseMe$Research_Discipline_recategorized)
summary(aov(UseMe$IUS_total_score ~ UseMe$Research_Discipline_recategorized))
hist(IUS_total_score ~ Research_Discipline_recategorized)

# by Race/Ethnicity -given
table(UseMe$Race_Ethnicity_as_given)
summary(aov(UseMe$IUS_total_score ~ UseMe$Race_Ethnicity_as_given))
hist(IUS_total_score ~ Race_Ethnicity_as_given)

# by Race/Ethnicity -recategorized
table(UseMe$Race_Ethnicity_recategorized)
t.test(UseMe$IUS_total_score ~ UseMe$Race_Ethnicity_recategorized)
hist(IUS_total_score ~ Race_Ethnicity_recategorized)

# by Prior Research Experience
table(UseMe$Prior_Research_exp_months)
summary(aov(UseMe$IUS_total_score ~ UseMe$Prior_Research_exp_months))
hist(IUS_total_score ~ Prior_Research_exp_months)

# by Humanities Courses Taken
table(UseMe$Humanities_Courses_Reported)
summary(aov(UseMe$IUS_total_score ~ UseMe$Humanities_Courses_Reported)) # kinda sig
hist(IUS_total_score ~ Humanities_Courses_Reported)
# who takes more humanities courses? -expand on this graph, # white apparently

ggplot(data = UseMe) +
  geom_boxplot(mapping = aes(x = Humanities_Courses_Reported, y = IUS_total_score, color = Race_Ethnicity_recategorized)) +
  theme_bw()

table(Humanities_Courses_Reported)
table(Race_Ethnicity_recategorized)
humanities_per_race <- UseMe %>%
  dplyr::select(Humanities_Courses_Reported, Race_Ethnicity_recategorized) %>%
  group_by(Race_Ethnicity_recategorized, Humanities_Courses_Reported) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))
humanities_per_race

hpr
hpr <- matrix(ncol = 2, nrow = 3)
colnames(hpr) <- c("White", "Non-White")
rownames(hpr) <- c("1 to 2 HC", "3-4 HC", "5+ HC")
hpr[1, 1] <- 25.9
hpr[1, 2] <- 18.5
hpr[2, 1] <- 22.2
hpr[2, 2] <- 29.6
hpr[3, 1] <- 51.9
hpr[3, 2] <- 51.9
hpr
chisq.test(hpr) # p-val of 0.3181 [need bigger study]
chisq_test(hpr)


power.chisq.test(n = 54, sig.level = 0.05, df = NULL, power = NULL, w = NULL)

######################################################################### -
############################ ANALYSIS OF TOLERANCE#########################
######################################################################### =

UseMe$Tolerance_Total_Score

############################################### by univ_level
table(UseMe$University_Level)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$University_Level))

############################################### by gender
table(UseMe$Gender)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$Gender)) # kinda sig

############################################### by sex orientation
table(UseMe$Sexual_Orientation)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$Sexual_Orientation))

############################################### by College Major---Recategorized
table(UseMe$Major_recategorized)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$Major_recategorized))

############################################### by Research Discipline---Recategorized
table(UseMe$Research_Discipline_recategorized)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$Research_Discipline_recategorized)) # kinda sig

############################################### by Race/Ethnicity -given
table(UseMe$Race_Ethnicity_as_given)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$Race_Ethnicity_as_given))


############################################### by Race/Ethnicity -recategorized
table(UseMe$Race_Ethnicity_recategorized)
t.test(UseMe$Tolerance_Total_Score ~ UseMe$Race_Ethnicity_recategorized)
summary(UseMe$Tolerance_Total_Score)


############################################### by Prior Research Experience
table(UseMe$Prior_Research_exp_months)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$Prior_Research_exp_months)) # significance
# sig---keep going

plotmeans(UseMe$Tolerance_Total_Score ~ UseMe$Prior_Research_exp_months, mean.labels = TRUE, digits = 2, col = "red", ccol = "black", xlab = "Time Researching", ylab = "Tolerance Score", main = c("Tolerance Score vs Time Researching"), maxbar = 50, minbar = 30)

#---------------------------GRAPHS--------------------------------#
PriorRsrch <- UseMe %>%
  dplyr::select(Tolerance_Total_Score, Prior_Research_exp_months) %>%
  filter(!is.na(Tolerance_Total_Score)) %>%
  filter(!is.na(Prior_Research_exp_months))

PriorRsrchtable <- PriorRsrch %>%
  group_by(Prior_Research_exp_months) %>%
  get_summary_stats(Tolerance_Total_Score, type = "mean_sd")
PriorRsrchtable$variable <- NULL
PriorRsrchtable <- PriorRsrchtable %>%
  mutate(across(c(mean, sd), ~ signif(round(., 3), digits = 4)))
PriorRsrchtable %>%
  kbl() %>%
  add_header_above(c("Tolerance to Uncertainty vs Prior Research Exp." = 4)) %>%
  kable_styling() %>%
  add_footnote("ANOVA test:p-val = 0.0194")



ggqqplot(PriorRsrch, "Tolerance_Total_Score", facet.by = "Prior_Research_exp_months")
PriorRsrchaov <- PriorRsrch %>% anova_test(Tolerance_Total_Score ~ Prior_Research_exp_months)
PriorRsrchaov
PriorRsrchTuk <- PriorRsrch %>% tukey_hsd(Tolerance_Total_Score ~ Prior_Research_exp_months)
PriorRsrchTuk <- PriorRsrchTuk %>% add_xy_position(x = "Prior_Research_exp_months")
PriorRsrchTuk

ggboxplot(PriorRsrch, x = "Prior_Research_exp_months", y = "Tolerance_Total_Score") +
  stat_pvalue_manual(PriorRsrchTuk, hide.ns = TRUE, step.increase = 0.04) +
  labs(
    title = "Tolerance Total Score vs Time Research Experience",
    subtitle = get_test_label(PriorRsrchaov, detailed = TRUE),
    caption = get_pwc_label(PriorRsrchTuk)
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.title = element_text(size = 14), # Increase axis label size
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  ) +
  xlab(label = "Time Prior Research Experience") +
  ylab(label = "Tolerance Total Score")



######################## by Humanities Courses Taken
table(UseMe$Humanities_Courses_Reported)
summary(aov(UseMe$Tolerance_Total_Score ~ UseMe$Humanities_Courses_Reported)) # not sig



###################################### ANALYSIS OF SELF EFFICACY###############################################
self_efficacy <- UseMe %>%
  select(confident_techscy_skill, confident_rsrch_QA, confident_collect_data, confident_create_explanation, confident_dev_theory, confident_lit_research) %>%
  drop_na()
dim(self_efficacy)
self_efficacy1 <- (mirt(self_efficacy, 1, verbose = FALSE, itemtype = "graded", SE = TRUE))
M2(self_efficacy1, type = "C2", calcNULL = FALSE) # RMSEA 0.101, not reasonable fit, p = ns (0.1)
itemfit(self_efficacy1)
self_efficacy_parms <- coef(self_efficacy1, self_efficacypars = TRUE, simplify = TRUE)
self_efficacy_parms$items
summary(self_efficacy1)
# 4 items explaining stuff
plot(self_efficacy1,
  type = "trace", which.item = c(1, 2, 3, 4, 5, 6), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 4, space = "top", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
)
# per item
plot(self_efficacy1,
  type = "infotrace", which.item = c(1, 2, 3, 4, 5), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 1, space = "right", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
) # thinking of as scientist provides the most amount of info
# scale information and conditional standard errors
plot(self_efficacy1,
  type = "infoSE", theta_lim = c(-3, 3),
  main = ""
)
# conditional reliability
plot(self_efficacy1,
  type = "rxx", theta_lim = c(-3, 3),
  main = ""
)
# marginal reliability??
marginal_rxx(self_efficacy1)
# scale characteristic curve
plot(self_efficacy1, type = "score", theta_lim = c(-3, 3), main = "")

??verbose
###################################### ANALYSIS OF SCIENCE IDENTITY############################################
science_ident <- UseMe %>%
  select(agree_belong_scifam, agree_satisfct_rsrch, agree_think_me_scntist, agree_belong_sci, agree_daywrk_appeal) %>%
  drop_na()
science_ident1 <- (mirt(science_ident, 1, verbose = FALSE, itemtype = "graded", SE = TRUE))
M2(science_ident1, type = "C2", calcNULL = FALSE) # RMSEA too high 0.188, p = significant 0.0155
itemfit(science_ident1)
science_ident_parms <- coef(science_ident1, science_identpars = TRUE, simplify = TRUE)
science_ident_parms$items
summary(science_ident1)
# 4 items explaining stuff
plot(science_ident1,
  type = "trace", which.item = c(1, 2, 3, 4, 5), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 4, space = "top", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
)
# per item
plot(science_ident1,
  type = "infotrace", which.item = c(1, 2, 3, 4, 5), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 1, space = "right", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
) # thinking of as scientist provides the most amount of info
# scale information and conditional standard errors
plot(science_ident1,
  type = "infoSE", theta_lim = c(-3, 3),
  main = ""
)
# conditional reliability
plot(science_ident1,
  type = "rxx", theta_lim = c(-3, 3),
  main = ""
)
# marginal reliability??
marginal_rxx(science_ident1)
# scale characteristic curve
plot(science_ident1, type = "score", theta_lim = c(-3, 3), main = "")



############################### ANALYSIS INHIBITORY ANXIETY################################
inhibitory <- UseMe %>%
  select(inhi_uncertainty_Keep_Live_Full, inhi_uncertainty_paralyze, inhi_uncertainty_canot_function, inhi_smallest_doubt_stops, inhi_escape_uncerainty_must) %>%
  drop_na()
dim(inhibitory)
inhibitory1 <- (mirt(inhibitory, 1, verbose = FALSE, itemtype = "graded", SE = TRUE))
M2(inhibitory1, type = "C2", calcNULL = FALSE) # RMSEA of 0, p-val is 0.912 tho
itemfit(inhibitory1)
inhibitory_parms <- coef(inhibitory1, inhibitorypars = TRUE, simplify = TRUE)
inhibitory_parms$items
summary(inhibitory1)
# 4 items explaining stuff
plot(inhibitory1,
  type = "trace", which.item = c(1, 2, 3, 4, 5), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 4, space = "top", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
) # cannot function under uncertainty gives the most information
# per item
plot(inhibitory1,
  type = "infotrace", which.item = c(1, 2, 3, 4, 5), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 1, space = "right", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
) # unforeseen events causing upset gives the most information
# scale information and conditional standard errors
plot(inhibitory1,
  type = "infoSE", theta_lim = c(-3, 3),
  main = ""
)
# conditional reliability
plot(inhibitory1,
  type = "rxx", theta_lim = c(-3, 3),
  main = ""
)
# marginal reliability??
marginal_rxx(inhibitory1)
# scale characteristic curve
plot(inhibitory1, type = "score", theta_lim = c(-3, 3), main = "")





############################### ANALYSIS PROSPECTIVE ANXIETY################################
prospective <- UseMe %>%
  select(pro_Unforeseen_evnt_upset, pro_frustation_no_info, pro_avoid_surprises_glance, pro_small_unk_spoil, pro_always_know_future, pro_hate_surprise, pro_organize_all_advamce) %>%
  drop_na()
dim(prospective)
prospective1 <- (mirt(prospective, 1, verbose = FALSE, itemtype = "graded", SE = TRUE))
M2(prospective1, type = "C2", calcNULL = FALSE) # RMSEA too high 1.3 ns
itemfit(prospective1)
prospective_parms <- coef(prospective1, prospectivepars = TRUE, simplify = TRUE)
prospective_parms$items
summary(prospective1)
# 4 items explaining stuff
plot(prospective1,
  type = "trace", which.item = c(1, 2, 3, 4, 5, 6, 7), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 4, space = "top", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
)
# per item
plot(prospective1,
  type = "infotrace", which.item = c(1, 2, 3, 4, 5), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 1, space = "right", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
) # unforeseen events causing upset gives the most information
# scale information and conditional standard errors
plot(prospective1,
  type = "infoSE", theta_lim = c(-3, 3),
  main = ""
)
# conditional reliability
plot(prospective1,
  type = "rxx", theta_lim = c(-3, 3),
  main = ""
)
# marginal reliability??
marginal_rxx(prospective1)
# scale characteristic curve
plot(prospective1, type = "score", theta_lim = c(-3, 3), main = "")



############################# GENERAL TOLERANCE CFA#######################

UseMe$visit_foreign_over_home <- as.numeric(UseMe$visit_foreign_over_home)
TotalTolerance <- UseMe %>%
  dplyr::select(avoid_ppl_diff_values, enjoy_ppl_diff_values, like_live_foreign_country, like_surround_familiar, homogenize_values_better, comfy_every_kind_ppl, visit_foreign_over_home, good_teacher_wonder, good_job_clarity, regular_life_be_grateful, all_prefer_familiar, like_know_everyone_party) %>%
  drop_na()
TotalTolerance1 <- (mirt(TotalTolerance, 1, verbose = FALSE, itemtype = "graded", SE = TRUE))
M2(TotalTolerance1, type = "C2", calcNULL = FALSE)
itemfit(TotalTolerance1)
TotalTolerance_parms <- coef(TotalTolerance1, TotalTolerancepars = TRUE, simplify = TRUE)
TotalTolerance_parms$items
summary(TotalTolerance1)
# 4 items explaining stuff
plot(TotalTolerance1,
  type = "trace", which.item = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 4, space = "top", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
)
# per item
plot(TotalTolerance1,
  type = "infotrace", which.item = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), facet_items = T,
  as.table = TRUE, auto.key = list(points = F, lines = T, columns = 1, space = "right", cex = .8),
  theta_lim = c(-3, 3),
  main = ""
)
# scale information and conditional standard errors
plot(TotalTolerance1,
  type = "infoSE", theta_lim = c(-3, 3),
  main = ""
)
# conditional reliability
plot(TotalTolerance1,
  type = "rxx", theta_lim = c(-3, 3),
  main = ""
)
# marginal reliability??
marginal_rxx(TotalTolerance1)
# scale characteristic curve
plot(TotalTolerance1, type = "score", theta_lim = c(-3, 3), main = "")
