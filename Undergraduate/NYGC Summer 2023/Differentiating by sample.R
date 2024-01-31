install.packages("taRifx")
install.packages("corrr")
install.packages("kableExtra")
library(BiocManager)




library(kableExtra)
library(corrr)
library(ggrepel)

Idents(for_percent)
Idents(for_percent) <- for_percent$Azimuth_Predicts
table(for_percent$Azimuth_Predicts)

table(for_percent$Azimuth_Predicts)

# comp_Azi <- as.data.frame(Batch_Effect$Azimuth_Predicts, Batch_Effect$sample)

Batch_Effect$Percent <- Percent
Idents(Batch_Effect) <- Batch_Effect$Azimuth_Predicts

# compare cell type percentage by sample
Subs


# stop using####

EMP_Eli <- subset(x = Batch_Effect, subset = celltype == "EMP")
DimPlot(EMP_Eli)
Batch_Effect@meta.data$Azimuth_Predicts
# Confusion matrix
# 1. e-only 2. e & a agree 3. a-only
##### Create a vector to indicate the conflicting labels#####

conflicting_labels <- (Batch_Effect$celltype == "HSC") & (Batch_Effect$Azimuth_Predicts == "EMP")
# create column for metadata
Batch_Effect$conflict_HSC <- conflicting_labels
table(Batch_Effect$conflict_HSC)
DimPlot(Batch_Effect, group.by = "conflict_HSC")


# Subset cells by HSC me -> EMP Azimuth
######## Subset cells into Group 1-me_HSC####
# create the column
me_HSC <- (Batch_Effect$celltype == "HSC") &
  (Batch_Effect$Azimuth_Predicts == "HSC")
# now it's a huge vector
me_HSC
DimPlot(Batch_Effect, group.by = "me_HSC")
# Me and Azimuth coincide on 2459 HSC cells/ 490 of my cells labeled as something else //
# 2000 cells missing from Azimuth, label = ?
# create metadata column
Batch_Effect$me_HSC <- me_HSC
# plot
DimPlot(Batch_Effect, group.by = "conflict_HSC")
#
Idents(Batch_Effect) <- Batch_Effect$me_HSC
Idents(Batch_Effect)
subset(Batch_Effect, subset = me_HSC == TRUE)
table(Batch_Effect@meta.data$celltype)




##### Perform differential expression analysis between Group 1 and Group 2-STUCK-#####

Idents(Batch_Effect)

table(Idents(Batch_Effect))
Progenitor <-
  # To all cells that meet criteria, name me_HSC
  # To all cells that meet criteria, name conflict_HSC
  #
  use_now <- as.data.frame(table(for_percent$Azimuth_Predicts))
percent
as.data.frame(table(percent))
table(Idents(Batch_Effect)) # <- Batch_Effect$me_HSC
HSCvsEMP <- FindMarkers(Batch_Effect, ident.1 = "FALSE", ident.2 = "TRUE", logfc.threshold = 0.25, verbose = FALSE)

Batch_Effect$anno_conflict <- "other"
Batch_Effect$anno_conflict[Batch_Effect$celltype == "HSC" & Batch_Effect$Azimuth_Predicts == "EMP"] <- "conflict"
Batch_Effect$anno_conflict[Batch_Effect$celltype == "HSC" & Batch_Effect$Azimuth_Predicts == "HSC"] <- "agree"
table(Batch_Effect$anno_conflict)
DimPlot(Batch_Effect, group.by = "anno_conflict")
HSCvsEMP <- FindMarkers(Batch_Effect,
  ident.1 = "conflict", ident.2 = "agree",
  group.by = "anno_conflict", logfc.threshold = 1, verbose = FALSE
)


##### Work on variance
# Keep progenitors only

{
  for_percent <- subset(x = Batch_Effect, subset = Azimuth_Predicts != c("ASDC"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD14 Mono"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD16 Mono"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD4 Effector"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD4 Memory"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD4 Naive"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD8 Effector_1"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD8 Effector_2"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD8 Effector_3"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD8 Memory"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("CD8 Naive"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("cDC1"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("cDC2"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("MAIT"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("Memory B"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("Naive B"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("NK"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("NK CD56+"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("pDC"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("Plasma"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("Platelet"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("pre-mDC"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("pre-pDC"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("T Proliferating"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("transitional B"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("Macrophage"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("Stromal"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("ILC"))
  for_percent <- subset(x = for_percent, subset = Azimuth_Predicts != c("transitional B"))
  for_percent <- subset(x = for_percent, subset = sample != c("SPN-NYU1"))
  for_percent <- subset(x = for_percent, subset = sample != c("SPN11"))
  for_percent <- subset(x = for_percent, subset = sample != c("SPN3"))
}
# adding a pattern for Percentage Feature Set

###### Statistical tests#####
use_now <- as.data.frame(table(for_percent$Azimuth_Predicts, for_percent$sample))
use_now
as.data.frame(table(use_now))


use_now <- use_now %>%
  group_by(Var2) %>%
  add_count(wt = Freq)

# use_now %>% filter(Var2 == "SP21") %>% mutate(sum = sum(Freq))
use_now <- use_now %>% mutate(perAll = (Freq / n))
use_now <- use_now %>%
  group_by(Var1) %>%
  mutate(varr = var(perAll))
use_now_percent <- use_now %>% mutate(perAll = (Freq / n) * 100)
use_now <- use_now %>% mutate(nn = NULL)
table(use_now)
var(subset(use_now, Var1 == "HSC")$perAll)
View(use_now)
use_now_percent %>% group_by(Var1)
table(use_now_percent)
# Thanks to mentor now we know the most variable cell type
varXcell <- use_now %>%
  arrange(desc(varr)) %>%
  select(Var1, varr) %>%
  unique()
varXcell
# doing plots for var
colnames(varXcell) <- c("Celltype", "Variance")
varXcell$Celltype <- factor(varXcell$Celltype, levels = varXcell$Celltype[order(varXcell$Variance, decreasing = TRUE)])
VariancePlot <- varXcell %>% ggplot(aes(x = Celltype, y = Variance, fill = Celltype)) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  ylim(0, 0.03) +
  scale_fill_manual(values = clores)


#######color shenanigans
VariancePlot
barsss
num_color<- nlevels(varXcell$Celltype)
num_color
clores<-colorES(num_color)

######
varXcell <- varXcell %>% filter(Celltype != "pro B" & Celltype != "CLP")
varXcell %>% ggplot(aes(x = Celltype, y = Variance, fill = Celltype)) +
  geom_histogram()

# adding corr

setwd("/home/ciudade")
getwd()

Allele_var <- as_tibble(read_csv("ClinicalData.csv"))
Allele_var

##### Look at Late Eryth, HSC, GMP#####
# percentage of cells per sample
find_subgroups <- use_now_percent %>% select(Var1, Var2, perAll)
find_subgroups #This variable holds the cell % data
View(find_subgroups)

find_subgroups <- find_subgroups %>%
  mutate(Var3 = as.character(Var2)) %>%
  select(-Var2)
# find_subgroups<- select(-Var3)
# find_subgroups$Var1 <- NULL
find_subgroups <- find_subgroups %>% pivot_wider(names_from = Var1, values_from = perAll)

find_subgroups$Var1 <- NULL
find_subgroups
# take out rows and do aplot (1x7)

# now have the allele thing
Allele_var <- Allele_var %>% select(-Gene)
Allele_var
# try using corrr
full_corr <- Allele_var %>% ggplot(x = Gene, y = )


##### row by row####
##### Genes
find_subgroups$Var1 <- NULL

# run for the 2nd time
find_subgroups <- find_subgroups[-c(4, 5), ]
find_subgroups #obtain only the percent of cellspertype
{
  BaEoMa <- (find_subgroups[, 2])
  CLP <- (find_subgroups[, 3])
  Early_Eryth <- (find_subgroups[, 4])
  EMP <- (find_subgroups[, 5])
  GMP <- (find_subgroups[, 6])
  HSC <- (find_subgroups[, 7])
  Late_Eryth <- (find_subgroups[, 8])
  LMPP <- (find_subgroups[, 9])
  pro_B <- (find_subgroups[, 10])
  Prog_Mk <- (find_subgroups[, 11])
}

##### CELLS#####
{
  # BaEoMa
  BaEoMa$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  BaEoMa <- BaEoMa %>% pivot_wider(names_from = Sample, values_from = BaEoMa)
  BaEoMa <- as.numeric(BaEoMa)
  BaEoMa
  # CLP
  CLP$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  CLP <- CLP %>% pivot_wider(names_from = Sample, values_from = CLP)
  CLP <- as.numeric(CLP)
  CLP
  # Early
  Early_Eryth$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  Early_Eryth <- Early_Eryth %>% pivot_wider(names_from = Sample, values_from = `Early Eryth`)
  Early_Eryth <- as.numeric(Early_Eryth)
  Early_Eryth
  # EMP
  EMP$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  EMP <- EMP %>% pivot_wider(names_from = Sample, values_from = EMP)
  EMP <- as.numeric(EMP)
  EMP
  # GMP
  GMP$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  GMP <- GMP %>% pivot_wider(names_from = Sample, values_from = GMP)
  GMP <- as.numeric(GMP)
  GMP
  # HSC
  HSC$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  HSC <- HSC %>% pivot_wider(names_from = Sample, values_from = HSC)
  HSC <- as.numeric(HSC)
  HSC
  # Late
  Late_Eryth$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  Late_Eryth <- Late_Eryth %>% pivot_wider(names_from = Sample, values_from = `Late Eryth`)
  Late_Eryth <- as.numeric(Late_Eryth)
  Late_Eryth
  # LMPP
  LMPP$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  LMPP <- LMPP %>% pivot_wider(names_from = Sample, values_from = LMPP)
  LMPP <- as.numeric(LMPP)
  LMPP
  # pro_B
  pro_B$Sample <- c("SP21", "SP22", "SP28", "SP31", "SP32")
  pro_B <- pro_B %>% pivot_wider(names_from = Sample, values_from = `pro B`)
  pro_B <- as.numeric(pro_B)
  pro_B
  # Prog_Mk
  Prog_Mk$Sample <- c("SP21", "SP22", "SP28", "SP29", "SP30", "SP31", "SP32")
  Prog_Mk <- Prog_Mk %>% pivot_wider(names_from = Sample, values_from = `Prog Mk`)
  Prog_Mk <- as.numeric(Prog_Mk)
  Prog_Mk
}
#this will yield a 2-column table of n|sample
Allele_var
#### Mutations#####
{
  JAK2 <- as.numeric(Allele_var[1, ])
  CALR <- as.numeric(Allele_var[2, ])
  NRAS <- (Allele_var[3, ])
  GNB1 <- (Allele_var[4, ])
  ASXL3 <- (Allele_var[5, ])
  EZH2 <- (Allele_var[6, ])
  SETBP1 <- (Allele_var[7, ])
  ASXL1 <- (Allele_var[8, ])
  DNMT3A <- (Allele_var[9, ])
  TET2 <- (Allele_var[10, ])
  CEBPA <- (Allele_var[11, ])
  TP53 <- (Allele_var[12, ])
  MYB <- (Allele_var[13, ])
  RUNX1 <- (Allele_var[14, ])
  SF3B1 <- (Allele_var[15, ])
  SRSF2 <- (Allele_var[16, ])
  PHF6 <- (Allele_var[17, ])
  CBL <- (Allele_var[18, ])
  CDKN1A <- (Allele_var[19, ])
}
#####
find_subgroups
Allele_var$Gene <- NULL
Allele_var
View(Genes)
JAK2 # shows variant allelic frequency by sample
BaEoMa # shows percent of cells per sample
# JAK2
tibble(BaEoMa)
BaEoMa
#### JAK2####
cor.test(JAK2, BaEoMa)
cor.test(JAK2, HSC)
cor.test(JAK2, Late_Eryth)
cor.test(JAK2, CLP)
cor.test(JAK2, Early_Eryth)
cor.test(JAK2, EMP)
cor.test(JAK2, GMP)
cor.test(JAK2, LMPP)
cor.test(JAK2, pro_B)
cor.test(JAK2, Prog_Mk) # yes cor
#Do it to a list of genes, (i in 1 and do function for gene.list)
##### CALR#####
CALR
cor.test(CALR, BaEoMa)
cor.test(CALR, HSC)
cor.test(CALR, Late_Eryth)
cor.test(CALR, CLP)
cor.test(CALR, Early_Eryth)
cor.test(CALR, EMP)
cor.test(CALR, GMP)
cor.test(CALR, LMPP)
cor.test(CALR, pro_B)
cor.test(CALR, Prog_Mk) # yes cor
#### NRAS####
cor.test(NRAS, BaEoMa)
cor.test(NRAS, HSC)
cor.test(NRAS, Late_Eryth)
cor.test(NRAS, CLP)
cor.test(NRAS, Early_Eryth)
cor.test(NRAS, EMP)
cor.test(NRAS, GMP)
cor.test(NRAS, LMPP)
cor.test(NRAS, pro_B)
cor.test(NRAS, Prog_Mk)
#### GNB1####
cor.test(GNB1, BaEoMa)
cor.test(GNB1, HSC)
cor.test(GNB1, Late_Eryth)
cor.test(GNB1, CLP)
cor.test(GNB1, Early_Eryth)
cor.test(GNB1, EMP)
cor.test(GNB1, GMP)
cor.test(GNB1, LMPP)
cor.test(GNB1, pro_B)
cor.test(GNB1, Prog_Mk)
#### ASXL3####
cor.test(ASXL3, BaEoMa)
cor.test(ASXL3, HSC)
cor.test(ASXL3, Late_Eryth)
cor.test(ASXL3, CLP) # yes cor
cor.test(ASXL3, Early_Eryth)
cor.test(ASXL3, EMP)
cor.test(ASXL3, GMP)
cor.test(ASXL3, LMPP)
cor.test(ASXL3, pro_B) # yes cor
cor.test(ASXL3, Prog_Mk)
#### EZH2####
cor.test(EZH2, BaEoMa)
cor.test(EZH2, HSC)
cor.test(EZH2, Late_Eryth)
cor.test(EZH2, CLP)
cor.test(EZH2, Early_Eryth)
cor.test(EZH2, EMP)
cor.test(EZH2, GMP)
cor.test(EZH2, LMPP)
cor.test(EZH2, pro_B)
cor.test(EZH2, Prog_Mk)
#### SETBP1####
cor.test(SETBP1, BaEoMa)
cor.test(SETBP1, HSC)
cor.test(SETBP1, Late_Eryth)
cor.test(SETBP1, CLP)
cor.test(SETBP1, Early_Eryth)
cor.test(SETBP1, EMP)
cor.test(SETBP1, GMP)
cor.test(SETBP1, LMPP)
cor.test(SETBP1, pro_B)
cor.test(SETBP1, Prog_Mk)
correlate(SETBP1, BaEoMa)
#### ASXL1####
EMP
cor.test(ASXL1, BaEoMa)
cor.test(ASXL1, HSC)
cor.test(ASXL1, Late_Eryth)
cor.test(ASXL1, CLP)
cor.test(ASXL1, Early_Eryth)
cor.test(ASXL1, EMP)
cor.test(ASXL1, GMP)
cor.test(ASXL1, LMPP)
cor.test(ASXL1, pro_B)
cor.test(ASXL1, Prog_Mk)
#### DNMT3A####
cor.test(DNMT3A, BaEoMa)
cor.test(DNMT3A, HSC)
cor.test(DNMT3A, Late_Eryth)
cor.test(DNMT3A, CLP)
cor.test(DNMT3A, Early_Eryth)
cor.test(DNMT3A, EMP)
cor.test(DNMT3A, GMP)
cor.test(DNMT3A, LMPP)
cor.test(DNMT3A, pro_B)
cor.test(DNMT3A, Prog_Mk) # yes cor
#### TET2####
cor.test(TET2, BaEoMa)
cor.test(TET2, HSC)
cor.test(TET2, Late_Eryth)
cor.test(TET2, CLP)
cor.test(TET2, Early_Eryth)
cor.test(TET2, EMP)
cor.test(TET2, GMP)
cor.test(TET2, LMPP)
cor.test(TET2, pro_B)
cor.test(TET2, Prog_Mk)
#### CEBPA####
cor.test(CEBPA, BaEoMa)
cor.test(CEBPA, HSC)
cor.test(CEBPA, Late_Eryth)
cor.test(CEBPA, CLP)
cor.test(CEBPA, Early_Eryth)
cor.test(CEBPA, EMP)
cor.test(CEBPA, GMP)
cor.test(CEBPA, LMPP)
cor.test(CEBPA, pro_B)
cor.test(CEBPA, Prog_Mk)
#### TP53####
cor.test(TP53, BaEoMa)
cor.test(TP53, HSC)
cor.test(TP53, Late_Eryth)
cor.test(TP53, CLP)
cor.test(TP53, Early_Eryth)
cor.test(TP53, EMP)
cor.test(TP53, GMP)
cor.test(TP53, LMPP)
cor.test(TP53, pro_B)
cor.test(TP53, Prog_Mk)
#### MYB####
cor.test(MYB, BaEoMa)
cor.test(MYB, HSC)
cor.test(MYB, Late_Eryth)
cor.test(MYB, CLP) # yes cor
cor.test(MYB, Early_Eryth)
cor.test(MYB, EMP)
cor.test(MYB, GMP)
cor.test(MYB, LMPP)
cor.test(MYB, pro_B)
cor.test(MYB, Prog_Mk)
#### RUNX1####
BaEoMa
RUNX1
cor.test(RUNX1, BaEoMa)
cor.test(RUNX1, HSC)
cor.test(RUNX1, Late_Eryth)
cor.test(RUNX1, CLP) # yes cor
cor.test(RUNX1, Early_Eryth)
cor.test(RUNX1, EMP)
cor.test(RUNX1, GMP)
cor.test(RUNX1, LMPP)
cor.test(RUNX1, pro_B) # yes_cor
cor.test(RUNX1, Prog_Mk)
#### SF3B1####
cor.test(SF3B1, BaEoMa)
cor.test(SF3B1, HSC)
cor.test(SF3B1, Late_Eryth)
cor.test(SF3B1, CLP)
cor.test(SF3B1, Early_Eryth)
cor.test(SF3B1, EMP)
cor.test(SF3B1, GMP)
cor.test(SF3B1, LMPP)
cor.test(SF3B1, pro_B)
cor.test(SF3B1, Prog_Mk)
#### SRSF2####
cor.test(SRSF2, BaEoMa)
cor.test(SRSF2, HSC)
cor.test(SRSF2, Late_Eryth)
cor.test(SRSF2, CLP) # yes cor
cor.test(SRSF2, Early_Eryth)
cor.test(SRSF2, EMP)
cor.test(SRSF2, GMP)
cor.test(SRSF2, LMPP)
cor.test(SRSF2, pro_B) # yes cor
cor.test(SRSF2, Prog_Mk)
#### PHF6####
cor.test(PHF6, BaEoMa)
cor.test(PHF6, HSC)
cor.test(PHF6, Late_Eryth)
cor.test(PHF6, CLP)
cor.test(PHF6, Early_Eryth)
cor.test(PHF6, EMP)
cor.test(PHF6, GMP)
cor.test(PHF6, LMPP)
cor.test(PHF6, pro_B)
cor.test(PHF6, Prog_Mk)
#### CBL####
cor.test(CBL, BaEoMa)
cor.test(CBL, HSC)
cor.test(CBL, Late_Eryth)
cor.test(CBL, CLP)
cor.test(CBL, Early_Eryth)
cor.test(CBL, EMP)
cor.test(CBL, GMP)
cor.test(CBL, LMPP)
cor.test(CBL, pro_B)
cor.test(CBL, Prog_Mk) # yes_cor
#### CDKN1A####
cor.test(CDKN1A, BaEoMa)
cor.test(CDKN1A, HSC)
cor.test(CDKN1A, Late_Eryth)
cor.test(CDKN1A, CLP) # yes_cor
cor.test(CDKN1A, Early_Eryth)
cor.test(CDKN1A, EMP)
cor.test(CDKN1A, GMP)
cor.test(CDKN1A, LMPP)
cor.test(CDKN1A, pro_B) # yes_cor
cor.test(CDKN1A, Prog_Mk)
RUNX1
CLP

##### trash#####
?cor.test
find_subgroups
as.data.frame(find_subgroups)
find_subgroups <- find_subgroups %>% select(-Var3)
remove.factor(find_subgroups)
find_subgroups <- find_subgroups %>%
  pivot_longer(everything(), names_to = "celltype", values_to = "perAll")


correlation

Allele <- Allele_var %>%
  pivot_longer(everything(), names_to = "Sample", values_to = "VAFValue") # Replace NA values with 0 in the VAF dataframe
# first unique values indicate mutation type
Allele # use for mutations and stuff = carries variant allele frequency
###### found_subgroups#####
# percentage of GMP,HSC,Late Eryth per sample
found_subgroups <- find_subgroups %>% filter(Var1 %in% c("GMP", "HSC", "Late Eryth"))
found_subgroups
found_subgroups <- found_subgroups %>%
  pivot_wider(
    names_from = c(Var2),
    values_from = c(perAll)
  )
find_subgroups
found_subgroups
table(found_subgroups)


##### Mutationals- Vln plots####
###### Driver#####
Idents(for_percent) <- for_percent$Azimuth_Predicts
DimPlot(for_percent, label = TRUE)
# JAK2 mutation
J1 <- VlnPlot(for_percent, features = "JAK2", split.by = "Azimuth_Predicts")
J2 <- VlnPlot(for_percent, features = "JAK2", split.by = "sample") + NoLegend()
J1 + J2

# CALR mutation
C1 <- VlnPlot(for_percent, features = "CALR", split.by = "sample")
C2 <- VlnPlot(for_percent, features = "CALR", split.by = "Azimuth_Predicts")
C1 + C2

###### Signaling####

# NRASG12D
N1 <- VlnPlot(for_percent, features = "NRAS", split.by = "sample")
N2 <- VlnPlot(for_percent, features = "NRAS", split.by = "Azimuth_Predicts")
N1 + N2

# GNB1
G1 <- VlnPlot(for_percent, features = "GNB1", split.by = "sample")
G2 <- VlnPlot(for_percent, features = "GNB1", split.by = "Azimuth_Predicts")
G1 + G2
###### Epigenetic####

# ASXL3
AS1 <- VlnPlot(for_percent, features = "ASXL3", split.by = "sample")
AS2 <- VlnPlot(for_percent, features = "ASXL3", split.by = "Azimuth_Predicts")
AS1 + AS2
# EZH2
EZH1 <- VlnPlot(for_percent, features = "EZH2", split.by = "sample")
EZH1.1 <- VlnPlot(for_percent, features = "EZH2", split.by = "Azimuth_Predicts")
EZH1 + EZH1.1
# ASXL1
A1 <- VlnPlot(for_percent, features = "ASXL1", split.by = "sample")
A2 <- VlnPlot(for_percent, features = "ASXL1", split.by = "Azimuth_Predicts")
A1 + A2
# DNMT3A
D1 <- VlnPlot(for_percent, features = "DNMT3A", split.by = "sample")
D2 <- VlnPlot(for_percent, features = "DNMT3A", split.by = "Azimuth_Predicts")
D1 + D2
# TET2
T1 <- VlnPlot(for_percent, features = "TET2", split.by = "sample")
T2 <- VlnPlot(for_percent, features = "TET2", split.by = "Azimuth_Predicts")
T1 + T2
###### Transcrip Fact#####
# MYB
M1 <- VlnPlot(for_percent, features = "MYB", split.by = "sample")
M2 <- VlnPlot(for_percent, features = "MYB", split.by = "Azimuth_Predicts")
M1 + M2
# CEB
CEB1 <- VlnPlot(for_percent, features = "CEBPA", split.by = "sample")
CEB2 <- VlnPlot(for_percent, features = "CEBPA", split.by = "Azimuth_Predicts")
CEB1 + CEB2
# TP53
TP1 <- VlnPlot(for_percent, features = "TP53", split.by = "sample")
TP2 <- VlnPlot(for_percent, features = "TP53", split.by = "Azimuth_Predicts")
TP1 + TP2
# RUNX1
R1 <- VlnPlot(for_percent, features = "RUNX1", split.by = "sample")
R2 <- VlnPlot(for_percent, features = "RUNX1", split.by = "Azimuth_Predicts")
R1 + R2
###### Splicing####
# SF3B1
SF1 <- VlnPlot(for_percent, features = "SF3B1", split.by = "sample")
SF2 <- VlnPlot(for_percent, features = "SF3B1", split.by = "Azimuth_Predicts")
SF1 + SF2
# SRSF2
SR1 <- VlnPlot(for_percent, features = "SRSF2", split.by = "sample")
SR2 <- VlnPlot(for_percent, features = "SRSF2", split.by = "Azimuth_Predicts")
FeaturePlot(for_percent, features = "JAK2", split.by = "sample", label = TRUE, repel = TRUE)
SR1 + SR2
###### Ubiquitin Ligase#####
# PHF6
PH1 <- VlnPlot(for_percent, features = "PHF6", split.by = "sample")
PH2 <- VlnPlot(for_percent, features = "PHF6", split.by = "Azimuth_Predicts")
PH1 + PH2
# CBL
CBL1 <- VlnPlot(for_percent, features = "CBL", split.by = "sample")
CBL2 <- VlnPlot(for_percent, features = "CBL", split.by = "Azimuth_Predicts")
CBL1 + CBL2




##### Yes#####
DimPlot(for_percent, split.by = "sample")
jj <- FeaturePlot(for_percent, features = "JAK2", split.by = "Azimuth_Predicts", order = TRUE)
FeaturePlot(for_percent, split.by = "sample", order = TRUE)
FeaturePlot(for_percent, features = "CALR", split.by = "Azimuth_Predicts", order = TRUE)
FindMarkers(for_percent, ident.1 = "EMP", group.by = "Azimuth_Predicts")
jj
for_percent$
  table(Idents(for_percent))
JAK2
####### STH plot#####
JAK2
as.data.frame(GMP)
x1 <- ggplot(data = JAK2, aes(x = sample, y = VAF)) +
  geom_bar(position = "dodge", stat = "identity")
x1
x2 <- ggplot + geom_col(data = find_subgroups, aes(x = Var2, y = GMP))
x2
x1 + x2

as.data.frame(JAK2)
sp <- tibble(rating = 1:7, c("SP21", "SP22", "SP28", "SP29", "SP30", "SP31", "SP32"))
colnames(sp) <- c("yes", "sample")

JAK2 <- tibble(JAK2)
JAK2$sample <- sp$sample
colnames(JAK2) <- c("VAF", "sample")
JAK2
find_subgroups <- use_now_percent %>% select(Var1, Var2, perAll)
find_subgroups <- find_subgroups %>% pivot_wider(names_from = Var1, values_from = perAll)
# plotf for prog_Mk
find_subgroups
okay <- as.data.frame(c(find_subgroups[, 6], find_subgroups[, 1]))
okay2 <- as.data.frame(c(find_subgroups[, 11], find_subgroups[, 1]))
okay2 %>% relocate(Var2)
colnames(okay2) <- c("Prog_Mk", "Sample", "VAFJAK2")
okay2$VAFJAK2 <- as.double(okay2$VAFJAK2)
okay
okay2
find_subgroups
GMP
okay$VAFJAK2 <- JAK2$VAF
okay
tibble(okay)
tibble(okay2)

colnames(okay) <- c("GMP", "Sample", "VAFJAK2")
okay
okay2

okay$VAFJAK2 <- as.character(okay$VAFJAK2)
tibble(okay)
# useful graph for GMP with CALR patients
ggplot(data = okay, aes(x = VAFJAK2, y = GMP, label = rownames(Sample), label.size = 4)) +
  geom_point(aes(color = Sample, group = Sample, size = 0.2)) +
  geom_text_repel(aes(label = Sample)) +
  theme_bw() +
  guides(Sample = "legend", size = "none")
# Now for Prog_Mk
ggplot(data = okay2, aes(x = VAFJAK2, y = Prog_Mk, label = rownames(Sample), label.size = 3)) +
  geom_point(aes(color = Sample, group = Sample, size = 0.5)) +
  geom_text_repel(aes(label = Sample)) +
  theme_bw() +
  guides(Sample = "legend", size = "none")
#
okay2
okay
okay2$VAFJAK2 <- as.character(okay2$VAFJAK2)

# create df without CALR patients
cal_out <- okay %>% filter(Sample != "SP29" & Sample != "SP30")
tibble(cal_out)

ggplot(data = cal_out, aes(x = VAFJAK2, y = GMP, label = rownames(Sample))) +
  geom_point(aes(color = Sample, group = Sample)) +
  geom_text_repel(aes(label = Sample)) +
  theme_bw()
##### CALR things####

Idents(Batch_Effect) <- Batch_Effect$seurat_clusters
DimPlot(Batch_Effect, label = TRUE)
okay1 <- tibble(CALR)
okay1$sample <- find_subgroups$Var2
okay1$GMP <- tibble(GMP)
colnames(okay1) <- c("VAFCALR", "Sample", "GMP")
okay1


caliplot <- ggplot(data = okay1, aes(x = VAFCALR, y = GMP$GMP, label = rownames(Sample))) +
  geom_point(aes(color = Sample, group = Sample)) +
  geom_text_repel(aes(label = Sample)) +
  theme_bw()
caliplot

##### Factors and Levels#####
for_fun <- subset(for_percent, subset = sample != "SPN3" & sample != "SPN11" & sample != "SPN-NYU1")
Idents(for_fun)
levels(for_fun@active.ident)
feet <- DimPlot(for_fun, split.by = "sample")
feet$data$sample <- factor(x = feet$data$sample, levels = c("SP31", "SP22", "SP28", "SP32", "SP21", "SP30", "SP29"))
feet
ggplot(data = okay, aes(x = VAFJAK2, y = GMP))
table(Idents(for_fun))

for_fun$data$sample <- factor(for_fun$sample, levels = c("SP31", "SP22", "SP28", "SP32", "SP21", "SP30", "SP29"))

Idents(for_fun)<- for_fun$Azimuth_Predicts
DimPlot(for_fun, split.by = "mutation", group.by = "Azimuth_Predicts", order = TRUE, label = TRUE,sizes.highlight = 0.5,label.size = 6.1, repel = TRUE ,cells.highlight = WhichCells(for_percent ,idents = c("Prog Mk")),cols.highlight = c("red","blue")) + NoLegend()  


mutis %>% 
?guides

table(for_fun$Azimuth_Predicts)
table(for_fun$mutation)

for_fun$mutation<-if_else(for_fun$sample %in% c("SP29","SP30"), "CALR Mutants","No CALR Mutations")
progi <- for_fun$Azimuth_Predicts == "Prog Mk"
progi<- tibble(progi)
# sample ordered by JAK2 VAF
feet
####
full_corr
# sample ordered by CALR
Idents(for_fun) <- for_fun$sample

CALR
Hand <- DimPlot(for_fun, split.by = "sample")
# CALR
Hand
####### Graph Corr#####
r1 <- c(0.13782, -0.254)
r1
r2 <-
  full_corr <- read_csv("Full_Cor.csv")
full_corr
colnames(full_corr) <- c("Gene", "BaEoMa", "HSC", "Late_Eryth", "CLP", "Early_Eryth", "EMP", "GMP", "LMPP", "pro_B", "Prog_Mk")

# this one filters following p-value
full_corr %>%
  ggplot(aes(x = Celltype, y = Corr, fill = Gene, size = p_value, shape = as_factor(p_value))) +
  ylim(-0.6, 1) +
  geom_dotplot(aes(size = p_value), binaxis = "y") +
  theme_bw()


# this plot is in decreasing p-value
full_corr %>% ggplot(aes(x = Celltype, y = Corr, fill = Gene)) +
  ylim(-0.9, 1) +
  geom_point(aes(size = p_value, binaxis = "y", color = Gene)) +
  guides(Gene = "legend", size = "none") +
  scale_size(trans = "reverse") +
  theme_bw()

# y is the p-value
full_corr %>% ggplot(aes(x = Celltype, y = p_value, fill = Gene)) +
  ylim(0.001, 1) +
  geom_point(aes(size = p_value, binaxis = "y", color = Gene)) +
  guides(Gene = "legend", size = "none") +
  scale_size(trans = "reverse") +
  theme_bw() +
  scale_y_continuous(limits = c(0.01, 1), breaks = seq(0.01, 1, by = 0.1))


##### partial-corr#####
Part_Corr <- read_csv("PartialCorr.csv")
Part_Corr
full_corr
# filters p-value
Part_Corr %>% ggplot(aes(x = Celltype, y = Corr, fill = Gene, size = p_value, shape = as_factor(p_value))) +
  ylim(-0.6, 1) +
  geom_dotplot(aes(size = p_value), binaxis = "y") +
  theme_bw()

# plot in increasing corr
Part_Corr %>% ggplot(aes(x = Celltype, y = Corr, fill = Gene)) +
  ylim(-1, 1) +
  geom_point(aes(size = p_value, binaxis = "y", color = Gene)) +
  guides(Gene = "legend", size = "none") +
  scale_size(trans = "reverse") +
  theme_bw()



Part_Corr %>% ggplot(aes(x = Celltype, y = p_value, fill = Gene)) +
  ylim(0.01, 1) +
  geom_point(aes(size = p_value, binaxis = "y", color = Gene)) +
  guides(Gene = "legend", size = "none") +
  scale_size(trans = "reverse") +
  theme_bw() +
  scale_y_continuous(limits = c(0.01, 1), breaks = seq(0.01, 1, by = 0.1))


##### Matrix#####
matri <- c(0.768, 0.150, 0.581, 0.633, 0.573, 0.775, 0.292, 0.22, 0.935, 0.75, 0.07, 0.219, 0.055, 0.186, 0.07, 0.4, 0.349, 0.577, 0.349, 0.01)
matri <- matrix(matri, nrow = 2)
matri <- as_tibble(matri)
colnames(matri) <- c("BaEoMa", "HSC", "Late_Eryth", "CLP", "Early_Eryth", "EMP", "GMP", "LMPP", "pro_B", "Prog_Mk")
rownames(matri) <- c("JAK2", "CALR")
matri$Gene <- c("JAK2", "CALR")
matri <- matri %>% relocate(Gene)
matri
matrr <- kbl(matri) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
matrr %>% footnote(general = "  p-val <0.05 =  * \n   .p-val <0.1(borderline significant) = yellow ", general_title = "Notes:", footnote_as_chunk = T, title_format = c("italic", "underline"))
JAK2
Part_Corr

# now with corr
mas <- read_csv("Full.csv")
names(mas)[names(mas) == "...1"] <- "Gene"
mas <- kbl(mas) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
mas %>% footnote(general = " red = negative correlation \n       yellow = positive correlation ", general_title = "Notes:", footnote_as_chunk = T, title_format = c("italic", "underline"))

##### Heatmap for one ProgMk only across 5 samples####


# Perform differential expression between samples with lesss than 50VAF
DifGMP <- subset(for_fun, subset = Azimuth_Predicts == "GMP")
Diferente <- subset(for_fun, subset = Azimuth_Predicts == "Prog Mk")
Diferente
table(Idents(Diferente))
Idents(Diferente) <- Diferente$sample
# ident 1 = sample < 50 JAK2VAF, ident2 = sample >50 VAf #prog_Mk
Mk29 <- FindMarkers(Diferente, ident.1 = c("SP29"), ident.2 = c("SP28", "SP31", "SP22"), min.pct = 0.25, )
Mk29
Prog_Mk_Markers <- FindAllMarkers(Diferente, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)

# Filter for positives and p-value above 0.05
Mk29posi <- Mk29 %>% filter(p_val < 0.05, p_val_adj < 0.05, avg_log2FC > 0)
Mk29posi <- rownames(Mk29posi)
Mk29nega <- Mk29 %>% filter(p_val < 0.05, p_val_adj < 0.05, avg_log2FC < 0)
Mk29nega <- rownames(Mk29nega)


Mk30 <- FindMarkers(Diferente, ident.1 = c("SP30"), ident.2 = c("SP28", "SP31", "SP22"), min.pct = 0.25)
Mk30
Mk30posi <- Mk30 %>% filter(p_val < 0.05, p_val_adj < 0.05, avg_log2FC > 0)
Mk30posi <- rownames(Mk30posi)
Mk30nega <- Mk30 %>% filter(p_val < 0.05, p_val_adj < 0.05, avg_log2FC < 0)
Mk30nega <- rownames(Mk30nega)
# positive across mk29 and mk30
upgenMk <- intersect(Mk29posi, Mk30posi)
downgenMk <- intersect(Mk29nega, Mk30nega)

upgenMk <- as.data.frame(upgenMk)
downgenMk <- as.data.frame(downgenMk)
colnames(upgenMk) <- "gene"
colnames(downgenMk) <- "gene"
Heat <- rbind(upgenMk, downgenMk)
Heat<-as_tibble(Heat)
Heat
list(as.character(Heat))
View(Heat)
Diferente
Diferente <- ScaleData(Diferente, features = Heat$gene)

#subgroups for ProgMk only
Diferente <- subset(Diferente, subset = sample != "SP32" & sample != "SP21")
DoHeatmap(Diferente, features = Heat$gene) + NoLegend()

####Plots Mk diff expressed cells across all HSPC cell types####
for_percentMk<- for_percent


for_percentMk<- ScaleData(for_percentMk, features = Heat$gene)
for_percentMk<- subset(for_percent, subset = sample != c("SP21") & sample != "SP32")
table(for_percentMk$sample)
table(Heat$gene)
DoHeatmap(for_percentMk,features = Heat$gene)+NoLegend()
Idents(for_percentMk)<- for_percentMk$sample
table(for_percentMk$sample)
dev.off()

table(for_percentMk$broad)
table(for_percentMk$sample)
#something here should work

for_fun$mutation<-if_else(for_fun$sample %in% c("SP29","SP30"), "CALR Mutants","No CALR Mutations")
progi <- for_fun$Azimuth_Predicts == "Prog Mk"
progi <- tibble(progi)
progi

#####Do heatmap####

#something here should work

cool$combined <- if_else(cool$Azimuth_Predicts %in% c("Late Eryth"), "Late Eryth","HSPC")
table(cool$combined)
rownames(cool$Azimuth_Predicts == "Late Eryth")
table(cool$Azimuth_Predicts) # 3403 late eryth
table(cool$basic_predict) #18859
table(cool$basic_predict == "HSPC")
table(cool$combined)
intersect(cool$combined == "Late Eryth", cool$basic_predict == "HSPC")
one<-tibble(cool$combined %in% c("Late Eryth"))
two<-tibble(cool$basic_predict %in% c("HSPC"))
#how to deal with characters..
class(cool$basic_predict)
class(cool$combined)

cool$broad <- cool$basic_predict
table(cool$broad)
cool$thing <- cool$Azimuth_Predicts == "Late Eryth"
table(cool$thing)
colnames(cool$thing)<- c("nope","Late Eryth")
table(cool$Azimuth_Predicts[cool$thing]) 
cool$broad<-(cool$Azimuth_Predicts[cool$thing]) 
cool
table(cool$broad)
table(cool$basic_predict)
table(Azimuth_Predicts)
cool <- ScaleData(cool, features = Basic10$gene)
Idents(cool)<- cool$basic_predict
DoHeatmap(cool, group.by = "ident", features = Basic10$gene)
b
ggsave(filename = "yup.png", plot = b, width = 14, height = 7)
dev.off()
DimPlot(cool, group.by = "sample")
table(cool$Azimuth_Predicts)
cool<- ScaleData(cool, features = Heat$gene)


table(cool$sample)
View(Heat)
table(Batch_Effect$sample)

#####Only MF Patients#### 
infect_only<- subset(Batch_Effect, subset = sample != "SPN-NYU1" & sample != "SPN11" & sample != "SPN3" & sample != "SP21" & sample != "SP32")
table(infect_only$sample)

Idents(infect_only)
table(infect_only$)
infect_only<- ScaleData(infect_only, features = Heat$gene)

xxx<-DoHeatmap(infect_only, group.by = "ident", features = Heat$gene)
ggsave(filename = "xxx.png", plot = xxx, width = 14, height = 7)
Diferente
table(for_fun$sample)
for_fun <- subset(for_fun, subset = )

dev.off()

rm(d1)
infect_only
#
DoHeatmap(infect_only, group.by = "ident", features = Heat$gene)
table(infect_only$sample)
table(Idents(infect_only))
#####HSPC Only but everything else######
{
HSPC_only <- subset(x = Batch_Effect, subset = Azimuth_Predicts != c("ASDC"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD14 Mono"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD16 Mono"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD4 Effector"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD4 Memory"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD4 Naive"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD8 Effector_1"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD8 Effector_2"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD8 Effector_3"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD8 Memory"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("CD8 Naive"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("cDC1"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("cDC2"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("MAIT"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("Memory B"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("Naive B"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("NK"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("NK CD56+"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("pDC"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("Plasma"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("Platelet"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("pre-mDC"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("pre-pDC"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("T Proliferating"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("transitional B"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("Macrophage"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("Stromal"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("ILC"))
HSPC_only <- subset(x = HSPC_only, subset = Azimuth_Predicts != c("transitional B"))
}
#Do heatmap for this
HSPC_only<- ScaleData(HSPC_only, features = Heat$gene)
Idents(HSPC_only)<- HSPC_only$tissue
DoHeatmap(HSPC_only, group.by = "ident",features = Heat$gene)
HSPC_but_SP32 <- subset(HSPC_only, sample != "SP32")
HSPC_but_SP32<- ScaleData(HSP)
DoHeatmap(HSPC_but_SP32, group.by = "ident",features = Heat$gene)
Healthy_Only<- subset(HSPC_only, subset = sample != "SP21" & sample != "SP22" & sample != "SP28" & sample != "SP29" & sample != "SP30" & sample != "SP31" & sample != "SP32")
table(Healthy_Only$sample)
DoHeatmap(Healthy_Only, group.by = "ident",  features = Heat$gene)
HSPC_only
for_fun<- ScaleData(for_fun, features = Heat$gene)
DoHeatmap(for_fun, group.by = "sample", features = Heat$gene)+ NoLegend()
table(for_fun$Azimuth_Predicts)
#####Non_HSPC####

#####Cell Cycle Scoring#####
#
s.genes <- cc.genes$s.genes
g2m.genes <- cc.genes$g2m.genes


HSPC_only$s.genes <- cc.genes$s.genes
HSPC_only$g2m.genes <- cc.genes$g2m.genes


table(HSPC_only$s.genes)
HSPC_only <- NormalizeData(HSPC_only)
HSPC_only <- FindVariableFeatures(HSPC_only)
HSPC_only <- ScaleData(HSPC_only, features = rownames(HSPC_only))
HSPC_only <- RunPCA(HSPC_only, features = VariableFeatures(HSPC_only), ndims.print = 6:10, nfeatures.print = 10)
HSPC_only <- CellCycleScoring(HSPC_only, s.features = s.genes, g2m.features = g2m.genes, set.ident = TRUE)
H
head(HSPC_only[[]])
RidgePlot(HSPC_only, features = c("PCNA", "TOP2A", "MCM6", "MKI67"), ncol = 2)
HSPC_only <- RunPCA(HSPC_only, features = c(s.genes, g2m.genes))
HSPC_only$<-DimPlot(HSPC_only)
table(HSPC_only$Phase)
table(HSPC_only$sample)
DimHeatmap(HSPC_only, dims = c(8,10))
HSPC_onl$
DimPlot(HSPC_only, reduction = "umap")
head(HSPC_only[[]])
HSPC_only
#looking at diff, exp genes
#split by mutational status?
#G2 & M cycling. G1 not cycling. maybe related to prog Mk
#bar chart, quantify. by celltype and sample %. Bar chart for sure. 
#group 1 , group 2
#####don't run######
Batch_Effect<- ScaleData(Batch_Effect, features = rownames(Batch_Effect))
Batch_Effect<-  FindVariableFeatures(Batch_Effect,selection.method = "vst")
Batch_Effect <- RunPCA(Batch_Effect, features = FindVariableFeatures(Batch_Effect), ndims.print = 6:10, nfeatures.print = 10)
Batch_Effect<- CellCycleScoring(Batch_Effect,s.features = s.genes, g2m.features = g2m.genes, set.ident = TRUE)
#second try
HSPC_only$CC.Difference <- HSPC_only$S.Score - HSPC_only$G2M.Score
HSPC_only <- ScaleData(HSPC_only, vars.to.regress = "CC.Difference", features = rownames(HSPC_only))


