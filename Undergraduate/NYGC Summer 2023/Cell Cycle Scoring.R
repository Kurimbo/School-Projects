######INstalling Cluster Profiler#####
devtools::install_github(c("GuangchuangYu/DOSE", "GuangchuangYu/clusterProfiler"))
library(clusterProfiler)

s.genes <- cc.genes$s.genes
g2m.genes <- cc.genes$g2m.genes

library(remotes)
install.packages("remotes")
remotes::install_github("YuLab-SMU/clusterProfiler")
library(devtools)
library(clusterProfiler)
install.packages("clusterProfiler")
install.packages("installr")
#
{
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.16")
}
#
enrichGO(Heat, OrgDb = )
?enrichGO

#####
HSPC_only<- subset(HSPC_only, subset = sample!= "SP32" & sample!= "SP21"& sample != "SPN-NYU1" & sample != "SPN11" & sample != "SPN3")
table(HSPC_only$sample)
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

head(HSPC_only[[]])
RidgePlot(HSPC_only, features = c("PCNA", "TOP2A", "MCM6", "MKI67"), ncol = 2)

table(HSPC_only$Phase)

DimHeatmap(HSPC_only, dims = c(8,10))

DimPlot(HSPC_only, reduction = "umap")
head(HSPC_only[[]])



#things of every state

{HSPC_only$G1<- HSPC_only$Phase == "G1"
HSPC_only$G2M<- HSPC_only$Phase == "G2M"
HSPC_only$S<- HSPC_only$Phase == "S"

#Things of every celltype 
#BaEoMa
HSPC_only$Ba<- HSPC_only$Azimuth_Predicts== "BaEoMa"
table(HSPC_only$Ba)
table(HSPC_only$G1[HSPC_only$Ba])  #G1 Ba 1202
table(HSPC_only$G2M[HSPC_only$Ba]) #G2M Ba 59
table(HSPC_only$S[HSPC_only$Ba]) #S Ba 153
#CLP
HSPC_only$CLP<- HSPC_only$Azimuth_Predicts== "CLP"

table(HSPC_only$G1[HSPC_only$CLP])  #G1 CLP 
table(HSPC_only$G2M[HSPC_only$CLP]) #G2M CLP 
table(HSPC_only$S[HSPC_only$CLP]) #S CLP 
#Early Eryth
HSPC_only$Early<- HSPC_only$Azimuth_Predicts== "Early Eryth"

table(HSPC_only$G1[HSPC_only$Early])  #G1 Early Eryth 
table(HSPC_only$G2M[HSPC_only$Early]) #G2M Early Eryth 
table(HSPC_only$S[HSPC_only$Early]) #S Early Eryth 
#EMP
HSPC_only$EMP <- HSPC_only$Azimuth_Predicts == "EMP"

table(HSPC_only$G1[HSPC_only$EMP])  #G1 EMP 
table(HSPC_only$G2M[HSPC_only$EMP]) #G2M EMP 
table(HSPC_only$S[HSPC_only$EMP]) #S EMP 
#GMP
HSPC_only$GMP <- HSPC_only$Azimuth_Predicts == "GMP"

table(HSPC_only$G1[HSPC_only$GMP])  #G1 GMP 
table(HSPC_only$G2M[HSPC_only$GMP]) #G2M GMP
table(HSPC_only$S[HSPC_only$GMP]) #S GMP 
#HSC
HSPC_only$HSC<- HSPC_only$Azimuth_Predicts == "HSC"

table(HSPC_only$G1[HSPC_only$HSC])  #G1 HSC 
table(HSPC_only$G2M[HSPC_only$HSC]) #G2M HSC 
table(HSPC_only$S[HSPC_only$HSC]) #S HSC 
#Late Eryth 
HSPC_only$Late<- HSPC_only$Azimuth_Predicts == "Late Eryth"

table(HSPC_only$G1[HSPC_only$Late])  #G1 Late Eryth 
table(HSPC_only$G2M[HSPC_only$Late]) #G2M Late Eryth 
table(HSPC_only$S[HSPC_only$Late]) #S Late Eryth 
#LMPP
HSPC_only$LMPP<- HSPC_only$Azimuth_Predicts == "LMPP"

table(HSPC_only$G1[HSPC_only$LMPP])  #G1 LMPP 
table(HSPC_only$G2M[HSPC_only$LMPP]) #G2M LMPP 
table(HSPC_only$S[HSPC_only$LMPP]) #S LMPP 
#pro B
HSPC_only$pB <- HSPC_only$Azimuth_Predicts == "pro B"

table(HSPC_only$G1[HSPC_only$pB])  #G1 pro B 
table(HSPC_only$G2M[HSPC_only$pB]) #G2M pro B 
table(HSPC_only$S[HSPC_only$pB]) #S pro B 
#Prog Mk
HSPC_only$PrMk <- HSPC_only$Azimuth_Predicts == "Prog Mk"

table(HSPC_only$G1[HSPC_only$PrMk])  #G1 Prog Mk 
table(HSPC_only$G2M[HSPC_only$PrMk]) #G2M Prog Mk 
table(HSPC_only$S[HSPC_only$PrMk]) #S Prog Mk 
}

######
Phases<- tibble(read_csv("CellPhase.csv"))


#-#-#-#_#-_#-#
#Actual Plots#
#-#-#-#-#-#-#  
#Add count and percent
Phases<-Phases %>% group_by(CellType) %>%
  add_count(wt = Count) %>%
  mutate(perAll = (Count/n)*100)
#Add variance
Phases<- Phases %>% mutate(varr = var(perAll))
Phases
VarPha <- Phases %>% arrange(desc(varr))%>% select(CellType,varr) %>% unique()
VarPha
VarPha$CellType <- factor(VarPha$CellType, levels = VarPha$CellType[order(VarPha$varr, decreasing = TRUE)])
VarPha %>% ggplot(aes(x = CellType, y = varr, fill = CellType)) +
  geom_dotplot(binaxis = "y", stackdir = "center") 

#Percent of CellType per CellPhase
Phases %>% ggplot(aes(fill = CellType, y = Count, x = Phase)) +
  geom_bar(position = "fill", stat = "identity")
Phases

HSPC_only$G1<- HSPC_only$Phase == "G1"
HSPC_only$G2M<- HSPC_only$Phase == "G2M"
HSPC_only$S<- HSPC_only$Phase == "S"

#####By Cycling or Not#####
#cycling =  G2M & S
#no cycle = G1
#BaEoMa]=
#add cycle
HSPC_only$cycling <- HSPC_only$Phase %in% c("G2M","S")
HSPC_only$nocycle <- HSPC_only$Phase == "G1"
#now by sample percent
HSPC_only$CALR <- HSPC_only$sample %in% c("SP29","SP30")
HSPC_only$JAK2 <- HSPC_only$sample %in% c("SP22","SP28","SP31")

######Adding by cycle to non-cycling#######
HSPC_only$cycling <- HSPC_only$Phase %in% c("G2M","S")
HSPC_only$nocycle <- HSPC_only$Phase == "G1"

{
HSPC_only$Ba<- HSPC_only$Azimuth_Predicts== "BaEoMa"

table(HSPC_only$cycling[HSPC_only$Ba & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$Ba & HSPC_only$CALR])  
table(HSPC_only$Azimuth_Predicts)
table(HSPC_only$cycling[HSPC_only$Ba & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$Ba & HSPC_only$JAK2])
table(HSPC_only$Ba)
table(HSPC_only$sample)
#CLP
HSPC_only$CLP<- HSPC_only$Azimuth_Predicts== "CLP"

table(HSPC_only$cycling[HSPC_only$CLP & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$CLP & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$CLP & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$CLP & HSPC_only$JAK2]) 
#Early Eryth
HSPC_only$Early<- HSPC_only$Azimuth_Predicts== "Early Eryth"

table(HSPC_only$cycling[HSPC_only$Early & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$Early & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$Early & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$Early & HSPC_only$JAK2])
#EMP
HSPC_only$EMP <- HSPC_only$Azimuth_Predicts == "EMP"

table(HSPC_only$cycling[HSPC_only$EMP & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$EMP & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$EMP & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$EMP & HSPC_only$JAK2])
#GMP
HSPC_only$GMP <- HSPC_only$Azimuth_Predicts == "GMP"

table(HSPC_only$cycling[HSPC_only$GMP & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$GMP & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$GMP & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$GMP & HSPC_only$JAK2])
#HSC
HSPC_only$HSC<- HSPC_only$Azimuth_Predicts == "HSC"

table(HSPC_only$cycling[HSPC_only$HSC & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$HSC & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$HSC & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$HSC & HSPC_only$JAK2]) 
#Late Eryth 
HSPC_only$Late<- HSPC_only$Azimuth_Predicts == "Late Eryth"

table(HSPC_only$cycling[HSPC_only$Late & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$Late & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$Late & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$Late & HSPC_only$JAK2]) 
#LMPP
HSPC_only$LMPP<- HSPC_only$Azimuth_Predicts == "LMPP"

table(HSPC_only$cycling[HSPC_only$LMPP & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$LMPP & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$LMPP & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$LMPP & HSPC_only$JAK2]) 
#pro B
HSPC_only$pB <- HSPC_only$Azimuth_Predicts == "pro B"

table(HSPC_only$cycling[HSPC_only$pB & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$pB & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$pB & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$pB & HSPC_only$JAK2])
#Prog Mk
HSPC_only$PrMk <- HSPC_only$Azimuth_Predicts == "Prog Mk"

table(HSPC_only$cycling[HSPC_only$PrMk & HSPC_only$CALR])  #cycling Prog Mk
table(HSPC_only$nocycle[HSPC_only$PrMk & HSPC_only$CALR])  

table(HSPC_only$cycling[HSPC_only$PrMk & HSPC_only$JAK2]) #nocycle Prog Mk 
table(HSPC_only$nocycle[HSPC_only$PrMk & HSPC_only$JAK2]) #nocycle Prog Mk 
}



table(HSPC_only$sample)
table(Diferente$sample)
table(for_fun$sample)
table()

#####Missing diff per mutant subgroup#######
LeCycling <- tibble(read_csv("LastExcel.csv"))
LeCycling

LeCycling<- LeCycling %>% group_by(Celltype) %>%
  add_count(wt = Count) %>%
  mutate(perAll = (Count/n)*100)
LeCycling
#Add variance
LeCycling<- LeCycling %>% mutate(varr = var(perAll))
LeCycling
VarCirc <- LeCycling %>% arrange(desc(varr))%>% select(Celltype,varr) %>% unique()
VarCirc
VarCirc$Celltype <- factor(VarCirc$Celltype, levels = VarCirc$Celltype[order(VarCirc$varr, decreasing = TRUE)])
VarCirc%>% ggplot(aes(x = Celltype, y = varr, fill = Celltype)) +
  geom_dotplot(binaxis = "y", stackdir = "center") 

View(LeCycling)
LeCycling
#Percent of Celltype per CellPhase
LeCycling %>% ggplot(aes(fill = Celltype, y = Count, x = Phase)) +
  geom_bar(position = "fill", stat = "identity")
t.test(LeCycling$Celltype)
ProgCycle <- LeCycling[c(37:40),]
print(LeCycling, n =36)
t.test(Phase ~ perAll,data = ProgCycle)
ProgCycle%>%shapiro.test(ProgCycle$Phase~ProgCycle$Count)
tabla<-tibble(ProgCycle$Phase, ProgCycle$perAll)
tabla

LeCycling<-LeCycling[-c(33:36),]
LeCycling<-LeCycling[-c(5:8),]
LeCycling
t.test(LeCycling$Phase ~ LeCycling$perAll)
######contingency table#####
finally<-LeCycling%>% select(perAll,Mutant,Phase)
finally
lastone<- matrix(c(7.12,19,23.5,50.4,24.9,21.36,29,24.72,3.76,8.9,16,71.3,10.8,2.12,48.7,38.3,2.17,50.91,7.55,39.38,13.13,34.57,42.8,9.51,27.32,3.44,60.65,8.59,50,32.57,12.6,4.82),ncol = 2 ,byrow = TRUE)
colnames(lastone)<- c("cycling","non-cycling")
rownames(lastone)<- c("CALR","JAK2","CALR","JAK2","CALR","JAK2","CALR","JAK2","CALR","JAK2","CALR","JAK2","CALR","JAK2","CALR","JAK2")
lastone
expected<-chisq.test(lastone)$expected
expected  
chisq.test(lastone)
t.test(LeCycling$Mutant,LeCycling$perAll)

#####Cell Score for Prog Mks and across those samples only####
Diferente <- NormalizeData(Diferente)
Diferente <- FindVariableFeatures(Diferente)
Diferente <- ScaleData(Diferente, features = rownames(Diferente))
Diferente <- RunPCA(Diferente, features = VariableFeatures(Diferente), ndims.print = 6:10, nfeatures.print = 10)
Diferente <- CellCycleScoring(Diferente, s.features = s.genes, g2m.features = g2m.genes, set.ident = TRUE)
head(Diferente[[]])
table(Diferente$Phase)
table(Diferente$sample)
#####Now for ProgMk across samples#####


Diferente$cycling <- Diferente$Phase %in% c("G2M","S")
Diferente$nocycle <- Diferente$Phase == "G1"

Diferente$CALRpmk <- Diferente$sample %in% c("SP29","SP30")
Diferente$JAK2pmk <- Diferente$sample %in% c("SP22","SP28","SP31")

table(Diferente$cycling[Diferente$CALRpmk])
table(Diferente$nocycle[Diferente$CALRpmk])
table(Diferente$cycling[Diferente$JAK2pmk])
table(Diferente$nocycle[Diferente$JAK2pmk])

tab<- matrix(c(401,352,108,51),ncol = 2 ,byrow = TRUE)
colnames(tab)<- c("cycling","non-cycling")
rownames(tab)<- c("CALR","JAK2")
tab
expected<-chisq.test(tab)$expected
chisq.test(tab)
DimPlot(Diferente,reduction = "umap",group.by = "Phase", split.by = "sample")
#only for cycle
tab<- tibble(tab)
tab
colnames(tab) <- c("cycling","no cycle")


#cycling only
df<- tibble(Mutant =c("CALR","CALR","JAK2","JAK2"), Phase = c("cycling","no cycle","cycle","no cycle") , Count = c(401,352,108,51))
df
tab
#adding the percent
df<- df%>%group_by(Mutant) %>% add_count(wt = Count) %>% mutate(perAll = (Count/n)*100)
df
chisq.test(df$perAll,df$Mutant)
t.test(df$perAll,df$Mutant)
#####NOw we do another Chi-sq
jeje<- df %>% select(perAll,Mutant,Phase)
jeje
t.test(df$Mutant,df$perAll)
df
#
df
t.test(data = df, perAll~Mutant)
teb<- matrix(c(53.3,46.7,67.9,32.1),ncol = 2 ,byrow = TRUE)
teb
colnames(teb)<- c("cycling","non-cycling")
rownames(teb)<- c("CALR","JAK2")
chisq.test(teb)

#####instructions#####

#Prog Mk (in cycles)
##samples 29 & 30 vs 22,31,28
#looking at diff, exp genes
#split by mutational status?
#G2 & M cycling. G1 not cycling. maybe related to prog Mk
#bar chart, quantify. by celltype and sample %. Bar chart for sure. 
#group 1 , group 2
