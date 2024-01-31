install.packages("Seurat")
install.packages("styler")
install.packages("Azimuth")
install.packages("SeuratData")
 
devtools::install_github("satijalab/seurat", "seurat5")
devtools::install_github("satijalab/seurat-data", "seurat5")
devtools::install_github("satijalab/azimuth", "seurat5")
######
# remotes::install_github('satijalab/azimuth', ref = 'master')

library(styler)
library(patchwork)
library(Seurat)
library(tidyverse)
library(ggplot2)
library(SeuratData)
library(Azimuth)
# Batch_Effect <- readRDS("/brahms/kowalskim/data/share/elias/healthy_mf_spleen_seurat_obj.Rds")
class(Batch_Effect)
dim(Batch_Effect)
str(Batch_Effect)
length(Batch_Effect$nCount_RNA)
Batch_Effect <- SplitObject(Batch_Effect, split.by = "orig.ident")
######
(Batch_Effect$orig.ident)
#####
Batch_Effect <- lapply(X = Batch_Effect, FUN = function(x) {
  x <- NormalizeData(x)
  x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 2000)
})

features <- SelectIntegrationFeatures(object.list = Batch_Effect)
##### 2 Perform Integration ####
Batch.Anchors <- FindIntegrationAnchors(
  object.list = Batch_Effect,
  anchor.features = features
)
Batch_Effect <- IntegrateData(anchorset = Batch.Anchors)

##### 3 Perform integrated analysis####
DefaultAssay(Batch_Effect) <- "integrated"
Batch_Effect <- ScaleData(Batch_Effect, verbose = FALSE)
Batch_Effect <- RunPCA(Batch_Effect, npcs = 30, verbose = FALSE)
Batch_Effect <- RunUMAP(Batch_Effect, reduction = "pca", dims = 1:30)
Batch_Effect <- FindNeighbors(Batch_Effect, reduction = "pca", dims = 1:30)
Batch_Effect <- FindClusters(Batch_Effect, resolution = 0.5)
##### 4 Visualization #####
p1 <- DimPlot(Batch_Effect, reduction = "umap", group.by = "orig.ident")
p2 <- DimPlot(Batch_Effect, reduction = , label = TRUE, repel = TRUE)
p1
p2
p1 + p2
Splitss <- DimPlot(Batch_Effect, reduction = "umap", split.by = "orig.ident", label = TRUE)

##### 5 Identify conserved cell type markers#####
DefaultAssay(Batch_Effect) <- "RNA"

# Markers by cluster _Part of 5

##### Graphing It####
Batch_Marker <- FindAllMarkers(Batch_Effect, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
Batch_Marker %>%
  group_by(cluster) %>%
  top_n(n = 10, wt = avg_log2FC) -> top10
top10
Do_Gene <- Batch_Marker %>%
  group_by(cluster) %>%
  slice_max(n = 3, order_by = avg_log2FC)
Batch_Effect <- ScaleData(Batch_Effect, features = top10$gene)
length(intersect(top10$gene, rownames(Batch_Effect[["RNA"]]@scale.data)))
length(top10$gene)
DoHeatmap(Batch_Effect, features = top10$gene)
dev.off()

meta <- Batch_Effect@meta.data

p <- DoHeatmap(Batch_Effect, features = top10$gene)
ggsave(filename = "heatmap.png", plot = p, width = 18, height = 16)


##### Start Annotation###
###### Cluster 0-done stuff####
FeaturePlot(Batch_Effect, features = c(" "))
VlnPlot(Batch_Effect, features = c(""))
# c0= CD8-, CD3+ T
# BC the C1 right next to it has the same but adding in the CD8+ markers
##### Cluster 1-done Stuff#####
FeaturePlot(Batch_Effect, features = c("CCL5", "CCL4", "NKG7", "CD8A", "CCL4L2", "CD3G", "CD3D", "CD3E"), label = TRUE)
VlnPlot(Batch_Effect, features = c("NKG7", "CD8A", "CCL4L2", "CD3G", "CD3D", "CD3E"))
# c1 =CD8+ T (natural-killer T cells??? or cytotoxic t cells)

##### Cluster 2-doone? Stuff#####
FeaturePlot(Batch_Effect, features = c("IGHM", "IGHD", "MS4A1", "CD79A", "IGKC", "BANK1"))
c2_onnlly <- FindMarkers(Batch_Effect, ident.1 = 2, ident.2 = 4, min.pct = 0.25)
VlnPlot(Batch_Effect, features = c("AIM2", "RPS12", "RPS14", "RPS18", "CD27"))

# some sort of B cell with cluster4,10,12

##### Cluster 3- done? Stuff#####
VlnPlot(Batch_Effect, features = c("AVP", "MLLT3", "SOX4", "FAM30A", "NRIP1", "C1QTNF4", "CRHBP", "CD34"))
FeaturePlot(Batch_Effect, features = c("AVP", "MLLT3", "SOX4", "FAM30A", "NRIP1", "C1QTNF4", "CRHBP", "CD34", "ELANE"))

FeaturePlot(Batch_Effect, c("ACY3", "PRSS2", "C1QTNF4", "SPINK2", "SMIM24", "NREP", "CD34", "DNTT", "FLT3", "SPNS3"))

# HSPC cells most likely
# B or progenitor cells/hematopoietic cells crhbp for b cells
# 3,6,14
##### Cluster 4-done Stuff#####
VlnPlot(Batch_Effect, features = c("LINC01781", "MARCKS", "BANK1", "PAX5", "HLA-DOA1", "SCIMP"))
c4_only <- FindMarkers(Batch_Effect, ident.1 = 4, ident.2 = c(2, 10, 12, 17), min.pct = 0.25)

# B cells yup
# B with c2,10,12
##### Cluster 5-done Stuff#####
VlnPlot(Batch_Effect, features = c("S100A8", "S100A9", "LYZ", "FCN1", "CST3", "CD163"))
FeaturePlot(Batch_Effect, features = c("S100A8", "S100A9", "LYZ", "FCN1", "CST3", "CD163", "CD16", "CD14", "S100B", "S100A4", "ELANE"))
# 2 subcluster ( one with c14)
# c5 = Monocytes for SURE
####### Cluster 6-done? #####
VlnPlot(Batch_Effect, features = c("APOC1", "APOE", "HBD", "SLC40A1", "GIHCG", "PRKAR2B"))
FeaturePlot(Batch_Effect, features = c("HBB", "HBD", "GATA1", "GATA2")) # Hemoglobin rhings (erythrocytes)
# PC cells
# Diff subpopulations of B cells and PC cells
# FCER1A is Bcell marker
####### Cluster 7 -done?#####
VlnPlot(Batch_Effect, features = c("BCL11B", "SYNE2", "CD2", "TTN", "HSPA1B", "HSPA6"))
FeaturePlot(Batch_Effect, features = c("BCL11B", "SYNE2", "CD2", "TTN", "HSPA1B", "HSPA6", "VMP1", "NEU1", ""))
c7_only <- FindMarkers(Batch_Effect, ident.1 = 7, ident.2 = c(0, 1, 13, 10, 15))
-
  # Unsure if B or CD sth cells, no particular expression
  # basically c0 + 1
  ####### Cluster 8 -done?#####
  VlnPlot(Batch_Effect, features = c("XCL2", "XCL1", "NKG7", "IL2RB", "KLRD1", "CCL3", "GNLY"))
VlnPlot(Batch_Effect, features = c("CD56"))
# NK cells
# T-cells, don't know of what kind
# 2 subclusters, one with 13
####### Cluster 9 -done?#####
VlnPlot(Batch_Effect, features = c("BLVRB", "HBG2", "MKI67", "HIST1H4C", "HIST1H1B", "CENPF", "TOP2A"))

# 1B is a good PC gene marker (plasma-don/platelet)
# PC cells
# 9,11,6,14
####### Cluster 10 - done#####
c10 <- FindMarkers(Batch_Effect, ident.1 = 10) %>% head(n = 5)

VlnPlot(Batch_Effect, features = c("CD79A", "MS4A1", "SWAP70", "FCRL1", "FCRL2", "HSPA1B", "HSPB1"))
FeaturePlot(Batch_Effect, features = c("CD79A", "MS4A1", "SWAP70", "FCRL1", "FCRL2", "HSPA1B", "HSPB1"))
# Some Sort of B cell
# With a HES4 only cluster
####### Cluster 11 - done#####
VlnPlot(Batch_Effect, c("PPBP", "PF4", "GP1BB", "ITGA2B", "TIMP3", "PLEK", "NAA38", "CAVIN2"))
FeaturePlot(Batch_Effect, c("PPBP", "PF4", "GP1BB", "ITGA2B", "TIMP3", "PLEK", "NAA38", "CAVIN2"))




# platelets?
####### Cluster 12 -done #####
VlnPlot(Batch_Effect, c("HES4", "CR1", "CR2", "GPR183", "LILRA1"))


# B cells
####### Cluster 13 -done #####
VlnPlot(Batch_Effect, features = c("GZMB", "GZMA", "GNLY", "NKG7", "ARL4C", "FCGR3A", "KLRD1", "SPON2"))

# NK Cells
####### Cluster 14-done granular mono #####
VlnPlot(Batch_Effect)
FeaturePlot(Batch_Effect, c("SERPINB10", "RNASE3", "MS4A3", "PRTN3", "ELANE", "AZU1", "CTSG", "RNASE2", "RETN", "NPW"))
# monocytes with c5

####### Cluster 15 -done#####
FeaturePlot(Batch_Effect, c("BLVRB", "HBD", "HBG2", "HBB", "CA1", "HBA2", "HBA1", "HBG1", "AHSP", "SLC4A1", "HBM"))

VlnPlot(Batch_Effect, c("BLVRB", "HBD", "HBG2", "HBB", "CA1", "HBA2", "HBA1", "HBG1", "AHSP", "SLC4A1", "HBM"))
# HBA1, late erythrocites, I HAVE BETTER MARKERS :D
# Bilirubin reductase B, cancer gene? Novel therapeutic cancer marker
####### Cluster 16 #####
VlnPlot(Batch_Effect, c("TIMP3", "CAVIN2", "PLEK", "ITGA2B", "CLC", "GATA2", "FCER1A", "EREG", "GLUL", "PRG2", "ALAS1", "CPA3", "TPSAB1"))
FeaturePlot(Batch_Effect, c("TIMP3", "CAVIN2", "PLEK", "ITGA2B", "CLC", "GATA2", "FCER1A", "EREG", "GLUL", "PRG2", "ALAS1", "CPA3", "TPSAB1"))

# basophil
####### Cluster 17 #####




####### Cluster 19-done- #####

VlnPlot(Batch_Effect, c("MS4A1", "FCRL5", "FCRL3", "FCRL1", "FCRL2", "IGHG1", "IGHG3", "IGKC", "IGLC1", "JCHAIN", "IGLC2", "IC22"))
FeaturePlot(Batch_Effect, c("MS4A1", "FCRL5", "FCRL3", "FCRL1", "FCRL2", "IGHG1", "IGHG3", "IGKC", "IGLC1", "JCHAIN", "IGLC2", "IC22"))
# naive B cells


####### Cluster 20 -done#####
VlnPlot(Batch_Effect, features = c("IGHM", "RGCC", "FCRL5", "FCRL3", "JCHAIN", "IGKC", "IGHG1", "IGHG3"))
FeaturePlot(Batch_Effect, c("IGHM", "FCRL5", "FCRL3", "JCHAIN", "IGKC", "IGHG1", "IGHG3"))
# Mature B cells or plasma
# Plasmablasts for sure, or mature B cells
# IGKC is platelet
# Immunoglobulin or platelet or plasmoblast
####### Cluster 21-done #####
VlnPlot(Batch_Effect, features = c("C1QC", "C1QB", "SELENOP", "APOE", "C1QA", "CD5L", "VCAM1", "FCGR3A", "HMOX1"))
FeaturePlot(Batch_Effect, features = c("C1QC", "C1QB", "SELENOP", "APOE", "C1QA", "CD5L", "VCAM1", "FCGR3A", "HMOX1"))
# macrophage
#####
# Name the Clusters
Batch_Effect <- FindSubCluster(Batch_Effect,
  cluster = 0,
  graph.name = "integrated_snn"
)
DimPlot(Batch_Effect, group.by = "sub.cluster", label = TRUE)
Dim
Batch_Effect <- FindSubCluster(Batch_Effect,
  cluster = ,
  graph.name = "integrated_snn"
)
Batch_Effect@meta.data$sub.cluster <- "seurat_clusters"
Idents(Batch_Effect) <- "seurat_clusters"
labels(Batch_Effect@meta.data)
#####
# 12 and 4 are the same for sure

saveRDS(Batch_Effect, file = "SaveIfGoesWrong.RDS")
DimPlot(Batch_Effect, reduction = "umap", group.by = "sample")
# ARL4c is a cancer-like gene,CRHBP too
# multiple subcluster in 1,8 and 13. NKT cells somewhere for sure, and cytotoxic cells too
# Bank1 is mostly for B memory cells
rm(Batch_Effect)

### Now by Cell type###

###### Testing for Common Lymphoid Progenitor#####
# Wtf is this (CLPs)
# Personalized Markers ig FeaturePlot(Batch_Effect,c("CNRIP1","GATA2","ITGA2B","TFR2","GATA1","KLF1","CYTL1","MAP7","FSCN1","APOC1","PPBP","PF4","GP1BB","PLEK"))

# Canonical Markers
FeaturePlot(Batch_Effect, c("ACY3", "PRSS2", "C1QTNF4", "SPINK2", "SMIM24", "NREP", "CD34", "DNTT", "FLT3", "SPNS3"))
VlnPlot(Batch_Effect, c("ACY3", "PRSS2", "C1QTNF4", "SPINK2", "SMIM24", "NREP", "CD34", "DNTT", "FLT3", "SPNS3"))
###### For Early Erythroid##### -maybe 16#####

FeaturePlot(Batch_Effect, c("CNRIP1", "GATA2", "ITGA2B", "TFR2", "GATA1", "KLF1", "CYTL1", "MAP7", "FSCN1", "APOC1"))
VlnPlot(Batch_Effect, c("CNRIP1", "GATA2", "ITGA2B", "TFR2", "GATA1", "KLF1", "CYTL1", "MAP7", "FSCN1", "APOC1"))

###### For Erythroid Megakaryocyte Progenitor####

FeaturePlot(Batch_Effect, c("MYCT1", "CRHBP", "NPR3", "AVP", "GATA2", "HPGDS", "CYTL1", "CRYGD", "IGSF10", "PBX1"))

###### For granulocyte monocyte progenitor cell#####
# c14 most likely
FeaturePlot(Batch_Effect, c("SERPINB10", "RNASE3", "MS4A3", "PRTN3", "ELANE", "AZU1", "CTSG", "RNASE2", "RETN", "NPW"))


###### For hematopoietic stem cell #####
FeaturePlot(Batch_Effect, c("CRHBP", "AVP", "MYCT1", "BEX1", "NPR3", "CRYGD", "MSRB3", "CD34", "NPDC1", "MLLT3"))
# not enough supporting evidence

###### For Late Erythroid	####
FeaturePlot(Batch_Effect, c("CTSE", "TSPO2", "IFIT1B", "TMEM56", "RHCE", "RHAG", "SPTA1", "ADD2", "EPCAM", "HBG1"))
VlnPlot(Batch_Effect, c("CTSE", "TSPO2", "IFIT1B", "TMEM56", "RHCE", "RHAG", "SPTA1", "ADD2", "EPCAM", "HBG1"))

###### FOr Lymphoid Primed Multipotent Progenitor######

FeaturePlot(Batch_Effect, c("AVP", "CRHBP", "C1QTNF4", "BEX1", "NPR3", "CD34", "NPW", "SMIM24", "CSF3R", "NPDC1"))

###### For Progenitor Megakaryocyte #####
FeaturePlot(Batch_Effect, c("CLEC1B", "SPX", "WFDC1", "ANXA3", "CMTM5", "SELP", "RBPMS2", "ARHGAP6", "GP9", "LTBP1"))

# subcluster in 11
##### For Basophil Eosinophil Mast Progenitor ####
FeaturePlot(Batch_Effect, c("HDC", "MS4A2", "TPSAB1", "TPSB2", "CLC", "CPA3", "MS4A3", "IL1RL1", "PRG2", "HPGDS"))
# c16 for sure

# All signals on the left
FeaturePlot(Batch_Effect, features = c("HBB", "HBD", "GATA1", "GATA2", "SOX4", "SOX9", "DNTT", "ELANE", "CD34"))

##### COmmon Dendritic Progenitor######--NO EVIDENCE----#####
# no supporting evidence to comDC
FeaturePlot(Batch_Effect, c("ENHO", "CLEC10A", "RNASE2", "PLBD1", "FCER1A", "IGSF6", "MNDA", "SAMHD1", "ALDH2", "PAK1"))
###### Progenitor B######---NO EVIDENCE######
FeaturePlot(Batch_Effect, c("CYGB", "UMODL1", "EBF1", "MME", "VPREB1", "DNTT", "IGLL1", "UHRF1", "BLNK", "AGPS"))

##### PLASMA######
FeaturePlot(Batch_Effect, c("MZB1", "JCHAIN", "TNFRSF17", "ITM2C", "DERL3", "TXNDC5", "POU2AF1", "IGHA1", "TXNDC11", "CD79A"))


###### Platelet#####
# bone marrows
FeaturePlot(Batch_Effect, c("RGS18", "C2orf88", "TMEM40", "GP9", "PF4", "PPBP", "DAB2", "SPARC", "RUFY1", "F13A1"))
# pbmc cells
FeaturePlot(Batch_Effect, c("GNG11", "PPBP", "NRGN", "PF4", "CAVIN2", "TUBB1", "HIST1H2AC", "PRKAR2B", "CLU", "F13A1"))
# some platelets in c11
##### Stromal CElls####
FeaturePlot(Batch_Effect, c("IGFBP5", "CP", "TF", "DCN", "LUM", "COL1A2", "CRH", "TNC", "SPARCL1", "BMP5"), label = TRUE)


##### T Proliferating#####
FeaturePlot(Batch_Effect, c("RRM2", "MKI67", "BIRC5", "LCK", "SH2D1A", "CLSPN", "CDT1", "TK1", "TOP2A", "TRAC"), label = TRUE)
##### INnate Lymphoid####
FeaturePlot(Batch_Effect, c("LDB2", "CD160", "XCL2", "KLRB1", "XCL1", "KLRF1", "KLRD1", "KLRC1", "TRDC", "GZMM"))
##### Plasmacytoid Dendritic Cells#####
FeaturePlot(Batch_Effect, c("ASIP", "CLEC4C", "SCAMP5", "PROC", "PTCRA", "SCT", "SHD", "PPM1J", "LILRA4", "LRRC26"))
############################################## NOW NAMING THEM####################################
Idents(Batch_Effect) <- "integrated_snn_res.0.5"
ba
# Adding column to metadata
cluster_celltype_plot <- paste0("Cluster ", 0:21)
cluster_celltype_plot <- setNames(paste0("Cell Type ", 0:21), cluster_celltype_plot)
Batch_Effect$celltype <- cluster_celltype_plot[Batch_Effect$integrated_snn_res.0.5]
View(Batch_Effect$celltype)

Idents(Batch_Effect) <- "seurat_clusters" # do this more often to update the shit

# I don't know how but I fixed it/.......
###### Sub Clustering #####
Idents(Batch_Effect) <- Batch_Effect$integrated_snn_res.0.5
Batch_Effect <- FindSubCluster(Batch_Effect,
  cluster = 30, graph.name = "sub.cluster.3",
  subcluster.name = "sub.cluster.3"
)

DimPlot(Batch_Effect, group.by = "sub.cluster.11", label = TRUE)
##### Differentially Express Clusters ######

c3_if <- FindMarkers(Batch_Effect, ident.1 = 3, logfc.threshold = 0.25, test.use = "roc", only.pos = TRUE)
c9_sub <- FindMarkers(Batch_Effect, ident.1 = "9_0", ident.2 = "9_1", min.pct = 0.25) # , group.by="sub.cluster.9")

Idents(Batch_Effect) <- "sub.cluster"

DimPlot(Batch_Effect, group.by = "sub.cluster.6", label = TRUE, label.size = 2)

###### Primed Markers for cell types####

# EMP (GATA2,ZEB2, GATA1, CNRIP1,SLC40A1,CSF2RB,PHTF1,DGKE,MYCT1,PDZD8,PBX1,TAF1D,PRKACB)
FeaturePlot(Batch_Effect, c("GATA2", "ZEB2", "GATA1", "CNRIP1", "SLC40A1", "CSF2RB", "PHTF1", "DGKE", "MYCT1", "PDZD8", "PBX1", "TAF1D", "PRKACB"))

# Lympho (MME,MEF2A)
# LMPP (FLT3, CSF3R, SELL, CD99)
# FLT3, CSF3R, SELL, CD99


# Naming Clusters#####
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 0] <- "CD4+ T cells"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 1] <- "CD8+ T cells"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 2] <- "B"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 3] <- "HSPC"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 4] <- "B"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 5] <- "Mono"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 6] <- "X-change"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 7] <- "CD4 T cells"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 8] <- "NK"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 9] <- "Early erith"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 10] <- "B"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 11] <- ""
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 12] <- "B"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 13] <- "NK"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 14] <- "GMP"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 15] <- "Late eryth"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 17] <- "B"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 18] <- "B"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 19] <- "Naive B"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 20] <- "Plasma"
Batch_Effect$celltype[Batch_Effect$integrated_snn_res.0.5 == 21] <- "Macrophages"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_3"] <- "Prog Mk"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_5"] <- "Prog Mk"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_2"] <- "Prog Mk"
Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_2"] <- "Prog Mk"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_7"] <- "Prog Mk"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_6"] <- "Prog Mk"

Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_0"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_1"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_4"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.11 == "11_8"] <- "Early eryth"



Batch_Effect$celltype[Batch_Effect$sub.cluster.16 == "16_2"] <- "BaEoMa"
Batch_Effect$celltype[Batch_Effect$sub.cluster.16 == "16_3"] <- "BaEoMa"

Batch_Effect$celltype[Batch_Effect$sub.cluster.16 == "16_4"] <- "BaEoMa"
Batch_Effect$celltype[Batch_Effect$sub.cluster.16 == "16_0"] <- "BaEoMa"
Batch_Effect$celltype[Batch_Effect$sub.cluster.16 == "16_1"] <- "BaEoMa"

Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_1"] <- "HSC" # change to HSC
Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_4"] <- "HSC" # change to HSC
Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_2"] <- "HSC"
Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_3"] <- "HSC"
Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_5"] <- "HSC"

Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_6"] <- "EMP"
Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_7"] <- "EMP"
Batch_Effect$celltype[Batch_Effect$sub.cluster.3 == "3_0"] <- "EMP"

#
Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_0"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_4"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_5"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_1"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_2"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_6"] <- "Early eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_7"] <- "Early eryth"

Batch_Effect$celltype[Batch_Effect$sub.cluster.6 == "6_3"] <- "Late eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_0"] <- "Late eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_1"] <- "Late eryth"

Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_3"] <- "Late eryth"
Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_4"] <- "LMPP"
Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_5"] <- "LMPP"
Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_6"] <- "T Proliferating"
Batch_Effect$celltype[Batch_Effect$sub.cluster.9 == "9_7"] <- "Plasma"

Idents(Batch_Effect) <- "celltype"



###### Ask Mentor#####
# Should I subset the clusters? How do I filter for better cell type differentiation
# c3 is CLP or hematopostem

######
DimPlot(Batch_Effect, group.by = "celltype", label = TRUE, repel = TRUE)

DimPlot(Batch_Effect, group.by = "sub.cluster.9", label = TRUE, label.size = 2)
###### AZIMUTH CROSS REFERENCING######


AvailableData()
InstallData("bonemarrowref")
library(bonemarrowref)

BackUp <- Batch_Effect
list.files("/tmp/RtmpKFVcNa/downloaded_packages")
###### Azimuth and Comparisons#####

wtf <- readRDS("/tmp/RtmpKFVcNa/downloaded_packages/bonemarrowref.SeuratData_1.0.0.tar.gz")
Azimuth_Ref <- RunAzimuth(Batch_Effect, reference = "bonemarrowref")
View(Azimuth_Ref@meta.data)
Az1 <- DimPlot(Batch_Effect, group.by = "celltype", label = TRUE, label.size = 3) + NoLegend()
Az2 <- DimPlot(Batch_Effect, group.by = "Azimuth_Predicts", label = TRUE, label.size = 2)
## Side-by-side label comparison##
Az1 + Az2


######## Visualizing genes######
Azimuth_Ref <- NormalizeData(Azimuth_Ref)
Idents(Batch_Effect) <- "predicted.celltype.l2"
?table

t1 <- FeaturePlot(Batch_Effect, features = "AVP", Idents = c("HSC,LMPP"))
t1
t2 <- FeaturePlot(Batch_Effect, features = "MLLT3")
t3 <- VlnPlot(Batch_Effect, features = "AVP", group.by = "Azimuth_Predicts", idents = c("HSC,LMPP"))
t3
Idents(Batch_Effect) <- Batch_Effect$celltype
t4 <- FeaturePlot(Batch_Effect, features = "Azimuth_Score", label = TRUE)
t4
t1 + t2 + t3 + t4 + plot_layout(ncol = 2)

as.table(Azimuth_Ref@meta.data$predicted.celltype.l2.score, Azimuth_Ref@meta.data$predicted.celltype.l2)


# I want to see how did the Pro-B cells went down there, there ain't even B cells there

##### Rahul Graphics#####
# Compare my anot to Azimuth
# side-by-side umap (mine-Azimuth)
Az1 <- DimPlot(Batch_Effect, group.by = "celltype", label = TRUE, label.size = 3)
Az2 <- DimPlot(Batch_Effect, group.by = "Azimuth_Predicts", label = TRUE, label.size = 2)

Az1 + Az2

##### CELLS####
Idents(Batch_Effect) <- Batch_Effect$integrated_snn_res.0.5

# GMP

FeaturePlot(Batch_Effect, c("AZU1", "PRTN3"), label = TRUE, label.size = 5, repel = TRUE)
# Plasmacytoid Dendritic Cells
FeaturePlot(Batch_Effect, c("LRRC26", "CLEC4C"), label = TRUE, label.size = 5, repel = TRUE)
#B cells
FeaturePlot(Batch_Effect, c("MS4A1","CD79A"), label = TRUE, label.size = 5, repel = TRUE)

#NK

FeaturePlot(Batch_Effect,c("GNLY","KLRF1"))

#T
FeaturePlot(Batch_Effect, c("CD3D","IL7R"), label = TRUE, label.size = 5, repel = TRUE)
#MONocyte
FeaturePlot(Batch_Effect, features = c("LYZ", "S100A9"),label = TRUE,label.size = 5,repel = TRUE)
#macro
FeaturePlot(Batch_Effect,c("SELENOP","C1QB"),label = TRUE,label.size = 5,repel = TRUE)
#HSPC
FeaturePlot(Batch_Effect,c("SLC4A10","KLRB1","IL7R","TNFRSF25","RORA","SPOCK2","MAF","GZMK","ZFP36L2","LYAR"))
###### Work on After Wednesday#####
# Plotting both annotations by cell type and by prediction -Need to Refine#



# Azimuth anottations by sample
comp_Azi <- as.data.frame(table(Batch_Effect$Azimuth_Predicts, Batch_Effect$sample))
comp_Azi
colnames(comp_Azi) <- c("celltype", "Sample", "Cells")

# Azimuth annotations overall
all_Azi <- as.data.frame(table(Batch_Effect$Azimuth_Predicts))
all_Azi
colnames()
ggplot(data = Azi, mapping = aes(x = ))

# My anottations by sample
comp_Eli <- as.data.frame(table(Batch_Effect$celltype, Batch_Effect$sample))
comp_Eli
colnames(comp_Eli) <- c("celltype", "Sample", "Cells")

# My anottations overall
all_Eli <- as.data.frame.array(table(Batch_Effect$celltype))
all_Eli


##### Table Rahul#####
t_orig <- as.data.frame(table(Batch_Effect@meta.data$orig.ident))
colnames(t_orig) <- c("orig.ident", "cells")
delete1 <- sum(t_orig$`# cells`)
n_row1 <- data.frame(orig.ident = "Total", cells = delete1)
t_orig <- rbind(t_orig, n_row1)
t_orig
##
t_sample <- as.data.frame(table(Batch_Effect@meta.data$sample))
colnames(t_sample) <- c("Sample", "cells")
delete2 <- sum(t_sample$cells)
n_row2 <- data.frame(Sample = "Total", cells = delete2)
t_sample <- rbind(t_sample, n_row2)
t_sample
# table 2

##### Graph Rahul#####

# Mine
comp_Eli <- comp_Eli %>% filter(celltype != "B" & celltype != "Naive B" & celltype != "CD4+ T cells" & celltype != "CD8+ T cells" & celltype != "NK" & celltype != "CD4 T cells")

comp_Eli %>% ggplot(aes(fill = celltype, y = Cells, x = Sample)) +
  geom_bar(position = "dodge", stat = "identity")+ theme_bw()
  #facet_wrap(~Sample)

comp_Eli %>% ggplot() +
  geom_col(comp_Eli, mapping = aes(x = Sample, y = Cells, fill = celltype))


comp_Eli %>% ggplot(aes(fill = celltype, y = Cells, x = Sample)) +
  geom_bar(position = "fill", stat = "identity") #switch pos to dodge if needed
comp_Eli

######################################################################################## Azimuth
comp_Azi <- comp_Azi %>% filter(celltype != "B" & celltype != "Naive B" & celltype != "CD4+ T cells" & celltype != "CD8+ T cells" & celltype != "NK" & celltype != "CD4 T cells")

comp_Azi %>% ggplot(aes(fill = celltype, y = Cells, x = Sample)) +
  geom_bar(position = "dodge", stat = "identity")
comp_Azi %>% ggplot() +
  geom_col(comp_Azi, mapping = aes(x = Sample, y = Cells, fill = celltype))


comp_Azi %>% ggplot(aes(fill = celltype, y = Cells, x = Sample)) +
  geom_bar(position = "dodge", stat = "identity")
comp_Azi %>%
  ggplot(data, aes(fill = condition, y = value, x = specie)) +
  geom_bar(position = "dodge", stat = "identity")

##### Fixing up my own annotations w/Azimuth####

# Create column to add to SeuratObject (Batch_Effect)
Azimuth_Predicts <- Azimuth_Ref@meta.data$predicted.celltype.l2
# Add the column and then plot the references on my own UMAP

Batch_Effect$Azimuth_Predicts <- Azimuth_Predicts

Azimuth_Score <- Azimuth_Ref@meta.data$predicted.celltype.l2.score

Batch_Effect$Azimuth_Score <- Azimuth_Score
###### Experimenting#####

exp <- as.data.frame(Batch_Effect$Azmimuth_Score, Batch_Effect$Azimuth_Predicts)

exp <- exp %>% filter(`Batch_Effect$Azmimuth_Score` > 0.7)

exp
