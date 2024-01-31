install.packages("Seurat")
library(Seurat)
library(tidyverse)
library(ggplot2)
library(SeuratData)
library(patchwork)
getRversion()
Batch_Effect<- readRDS("/brahms/kowalskim/data/share/elias/healthy_mf_spleen_seurat_obj.Rds")
class(Batch_Effect)
dim(Batch_Effect)
str(Batch_Effect)
length(Batch_Effect$nCount_RNA)
Batch_Effect
#I'm pretty sure this needs to be also split by healthy and MF [Overall Working]
Batch_Effect[["percent.mt"]] <- PercentageFeatureSet(Batch_Effect, pattern = "^MT-")
head(Batch_Effect@meta.data,5)
VlnPlot(Batch_Effect, features = c("nFeature_RNA","nCount_RNA","percent.mt"),ncol =3)
FeatureScatter(Batch_Effect,feature1 = "nCount_RNA", feature2 = "nFeature_RNA") + geom_smooth() 
meta<- Batch_Effect@meta.data
summary(meta)
plot1 <- FeatureScatter(Batch_Effect, feature1 = "nFeature_RNA",feature2 = "percent.mt") + geom_smooth()
plot2 <- FeatureScatter(Batch_Effect, feature1 = "nCount_RNA", feature2 = "nFeature_RNA") + geom_smooth()
plot1 + plot2
plot1
#Filter by Feat_RNA & %.mt
Batch_Effect <- subset(Batch_Effect, subset =nFeature_RNA >500 & nFeature_RNA<4600 &percent.mt <12.5)
Batch_Effect
VlnPlot(Batch_Effect, features = c("nFeature_RNA","nCount_RNA","percent.mt"),ncol =3)
##### 3 Normalize Data#####
Batch_Effect<- NormalizeData(Batch_Effect)
##### 4 Identify variable features #####
Batch_Effect <- FindVariableFeatures(Batch_Effect, nfeatures = 10000)
#Identify the 10 most highly variable genes
tOOp10 <- head(VariableFeatures(Batch_Effect),10)
tOOp10
#plot variable features with and without labels
plot_var <- VariableFeaturePlot(Batch_Effect)
plot_var2 <- LabelPoints(plot = plot_var, points = tOOp10, repel = TRUE)
plot_var +plot_var2

#####5 Scale Data ####
all.genes <- rownames(Batch_Effect)
Batch_Effect <- ScaleData(Batch_Effect, features = all.genes)
Batch_Effect

#####6 Perform Linear Dimensional Reduction####
Batch_Effect <- RunPCA(Batch_Effect, features = VariableFeatures(object = Batch_Effect))
print(Batch_Effect[["pca"]], dims = 1:5 ,nfeature = 5)
VizDimLoadings(Batch_Effect, dims = 1:2)
DimPlot(Batch_Effect, reduction = "pca")
DimHeatmap(Batch_Effect, dims = 1, cells = 500,balanced = TRUE)
DimHeatmap(Batch_Effect, dims = 1:15, cells = 500,balanced = TRUE)

#####7 Determine the dimensionality of the dataset####
ElbowPlot(Batch_Effect) 
#####8 Cluster Cells ####
Batch_Effect <- FindNeighbors(Batch_Effect, dims =1:10)
Batch_Effect <- FindClusters(Batch_Effect, resolution = 0.5)
#Look at cluster IDs of the first 5 cells
head(Idents(Batch_Effect),5)
#####9 non-linear dimensional reduction UMAP####
attach(Batch_Effect)
Batch_Effect <- RunUMAP(Batch_Effect, dims = 1:10)

d1<-DimPlot(Batch_Effect, reduction = "umap", label = FALSE)
d1
d2<-DimPlot(Batch_Effect, reduction = "umap", group.by = "orig.ident") + labs(title = "Looks like batch effect to me")
DimPlot(Batch_Effect, reduction = "umap", group.by = "sample")
d1+d2
#batch-specific clusters which is a NO NO
# There is in fact a batch effect
Batch_Effect

#Batch_Effect has been fixed
dev.off()









#It is impossible to create a Seurat object from a Seurat Object
#healthy_spleen <- CreateSeuratObject(counts = healthy_spleen, project = "healthy spleen", min.cells = 3,min.features = 200)
##### 2 Standard pre-processing workflow####
attach(By_Tissue)
head(healthy_spleen@meta.data,5)
VlnPlot(healthy_spleen,features = c("nFeature_RNA","nCount_RNA","percent.mt"),ncol =3)
FeatureScatter(healthy_spleen,feature1 = "nCount_RNA", feature2 = "nFeature_RNA") + geom_smooth() 
plot(healthy_spleen@meta.data)
plot1 <- FeatureScatter(healthy_spleen,feature1 = "nCount_RNA", feature2 = "percent.mt")
plot2 <- FeatureScatter(healthy_spleen,feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
plot1 + plot2
See<- healthy_spleen@meta.data %>% arrange(desc(percent.mt))
#Filtrate featureRNA & percent.mt
healthy_spleen <- subset(By_Tissue$healthy_spleen, subset =nFeature_RNA >500 & nFeature_RNA<4500 &percent.mt <13)
VlnPlot(healthy_spleen,features = c("nFeature_RNA","nCount_RNA","percent.mt"),ncol =3)
healthy_spleen
#Should I do an SCT or a Data Normalization test here? Ask mentor


#try unintegrated analysis ...Batch_Effect vignette
#check if there seems to be clustering by batch effects 
# batch  = orig.ident in meta data 
#are there any batch-specific clusters? 

#can check if any clusters have high mitochondrial genes or 
#higher nCount_RNA...this might be sign of batch effects

#if so...can can use integration 
#integrated by sample OR batch (orig.ident)

#after integration (or not), can start annotating cell types

#SPN11, SPN-NYU1, SPN3 are healthy spleensP




##### 