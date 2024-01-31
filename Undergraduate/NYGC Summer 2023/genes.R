library(remotes)
library(ggplot2)
library(tidyverse)
library(devtools)
library(clusterProfiler)
library(installr)
library(DESeq2)
library(enrichplot)
#
{
  if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install(version = "3.17")
}
library(BiocManager)
BiocManager::install("org.Hs.eg.db")
BiocManager::install("AnnotationDbi")
BiocManager::install("DESeq2")
BiocManager::install("enrichplot", force = TRUE)

library(org.Hs.eg.db)
library(AnnotationDbi)
tibble(genes)
gene_names<-list(genes)
gene_names<-genes[,1]
gene_ids<-mapIds(org.Hs.eg.db, keys = gene_names,keytype = "SYMBOL" , column = "ENTREZID")

########################s
# running               #
# ontology              #
########################s

enriched <- enrichGO(
  gene = gene_ids,     # Your list of gene IDs
  OrgDb = org.Hs.eg.db, # Organism-specific database
  keyType = "ENTREZID",
  ont = "BP",           # Specify the ontology (e.g., Biological Process)
  pAdjustMethod = "BH", # Adjust p-values
  pvalueCutoff = 0.05# Set your desired significance threshold
)
########s
#Useless##
#{#rich_result<-enriched@result
#rich_result<-tibble(rich_result)
#rich_result
View(rich_result)}###
fit<- plot(barplot(enriched,showCategory = 8))
fit
class(enriched@result)
enriched@result<- enriched@result[-1,]
enriched
View(enriched@result)
ggplot(enriched, showCategory = 8, 
       aes(p.adjust, fct_reorder(Description, p.adjust))) + 
  geom_segment(aes(xend=0, yend = Description)) +
  geom_point(aes(color=p.adjust, size = Count)) +
  scale_color_viridis_c(guide=guide_colorbar(reverse=TRUE)) +
  scale_size_continuous(range=c(2, 10)) +
  theme_minimal() + 
  xlab("enrichment factor") +
  ylab(NULL) + 
  ggtitle("Enriched Disease Ontology")

ggplot(enriched, showCategory = 8, 
       aes(p.adjust, fct_reorder(Description, p.adjust))) + 
  geom_segment(aes(xend=0, yend = Description)) +
  geom_point(aes(color= Count, size = p.adjust)) +
  scale_color_viridis_c(guide=guide_colorbar(reverse=TRUE)) +
  scale_size_continuous(range=c(2, 10)) +
  theme_minimal() + 
  xlab("fold") +
  ylab(NULL) + 
  ggtitle("Enriched Disease Ontology")
#####


ggplot(enriched, showCategory = 8, 
       aes(p.adjust, fct_reorder(Description, p.adjust))) + 
  geom_segment(aes(x = Count,xend=0, yend = Description)) +
  geom_col(aes(x= Count,fill = p.adjust)) +
  scale_fill_continuous(low= "red",high="blue",guide=guide_colorbar(reverse=TRUE)) +
  scale_size_continuous(range=c(2, 10)) +
  theme_minimal(base_size  = 26) + 
  xlab("Gene Count") +
  ylab(NULL) + 
  ggtitle("Enriched Gene Ontology")

