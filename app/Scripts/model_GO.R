





if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("biomaRt")
BiocManager::install("clusterProfiler")
BiocManager::install("org.Hs.eg.db")  # Replace with relevant organism package if not human

library(biomaRt)
library(clusterProfiler)
library(org.Hs.eg.db)  # Replace if not human



genes <- c("BRCA1", "TP53", "EGFR", "MTOR")


# Connect to Ensembl database and select dataset for your species
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")  # Human example

# Retrieve GO terms for your gene list
go_annotations <- getBM(
  attributes = c("hgnc_symbol", "go_id", "name_1006", "namespace_1003"),
  filters = "hgnc_symbol",
  values = genes,
  mart = ensembl
)

# Display the GO annotations
print(go_annotations)


# Convert gene symbols to Entrez IDs for clusterProfiler
entrez_genes <- bitr(genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)

# Run GO enrichment analysis
enrich_result <- enrichGO(
  gene = entrez_genes$ENTREZID,
  OrgDb = org.Hs.eg.db,
  ont = "BP",  # Can be "BP", "MF", or "CC" for biological process, molecular function, cellular component
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.2
)

# View enrichment results
print(head(enrich_result))



###
BiocManager::install("rols")
library("rols")
bspo <- Ontology("bspo")


####

BiocManager::install("GO.db")
library(GO.db)



####

library(biomaRt)

listDatasets(useEnsembl(biomart = "ensembl"))


listDatasets(useMart("ensembl"))


listEnsemblGenomes()


listAttributes(mart = )


getHomologs(genes, from = "hsapiens_gene_ensembl", to = "mmusculus_gene_ensembl")


###





all_genes_list <- c()

library(biomartr)
t <- getGO(organism = 'Homo sapiens', filters = 'ensembl_gene_id', genes = all_genes_list)














