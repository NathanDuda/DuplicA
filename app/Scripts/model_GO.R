



source('./app/Scripts/multispecies_functions.R')


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("biomaRt")
BiocManager::install("clusterProfiler")

library(biomaRt)
library(clusterProfiler)



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


#library(goseq)
source('./app/Scripts/multispecies_functions.R')

library(biomaRt)






main_go <- function(dups_anc, file_organism_table) {
  file_organism_table$organism_scientific_name <- gsub('_', ' ', file_organism_table$organism_scientific_name)
  
    
  all_copies <- format_dups_for_db_search(dups_anc)
  
  # remove the characters after the first period in the gene name (causes issues with human genes)
  all_copies$gene <- sub("\\.[^.]*$", "", all_copies$gene) # remove the version indicator in human gene ids 

  
  
  # iterate over all organisms with data 
  all_go_output <- data.frame()
  for (chosen_organism in file_organism_table$organism_scientific_name) {
    
    # get file name for the chosen organism
    chosen_protein_file_name <- get_protein_file_name(chosen_organism, file_organism_table)
    
    # get all genes for the organism (any duplicate copies and any ancestral copies)
    species_name <- gsub('.fasta', '', basename(chosen_protein_file_name))
    genes <- all_copies %>% filter(protein_file_name == species_name)
    
    # check if data is available for the given organism
    avail_data <- get_avail_data_for_organism(chosen_organism, topic = 'go_id')
    if (nrow(avail_data) == 0) {return(paste0('No GO data is available for '), chosen_organism)}
    
    
    # get gene ontology data for the organism, when only one dataset is available
    if (nrow(avail_data) == 1) {
      go_output <- getGO(organism = chosen_organism, 
                         genes = genes$gene, 
                         filters = 'ensembl_gene_id')
      
      go_output <- go_output %>% 
        select(gene_id = ensembl_gene_id, 
               go_id = goslim_goa_accession,
               go_description = goslim_goa_description)
      
    }
    
    # specify the dataset to use when multiple are available 
    if (nrow(avail_data) > 1) {
      chosen_mart <- avail_data$mart[1] # CHANGEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEee
      chosen_dataset <- avail_data$dataset[1] 
      
      chosen_data <- useDataset(dataset = chosen_dataset, 
                                mart = useMart(chosen_mart))
      
      go_output <- getBM(attributes = c('ensembl_gene_id', "go_id", 'namespace_1003', 'goslim_goa_description', 'name_1006'),
                         filters = "ensembl_gene_id",
                         values = genes$gene,
                         mart = chosen_data)
      
      go_output <- go_output %>% 
        select(gene_id = ensembl_gene_id, 
               go_id = go_id,
               go_description = goslim_goa_description) 
      
      
      
      
    }
    
    # combine go output for all species 
    go_output <- go_output %>% mutate(protein_file_name = species_name)
    all_go_output <- rbind(all_go_output, go_output)
  }
  
  all_copy_go <- left_join(all_copies, all_go_output, by = c('gene' = 'gene_id', 'protein_file_name'))
  

}





compare_copy_go <- function(all_copy_go) {
  
  
  t <- all_copy_go %>%
    group_by(gene, go_id) %>%
    distinct() %>%
    ungroup() %>%
    group_by(Orthogroup) %>%
    filter(!any(is.na(across(everything())))) %>%
    summarize(
      all_d1_d2 = all(go_id[copy == "dup_1"] %in% go_id[copy == "dup_2"]),
      all_d2_d1 = all(go_id[copy == "dup_2"] %in% go_id[copy == "dup_1"]),
      all_d1_anc = all(go_id[copy == "dup_1"] %in% go_id[copy == "ancestral_copy"]),
      all_anc_d1 = all(go_id[copy == "ancestral_copy"] %in% go_id[copy == "dup_1"]),
      all_d2_anc = all(go_id[copy == "dup_2"] %in% go_id[copy == "ancestral_copy"]),
      all_anc_d2 = all(go_id[copy == "ancestral_copy"] %in% go_id[copy == "dup_2"]),
      any_d1_d2 = any(go_id[copy == "dup_1"] %in% go_id[copy == "dup_2"]),
      any_d2_d1 = any(go_id[copy == "dup_2"] %in% go_id[copy == "dup_1"]),
      any_d1_anc = any(go_id[copy == "dup_1"] %in% go_id[copy == "ancestral_copy"])
    )
  
}









