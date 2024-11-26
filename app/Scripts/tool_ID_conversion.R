

library(tidyverse)

# convert between gene symbols and ensembl ids



source('./app/Scripts/multispecies_functions.R')
library(biomaRt)
library(biomartr)


get_ids <- function(chosen_organism, from_to) {
  
  avail_data <- get_avail_data_for_organism(chosen_organism, topic = 'external_gene_name')
  
  chosen_mart <- avail_data$mart[1] # CHANGEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEee
  chosen_dataset <- avail_data$dataset[1] 
  
  chosen_data <- useDataset(dataset = chosen_dataset, 
                            mart = useMart(chosen_mart))
  
  ids <- getBM(attributes = from_to,
                     filters = '',
                     values = '',
                     mart = chosen_data)
  
  ids <- ids %>% select(all_of(from_to))
  
  return(ids)
}

replace_ids <- function(df, ids, id_column_number) {
  
  replaced_id_df <- merge(df, ids, 
                          by.x = colnames(df)[id_column_number], 
                          by.y = colnames(ids)[1], 
                          all.x = TRUE) 
    #mutate(!!colnames(df)[id_column_number] := !!sym(colnames(ids)[2])) %>%
    #select(-!!sym(colnames(ids)[2]))
  
  return(replaced_id_df)
}

main_id_convert <- function(df, id_column_number, chosen_organism, from = 'symbol', to = 'ensembl_gene') {
  
  to <- switch(to, symbol = "external_gene_name", ensembl_gene = "ensembl_gene_id", ensembl_transcript = '')
  from <- switch(from, symbol = "external_gene_name", ensembl_gene = "ensembl_gene_id", ensembl_transcript = '')
  from_to <- c(from, to)
  
  
  ids <- get_ids(chosen_organism, from_to)
  replaced_id_df <- replace_ids(df, ids, id_column_number)
  
  return(replaced_id_df)
}





chosen_organism <- 'Homo sapiens'

exon_data <- read.delim(paste0(here_results, "/Exon_Counts/Homo_sapiens_exon.tsv"))

e <- main_id_convert(df = exon_data,
                     id_column_number = 1,
                     chosen_organism = chosen_organism, 
                     from = 'symbol', 
                     to = 'ensembl')




