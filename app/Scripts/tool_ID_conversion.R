

library(tidyverse)
library(biomaRt)
library(biomartr)

# convert between gene symbols and ensembl ids



#source('./app/Scripts/multispecies_functions.R')




get_all_ids <- function(chosen_organism, from_to) {
  avail_data <- get_avail_data_for_organism(chosen_organism, topic = from_to[1])
  
  chosen_mart <- avail_data$mart[1]
  chosen_dataset <- avail_data$dataset[1]
  
  chosen_data <- useDataset(dataset = chosen_dataset, 
                            mart = useMart(chosen_mart))
  
  # Fetch all requested attributes
  ids <- getBM(attributes = from_to,
               filters = '',
               values = '', 
               mart = chosen_data)
  
  return(ids)
}



# Function to replace IDs in the dataframe
replace_ids <- function(df, ids, gene_column_number, from, to) {
  # Merge and replace IDs based on current step
  replaced_id_df <- merge(df, ids, 
                          by.x = colnames(df)[gene_column_number], 
                          by.y = from, 
                          all.x = TRUE)
  
  # Update the gene column to the new ID column
  colnames(replaced_id_df)[gene_column_number] <- to
  
  return(replaced_id_df)
}





# Main function for ID conversion
main_id_convert <- function(df, gene_column_number, chosen_organism, from_to = c('symbol', 'ensembl_transcript', 'ensembl_gene'), kept_transcript_ids_list = NA) {
  
  colnames(df)[gene_column_number] <- 'gene_id'
  
  from_to <- recode(from_to, 
                    symbol = "external_gene_name",
                    ensembl_transcript = "ensembl_transcript_id", 
                    ensembl_gene = "ensembl_gene_id")
  
  # Sequentially convert IDs through the from_to chain
  ids <- get_all_ids(chosen_organism, from_to)
  
  # replace the old ids with the new one
  ids <- ids %>% filter(ensembl_transcript_id %in% kept_transcript_ids_list)
  df$gene_id_2 <- ids$ensembl_transcript_id[match(df$gene_id, ids$external_gene_name)]
  df$gene_id <- df$gene_id_2
  df$gene_id_2 <- NULL
  df <- na.omit(df)
  
  
  any(duplicated(t$ensembl_transcript_id)) # must be false
  any(duplicated(t$ensembl_gene_id)) # must be false
  
  
  return(df)
}





#chosen_organism <- 'Homo sapiens'

#exon_data <- read.delim(paste0(here_results, "/Exon_Counts/Homo_sapiens_exon.tsv"))
#gene_column_number <- 1

#gn_tr <- read.delim("C:/Users/17735/Downloads/DuplicA/app/Results/Fastas/Homo_sapiens_transcript_kept_per_gene.tsv")
#gn_tr$gene_id <- sub("\\.[^.]*$", "", gn_tr$gene_id) # remove the version indicator in human gene ids 
#gn_tr$transcript_id <- sub("\\.[^.]*$", "", gn_tr$transcript_id) # remove the version indicator in human gene ids 


#t <- main_id_convert(df = exon_data, 
 #                    gene_column_number = gene_column_number, 
  #                   chosen_organism = chosen_organism, 
   #                  kept_transcript_ids_list = gn_tr$transcript_id)







