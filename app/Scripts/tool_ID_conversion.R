

library(tidyverse)
library(biomaRt)
library(biomartr)

# convert between gene symbols and ensembl ids



#source('./app/Scripts/multispecies_functions.R')




get_all_ids <- function(chosen_organism, from_to) {
  avail_data <- get_avail_data_for_organism(chosen_organism, topic = from_to[1])
  
  chosen_mart <- avail_data$mart[1]
  chosen_dataset <- avail_data$dataset[1]
  
  #if(!chosen_dataset %in% listDatasets(mart = useMart('ENSEMBL_MART_ENSEMBL'))) {stop(paste0('There is no exon data publicly available for ', chosen_organism, '.'))}
  
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


get_kept_transcript_ids_list <- function(selected_organism, kept_transcript_dir) {
  selected_organism <- gsub(' ', '_', selected_organism)
  
  gn_tr_file <- paste0(kept_transcript_dir, selected_organism, '_transcript_kept_per_gene.tsv')
  gn_tr <- read.delim(gn_tr_file)
  
  gn_tr$transcript_id <- sub("\\.[^.]*$", "", gn_tr$transcript_id) # remove the version indicator in human gene ids 
  kept_transcript_ids_list <- gn_tr$transcript_id
  
  
  return(kept_transcript_ids_list)
}



# Main function for ID conversion
main_id_convert <- function(df, gene_column_number, chosen_organism, from_to = c('symbol', 'ensembl_transcript', 'ensembl_gene'), kept_transcript_dir = NA) {
  
  if(!is.na(kept_transcript_dir)) {kept_transcript_ids_list <- get_kept_transcript_ids_list(chosen_organism, kept_transcript_dir)}
  
  from_to <- recode(from_to, 
                    symbol = "external_gene_name",
                    ensembl_transcript = "ensembl_transcript_id", 
                    ensembl_gene = "ensembl_gene_id")
  
  colnames(df)[gene_column_number] <- 'ensembl_transcript_id' #from_to[1]
  
  # get the names of the columns to keep (in the right position)
  cols_to_keep <- c(colnames(df)[-gene_column_number])
  new_id <- tail(from_to, 1)
  cols_to_keep <- append(cols_to_keep, new_id, after = gene_column_number - 1)
  
  # Sequentially convert IDs through the from_to chain
  ids <- get_all_ids(chosen_organism, from_to)
  
  # replace the old ids with the new one
  ids <- ids %>% filter(ensembl_transcript_id %in% kept_transcript_ids_list)
  
  df <- merge(ids, df, by = 'ensembl_transcript_id')
  df <- df %>% dplyr::select(all_of(cols_to_keep))
  
  any(duplicated(df$ensembl_transcript_id)) # must be false
  any(duplicated(df$ensembl_gene_id)) # must be false
  
  
  return(df)
}





#chosen_organism <- 'Homo sapiens'
#exon_data <- read.delim(paste0(here_results, "/Exon_Counts/Homo_sapiens_exon.tsv"))
#gene_column_number <- 1
#kept_transcript_dir <- paste0(here_results, '/Fastas/kept_transcript/')


#gn_tr <- read.delim("C:/Users/17735/Downloads/DuplicA/app/Results/Fastas/Homo_sapiens_transcript_kept_per_gene.tsv")
#gn_tr$gene_id <- sub("\\.[^.]*$", "", gn_tr$gene_id) # remove the version indicator in human gene ids 
#gn_tr$transcript_id <- sub("\\.[^.]*$", "", gn_tr$transcript_id) # remove the version indicator in human gene ids 


#t <- main_id_convert(df = exon_data, 
 #                    gene_column_number = gene_column_number, 
  #                   from_to = c('symbol', 'ensembl_transcript', 'ensembl_gene'),
   #                  chosen_organism = chosen_organism, 
    #                 kept_transcript_dir = kept_transcript_dir)







