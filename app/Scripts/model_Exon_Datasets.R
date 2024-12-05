
library(data.table)
library(R.utils)
library(biomartr)
source('./app/Scripts/multispecies_functions.R')

#library(conflicted)
#conflicts_prefer(dplyr::select)

get_gff_file <- function(selected_organism, selected_database_exon, must_be_reference) {
  
  selected_database_exon <- 'refseq'
  exon_data_dir <- paste0(here_results, '/public_datasets_output/exon_data')
  
  getGFF(db = selected_database_exon, 
         organism = selected_organism, 
         reference = must_be_reference, 
         path = exon_data_dir)
  
  selected_organism <- gsub(' ', '_', selected_organism)
  exon_gz_file_path <- paste0(exon_data_dir, "/", selected_organism, "_genomic_refseq.gff.gz")
  
}

get_exon_counts <- function(exon_gz_file_path) {
  # unzip .gz file 
  if(file.exists(exon_gz_file_path)) {R.utils::gunzip(exon_gz_file_path, overwrite = TRUE)}
  exon_file_path <- sub(".gz$", "", exon_gz_file_path)
  
  # Read the decompressed file
  gff_data <- fread(exon_file_path, header = FALSE, sep = "\t")
  
  gn_exon_counts <- gff_data %>%
    filter(V3 == 'exon') %>%
    mutate(gene_id = str_extract(V9, "(?<=gene=)[^;]+")) %>%
    dplyr::select(gene_id) %>%
    group_by(gene_id) %>%
    summarize(n_exons = n())
  rm(gff_data)
  
  return(gn_exon_counts)
}

write_exon_tsv <- function(gn_exon_counts, selected_organism) {
  # create a new directory for the exon files
  exon_output_dir <- paste0(here_results, '/Exon_Counts/')
  dir.create(exon_output_dir, recursive = T)
  
  # name the new file 
  selected_organism <- gsub(' ', '_', selected_organism)
  new_exon_file <- paste0(exon_output_dir, selected_organism, '_exon.tsv')
  
  # write the exon counts to file
  write.table(gn_exon_counts, new_exon_file, sep = "\t", row.names = FALSE)
}



main_exon_datasets <- function(selected_organisms, selected_database_exon, must_be_reference, id_type_of_gene_ids, kept_transcript_dir) {
  
  for (selected_organism in selected_organisms) {
    # get the gff file for the given organism
    exon_gz_file_path <- get_gff_file(selected_organism, selected_database_exon, must_be_reference)
    
    # count the exons in the gff file
    gn_exon_counts <- get_exon_counts(exon_gz_file_path)
    
    # convert the ids
    if (id_type_of_gene_ids == 'ensembl') {

      gn_exon_counts <- main_id_convert(df = gn_exon_counts, 
                                        gene_column_number = 1, 
                                        chosen_organism = selected_organism, 
                                        from_to = c('symbol', 'ensembl_transcript', 'ensembl_gene'),
                                        kept_transcript_dir = kept_transcript_dir)
      
      
    }
    
    # write the exon counts into a file 
    write_exon_tsv(gn_exon_counts, selected_organism)
    
  }
  
  unlink(paste0(here_results, '/public_datasets_output'), recursive = T)
}




# example inputs: 

#selected_database_exon <- 'ensembl'
#selected_organisms <- c('Homo sapiens', 'Pan troglodytes')
#must_be_reference <- T

#main_exon_datasets(selected_organisms = selected_organisms, 
 #                  selected_database_exon = selected_database_exon, 
  #                 must_be_reference = must_be_reference)





