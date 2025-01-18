
source(paste0(here_duplica, '/app/Scripts/model_expression_normalization.R'))



#

# create the directory for the data to go into 
data_dir <- paste0(here_linux_temp, '/Public_RNAseq_Datasets')
data_dir_windows <- paste0(here_temp, '/Public_RNAseq_Datasets')
if(dir.exists(data_dir_windows)) {dir_delete(data_dir_windows)}
dir.create(data_dir_windows)

# loop through each FTP link and download using wget
for (link in ftp_links) {system(paste0('wsl wget -P ', data_dir, ' ', link))}

# decompress every file 
system(paste0('wsl sh -c "gunzip ', data_dir, '/*.gz"'))





replicate_info <- as.data.frame(pData(phenoData(rnaseq_data[[1]])))[1:2]
colnames(replicate_info) <- c('title', 'GSM_accession')



UNIT_TESTS <- replicate_info %>%
  separate(title, into = c("line", "replicate_number"), sep = "(?<=\\D)(?=\\d+$)") 
# THIS NEEDS TO WORK FOR LATER ON REPLICATE USE 
# unit tests: 
# replicate_number should not be NA (that means the title did not end in a number)
# group by line, should match numbers in replicate number
# must have same number of replicates in each group
#


get_RNAseq_from_accession <- function(accession) {
  
  
} 



file_list <- list.files(data_dir_windows, full.names = TRUE)

# read and merge all files in data_dir
merged_data <- lapply(file_list, function(file) {
  df <- read.table(file, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  colnames(df) <- c("id", tools::file_path_sans_ext(basename(file)))
  return(df)
}) %>% reduce(full_join, by = "id")






for (i in 1:nrow(replicate_info)) {
  # Create a pattern based on GSM_accession
  gsm_pattern <- replicate_info$GSM_accession[i]
  
  # Check which columns in 'merged_data' start with the GSM_accession and rename them
  colnames(merged_data) <- gsub(paste0("^", gsm_pattern, ".*$"), replicate_info$title[i], colnames(merged_data))
}



# nuc_output_dir needed to get gene lengths

nuc_fasta_files <- list.files(nuc_output_dir, full.names = T)
nuc_fasta_file <- nuc_fasta_files[1]


get_gene_lengths <- function(nuc_fasta_file) {
  
  gene_lengths <- as.data.frame(readDNAStringSet(nuc_fasta_file)) %>%
    rownames_to_column('id') %>%
    mutate(length = nchar(x)) %>%
    dplyr::select(-x)
  
  return(gene_lengths)
} 



nuc_fasta_file <- nuc_fasta_files[species = species]
gene_lengths <- get_gene_lengths(nuc_fasta_file)


gene_lengths_file <- paste0(here_results, '/', species, '_gene_length.tsv')
write.table(gene_lengths, gene_lengths_file)


# replicates must be in order 





exp <- normalization_function(raw_counts_file = merged_data,
                              gene_lengths_file = gene_lengths_file, 
                              n_replicates = 3, 
                              normalization_type = 'RPKM', 
                              exp_cutoff = 1,
                              raw_counts_format = 'object')






