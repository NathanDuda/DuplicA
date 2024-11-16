


###




available_databases <- c('refseq', 'genbank', 'ensembl')
selected_database <- 'ensembl'

selected_organisms <- c('Drosophila mojavensis', 'Drosophila melanogaster', 'Drosophila ananassae')


get_data_for_organisms <- function(selected_organisms, data_types, selected_database) {
  
  if ('set_proteomes' %in% data_types){
    output_directory <- paste0(here_duplica, '/set_proteomes')
    getProteomeSet(db = selected_database, organism = selected_organisms, path = output_directory)
  }

  if ('set_CDS' %in% data_types){
    output_directory <- paste0(here_duplica, '/set_CDS')
    getCDSSet(db = selected_database, organism = selected_organisms, path = output_directory)
  }
  
  if ('set_genomes' %in% data_types){
    output_directory <- paste0(here_duplica, '/set_genomes')
    getGenomeSet(db = selected_database, organism = selected_organisms, path = output_directory)
  }
  
  
}

get_data_for_organisms(selected_organisms, c('set_proteomes', 'set_CDS', 'set_genomes'), selected_database)


#

library(Biostrings)


files <- list.files('C:/Users/17735/Downloads/DuplicA/set_proteomes/', full.names = TRUE)

i <- 1

selected_organisms <- gsub(' ', '_', selected_organisms)

aa_file <- files[grepl(selected_organisms[i], files) & grepl("\\.pep\\.", files)]
fasta_data <- readAAStringSet(aa_file)


t <- as.data.frame(fasta_data) %>%
  rownames_to_column('names') %>%
  mutate(
    protein_id = str_extract(names, "^[^ ]+"),
    gene_id = str_extract(names, "(?<=gene:)[^ ]+"),
    transcript_id = str_extract(names, "(?<=transcript:)[^ ]+")
  ) %>%
  select(gene_id, transcript_id, protein_id, prot_seq = x) %>%
  mutate(len = nchar(prot_seq)) 


any(duplicated(t$transcript_id)) # should be FALSE
any(duplicated(t$protein_id)) # should be FALSE

if (keep_which == 'longest'){
  x <- t %>%
    group_by(gene_id) %>%
    filter(len == max(len)) %>% 
    slice_head(n = 1) %>%  # keep only one even if max length is the same 
    ungroup() 
}

if (keep_which == 'first'){
  x <- t %>%
    group_by(gene_id) %>%
    slice_head(n = 1) %>%  
    ungroup() 
}
any(duplicated(x$gene_id)) # should be FALSE





files <- list.files('C:/Users/17735/Downloads/DuplicA/set_CDS/', full.names = TRUE)

cds_file <- files[grepl(selected_organisms[i], files) & grepl("\\.cds\\.", files)]
fasta_data <- readDNAStringSet(cds_file)


t <- as.data.frame(fasta_data) %>%
  rownames_to_column('names') %>%
  mutate(
    transcript_id = str_extract(names, "^[^ ]+")
  ) %>%
  select(transcript_id, cds_seq = x)



b <- merge(t, x, by = 'transcript_id')



# write to a fasta file 











genome_file <- files[grepl(selected_organisms[i], files) & grepl("\\.dna\\.", files)]
fasta_data <- readDNAStringSet(genome_file)




