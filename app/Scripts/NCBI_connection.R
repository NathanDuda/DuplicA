
library(Biostrings)
library(biomartr)


# the possible inputs 
available_databases <- c('refseq', 'genbank', 'ensembl') # for prot and cds
available_organisms <- listGenomes()
data_types <- c('Proteomes', 'CDS', 'Genomes')
keep_which_transcript <- c('longest', 'first')



###

get_data_for_organisms <- function(selected_organisms, data_types, selected_database_protein, selected_database_cds, selected_database_genome, prot_data_dir, cds_data_dir, genome_data_dir) {
  
  if ('Proteomes' %in% data_types){
    getProteomeSet(db = selected_database_protein, organism = selected_organisms, path = prot_data_dir)
  }

  if ('CDS' %in% data_types){
    getCDSSet(db = selected_database_cds, organism = selected_organisms, path = cds_data_dir)
  }
  
  if ('Genomes' %in% data_types){
    getGenomeSet(db = selected_database_genome, organism = selected_organisms, path = genome_data_dir)
  }
  
  
}

get_organisms_prot_fasta_data <- function(selected_organism, prot_files) {
  prot_file <- prot_files[grepl(selected_organism, prot_files) & grepl("\\.pep\\.", prot_files)]
  prot_fasta_data <- readAAStringSet(prot_file)
  
  return(prot_fasta_data)
}
get_organisms_cds_fasta_data <- function(selected_organism, cds_files) {
  cds_file <- cds_files[grepl(selected_organism, cds_files) & grepl("\\.cds\\.", cds_files)]
  cds_fasta_data <- readDNAStringSet(cds_file)
  
  return(cds_fasta_data)
  
}

get_prot_transcript_seq <- function(prot_fasta_data, keep_which_transcript) {
  prot_seqs_df <- as.data.frame(prot_fasta_data) %>%
    rownames_to_column('names') %>%
    mutate(
      protein_id = str_extract(names, "^[^ ]+"),
      gene_id = str_extract(names, "(?<=gene:)[^ ]+"),
      transcript_id = str_extract(names, "(?<=transcript:)[^ ]+")
    ) %>%
    select(gene_id, transcript_id, protein_id, prot_seq = x) %>%
    mutate(len = nchar(prot_seq)) 
  
  
  any(duplicated(prot_seqs_df$transcript_id)) # should be FALSE
  any(duplicated(prot_seqs_df$protein_id)) # should be FALSE
  
  if (keep_which_transcript == 'longest'){
    prot_seqs_df <- prot_seqs_df %>%
      group_by(gene_id) %>%
      filter(len == max(len)) %>% 
      slice_head(n = 1) %>%  # keep only one even if max length is the same 
      ungroup() 
  }
  
  if (keep_which_transcript == 'first'){
    prot_seqs_df <- prot_seqs_df %>%
      group_by(gene_id) %>%
      slice_head(n = 1) %>%  
      ungroup() 
  }
  any(duplicated(prot_seqs_df$gene_id)) # should be FALSE
  
  return(prot_seqs_df)
}
add_cds_transcript_seq <- function(cds_fasta_data, prot_seqs_df) {
  
  cds_seqs_df <- as.data.frame(cds_fasta_data) %>%
    rownames_to_column('names') %>%
    mutate(transcript_id = str_extract(names, "^[^ ]+")) %>%
    select(transcript_id, cds_seq = x)
  
  seqs_df <- merge(cds_seqs_df, prot_seqs_df, by = 'transcript_id')
  
  seqs_df <- seqs_df %>% select(gene_id, transcript_id, protein_id, cds_seq, prot_seq)
  
  return(seqs_df)
}

create_output_dirs <- function() {
  prot_output_dir <- paste0(here_results, '/Fastas/Protein_Fastas/')
  dir.create(prot_output_dir, recursive = T)
  cds_output_dir <- paste0(here_results, '/Fastas/Nucelotide_Fastas/')
  dir.create(cds_output_dir, recursive = T)
  
  return(list(prot_output_dir = prot_output_dir, cds_output_dir = cds_output_dir))
}
seq_df_to_fasta_files <- function(seqs_df, prot_output_dir, cds_output_dir, selected_organism) {
  
  prot_output_file <- paste0(prot_output_dir, selected_organism, '_prot.fasta')
  cds_output_file <- paste0(cds_output_dir, selected_organism, '_cds.fasta')
  
  seqs_df %>%
    mutate(fasta = paste0('>', gene_id, '\n', prot_seq)) %>%
    pull(fasta) %>%
    writeLines(con = prot_output_file)
  
  seqs_df %>%
    mutate(fasta = paste0('>', gene_id, '\n', cds_seq)) %>%
    pull(fasta) %>%
    writeLines(con = cds_output_file)
}

move_genome_files <- function(selected_organism, genome_files) {
  
  
  # get the genome of the given species 
  genome_file <- genome_files[grepl(selected_organism, genome_files) & grepl("\\.dna\\.", genome_files)]
  
  if (length(genome_file) == 0) {
    print(paste0('No genome file found for ', selected_organism))
    return()
  }
  
  # create a new directory for the genome files
  genome_output_dir <- paste0(here_results, '/Fastas/Genome_Fastas/')
  dir.create(genome_output_dir, recursive = T)
  
  # copy the file to the output directory
  file.copy(genome_file, new_genome_file)
  
  # rename the new file 
  new_genome_file <- paste0(genome_output_dir, selected_organism, '_genome.fasta')
  
  
  print(genome_file)
  print(length(genome_file))
  print(new_genome_file)
  print(length(new_genome_file))

  file.rename(genome_file, new_genome_file)
  file.remove(paste0(genome_output_dir, '/', basename(genome_file)))
  
  
  
}

main_ncbi <- function(selected_organisms, data_types, selected_database_protein, selected_database_cds, selected_database_genome, keep_which_transcript) {
  
  selected_organisms <- gsub(' ', '_', selected_organisms)
  
  prot_data_dir <- paste0(here_results, '/ncbi_output/protein_data')
  cds_data_dir <- paste0(here_results, '/ncbi_output/cds_data')
  genome_data_dir <- paste0(here_results, '/ncbi_output/genome_data')
  
  # get the data types from ncbi 
  get_data_for_organisms(selected_organisms, data_types, selected_database_protein, selected_database_cds, selected_database_genome, prot_data_dir, cds_data_dir, genome_data_dir)
  
  # list all files provided by ncbi 
  prot_files <- list.files(prot_data_dir, full.names = TRUE)
  cds_files <- list.files(cds_data_dir, full.names = TRUE)
  genome_files <- list.files(genome_data_dir, full.names = TRUE)

  # interate over each selected organism 
  for (selected_organism in selected_organisms) {
    
    # read in the fasta files for the given organism 
    prot_fasta_data <- get_organisms_prot_fasta_data(selected_organism, prot_files)
    cds_fasta_data <- get_organisms_cds_fasta_data(selected_organism, cds_files)
    
    # format the fasta data into a table. keep only one sequence per gene
    prot_seqs_df <- get_prot_transcript_seq(prot_fasta_data, keep_which_transcript = 'longest')
    seqs_df <- add_cds_transcript_seq(cds_fasta_data, prot_seqs_df)
    
    # write the sequences into fasta files 
    dirs <- create_output_dirs()
    seq_df_to_fasta_files(seqs_df, dirs$prot_output_dir, dirs$cds_output_dir, selected_organism)
    
    
    # write genome fastas if selected
    if ('Genomes' %in% data_types) {
      move_genome_files(selected_organism, genome_files)
    }
    
  }
  
  # delete the ncbi output directory
  unlink(paste0(here_results, '/ncbi_output'), recursive = T)
  
}






prot_output_dir <- paste0(here_results, '/Fastas/Protein_Fastas/')



# example input
selected_database_protein <- 'refseq'
selected_database_cds <- 'refseq'
selected_database_genome <- 'ensembl'

selected_organisms <- c('Deinococcus radiodurans', 'Piscirickettsia salmonis', 'Drosophila ananassae')
data_types <- c('Proteomes', 'CDS', 'Genomes')
keep_which_transcript <- 'longest'

main_ncbi(selected_organisms, data_types, selected_database_protein, selected_database_cds, selected_database_genome, keep_which_transcript)


