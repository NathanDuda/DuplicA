source(paste0(here_duplica, '/app/Scripts/Visualization_functions.R'))



get_prot_length <- function(dups, prot_output_dir) {
  
  prot_fasta_table <- cat_all_in_dir(dir = prot_output_dir, file_type = 'aa_fasta')
  
  prot_lengths <- prot_fasta_table %>%
    rename(aa_seq = colnames(.)) %>%
    mutate(id = rownames(.)) %>%
    mutate(aa_length = nchar(aa_seq)) %>%
    dplyr::select(id, aa_length)
  
  
  prot_lengths <- merge_by_gene(dups, data_to_add = prot_lengths, prefix = 'prot_length', merge_by_each = c('dup_1', 'dup_2'))
  
  return(prot_lengths)
}

get_gc_content_AND_cpg_count <- function(dups, nuc_output_dir) {
  
  nuc_fasta_table <- cat_all_in_dir(dir = nuc_output_dir, file_type = 'nuc_fasta')
  
  gc_and_cpg <- nuc_fasta_table %>%
    rename(nuc_seq = colnames(.)) %>%
    mutate(id = rownames(.)) %>%
    mutate(gc_content = stringr::str_count(nuc_seq, "[GCgc]") / nchar(nuc_seq) * 100,
           cpg_count = str_count(nuc_seq, "CG")) %>%
    dplyr::select(id, gc_content, cpg_count)
  
  gc_content <- gc_and_cpg %>% dplyr::select(id, gc_content)
  cpg_count <- gc_and_cpg %>% dplyr::select(id, cpg_count)
  
  dups <- merge_by_gene(dups, data_to_add = gc_content, prefix = 'gc_content', merge_by_each = c('dup_1', 'dup_2'))
  dups <- merge_by_gene(dups, data_to_add = cpg_count, prefix = 'cpg_count', merge_by_each = c('dup_1', 'dup_2'))
  
  return(dups)
}

get_misc_exon_counts <- function(dups, raw_dup_mechanism_output_file_path) {
  
  exon_counts <- read.delim(raw_dup_mechanism_output_file_path) 
  
  exon_counts <- exon_counts %>% 
    dplyr::select(Orthogroup, dup_1_n_exons, dup_2_n_exons) %>%
    merge(dups, by = 'Orthogroup')
  
  return(exon_counts)
}


main_get_misc_results <- function(dups, prot_output_dir, nuc_output_dir = NULL, raw_dup_mechanism_output_file_path) {
  dups <- get_prot_length(dups, prot_output_dir)
  
  if(!is.null(nuc_output_dir)) {
    dups <- get_gc_content_AND_cpg_count(dups, nuc_output_dir)
  }
  
  dups <- get_misc_exon_counts(dups, raw_dup_mechanism_output_file_path)
  
  
  write.table(dups, file = paste0(here_results, '/misc_results.tsv'))
}




