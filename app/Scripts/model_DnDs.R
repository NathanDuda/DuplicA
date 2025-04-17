


make_temp_dirs <- function(replace_dirs, temp_dir_list) {
  
  create_or_replace_dir <- function(dir_path) {
    if (replace_dirs == T) {system(paste0('wsl ', 'rm -rf ', dir_path), intern = TRUE)}
    system(paste0('wsl ', 'mkdir -p ', dir_path), intern = TRUE)
  }
  
  # create each directory
  for (temp_dir in temp_dir_list) {
    create_or_replace_dir(paste0(here_linux_temp, temp_dir))
  }
}



# this function allows two to two to twos
get_two_to_two_pairs_from_OF <- function(OF_dir_path) {

  
  orthogroup_gene_count <- read.delim(paste0(OF_dir_path, "Orthogroups/Orthogroups.GeneCount.tsv"))
  n_species <- ncol(orthogroup_gene_count) - 2 
  
  # get the two to one to ones to zeros 
  two_to_two_to_ones <- orthogroup_gene_count %>%
    filter(if_all(2:(n_species + 1), ~. <= 2)) %>%          # keep only pairs 
    #filter(rowSums(select(., 2:(n_species + 1)) == 2) == 1) # make sure there is only one species with 2 copies 
    select(Orthogroup)
  
  # merge back with the gene names 
  orthogroups <- read.delim(paste0(OF_dir_path, "/Orthogroups/Orthogroups.tsv"))
  two_to_two_to_ones <- merge(orthogroups, two_to_two_to_ones, by = 'Orthogroup')
  
  
  # extract the duplicate pair genes
  dups <- two_to_two_to_ones %>%
    pivot_longer(cols = 2:(1+n_species), names_to = 'species', values_to = 'gn_pair') %>%
    separate(col = 'gn_pair', sep = ', ', into = c('dup_1', 'dup_2')) %>%
    na.omit() %>%
    pivot_longer(cols = c('dup_1', 'dup_2'), names_to = NULL, values_to = 'gene') %>%
    select(species, gene, Orthogroup) %>%
    mutate(n = 2)
    
  return(dups)
  
}


get_paired_fastas <- function(pairs, nuc_file_path, prot_file_path, use_all_fastas_in_dir) {
  
  if(use_all_fastas_in_dir == T){
    nuc_file_path <- gsub(here, here_linux, nuc_file_path)
    nuc_file_path <- combine_raw_fastas(nuc_file_path, type = 'nuc')
    system(paste0('wsl unix2dos ', nuc_file_path))
    nuc_file_path <- gsub(here_linux, here, nuc_file_path)
    if (!is.na(prot_file_path)) {
      prot_file_path <- gsub(here, here_linux, prot_file_path)
      prot_file_path <- combine_raw_fastas(prot_file_path, type = 'prot')
      system(paste0('wsl unix2dos ', prot_file_path))
      prot_file_path <- gsub(here_linux, here, prot_file_path)
      }
  }
  

  if (is.na(prot_file_path)) {
    prot_file_path <- translate_nucs_to_prots(nuc_file_path)
  }
  
  
  
  pairs <- pairs %>%
    pivot_longer(cols = c('dup_1', 'dup_2'), names_to = 'copy', values_to = 'gene') %>%
    dplyr::select(-copy)
  
  
  nucs <- readDNAStringSet(nuc_file_path, format = "fasta")
  nucs <- data.frame(gene = names(nucs), nuc = as.character(nucs))  
  
  
  # individual name is characters before first _ (individual name cannot have _ (replaced with .))
  # gene name is characters after first _ (gene name can have _)
  
  
  #nucs$gene <- sub(".*_", "", nucs$gene) # REMOVE - SPECIFIC TO MY DATASET (unit test, names match)
  
  # split gene name and individual name from fasta names, req fasta names to be this (req individual files per indiv and implement this)
  #
  #
  ##############FIXIXIXIIXIXIOWDHIUHWEON
  
  nucs <- nucs %>%
    separate(gene, into = c("name", "gene"), sep = "_")
  
  pairs <- left_join(pairs, nucs, by = 'gene')
  rm(nucs)
  
  
  prots <- readAAStringSet(prot_file_path, format = 'fasta')
  prots <- data.frame(gene = names(prots), prot = as.character(prots))
  
  prots <- prots %>%
    separate(gene, into = c("name", "gene"), sep = "_")
  
  pairs <- left_join(pairs, prots, by = 'gene')
  rm(prots)
  
  
  # write pairs to fasta files 
  for (group_id in unique(pairs$Orthogroup)) {
    group_rows <- pairs %>% filter(Orthogroup == group_id)
    
    for (row_num in 1:nrow(group_rows)) {
      row <- group_rows[row_num,]
      group_indiv_id <- paste0(group_id, '_', row$duplicate_pair_species)
      
      output_file <- paste0(here_temp, '/Connected_Eq_Nucleotide_Sequences/group_', group_indiv_id, '.fa')  # NOT LINUX 
      cat(">", row$gene, '\n', row$nuc, "\n", file = output_file, append = T, sep = '')
      
      output_file <- paste0(here_temp, '/Connected_Eq_Protein_Sequences/group_', group_indiv_id, '.fa')  
      cat(">", row$gene, '\n', row$prot, "\n", file = output_file, append = T, sep = '')
    }
  }
}


get_prot_alignments <- function(aligner) {
  if (aligner == 'muscle') {
    muscle_path <- paste0(here_linux_dep, '/MUSCLE/muscle-linux-x86.v5.2')
    
    # align proteins of groups of pairs 
    muscle_command <- paste0(
      'for file in ', here_linux_temp, '/Connected_Eq_Protein_Sequences/*; do ',
      'filename=$(basename "$file"); ',
      muscle_path, ' -quiet -align "$file" -output "', here_linux_temp, '/Connected_Eq_Protein_Alignments/${filename}"; ',
      'done'
    )
    
    system(paste('wsl', muscle_command), intern = T)
    
  }
}


get_codon_alignments <- function() { # pal2nal
  pal2nal_path <- paste0(here_linux_dep, '/pal2nal.v14/pal2nal.pl')
  
  
  pal2nal_command <- paste0(
    'for file in ', here_linux_temp, '/Connected_Eq_Protein_Sequences/*; do ',
    'filename=$(basename "$file" .fa); ',  # Assumes .fa as the extension for protein and nucleotide files
    pal2nal_path, ' ', here_linux_temp, '/Connected_Eq_Protein_Alignments/${filename}.fa ', 
    here_linux_temp, '/Connected_Eq_Nucleotide_Sequences/${filename}.fa ', 
    '-output fasta > ', here_linux_temp, '/Connected_Eq_Codon_Alignments/${filename}.fa; ',
    'done'
  )
  system(paste('wsl', pal2nal_command), intern = T)
}


get_dnds <- function() {
  dnds_results <- data.frame()
  
  # remove empty codon alignments (caused by missing gene sequences)
  codon_alignment_files <- list.files(paste0(here_temp, '/Connected_Eq_Codon_Alignments'), full.names = T)
  empty_files <- codon_alignment_files[file.info(codon_alignment_files)$size == 0]
  file.remove(empty_files) 
  non_empty_codon_alignment_files <- list.files(paste0(here_temp, '/Connected_Eq_Codon_Alignments'), full.names = T)
  
  for (file in non_empty_codon_alignment_files) {
    basename <- basename(file)
    group <- str_extract(basename, "(?<=_)[^_]+(?=_)")
    name <- str_match(basename, "OG\\d+_([^\\.]+)\\.fa")[,2]
    
    codon_alignment <- read.alignment(file = file, format = 'fasta')
    dnds <- kaks(codon_alignment)
    
    row <- data.frame(name = name, group = group,dn = dnds['ka'], ds = dnds['ks'])
    dnds_results <- rbind(dnds_results, row)
  }
  return(dnds_results)
}


main_pop_dnds <- function(cnvs_path, nuc_file_path, prot_file_path = NA, aligner = 'muscle', replace_dirs, use_all_fastas_in_dir) {
  replace_dirs <- F ############## remove 
  temp_dir_list <- c('/Connected_Eq_Protein_Sequences', '/Connected_Eq_Protein_Alignments', '/Connected_Eq_Nucleotide_Sequences', '/Connected_Eq_Codon_Alignments')
  make_temp_dirs(replace_dirs, temp_dir_list)
  
  
  #   OF_dir_path <- paste0(OF_dir_path, '/')
  # if compgen: pairs <- get_pairs_from_OF(OF_dir_path)
  pairs <- get_pairs(cnvs_path) # if popgen

  get_paired_fastas(pairs, nuc_file_path, prot_file_path, use_all_fastas_in_dir)
  get_prot_alignments(aligner)
  get_codon_alignments()
  dnds_results <- get_dnds()
  
  write.table(dnds_results, 'C:/Users/17735/Downloads/DuplicA/TRASH_dnds_results.tsv') # CHANGEEE

}


main_multispecies_dnds <- function(OF_dir_path, dups, allow_two_to_twos, nuc_file_path, prot_file_path, aligner) {
  
  # make temp directories
  temp_dir_list <- c('/Connected_Eq_Protein_Sequences', '/Connected_Eq_Nucleotide_Sequences',
                     '/Connected_Eq_Protein_Alignments', '/Connected_Eq_Codon_Alignments')
  make_temp_dirs(replace_dirs = T, temp_dir_list)
  
  # empty out temp directories
  unlink(paste0(here_temp, '/Connected_Eq_Nucleotide_Sequences/*'))
  unlink(paste0(here_temp, '/Connected_Eq_Protein_Sequences/*'))
  
  # overwrite dups to include two_to_twos if chosen 
  if (allow_two_to_twos == T) {dups <- get_two_to_two_pairs_from_OF(OF_dir_path)}
  
  # calculate dnds
  get_paired_fastas(dups, nuc_file_path, prot_file_path, use_all_fastas_in_dir = TRUE)
  get_prot_alignments(aligner)
  get_codon_alignments()
  dnds_results <- get_dnds()
  
  write.table(dnds_results, paste0(here_results, '/main_DnDs_output.tsv')) 
  return(dnds_results)
  
}

