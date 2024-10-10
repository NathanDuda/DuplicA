

source('C:/Users/17735/Downloads/DuplicA/app/Scripts/pop_functions.R') ######## CHANGE

# this function allows two to two to twos
get_pairs_from_OF <- function(OF_dir_path) {

  
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
  
  use_all_fastas_in_dir <- T
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
  
  
  
  
  nucs <- readDNAStringSet(nuc_file_path, format = "fasta")
  nucs <- data.frame(gene = names(nucs), nuc = as.character(nucs))  
  
  
  # individual name is characters before first _ (individual name cannot have _ (replaced with .))
  # gene name is characters after first _ (gene name can have _)
  
  
  #nucs$gene <- sub(".*_", "", nucs$gene) # REMOVE - SPECIFIC TO MY DATASET (unit test, names match)
  
  # split gene name and individual name from fasta names, req fasta names to be this (req individual files per indiv and implement this)
  #
  #
  ##############FIXIXIXIIXIXIOWDHIUHWEON
  
  pairs <- left_join(pairs, nucs, by = 'gene')
  rm(nucs)
  
  
  prots <- readAAStringSet(prot_file_path, format = 'fasta')
  prots <- data.frame(gene = names(prots), prot = as.character(prots))
  
  pairs <- left_join(pairs, prots, by = 'gene')
  rm(prots)
  
  
  # write pairs to fasta files 
  for (group_id in unique(pairs$group)) {
    group_rows <- pairs %>% filter(group == group_id)
    
    for (row_num in 1:nrow(group_rows)) {
      row <- group_rows[row_num,]
      group_indiv_id <- paste0(group_id, '_', row$individual)
      
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
    
    # mkdirs if don't exist 
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


# change this to paml or something in the cl 
get_dnds <- function() {
  dnds_results <- data.frame()
  
  for (file in list.files(paste0(here_temp, '/Connected_Eq_Codon_Alignments'), full.names = T)) {
    basename <- basename(file)
    group <- str_extract(basename, "(?<=_)[^_]+(?=_)")
    name <- str_match(basename, "group_\\d+_([^\\.]+)\\.fa")[,2]
    
    codon_alignment <- read.alignment(file = file, format = 'fasta')
    dnds <- kaks(codon_alignment)
    
    row <- data.frame(name = name, group = group,dn = dnds['ka'], ds = dnds['ks'])
    dnds_results <- rbind(dnds_results, row)
  }
  return(dnds_results)
}


#####
# prot_file_path IS NOT ALLOWED TO BE PROVIDED BY THE USER (because cant make the file optional)
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



#main_pop_dnds(cnvs_path = 'C:/Users/17735/Downloads/AAAAA_Seg_Dups_Input_Example/connected_dups_sep.tsv',
 #             nuc_file_path = "C:/Users/17735/Downloads/DuplicA/app/Temp/Connected_Eq_Nucleotide_Sequences/group_24_TOM_008.fa",
  #            prot_file_path = NA, # "C:/Users/17735/Downloads/DuplicA/app/Temp/Connected_Eq_Protein_Sequences/group_24_TOM_008.fa"
   #           aligner = 'muscle', 
    #          replace_dirs = F, 
     #         use_all_fastas_in_dir = T)

#cnvs_path = 'C:/Users/17735/Downloads/AAAAA_Seg_Dups_Input_Example/connected_dups_sep.tsv'
#nuc_file_path = "C:/Users/17735/Downloads/DuplicA/app/Temp/Connected_Eq_Nucleotide_Sequences/group_24_TOM_008.fa"
#prot_file_path = NA # "C:/Users/17735/Downloads/DuplicA/app/Temp/Connected_Eq_Protein_Sequences/group_24_TOM_008.fa"
#aligner = 'muscle' 
#replace_dirs = F
#use_all_fastas_in_dir = T


