

source('C:/Users/17735/Downloads/DuplicA/app/Scripts/pop_functions.R') ######## CHANGE


combine_raw_fastas <- function() {
  
  nuc_file_extension <- file_ext(nuc_file)
  prot_file_extension <- file_ext(prot_file)
  
  nuc_dir_path <- dirname(nuc_file)
  prot_dir_path <- dirname(prot_file)
  
  nuc_output_file_path <- paste0(nuc_dir_path, '/combined_nuc_fasta.fa')
  prot_output_file_path <- paste0(prot_dir_path, '/combined_prot_fasta.fa')
  
  cat_nuc_command <- paste('wsl', "cat", file.path(nuc_dir_path, '.', nuc_file_extension), ">", nuc_output_file_path)
  system(cat_nuc_command)
  cat_prot_command <- paste('wsl', "cat", file.path(prot_dir_path, '.', prot_file_extension), ">", prot_output_file_path)
  system(cat_prot_command)
  
  
  fasta_files <- list(nuc_file = nuc_output_file_path, prot_file = prot_output_file_path)
  
  return(fasta_files)
}

translate_nucs_to_prots <- function() {
  
  
}

get_paired_fastas <- function(cnvs, nuc_file, prot_file, use_all_fastas_in_dir) {
  
  if(use_all_fastas_in_dir == T){
    fasta_files <- combine_raw_fastas()
    nuc_file <- fasta_files$nuc_file
    prot_file <- fasta_files$prot_file
  }
  
  
  nucs <- readDNAStringSet(nuc_file)
  nucs <- data.frame(gene = names(nucs), nuc = as.character(nucs))  
  
  cnvs <- left_join(cnvs, nucs, by = 'gene')
  rm(nucs)
  
  
  prots <- readAAStringSet(prot_file)
  prots <- data.frame(gene = names(prots), prot = as.character(prots))
  
  cnvs <- left_join(cnvs, prots, by = 'gene')
  rm(prots)
  
  
  # write pairs to fasta files 
  for (group_id in unique(cnvs$group)) {
    group_rows <- cnvs %>% filter(group == group_id)
    
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

main_pop_dnds <- function(cnvs_path, nuc_file, prot_file = NA, aligner = 'muscle', replace_dirs) {
  replace_dirs <- F ############## remove 
  make_temp_dirs(replace_dirs)
  
  pairs <- get_pairs(cnvs_path)
  
  
  get_paired_fastas(pairs, nuc_file, prot_file)
  get_prot_alignments(aligner)
  get_codon_alignments()
  dnds_results <- get_dnds()
  
  write.table(dnds_results, 'C:/Users/17735/Downloads/DuplicA/TRASH_dnds_results.tsv') # CHANGEEE

}







