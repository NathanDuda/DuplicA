



get_pairs <- function(cnvs_path) {
  
  cnvs <- read.csv(cnvs_path, sep="")
  colnames(cnvs) <- c('individual', 'gene', 'group')
  
  pairs <- cnvs %>%
    group_by(individual, group) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 2)
  # underscores in individual name and group name must be changed 
  
  return(pairs)
}



make_temp_dirs <- function(replace_dirs) {
  
  create_or_replace_dir <- function(dir_path) {
    if (replace_dirs == T) {system(paste0('wsl ', 'rm -rf ', dir_path), intern = TRUE)}
    system(paste0('wsl ', 'mkdir -p ', dir_path), intern = TRUE)
  }
  
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Protein_Sequences'))
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Protein_Alignments'))
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Nucleotide_Sequences'))
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Codon_Alignments'))
  
  
}




