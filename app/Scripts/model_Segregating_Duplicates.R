
library(tidyverse)
library(Biostrings)
library(seqinr)
#install_github("peterbchi/CNVSelectR")
library(CNVSelectR)
source(paste0(here_duplica, '/app/Dependencies/CNVSelectR/CNVSelectR_Source.R')) ######## CHANGE
source(paste0(here_duplica, '/app/Scripts/pop_functions.R')) ######## CHANGE
#######################




get_freqs <- function(pairs) {
  freqs <- pairs %>%
    group_by(group) %>%
    summarise(freq = n_distinct(individual))
  
  return(freqs)
}



##################################################


run_CNVSelectR <- function(freqs, dnds_results_path, neut_matrix, n_individuals, ploidy, ks_oversaturation_cutoff, filter_whole_group) {
  
  freqs <- freqs %>%
    mutate(group = as.character(group))
  
  dnds_results <- read.csv(dnds_results_path, sep = '')
  dnds <- dnds_results %>%
    mutate(group = as.character(group)) %>%
    left_join(freqs, ., by = 'group') %>%
    na.omit() # remove non-pairs that were in freqs but not dnds_results 
    
  
  if (filter_whole_group == T){
    dnds <- dnds %>%
      filter(ks > 0 & ks < ks_oversaturation_cutoff) %>%
      mutate(freq = freq / n_individuals) 
  }
    
  if (filter_whole_group == F){
    dnds <- dnds %>%
      group_by(group) %>% 
      filter(all(ks > 0 & ks < ks_oversaturation_cutoff)) %>%
      ungroup() %>%
      mutate(freq = freq / n_individuals) 
  }
      
  
  total_result <- data.frame()
  
  for (group in unique(dnds$group)) {
    
    dS <- dnds$ks[dnds$group == group]
    names <- dnds$name[dnds$group == group]
    freq <- dnds$freq[dnds$group == group] 
    
    nrep <- length(freq)
    
    
    input_file_table <- data.frame(
      A = c("Ne", "ploidy", "full/approximate", "frequency"),
      B = c(n_individuals, ploidy, NA, freq[1])) # full/approx is never implemented in any code 
    
    
    group_result <- CNVSelect_test_altered(neut_matrix, input_file_table, dS, names)
    
    group_result$group <- group
    group_result <- na.omit(group_result)
    group_result$name <- names
    total_result <- rbind(total_result, group_result)
    
    
    print(group)

  }
  return(total_result)
}




##########

main_Segregating_Duplications <- function(cnvs_path, dnds_results_path, n_individuals, ploidy, ks_oversaturation_cutoff, filter_whole_group) {
  
  pairs <- get_pairs(cnvs_path)
  freqs <- get_freqs(pairs)
  
  neut_matrix <- CNVSelectR::Genedupdip_neutralgenerator(n_individuals, up = 1e-3)
  output <- run_CNVSelectR(freqs, dnds_results_path, neut_matrix, n_individuals, ploidy, ks_oversaturation_cutoff, filter_whole_group)
  

}

###########




#main_Segregating_Duplications(cnvs_path = "C:/Users/17735/Downloads/Dmel_Duplicate_Genes/connected_dups_sep.tsv", 
#                              dnds_results_path = "C:/Users/17735/Downloads/AAAAA_Seg_Dups_input_Example/TRASH_dnds_results.tsv", 
 #                             n_individuals = 47, 
  #                            ploidy = 2, 
   #                           ks_oversaturation_cutoff = 0.8,
    #                          filter_whole_group = T)


