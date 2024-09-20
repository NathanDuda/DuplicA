
library(tidyverse)
library(Biostrings)
library(seqinr)
#install_github("peterbchi/CNVSelectR")
library(CNVSelectR)
source('C:/Users/17735/Downloads/DuplicA/app/Dependencies/CNVSelectR/CNVSelectR_Source.R') ######## CHANGE
source('C:/Users/17735/Downloads/DuplicA/app/Scripts/pop_functions.R') ######## CHANGE
#######################




get_freqs <- function(pairs) {
  freqs <- pairs %>%
    group_by(group) %>%
    summarise(freq = n_distinct(individual))
  
  return(freqs)
}



##################################################


run_CNVSelectR <- function(freqs, dnds_results, neut_matrix, n_individuals, ploidy, ks_oversaturation_cutoff, filter_whole_group) {
  
  freqs <- freqs %>%
    mutate(group = as.character(group))
  
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
      
    total_result <- rbind(total_result, group_result)

  }
  return(total_result)
}




##########

main_Segregating_Duplications <- function(cnvs_path, popgen_dnds_exists = F, n_individuals, ploidy, ks_oversaturation_cutoff, filter_whole_group) {
  
  pairs <- get_pairs(cnvs_path)
  freqs <- get_freqs(pairs)
  
  popgen_dnds_exists <- T # REMOVEEEEEEEEEEEEEEEEEEEEEE
  if (popgen_dnds_exists == F){source('C:/Users/17735/Downloads/DuplicA/app/Scripts/model_DnDs.R') ######## CHANGE
                               main_pop_dnds()
                               popgen_dnds_exists <- T}
  
  if (popgen_dnds_exists == T){dnds_results <- read.csv('C:/Users/17735/Downloads/DuplicA/TRASH_dnds_results.tsv', sep = '')} # makes group numeric when all numbers 
  
  
  neut_matrix <- CNVSelectR::Genedupdip_neutralgenerator(n_individuals, up = 1e-3)
  output <- run_CNVSelectR(freqs, dnds_results, neut_matrix, n_individuals, ploidy, ks_oversaturation_cutoff, filter_whole_group)
  

}

###########




#main_Segregating_Duplications(cnvs_path = "C:/Users/17735/Downloads/Dmel_Duplicate_Genes/connected_dups_sep.tsv", 
 #                             popgen_dnds_exists = T, 
  #                            n_individuals = 47, 
   #                           ploidy = 2, 
    #                          ks_oversaturation_cutoff = 0.8,
     #                         filter_whole_group = T)


