
source('./app/Scripts/multispecies_functions.R')




calc_dosage_ratio <- function(dups_anc, clean_expression) {
  
  # make dataframe for each copy 
  dups <- dups_anc %>% select(dup_1, dup_2, ancestral_copy)
  
  dup_1 <- dups[1]
  dup_2 <- dups[2]
  anc <- dups[3]
  
  # merge dups with their expression data 
  colnames(clean_expression)[1] <- 'dup_1'
  dup_1 <- merge(dup_1, clean_expression, by = 'dup_1')
  
  colnames(clean_expression)[1] <- 'dup_2'
  dup_2 <- merge(dup_2, clean_expression, by = 'dup_2')
  
  colnames(clean_expression)[1] <- 'ancestral_copy'
  anc <- merge(anc, clean_expression, by = 'ancestral_copy')
  
  # add up the expression values across every tissue for all genes 
  dup_1 <- dup_1 %>%  mutate(sum_exp = rowSums(across(2:ncol(.)))) %>% select(sum_exp)
  dup_2 <- dup_2 %>%  mutate(sum_exp = rowSums(across(2:ncol(.)))) %>% select(sum_exp)
  anc <- anc %>%  mutate(sum_exp = rowSums(across(2:ncol(.)))) %>% select(sum_exp)
  
  # calculate the dosage ratio 
  ratio <- median((dup_1$sum_exp + dup_2$sum_exp)) / median(anc$sum_exp)
  
  return(ratio)
}


main_dosage_ratio <- function(OF_dir_path, exp_path, exp_cutoff, PC) {
  
  out1 <- get_dups_from_OF(OF_dir_path)
  dups <- out1$dups
  dup_pair_orthologs <- out1$dup_pair_orthologs
  
  
  out2 <- clean_exp_and_pseudo(exp_path, dups, add_pseudofunc = F, missing_expr_is_pseudo = F, exp_cutoff, PC)
  clean_expression <- out2$clean_expression
  
  dups_anc <- get_anc_copy(OF_dir_path, dups, dup_pair_orthologs, clean_expression)
  
  ratio <- calc_dosage_ratio(dups_anc, clean_expression)
  
  return(ratio)
}



#OF_dir_path <- 'C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/'
#exp_path <- 'C:/Users/17735/Downloads/Eight_Species/All_Expression_Data.tsv'
#exp_cutoff <- 1
#PC <- T
# 1:0.9499665 ratio compared to 1:2, suggesting dosage is in effect (explains conservation)

