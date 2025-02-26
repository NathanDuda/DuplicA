curDir<-getwd()
curDir<-dirname(curDir)
curDir<-dirname(curDir)
source(paste0(curDir,'/multispecies_functions.R'))




calc_dosage_ratio <- function(dups_anc, clean_expression) {
  
  # get expression dataframe for each copy 
  dup_1 <- get_exp_df_for_copy(copy = 'dup_1', dups_anc, clean_expression) %>% select(-Orthogroup)
  dup_2 <- get_exp_df_for_copy(copy = 'dup_2', dups_anc, clean_expression) %>% select(-Orthogroup)
  anc <- get_exp_df_for_copy(copy = 'ancestral_copy', dups_anc, clean_expression) %>% select(-Orthogroup)
  
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

