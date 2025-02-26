

source('C:/Users/17735/Downloads/DuplicA/app/Scripts/multispecies_functions.R')


find_new_expressed_tissues <- function(dups_anc, clean_expression, tissue_list, neo_exp_threshold) {
  
  dups_anc <- dups_anc %>%
    select(Orthogroup, dup_1, dup_2, ancestral_copy) %>%
    pivot_longer(cols = c('dup_1', 'dup_2', 'ancestral_copy'), values_to = 'id', names_to = 'copy') %>%
    merge(., clean_expression, by = 'id')
  
  dups_neo_exp_func <- dups_anc %>%
    pivot_longer(cols = c(4:ncol(.)), names_to = 'tissue', values_to = 'exp') %>%
    group_by(Orthogroup, tissue) %>%
    mutate(
      func = case_when(
        any(copy == "ancestral_copy" & exp == 0) & any(copy == "dup_2" & exp == 0) & any(copy == "dup_1" & exp > neo_exp_threshold) ~ 'neo_dup_1',
        any(copy == "ancestral_copy" & exp == 0) & any(copy == "dup_1" & exp == 0) & any(copy == "dup_2" & exp > neo_exp_threshold) ~ 'neo_dup_2',
        any(copy == "ancestral_copy" & exp == 0) & any(copy == "dup_1" & exp > neo_exp_threshold) & any(copy == "dup_2" & exp > neo_exp_threshold) ~ 'spec',
        TRUE ~ NA))
  
  return(dups_neo_exp_func)
}


main_new_tissue_expression <- function(exp_path, OF_dir_path, tissue_list, neo_exp_threshold, rm_exp_lower_than, PC, min_dups_per_species_pair, useAbsExpr){
  
  OF_dir_path <- paste0(OF_dir_path, '/')
  
  out1 <- get_dups_from_OF(OF_dir_path)
  dups <- out1$dups
  dup_pair_orthologs <- out1$dup_pair_orthologs
  
  
  out2 <- clean_exp_and_pseudo(exp_path, dups, add_pseudofunc = F, missing_expr_is_pseudo = F, rm_exp_lower_than, PC)
  clean_expression <- out2$clean_expression
  
  dups_anc <- get_anc_copy(OF_dir_path, dups, dup_pair_orthologs, clean_expression)
  
  
  if (tissue_list == 'all') {tissue_list <- colnames(clean_expression)[-1]}
  
  dups_neo_exp_func <- find_new_expressed_tissues(dups_anc, clean_expression, tissue_list, neo_exp_threshold)
  write.table(dups_neo_exp_func, file = './NewTissueExpression_Results.tsv') #CHANGE
  
  
}





#args <- c('C:/Users/17735/Downloads/AAAAA___EXAMPLE_Expression.tsv', 'C:/Users/17735/Downloads/AAAAA_Results_Jan01', '1', 'False', '10', 'False')

#exp_path <- args[1]
#OF_dir_path <- args[2]
#rm_exp_lower_than <- as.numeric(args[3])
#PC <- as.logical(args[4])
#min_dups_per_species_pair <- as.numeric(args[5])
#useAbsExpr <- as.logical(args[6])

