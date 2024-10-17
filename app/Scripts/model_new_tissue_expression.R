




source('C:/Users/17735/Downloads/DuplicA/app/Scripts/multispecies_functions.R')



main_new_tissue_expression <- function(exp_path, OF_dir_path, rm_exp_lower_than, PC, min_dups_per_species_pair, useAbsExpr){
  
  OF_dir_path <- paste0(OF_dir_path, '/')
  
  out1 <- get_dups_from_OF(OF_dir_path)
  dups <- out1$dups
  dup_pair_orthologs <- out1$dup_pair_orthologs
  
  
  out2 <- clean_exp_and_pseudo(exp_path, dups, add_pseudofunc = F, missing_expr_is_pseudo = F, rm_exp_lower_than, PC)
  clean_expression <- out2$clean_expression
  
  dups_anc <- get_anc_copy(OF_dir_path, dups, dup_pair_orthologs, clean_expression)
  
  
  
}


