
source('./app/scripts/setup.R')
source('./app/scripts/multispecies_workflow_functions.R')
source('./app/scripts/multispecies_functions.R')

source('./app/scripts/model_Public_Datasets.R')
source('./app/scripts/model_OrthoFinder.R')
source('./app/scripts/model_CDROM.R')
source('./app/scripts/model_EVE.R')
source('./app/scripts/model_DnDs.R')
source('./app/scripts/model_alphafold_db.R')

selected_database_protein <- 'ensembl'
selected_database_cds <- 'ensembl'
selected_database_genome <- NA

selected_organisms <- c('Homo sapiens', 'Pan troglodytes')
data_types <- c('Proteomes', 'CDS')
keep_which_transcript <- 'longest'
must_be_reference <- F

# get protein and cds data from ncbi 
main_ncbi(selected_organisms, data_types, selected_database_protein, selected_database_cds, selected_database_genome, keep_which_transcript, must_be_reference)
prot_output_dir <- paste0(here_results, '/Fastas/Protein_Fastas/')

# run orthofinder on the human + chimp proteins to find duplicate genes
main_OrthoFinder(prot_output_dir, result_name = 'Results', result_dir = here_results)
of_output_dir <- paste0(here_results, '/OrthoFinder/Results/')


# format duplicates from orthofinder results
out <- get_dups_from_OF(of_output_dir)

dups <- out$dups
dup_pair_orthologs <- out$dup_pair_orthologs


#all_dups <- format_dups_for_alphafold_db(dups)

use_ancestral_copy <- T


if (use_ancestral_copy == F) {
  dups$anc <- NA
}


# get ancestral copy
if (use_ancestral_copy == T) {
  
  dups <- get_anc_copy(OF_dir_path = of_output_dir, 
                       dups = dups,
                       dup_pair_orthologs = dup_pair_orthologs, 
                       clean_expression = NA)
  
}

all_dups <- format_dups_for_alphafold_db(dups)


for (i in length(all_dups)) {
  
  dups <- all_dups[[i]]

  chosen_organism <- gsub('_prot', '', dups$duplicate_pair_species[1])
  
  dups <- dups %>%
    select(dup_1, dup_2, anc = ancestral_copy, func)
  

  main_alphafold(dups, chosen_organism)
  
}





# allow no ancestral copy 
# fix proper finding of ancestral copy data since different species 
# deal with error when data does not exist


#######################################################



# example inputs

# for main_public_datasets
selected_organisms <- c('Homo sapiens', 'Pan troglodytes')

## additional for main_public_datasets
selected_database_protein <- 'ensembl'
selected_database_cds <- 'ensembl'
selected_database_genome <- 'ensembl'
data_types <- c('Proteomes', 'CDS')
keep_which_transcript <- 'longest'
must_be_reference <- F

# for main_orthofinder
user_provided_path_to_protein_directory # If Public Datasets not selected 

## additional


# 
user_provided_path_to_orthofinder_output # If OrthoFinder not selected



main_run_workflow <- function(selected_models, input) {
  
  if('Public Datasets' %in% selected_models) {
    main_public_datasets(selected_organisms = input$selected_organisms, 
                         data_types = input$data_types, 
                         selected_database_protein = input$selected_database_protein, 
                         selected_database_cds = input$selected_database_cds, 
                         selected_database_genome = input$selected_database_genome, 
                         keep_which_transcript = input$keep_which_transcript, 
                         must_be_reference = input$must_be_reference)
    prot_output_dir <- paste0(here_results, '/Fastas/Protein_Fastas/')
  }
  # get protein folder path when public datasets not chosen
  if(!('Public Datasets' %in% selected_models) & ('OrthoFinder' %in% selected_models)) {
    prot_output_dir <- user_provided_path_to_protein_directory # CHANGEEEEEEEEEEEEEEEEEEEEEEEEE
  }
  
  
  if('OrthoFinder' %in% selected_models) {
    
    dir.create(paste0(here_results, '/OrthoFinder'))
    
    main_OrthoFinder(protein_folder = input$protein_folder,
                     is_dna = input$nuc_not_prot, 
                     method = input$gene_tree_inference_method,
                     sequence_search = input$sequence_search_method, 
                     msa_program = input$msa_method,
                     tree_method = input$tree_method,
                     species_tree = input$species_tree_path, 
                     mcl_inflation = input$mcl_inflation,
                     split_hogs = input$split_hogs, 
                     no_msa_trim = input$msa_trim,
                     result_dir = paste0(here_results, '/OrthoFinder/'),
                     result_name = 'Results')
    
    of_output_dir <- paste0(here_results, '/OrthoFinder/Results/')
    
  }
  # get OrthoFinder output path when OrthoFinder not selected
  if(!('OrthoFinder' %in% selected_models)) {
    of_output_dir <- user_provided_path_to_orthofinder_output # CHANGEEEEEEEEEEEEEEEEEEEEEEEEE
  }
  
  
  
  # get dups from OrthoFinder and format expression (if expression file path exists)
  if (!exists(input$exp_path)) {
    exp_path <- NA
    add_pseudofunc <- NA
    missing_expr_is_pseudo <- NA
    rm_exp_lower_than <- NA
  }
  out <- main_get_dups_anc_exp_from_OF(OF_dir_path, exp_path, add_pseudofunc, missing_expr_is_pseudo, rm_exp_lower_than)
  dups_anc <- out$dups_anc
  dups <- out$dups
  clean_expression <- out$clean_expression
  
  
  if ('CDROM' %in% selected_models) {
    main_CDROM(dups = dups, 
               dups_anc = dups_anc, 
               clean_expression = clean_expression, 
               OF_dir_path = input$OF_dir_path,
               add_pseudofunc = input$add_pseudofunc_custom,
               missing_expr_is_pseudo = input$missing_expr_is_pseudo_custom,
               exp_cutoff = input$exp_cutoff_custom,
               PC = input$PC,
               min_dups_per_species_pair = input$min_dups_per_species_pair_custom,
               use_absolute_exp = input$use_absolute_exp_custom
    )
  }
  
  if ('dnds' %in% selected_models) {
    main_multispecies_dnds(OF_dir_path = of_output_dir, 
                           dups = dups, 
                           allow_two_to_twos = input$allow_two_to_twos)
  }
  
  if ('expression_shift' %in% selected_models) {
    main_Expression_Shift(OF_dir_path = of_output_dir, 
                          clean_expression = clean_expression, 
                          dup_species_list = input$dup_species_list, 
                          tissue_list = input$tissue_list,
                          copy_amount = input$copy_amount, 
                          nondup_species_need_onecopy = input$nondup_species_need_onecopy, 
                          use_gene_trees = input$use_gene_trees
    )
  }
  
  if ('diversity_divergence' %in% selected_models) {
    main_DiversityDivergence(OF_dir_path = of_output_dir,
                             clean_expression = clean_expression, 
                             copy_amount = input$copy_amount,
                             dup_species_list = input$dup_species_list, 
                             tissue_list = input$tissue_list, 
                             nondup_species_need_onecopy = input$nondup_species_need_onecopy, 
                             use_gene_trees = input$use_gene_trees, 
                             lower_beta_lim = input$lower_beta_lim, 
                             upper_beta_lim = input$upper_beta_lim
    )
  }
  
  
  if ('alphafold_db' %in% selected_models) {
    main_alphafold(dups = dups_anc,
                   chosen_organisms = input$chosen_organisms)
  }
  
  
  
  
}







