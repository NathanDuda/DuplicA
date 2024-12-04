
source('./app/scripts/setup.R')
source('./app/scripts/multispecies_workflow_functions.R')
source('./app/scripts/multispecies_functions.R')

source('./app/scripts/model_Public_Datasets.R')
source('./app/scripts/model_OrthoFinder.R')
source('./app/scripts/model_CDROM.R')
source('./app/scripts/model_EVE.R')
source('./app/scripts/model_DnDs.R')
source('./app/scripts/model_alphafold_db.R')
source('./app/scripts/model_postduplication_fates.R')
source('./app/scripts/model_Exon_Datasets.R')
source('./app/scripts/model_duplication_mechanism.R')

library(R.utils)
#library(conflicted)
#conflicts_prefer(dplyr::select)








# allow no ancestral copy 
# fix proper finding of ancestral copy data since different species 
# deal with error when data does not exist


#######################################################



# example inputs
input <- list()

# for main_public_datasets
input$selected_organisms <- c('Homo sapiens', 'Drosophila melanogaster', 'Arabidopsis thaliana')

## additional for main_public_datasets
input$selected_database_protein <- 'ensembl'
input$selected_database_cds <- 'ensembl'
input$selected_database_genome <- 'ensembl'
input$data_types <- c('Proteomes', 'CDS')
input$keep_which_transcript <- 'longest'
input$must_be_reference <- F

# for main_orthofinder



user_provided_path_to_protein_directory # If Public Datasets not selected 

## additional
input$nuc_not_prot = F
input$gene_tree_inference_method = 'dendroblast'
input$sequence_search_method = 'diamond'
input$msa_method = NA
input$tree_method = NA
input$species_tree_path = NULL
input$mcl_inflation = 1.5
input$split_hogs = F
input$msa_trim = F

# 
user_provided_path_to_orthofinder_output # If OrthoFinder not selected

user_provided_path_to_exon_output_dir # If no exon_datasets but yes duplication_mechanism

#
input$selected_database_exon = 'refseq'



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
    kept_transcript_dir <- paste0(here_results, '/Fastas/kept_transcript/')
  }
  # get protein folder path when public datasets not chosen
  if(!('Public Datasets' %in% selected_models) & ('OrthoFinder' %in% selected_models)) {
    prot_output_dir <- user_provided_path_to_protein_directory # CHANGEEEEEEEEEEEEEEEEEEEEEEEEE
  }
  
  
  if('OrthoFinder' %in% selected_models) {
    

    main_OrthoFinder(protein_folder = prot_output_dir,
                     is_dna = input$nuc_not_prot, 
                     method = input$gene_tree_inference_method,
                     sequence_search = input$sequence_search_method, 
                     msa_program = input$msa_method,
                     tree_method = input$tree_method,
                     species_tree = input$species_tree_path, 
                     mcl_inflation = input$mcl_inflation,
                     split_hogs = input$split_hogs, 
                     no_msa_trim = input$msa_trim,
                     result_dir = paste0(here_linux_results, '/OrthoFinder/'),
                     result_name = 'Results')
    
    of_output_dir <- paste0(here_results, '/OrthoFinder/Results_Results')
    
  }
  # get OrthoFinder output path when OrthoFinder not selected
  if(!('OrthoFinder' %in% selected_models)) {
    of_output_dir <- user_provided_path_to_orthofinder_output # CHANGEEEEEEEEEEEEEEEEEEEEEEEEE
  }
  
  
  
  # get dups from OrthoFinder and format expression (if expression file path exists)
  if (!exists(input$exp_path)) {
    input$exp_path <- NA
    input$normalization_type <- NA
    input$add_pseudofunc <- NA
    input$missing_expr_is_pseudo <- NA
    input$rm_exp_lower_than <- NA
  }
  out <- main_get_dups_anc_exp_from_OF(OF_dir_path = of_output_dir, 
                                       exp_path = input$exp_path, 
                                       normalization_type = input$normalization_type, 
                                       add_pseudofunc = input$add_pseudofunc, 
                                       missing_expr_is_pseudo = input$missing_expr_is_pseudo, 
                                       rm_exp_lower_than = input$rm_exp_lower_than)
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
  
  # format chosen organisms (get file_organism_table) and format_dups_for_db_search
  if(('alphafold_db' %in% selected_models) | ('go' %in% selected_models)) {
    
    if ('Public Datasets' %in% selected_models) {
      file_organism_table <- data.frame(protein_file_name = list.files(prot_output_dir, full.names = T),
                                        organism_scientific_name = gsub('_prot.fasta', '', list.files(prot_output_dir)))
    }
    
    if (!'Public Datasets' %in% selected_models) {
      # match: prot_output_dir
      # to: listGenomes() 
      file_organism_table <- as.data.frame(input$file_organism_table)
      colnames(file_organism_table) <- c('protein_file_name', 'organism_scientific_name')
    }
    

    all_copies <- format_dups_for_db_search(dups_anc)
  }
  
  if ('alphafold_db' %in% selected_models) {
    main_alphafold(all_copies = all_copies,
                   file_organism_table = file_organism_table)
  }
  
  if('postduplication_fates' %in% selected_models) {
    main_postduplication_fates(dups_anc, clean_expression, input$v, input$p)
  }
  
  if(('exon_datasets' %in% selected_models) & ('Public Datasets' %in% selected_models)) {
    main_exon_datasets(selected_organisms = input$selected_organisms, 
                       selected_database_exon = input$selected_database_exon, 
                       must_be_reference = input$must_be_reference)
    exon_output_dir <- paste0(here_results, '/Exon_Counts/')
    
  }
  
  if(!('exon_datasets' %in% selected_models) & ('duplication_mechanism' %in% selected_models)) {
    exon_output_dir <- user_provided_path_to_exon_output_dir
    # species names need to match up 
    
  }
  
  if('duplication_mechanism' %in% selected_models) {
    main_dup_mechanism(input$exons_path, dups_anc, input$mech_type)
  }
  
  if('go' %in% selected_models) {
    main_go(all_copies, file_organism_table)
  }
  
  
  
  
}







