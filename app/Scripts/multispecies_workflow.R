
source('./app/Scripts/setup.R') # CHANGE PATH 
source(paste0(here_duplica, '/app/Scripts/multispecies_functions.R'))

source(paste0(here_duplica, '/app/Scripts/model_Public_Datasets.R'))
source(paste0(here_duplica, '/app/Scripts/model_OrthoFinder.R'))
source(paste0(here_duplica, '/app/Scripts/model_CDROM.R'))
source(paste0(here_duplica, '/app/Scripts/model_EVE.R'))
source(paste0(here_duplica, '/app/Scripts/model_DnDs.R'))
source(paste0(here_duplica, '/app/Scripts/model_alphafold_db.R'))
source(paste0(here_duplica, '/app/Scripts/model_postduplication_fates.R'))
source(paste0(here_duplica, '/app/Scripts/model_Exon_Datasets.R'))
source(paste0(here_duplica, '/app/Scripts/model_duplication_mechanism.R'))
source(paste0(here_duplica, '/app/Scripts/model_GO.R'))

source(paste0(here_duplica, '/app/Scripts/tool_ID_conversion.R'))








# allow no ancestral copy 
# fix proper finding of ancestral copy data since different species 
# deal with error when data does not exist


#######################################################



# example inputs
input <- list()

input$exp_path <- 'C:/Users/17735/Downloads/AAAAA_Expression_Input_Example/dana_dmel_dmoj_exp.tsv'
input$normalization_type <- NA
input$add_pseudofunc <- T
input$missing_expr_is_pseudo <- F
input$rm_exp_lower_than <- 1
input$min_dups_per_species_pair_custom <- 10

input$use_absolute_exp = T
input$PC = F

input$selected_organisms <- c('Drosophila melanogaster', 'Drosophila ananassae', 'Drosophila mojavensis')

# for expression shift
input$dup_species_list <- c('Drosophila_melanogaster_prot', 'Drosophila_ananassae_prot', 'Drosophila_mojavensis_prot')
input$copy_amount = 2
input$tissue_list = 'All Tissues'
input$use_gene_trees = T
# for diversity divergence
input$lower_beta_lim = 1 # guess
input$upper_beta_lim = 10 # guess

# for postduplication fates
input$v = 0.2
input$p = 0.05

# for main_public_datasets
#input$selected_organisms <- c('Drosophila melanogaster', 'Arabidopsis thaliana', 'Saccharomyces cerevisiae')


## additional for main_public_datasets
input$selected_database_protein <- 'ensembl'

if(input$selected_database_protein == 'ensembl') {input$id_type_of_gene_ids <- 'ensembl'} # will convert exon datasets id to this format  


input$selected_database_cds <- 'ensembl'
input$selected_database_genome <- 'ensembl'
input$data_types <- c('Proteomes', 'CDS')
input$keep_which_transcript <- 'longest'
input$must_be_reference <- F

# for main_orthofinder



#user_provided_path_to_protein_directory # If Public Datasets not selected 
#nuc_output_dir <- user_provided_path_to_nucleotide_directory # If Public Datasets not selected 

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
#user_provided_path_to_orthofinder_output # If OrthoFinder not selected

#user_provided_path_to_exon_output_dir # If no exon_datasets but yes duplication_mechanism

#
input$selected_database_exon = 'refseq'

# duplciation mechanism
input$mech_type <- 'standard'

# for dnds
input$allow_two_to_twos <- F

# for eve
input$nondup_species_need_onecopy = F

###
selected_models <- list('Public Datasets', 'OrthoFinder')

main_run_workflow <- function(selected_models, input) {
  
  if('Public Datasets' %in% selected_models) {
    
    result_dir <- paste0(here_results, '/Fastas/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)}
    
    kept_transcript_dir <- paste0(here_results, '/Fastas/kept_transcript/')
    dir.create(dirname(kept_transcript_dir))
    dir.create(kept_transcript_dir)
    
    main_public_datasets(selected_organisms = input$selected_organisms, 
                         data_types = input$data_types, 
                         selected_database_protein = input$selected_database_protein, 
                         selected_database_cds = input$selected_database_cds, 
                         selected_database_genome = input$selected_database_genome, 
                         keep_which_transcript = input$keep_which_transcript, 
                         must_be_reference = input$must_be_reference)
    prot_output_dir <- paste0(here_results, '/Fastas/Protein_Fastas/')
    nuc_output_dir <- paste0(here_results, '/Fastas/Nucleotide_Fastas/')
  }
  # get protein folder path when public datasets not chosen
  if(!('Public Datasets' %in% selected_models) & ('OrthoFinder' %in% selected_models)) {
    prot_output_dir <- user_provided_path_to_protein_directory # CHANGEEEEEEEEEEEEEEEEEEEEEEEEE
    nuc_output_dir <- user_provided_path_to_nucleotide_directory
  }
  
  if('OrthoFinder' %in% selected_models) {
    
    result_dir <- paste0(here_results, '/OrthoFinder/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)} # delete result directory if exists already
    
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
                     result_dir = gsub('C:/', '/mnt/c/', result_dir),
                     result_name = 'Results')
    
    of_output_dir <- paste0(here_results, '/OrthoFinder/Results_Results')
    
  }
  # get OrthoFinder output path when OrthoFinder not selected
  if(!('OrthoFinder' %in% selected_models)) {
    of_output_dir <- user_provided_path_to_orthofinder_output # CHANGEEEEEEEEEEEEEEEEEEEEEEEEE
  }
  
  # get dups from OrthoFinder and format expression (if expression file path exists)
  if (is.null(input$exp_path)) {
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
  
  if(input$add_pseudo == T) {pseudo = out$pseudo}
  clean_expression <- out$clean_expression
  
  
  if ('CDROM' %in% selected_models) {
    main_CDROM_output <- main_CDROM(dups = dups, 
               dups_anc = dups_anc, 
               clean_expression = clean_expression, 
               OF_dir_path = OF_dir_path,
               add_pseudofunc = input$add_pseudofunc,
               missing_expr_is_pseudo = input$missing_expr_is_pseudo,
               PC = input$PC,
               min_dups_per_species_pair = input$min_dups_per_species_pair_custom,
               useAbsExpr = input$use_absolute_exp,
               pseudo = pseudo
    )
  }
  
  if ('dnds' %in% selected_models) {
    main_DnDs_output <- main_multispecies_dnds(OF_dir_path = of_output_dir, 
                           dups = dups, 
                           allow_two_to_twos = input$allow_two_to_twos,
                           nuc_file_path = list.files(nuc_output_dir, full.names = T)[1], 
                           prot_file_path = list.files(prot_output_dir, full.names = T)[1],
                           aligner = 'muscle')
  }
  
  if ('expression_shift' %in% selected_models) {
    main_ExpressionShift_output <- main_Expression_Shift(OF_dir_path = of_output_dir, 
                          clean_expression = clean_expression, 
                          dup_species_list = input$dup_species_list, 
                          tissue_list = input$tissue_list,
                          copy_amount = input$copy_amount, 
                          nondup_species_need_onecopy = input$nondup_species_need_onecopy, 
                          use_gene_trees = input$use_gene_trees
    )
  }
  
  if ('diversity_divergence' %in% selected_models) {
    main_DiversityDivergence_output <- main_DiversityDivergence(OF_dir_path = of_output_dir,
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
    main_postduplication_fates_output <- main_postduplication_fates(dups_anc = dups_anc, 
                                                                    clean_expression = clean_expression, 
                                                                    v = input$v, 
                                                                    p = input$p)
  }
  
  if(('exon_datasets' %in% selected_models) & ('Public Datasets' %in% selected_models)) {
    
    result_dir <- paste0(here_results, '/Exon_Counts/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)}
    
    result_dir <- paste0(here_results, '/public_datasets_output/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)}
    
    main_exon_datasets(selected_organisms = input$selected_organisms, 
                       selected_database_exon = input$selected_database_exon, 
                       must_be_reference = input$must_be_reference,
                       id_type_of_gene_ids = 'ensembl',
                       kept_transcript_dir = kept_transcript_dir) # remove last '/'
    exon_output_dir <- paste0(here_results, '/Exon_Counts/')
    
  }
  
  if(!('exon_datasets' %in% selected_models) & ('duplication_mechanism' %in% selected_models)) {
    exon_output_dir <- user_provided_path_to_exon_output_dir
    # species names need to match up 
    
  }
  
  if('duplication_mechanism' %in% selected_models) {
    main_dup_mechanism_output <- main_dup_mechanism(exon_output_dir, dups_anc, input$mech_type, input$selected_organisms)
  }
  
  if('go' %in% selected_models) {
    main_go(all_copies, file_organism_table)
  }
  
  if('pathway' %in% selected_models) {}
  
  
  write.table(dups, paste0(here_results, '/Duplicates.tsv'))
}



selected_models <- list('Public Datasets', 'OrthoFinder', 'CDROM', 'dnds', 'expression_shift', 
                        'diversity_divergence', 'alphafold_db', 'postduplication_fates', 
                        'duplication_mechanism')

main_get_button_list <- function(selected_models, input) {
  
  # NOTE: if 'Public Datasets' in selected_models, 'OrthoFinder' must be chosen
  
  required_button_list <- list() 
  additional_button_list <- list() 
  
  if('Public Datasets' %in% selected_models) {
    required_button_list <- c(required_button_list, 'selected_organisms')
    additional_button_list <- c(additional_button_list, 'data_types', 'selected_database_protein', 'selected_database_cds', 'selected_database_genome', 'must_be_reference', 'keep_which_transcript')
    if('OrthoFinder' %in% selected_models) {
      if(!'Proteomes' %in% input$data_types) {required_button_list <- c(required_button_list, 'Protein Folder')}
      additional_button_list <- c(additional_button_list, 'custom_species_tree', 'sequence_search_method', 'gene_tree_inference_method', 'mcl_inflation', 'split_hogs', 'nuc_not_prot')
      if(input$gene_tree_inference_method == 'msa') {additional_button_list <- c(additional_button_list, 'msa_method', 'tree_method', 'msa_trim')}
    }
  }
  
  if(!'Public Datasets' %in% selected_models) {
    if('OrthoFinder' %in% selected_models) {
      required_button_list <- c(required_button_list, 'Protein Folder')
      additional_button_list <- c(additional_button_list, 'custom_species_tree', 'sequence_search_method', 'gene_tree_inference_method', 'mcl_inflation', 'split_hogs', 'nuc_not_prot')
    }
    if(!'OrthoFinder' %in% selected_models) {
      required_button_list <- c(required_button_list, 'ortho_dir')
    }
  }
  
  
  
  
  if('CDROM' %in% selected_models) {
    
  }
  
  
  
  
  #if(all(c('Public Datasets', 'OrthoFinder') %in% selected_models)) {}
  
  

  
  
  
  return(list(additional_button_list = additional_button_list,
              required_button_list = required_button_list))
  
  
}




# add gene length and gc content to miscellaneous results 
# add option for exon datasets into duplication mechanism,
# only then add exon counts to miscellaneous



