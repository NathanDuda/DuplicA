
prefix <- 'C:'

source(paste0(prefix, '/Users/17735/Downloads/DuplicA/app/Scripts/setup.R')) # CHANGE PATH 
here_duplica <- paste0(prefix, '/Users/17735/Downloads/DuplicA') # for sourcing this script in wsl  

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
#source(paste0(here_duplica, '/app/Scripts/model_GO.R'))
source(paste0(here_duplica, '/app/Scripts/model_misc.R'))

source(paste0(here_duplica, '/app/Scripts/tool_ID_conversion.R'))








# allow no ancestral copy 
# fix proper finding of ancestral copy data since different species 
# deal with error when data does not exist


#######################################################



# example parameterss
parameters <- list()

parameters$exp_path <- 'C:/Users/17735/Downloads/AAAAA_Expression_Input_Example/dana_dmel_dmoj_exp.tsv'
parameters$normalization_type <- NA
parameters$add_pseudofunc <- T
parameters$missing_expr_is_zero <- F
parameters$rm_exp_lower_than <- 1
parameters$min_dups_per_species_pair_custom <- 10

parameters$use_absolute_exp = T
parameters$PC = F

####parameters$selected_organisms <- 'Drosophila melanogaster, Drosophila ananassae, Drosophila mojavensis'
parameters$selected_organisms <- 'Drosophila ananassae, Drosophila melanogaster'
parameters$exp_path <- NULL

# for expression shift
####parameters$dup_species_list <- 'Drosophila_melanogaster_prot, Drosophila_ananassae_prot, Drosophila_mojavensis_prot'
parameters$dup_species_list <- 'Drosophila ananassae, Drosophila melanogaster'

parameters$copy_amount = 2
parameters$tissue_list = 'All Tissues'
parameters$use_gene_trees = T
# for diversity divergence
parameters$lower_beta_lim = 1 # guess
parameters$upper_beta_lim = 10 # guess

# for postduplication fates
parameters$v = 0.2
parameters$p = 0.05

# for main_public_datasets
#parameters$selected_organisms <- c('Drosophila melanogaster', 'Arabidopsis thaliana', 'Saccharomyces cerevisiae')


## additional for main_public_datasets
parameters$selected_database_protein <- 'ensembl'

if(parameters$selected_database_protein == 'ensembl') {parameters$id_type_of_gene_ids <- 'ensembl'} # will convert exon datasets id to this format  


parameters$selected_database_cds <- 'ensembl'
parameters$selected_database_genome <- 'ensembl'
parameters$data_types <- c('Proteomes', 'CDS')
parameters$keep_which_transcript <- 'longest'
parameters$must_be_reference <- F

# for main_orthofinder



#user_provided_path_to_protein_directory # If Public Datasets not selected 
#nuc_output_dir <- user_provided_path_to_nucleotide_directory # If Public Datasets not selected 

## additional
parameters$nuc_not_prot = F
parameters$gene_tree_inference_method = 'dendroblast'
parameters$sequence_search_method = 'diamond'
parameters$msa_method = NA
parameters$tree_method = NA
parameters$species_tree_path = NULL
parameters$mcl_inflation = 1.5
parameters$split_hogs = F
parameters$msa_trim = F

# 
#user_provided_path_to_orthofinder_output # If OrthoFinder not selected

#user_provided_path_to_exon_output_dir # If no exon_datasets but yes duplication_mechanism

#
parameters$selected_database_exon = 'refseq'

# duplciation mechanism
parameters$mech_type <- 'standard'

# for dnds
parameters$allow_two_to_twos <- F
parameters$aligner <- 'muscle'

# for eve
parameters$nondup_species_need_onecopy = F


###
selected_models <- list('Public Datasets', 'OrthoFinder', 'expression_shift', 'diversity_divergence')
parameters$get_public_exon_data <- T


main_run_workflow <- function(selected_models, parameters) {
  
  if(parameters$get_public_exon_data) {selected_models <- c(selected_models, 'exon_datasets')}
  
  #if('Public Datasets' %in% selected_models) {selected_models <- c(selected_models, 'OrthoFinder')}
  
  # split strings into lists 
  if (!is.null(parameters$selected_organisms)) {parameters$selected_organisms <- split_into_list(parameters$selected_organisms)}
  if (!is.null(parameters$dup_species_list)) {parameters$dup_species_list <- split_into_list(parameters$dup_species_list)}
  if(!is.null(parameters$tissue_list)) {parameters$tissue_list <- split_into_list(parameters$tissue_list)}
  
  
  
  if('Public Datasets' %in% selected_models) {
    
    result_dir <- paste0(here_results, '/Fastas/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)}
    
    kept_transcript_dir <- paste0(here_results, '/Fastas/kept_transcript/')
    dir.create(dirname(kept_transcript_dir))
    dir.create(kept_transcript_dir)
    
    main_public_datasets(selected_organisms = parameters$selected_organisms, 
                         data_types = parameters$data_types, 
                         selected_database_protein = parameters$selected_database_protein, 
                         selected_database_cds = parameters$selected_database_cds, 
                         selected_database_genome = parameters$selected_database_genome, 
                         keep_which_transcript = parameters$keep_which_transcript, 
                         must_be_reference = parameters$must_be_reference)
    prot_output_dir <- paste0(here_results, '/Fastas/Protein_Fastas/')
    nuc_output_dir <- paste0(here_results, '/Fastas/Nucleotide_Fastas/')
  }
  # get protein folder path when public datasets not chosen
  if(!('Public Datasets' %in% selected_models) & ('OrthoFinder' %in% selected_models)) {
    prot_output_dir <- parameters$protein_folder 
    if ('dnds' %in% selected_models) {nuc_output_dir <- parameters$nuc_folder}
  }
  
  if('OrthoFinder' %in% selected_models) {
    
    result_dir <- paste0(here_results, '/OrthoFinder/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)} # delete result directory if exists already
    
    main_OrthoFinder(protein_folder = prot_output_dir,
                     is_dna = parameters$nuc_not_prot, 
                     method = parameters$gene_tree_inference_method,
                     sequence_search = parameters$sequence_search_method, 
                     msa_program = parameters$msa_method,
                     tree_method = parameters$tree_method,
                     species_tree = parameters$species_tree_path, 
                     mcl_inflation = parameters$mcl_inflation,
                     split_hogs = parameters$split_hogs, 
                     no_msa_trim = parameters$msa_trim,
                     result_dir = gsub('C:/', '/mnt/c/', result_dir),
                     result_name = 'Results')
    
    of_output_dir <- paste0(here_results, '/OrthoFinder/Results_Results')
    
  }
  # get OrthoFinder output path when OrthoFinder not selected
  if(!('OrthoFinder' %in% selected_models)) {
    of_output_dir <- parameters$ortho_dir
  }
  
  
  # get dups from OrthoFinder and format expression (if expression file path exists)
  if (is.null(parameters$exp_path)) {
    parameters$exp_path <- NA
    parameters$normalization_type <- NA
    parameters$add_pseudofunc <- F
    parameters$missing_expr_is_zero <- NA
    parameters$rm_exp_lower_than <- NA
  }
  out <- main_get_dups_anc_exp_from_OF(OF_dir_path = of_output_dir, 
                                       exp_path = parameters$exp_path, 
                                       normalization_type = parameters$normalization_type, 
                                       add_pseudofunc = parameters$add_pseudofunc, 
                                       missing_expr_is_zero = parameters$missing_expr_is_zero, 
                                       rm_exp_lower_than = parameters$rm_exp_lower_than)
  dups_anc <- out$dups_anc
  dups <- out$dups
  
  if(parameters$add_pseudo == T) {pseudo = out$pseudo}
  clean_expression <- out$clean_expression
  
  
  if ('CDROM' %in% selected_models) {
    main_CDROM_output <- main_CDROM(dups = dups, 
               dups_anc = dups_anc, 
               clean_expression = clean_expression, 
               OF_dir_path = OF_dir_path,
               add_pseudofunc = parameters$add_pseudofunc,
               missing_expr_is_zero = parameters$missing_expr_is_zero,
               PC = parameters$PC,
               min_dups_per_species_pair = parameters$min_dups_per_species_pair_custom,
               useAbsExpr = parameters$use_absolute_exp,
               pseudo = pseudo
    )
  }
  
  if ('dnds' %in% selected_models) {
    main_DnDs_output <- main_multispecies_dnds(OF_dir_path = of_output_dir, 
                           dups = dups, 
                           allow_two_to_twos = parameters$allow_two_to_twos,
                           nuc_file_path = list.files(nuc_output_dir, full.names = T)[1], # gets all fastas in dir by default
                           prot_file_path = list.files(prot_output_dir, full.names = T)[1],
                           aligner = parameters$aligner)
  }
  
  if ('expression_shift' %in% selected_models) {
    main_ExpressionShift_output <- main_Expression_Shift(OF_dir_path = of_output_dir, 
                          clean_expression = clean_expression, 
                          dup_species_list = parameters$dup_species_list, 
                          tissue_list = parameters$tissue_list,
                          copy_amount = parameters$copy_amount, 
                          nondup_species_need_onecopy = parameters$nondup_species_need_onecopy, 
                          use_gene_trees = parameters$use_gene_trees
    )
  }
  
  if ('diversity_divergence' %in% selected_models) {
    main_DiversityDivergence_output <- main_DiversityDivergence(OF_dir_path = of_output_dir,
                             clean_expression = clean_expression, 
                             copy_amount = parameters$copy_amount,
                             dup_species_list = parameters$dup_species_list, 
                             tissue_list = parameters$tissue_list, 
                             nondup_species_need_onecopy = parameters$nondup_species_need_onecopy, 
                             use_gene_trees = parameters$use_gene_trees, 
                             lower_beta_lim = parameters$lower_beta_lim, 
                             upper_beta_lim = parameters$upper_beta_lim
    )
  }
  
  # format chosen organisms (get file_organism_table) and format_dups_for_db_search
  if(('alphafold_db' %in% selected_models) | ('go' %in% selected_models)) {
    
    # get file_organism_table 
    selected_organisms <- colnames(read.delim(paste0(of_output_dir, '/Comparative_Genomics_Statistics/Statistics_PerSpecies.tsv'), nrows = 0))[-1]
    file_organism_table <- data.frame(protein_file_name = sort(list.files(prot_output_dir, full.names = T)),
                                      organism_names = sort(selected_organisms))
    

    all_copies <- format_dups_for_db_search(dups_anc)
  }
  
  if ('alphafold_db' %in% selected_models) {
    main_alphafold(all_copies = all_copies,
                   file_organism_table = file_organism_table)
  }
  
  if('postduplication_fates' %in% selected_models) {
    main_postduplication_fates_output <- main_postduplication_fates(dups_anc = dups_anc, 
                                                                    clean_expression = clean_expression, 
                                                                    v = parameters$v, 
                                                                    p = parameters$p)
  }
  
  if(('exon_datasets' %in% selected_models) & ('Public Datasets' %in% selected_models)) {
    
    result_dir <- paste0(here_results, '/Exon_Counts/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)}
    
    result_dir <- paste0(here_results, '/public_datasets_output/')
    if(dir.exists(result_dir)) {dir_delete(result_dir)}
    
    main_exon_datasets(selected_organisms = parameters$selected_organisms, 
                       selected_database_exon = parameters$selected_database_exon, 
                       must_be_reference = parameters$must_be_reference,
                       id_type_of_gene_ids = 'ensembl',
                       kept_transcript_dir = kept_transcript_dir) # remove last '/'
    gn_exons_dir <- paste0(here_results, '/Exon_Counts/')
    
  }
  
  if(!('exon_datasets' %in% selected_models) & ('duplication_mechanism' %in% selected_models)) {
    gn_exons_dir <- parameters$exons_folder
    # file names do not need to match up, all files are combined. merged by gene (genes cant be shared across species)
  }
  
  if('duplication_mechanism' %in% selected_models) {
    main_dup_mechanism_output <- main_dup_mechanism(gn_exons_dir = gn_exons_dir, 
                                                    dups_anc = dups_anc, 
                                                    mech_type = parameters$mech_type, 
                                                    selected_organisms = parameters$selected_organisms)
  }
  
  if('go' %in% selected_models) {
    main_go(all_copies, file_organism_table)
  }
  
  if('pathway' %in% selected_models) {
    
  }
  
  main_get_misc_results(dups, prot_output_dir, nuc_output_dir, 
                        raw_dup_mechanism_output_file_path = paste0(here_results, '/raw_dup_mechanism_output.tsv'))
  
  
  # read in all results from the (default input) Results directory
  all_data <- get_all_results()
  write.table(all_data, paste0(here_results, '/All_data.tsv'))
  
  # make json file for visualization 
  make_visualization_json_file(all_data)
  
  write.table(dups, paste0(here_results, '/Duplicates.tsv'))
}




all_models <- list('Public Datasets', 'OrthoFinder', 'CDROM', 'dnds', 'expression_shift', 'diversity_divergence', 'alphafold_db', 'postduplication_fates', 'duplication_mechanism', 'pathway')

sequential_models <- list('Public Datasets', 'OrthoFinder',
                          'alphafold_db', 'exon_datasets')
parallel_models <- list('CDROM', 'dnds', 'expression_shift', 'diversity_divergence', 'postduplication_fates', 'pathway',
                        'duplication_mechanism', 'go')



#future_map()





#selected_models <- "OrthoFinder, CDROM, Public Datasets" 

#selected_models <- list('Public Datasets', 'OrthoFinder', 'CDROM')

main_get_relevant_parameter_list <- function(selected_models, parameters) {
  
  print(selected_models)
  #print(parameters)
  
  #print(parameters[["missing_exp_is_zero"]])
  #print('CDROM' %in% selected_models)
  
    
  ######selected_models <- strsplit(selected_models, ',')[[1]]
  #######selected_models <- trimws(selected_models)
  

  
  # NOTE: if 'Public Datasets' in selected_models, 'OrthoFinder' must be chosen
  
  required_parameter_list <- list() 
  additional_parameter_list <- list() 

  if('Public Datasets' %in% selected_models) {
    required_parameter_list <- c(required_parameter_list, 'selected_organisms')
    additional_parameter_list <- c(additional_parameter_list, 'data_types', 'must_be_reference', 'keep_which_transcript')
    
    
    if('Proteomes' %in% parameters[['data_types']]) {additional_parameter_list <- c(additional_parameter_list, 'selected_database_protein')}
    if('CDS' %in% parameters[['data_types']]) {additional_parameter_list <- c(additional_parameter_list, 'selected_database_cds')}
    if('Genomes' %in% parameters[['data_types']]) {additional_parameter_list <- c(additional_parameter_list, 'selected_database_genome')}
    
    
    if('OrthoFinder' %in% selected_models) {
      if(!'Proteomes' %in% parameters[['data_types']]) {required_parameter_list <- c(required_parameter_list, 'protein_folder')}
      additional_parameter_list <- c(additional_parameter_list, 'custom_species_tree', 'sequence_search_method', 'gene_tree_inference_method', 'mcl_inflation', 'split_hogs', 'nuc_not_prot')
      if(parameters[['gene_tree_inference_method']] == 'msa') {additional_parameter_list <- c(additional_parameter_list, 'msa_method', 'tree_method', 'msa_trim')}
    }
  }
  
  if(!'Public Datasets' %in% selected_models) {
    if('OrthoFinder' %in% selected_models) {
      required_parameter_list <- c(required_parameter_list, 'protein_folder')
      additional_parameter_list <- c(additional_parameter_list, 'custom_species_tree', 'sequence_search_method', 'gene_tree_inference_method', 'mcl_inflation', 'split_hogs', 'nuc_not_prot')
    }
    if(!'OrthoFinder' %in% selected_models) {
      required_parameter_list <- c(required_parameter_list, 'ortho_dir')
    }
  }

  # get expression, if expression models selected
  if(any(c('CDROM', 'expression_shift', 'diversity_divergence', 'postduplication_fates') %in% selected_models)) {
    required_parameter_list <- c(required_parameter_list, 'expression_directory')
    additional_parameter_list <- c(additional_parameter_list, 'exp_cutoff', 'missing_expr_is_zero')
  }
  
  if('CDROM' %in% selected_models) {
    additional_parameter_list <- c(additional_parameter_list, 'min_dups_per_species_pair', 'add_pseudofunc', 'use_absolute_exp')
  }
  
  
  if('duplication_mechanism' %in% selected_models) {
    required_parameter_list <- c(required_parameter_list, 'get_public_exon_data')
    
    if(isFALSE(parameters[['get_public_exon_data']])) { 
      required_parameter_list <- c(required_parameter_list, 'exons_folder')
    }
    additional_parameter_list <- c(additional_parameter_list, 'mech_type')
  }
  
  if('postduplication_fates' %in% selected_models) {
    additional_parameter_list <- c(additional_parameter_list, 'v', 'p')
  }
  
  if('dnds' %in% selected_models) {
    if(!'CDS' %in% parameters[['data_types']]) {required_parameter_list <- c(required_parameter_list, 'nuc_folder')}
    additional_parameter_list <- c(additional_parameter_list, 'dnds_aligner')
  }

  if(any(c('expression_shift', 'diversity_divergence') %in% selected_models)) {
    additional_parameter_list <- c(additional_parameter_list, 'dup_species_list', 'use_gene_trees', 'tissue_list', 'copy_amount', 'nondup_species_need_onecopy')
    
  }

  print('returning :D')
  print(required_parameter_list)
  return(list(additional_parameter_list = additional_parameter_list,
              required_parameter_list = required_parameter_list))
}



# Example main_get_relevant_parameter_list usage:

# necessary options to test
#parameters <- list()
#parameters$data_types <- NULL
#parameters$gene_tree_inference_method <- 'dendroblast'

# custom options added to test
#parameters$data_types <- 'Proteomes'


#l <- main_get_relevant_parameter_list(c('OrthoFinder', 'alphafold_db', 'CDROM'), parameters)
#l$required_parameter_list
#l$additional_parameter_list



# removed nuc_not_prot - deal with detecting if aa or dna and translate when dna






#selected_organisms <- c('Drosophila melanogaster', 'Drosophila ananassae', 'Drosophila mojavensis')
# selected_organisms is already split up 
#prot_output_dir <- 'C:/Users/17735/Downloads/AAAAA_Protein_Folder'

#generate_file_organism_table(prot_output_dir, selected_organisms)


# its assumed that no gene ids are the same across species 


