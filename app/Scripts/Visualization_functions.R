
import_if_exists <- function(file_name) {
  if (file.exists(file_name)) {read.delim(file_name, sep = ' ')}
}


merge_by_gene <- function(dups, data_to_add, prefix, merge_by_each) {
  for (merge_col in merge_by_each) {
    colnames(data_to_add) <- c(merge_col, paste0(prefix, '_', merge_col))
    dups <- left_join(dups, data_to_add, by = merge_col)
  }
  return(dups)
}


add_dataset <- function(dataset_name, merge_with, merge_by, avail_results, new_colnames) {
  dataset <- avail_results[names(avail_results) == dataset_name][[1]]
  
  if(!is.null(new_colnames)) {colnames(dataset) <- new_colnames}
  
  merged_dataset <- left_join(merge_with, dataset, by = merge_by)
  
  return(merged_dataset)
}


get_all_results <- function(results_directory = here_results) {
  
  # read in the base file
  dups <- read.csv(paste0(results_directory, '/Duplicates.tsv'), sep="")
  colnames(dups)[1] <- 'Orthogroup'
  
  # list all of the files available for visualization
  files_for_visualization <- c("main_CDROM_output.tsv", "main_DiversityDivergence_output.tsv", "main_ExpressionShift_output.tsv", "main_postduplication_fates_output.tsv", 'Expression.tsv', 'misc_results.tsv')
  
  file_names <- list.files(results_directory, full.names = F)
  file_names <- intersect(file_names, files_for_visualization)
  file_names <- paste0(results_directory, '/', file_names)

  
  # get expression per gene 
  if('Expression.tsv' %in% basename(file_names)) {
    exp <- read.delim(paste0(results_directory, '/Expression.tsv'), sep = ' ')
    dups <- merge_by_gene(dups, data_to_add = exp, prefix = 'exp', merge_by_each = c('dup_1', 'dup_2'))
  }
  
  # organize results for merging 
  avail_results <- lapply(file_names, import_if_exists)
  names(avail_results) <- basename(file_names)
  names_avail_results <- names(avail_results)
  
  names_avail_results <- setdiff(names_avail_results, 'Expression.tsv') # remove Expression.tsv from file names
  
  
  #orthogroup_based_files <- c("main_DiversityDivergence_output.tsv", "main_ExpressionShift_output.tsv", "main_postduplication_fates_output.tsv")
  
  merge_by_list <- list(
    'main_CDROM_output.tsv' = c('dup_1', 'dup_2'),
    'main_DiversityDivergence_output.tsv' = 'Orthogroup',
    'main_ExpressionShift_output.tsv' = 'Orthogroup',
    'main_postduplication_fates_output.tsv' = 'Orthogroup',
    'misc_results.tsv' = c('Orthogroup', 'dup_1', 'dup_2')
  )
  new_colnames_list <- list(
    'main_CDROM_output.tsv' = c('dup_1', 'dup_2', 'ancestral_copy', 'Functional_Category'),
    'main_DiversityDivergence_output.tsv' = c('Orthogroup', 'Tissue', 'LRT_Diversity_Divergence'),
    'main_ExpressionShift_output.tsv' = c('Orthogroup', 'Tissue', 'LRT_ExpressionShift')
    #  'main_postduplication_fates_output.tsv' = NA
    #  'misc_results.tsv' = NA
  )
  
  if('main_DiversityDivergence_output.tsv' %in% names_avail_results &
     'main_ExpressionShift_output.tsv' %in% names_avail_results) {
    
    merge_by_list[['main_ExpressionShift_output.tsv']] <- c('Orthogroup', 'Tissue')
  }
  
  
  # merge all datasets 
  merge_with <- dups
  for (dataset_name in names_avail_results) {
    
    merge_by <- merge_by_list[[dataset_name]]
    new_colnames <- new_colnames_list[[dataset_name]]
    
    all_data <- add_dataset(dataset_name, merge_with, merge_by, avail_results, new_colnames)
    
    merge_with <- all_data
    
  }
  
  
  # misc_results adds another duplicate_pair_species
  if ('duplicate_pair_species.x' %in% colnames(all_data)) {
    all_data <- all_data %>%
      dplyr::select(-duplicate_pair_species.x) %>%
      dplyr::select(duplicate_pair_species = duplicate_pair_species.y, everything())
    
  }
  
  return(all_data)
}


get_visualization_button_options_lists <- function(all_data) {
  cols <- colnames(all_data)
  
  # UPDATE THESE LISTS 
  cols_for_axes <- c('Functional_Category', 'Tissue', 'duplicate_pair_species', 
                     'PS_dup_1', 'PS_dup_2', 'N_dup_1', 'N_dup_2', 'DN', 'C', 'SB', 'SP', 
                     'LRT_Diversity_Divergence', 'LRT_ExpressionShift', 
                     'prot_length_dup_1', 'prot_length_dup_2', 
                     'gc_content_dup_1', 'gc_content_dup_2',
                     'cpg_count_dup_1', 'cpg_count_dup_2') 
  cols_for_groups <- c('Functional_Category', 'Tissue', 'duplicate_pair_species')

  x_and_y_axis_options <- intersect(cols, cols_for_axes)
  group_options <- intersect(cols, cols_for_groups)
  
  return(list(x_and_y_axis_options = x_and_y_axis_options, group_options = group_options))
}


make_visualization_json_file <- function(all_data) {
  
  # get options for visualization page 
  vis_options <- get_visualization_button_options_lists(all_data)

  # read in json file
  json <- fromJSON(paste0(here_duplica, '/app/Front_end/blank_visualization_options.json'))
  
  # update json file with parameters
  json$x_axis$options <- vis_options$x_and_y_axis_options
  json$y_axis$options <- vis_options$x_and_y_axis_options
  
  json$color_groups$options <- vis_options$group_options
  json$separate_figure$options <- vis_options$group_options
  
  json <- toJSON(json, pretty = T)
  write(json, file = paste0(here_duplica, '/app/frontend/duplic-a/src/components/visualization_options.json')) # CHANGE VITE 
  
}



