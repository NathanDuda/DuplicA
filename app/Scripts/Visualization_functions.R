library(RColorBrewer)


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
  files_for_visualization <- c("main_CDROM_output.tsv", "main_DiversityDivergence_output.tsv", "main_ExpressionShift_output.tsv", "main_postduplication_fates_output.tsv")
  
  file_names <- list.files(results_directory, full.names = F)
  file_names <- intersect(file_names, files_for_visualization)
  file_names <- paste0(results_directory, '/', file_names)
  
  
  # get expression per gene 
  if('Expression.tsv' %in% basename(file_names)) {
    exp <- read.delim(paste0(here_results, '/Expression.tsv'), sep = ' ')
    dups <- merge_by_gene(dups, data_to_add = exp, prefix = 'exp', merge_by_each = c('dup_1', 'dup_2'))
  }
  
  # organize results for merging 
  avail_results <- lapply(file_names, import_if_exists)
  names(avail_results) <- basename(file_names)
  names_avail_results <- names(avail_results)
  
  
  #orthogroup_based_files <- c("main_DiversityDivergence_output.tsv", "main_ExpressionShift_output.tsv", "main_postduplication_fates_output.tsv")
  
  merge_by_list <- list(
    'main_CDROM_output.tsv' = c('dup_1', 'dup_2'),
    'main_DiversityDivergence_output.tsv' = 'Orthogroup',
    'main_ExpressionShift_output.tsv' = 'Orthogroup',
    'main_postduplication_fates_output.tsv' = 'Orthogroup'
  )
  new_colnames_list <- list(
    'main_CDROM_output.tsv' = c('dup_1', 'dup_2', 'ancestral_copy', 'Functional_Category'),
    'main_DiversityDivergence_output.tsv' = c('Orthogroup', 'Tissue', 'LRT_Diversity_Divergence'),
    'main_ExpressionShift_output.tsv' = c('Orthogroup', 'Tissue', 'LRT_ExpressionShift')
    #  'main_postduplication_fates_output.tsv' = NA
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
  
  return(all_data)
}


get_visualization_button_options_lists <- function(all_data) {
  cols <- colnames(all_data)
  
  # UPDATE THESE LISTS 
  cols_for_axes <- c('Functional_Category', 'Tissue', 'duplicate_pair_species', 
                     'PS_dup_1', 'PS_dup_2', 'N_dup_1', 'N_dup_2', 'DN', 'C', 'SB', 'SP', 'LRT_Diversity_Divergence', 'LRT_ExpressionShift') 
  cols_for_groups <- c('Functional_Category', 'Tissue', 'duplicate_pair_species')

  x_and_y_axis_options <- intersect(cols, cols_for_axes)
  group_options <- intersect(cols, cols_for_groups)
  
  return(x_and_y_axis_options = x_and_y_axis_options, group_options = group_options)
}


# main function to make the figure 
generate_figure <- function(data,
                            
                            figure_type = NULL,
                            x = NULL,
                            y = NULL,
                            color_groups = NULL,
                            separate_figure = NULL,
                            
                            title = NULL,
                            x_label = NULL,
                            y_label = NULL,
                            legend_label = NULL,
                            point_size = 3,
                            
                            custom_theme = NULL,
                            x_log = FALSE,
                            y_log = FALSE,
                            color_set = NULL
) {
  
  
  # test for proper inputs 
  if (x_log && !is.numeric(data[[x]])) {
    stop("Log scale can only be applied to numeric variables for x.")
  }
  if (y_log && !is.numeric(data[[y]])) {
    stop("Log scale can only be applied to numeric variables for y.")
  }
  
  # Ensure plot type matches the variable types
  if (figure_type == "boxplot" | figure_type == 'violin') {
    if (!is.factor(data[[x]]) && !is.character(data[[x]]) &&
        !is.factor(data[[y]]) && !is.character(data[[y]])) {
      stop("Boxplot requires at least one of x or y to be categorical.")
    }
  }
  
  # remove NA values for x and y
  data <- data %>% filter(if_any(c(x, y), ~ !is.na(.)))
  
  # start building the plot
  p <- ggplot(data, aes_string(x = x, y = y, color = color, size = size))
  
  # add plot type
  if (figure_type == "scatterplot") {p <- p + geom_point(size = point_size, ...)}
  if (figure_type == "boxplot") {p <- p + geom_boxplot(...)}
  if (figure_type == "violin") {p <- p + geom_violin(...)}
  
  # apply log scaling if chosen
  if (x_log) {p <- p + scale_x_log10()}
  if (y_log) {p <- p + scale_y_log10()}
  
  # add custom color set 
  if (!is.null(color_set)) {
    p <- p + scale_color_manual(values = brewer.pal(2, color_set)
    )
  }
  
  # add faceting if chosen
  if (!is.null(separate_figure)) {
    p <- p + facet_wrap(as.formula(paste("~", separate_figure)))
  }
  
  # add custom labels
  if (!is.null(title)) {p <- p + ggtitle(title)}
  if (!is.null(x_label)) {p <- p + xlab(x_label)}
  if (!is.null(y_label)) {p <- p + ylab(y_label)}
  
  # add custom theme
  if(is.null(custom_theme)) {p <- p + theme_classic()}
  if(!is.null(custom_theme)) {
    if(custom_theme == 'classic') {p <- p + theme_classic()}
    if(custom_theme == 'bw') {p <- p + theme_bw()}
    if(custom_theme == 'minimal') {p <- p + theme_minimal()}
    if(custom_theme == 'linedraw') {p <- p + theme_linedraw()}
  }
  return(p)
}



