

# example expression file generation 
files <- c('C:/Users/17735/Downloads/Eight_Species/Raw_Data/YO_Expression/GSE99574_HiSAT2_dana.nrc.YO.txt',
           'C:/Users/17735/Downloads/Eight_Species/Raw_Data/YO_Expression/GSE99574_HiSAT2_dmel.nrc.YO.txt',
           'C:/Users/17735/Downloads/Eight_Species/Raw_Data/YO_Expression/GSE99574_HiSAT2_dmoj.nrc.YO.txt')
all_exp <- data.frame()
for (file in files) {
  
  exp <- read.delim(file)
  exp <- exp %>%
    dplyr::select(id = FBgnID, exp = 4) %>%
    filter(grepl("\\d", id))
  
  all_exp <- rbind(all_exp, exp)
  
  
}
write.table(all_exp, file = 'C:/Users/17735/Downloads/AAAAA_Expression_Input_Example/dana_dmel_dmoj_exp.tsv')

#######################################################################################



import_if_exists <- function(file_name) {
  if (file.exists(file_name)) {read.delim(file_name, sep = ' ')}
}


#main_run_workflow(selected_models, input) 




dups <- read.csv(paste0(here_results, '/Duplicates.tsv'), sep="")
colnames(dups)[1] <- 'Orthogroup'


orthogroup_based_files <- c(
  "main_DiversityDivergence_output.tsv",
  "main_ExpressionShift_output.tsv",
  "main_postduplication_fates_output.tsv"
)

files_for_visualization <- c(
  "main_CDROM_output.tsv",
  "main_DiversityDivergence_output.tsv",
  "main_ExpressionShift_output.tsv",
  "main_postduplication_fates_output.tsv")


file_names <- list.files(here_results, full.names = F)
file_names <- intersect(file_names, files_for_visualization)
file_names <- paste0(here_results, '/', file_names)


merge_by_gene <- function(dups, data_to_add, prefix, merge_by_each) {
  for (merge_col in merge_by_each) {
    colnames(data_to_add) <- c(merge_col, paste0(prefix, '_', merge_col))
    dups <- left_join(dups, data_to_add, by = merge_col)
  }
  return(dups)
}


if('Expression.tsv' %in% basename(file_names)) {
  
  exp <- read.delim(paste0(here_results, '/Expression.tsv'), sep = ' ')
  dups <- merge_by_gene(dups, data_to_add = exp, prefix = 'exp', merge_by_each = c('dup_1', 'dup_2'))
  
  
}








if (dir.exists("OrthoFinder")) {}



avail_results <- lapply(file_names, import_if_exists)

names(avail_results) <- basename(file_names)

names_avail_results <- names(avail_results)



all_categor_cols <- c('func', 'Tissue', 'duplicate_pair_species')
all_numeric_cols <- c('PS_dup_1', 'PS_dup_2', 'N_dup_1', 'N_dup_2', 'DN', 'C', 'SB', 'SP', 'LRT_Diversity_Divergence', 'LRT_ExpressionShift')

merge_by_list <- list(
  'main_CDROM_output.tsv' = c('dup_1', 'dup_2'),
  'main_DiversityDivergence_output.tsv' = 'Orthogroup',
  'main_ExpressionShift_output.tsv' = 'Orthogroup',
  'main_postduplication_fates_output.tsv' = 'Orthogroup'
)
new_colnames_list <- list(
  'main_CDROM_output.tsv' = c('dup_1', 'dup_2', 'ancestral_copy', 'func'),
  'main_DiversityDivergence_output.tsv' = c('Orthogroup', 'Tissue', 'LRT_Diversity_Divergence'),
  'main_ExpressionShift_output.tsv' = c('Orthogroup', 'Tissue', 'LRT_ExpressionShift')
#  'main_postduplication_fates_output.tsv' = NA
)





if('main_DiversityDivergence_output.tsv' %in% names_avail_results &
   'main_ExpressionShift_output.tsv' %in% names_avail_results) {
  
  merge_by_list[['main_ExpressionShift_output.tsv']] <- c('Orthogroup', 'Tissue')
}


add_dataset <- function(dataset_name, merge_with, merge_by, avail_results, new_colnames) {
  dataset <- avail_results[names(avail_results) == dataset_name][[1]]
  
  if(!is.null(new_colnames)) {colnames(dataset) <- new_colnames}
  
  merged_dataset <- left_join(merge_with, dataset, by = merge_by)
  
  return(merged_dataset)
}



merge_with <- dups
for (dataset_name in names_avail_results) {
  
  merge_by <- merge_by_list[[dataset_name]]
  new_colnames <- new_colnames_list[[dataset_name]]
  
  all_data <- add_dataset(dataset_name, merge_with, merge_by, avail_results, new_colnames)
  
  merge_with <- all_data
  
}



#####################################################
# VISUALIZATION


dynamic_plot <- function(
    data,
    plot_type,
    x = NULL,
    y = NULL,
    color = NULL,
    size = NULL,
    facet = NULL,
    title = NULL,
    x_label = NULL,
    y_label = NULL,
    legend_label_color = NULL,
    legend_label_size = NULL,
    facet_label = NULL,
    custom_theme = theme_minimal(),
    x_log = FALSE,
    y_log = FALSE,
    ...
) {
  
  # remove NA values for x and y
  data <- data %>% filter(if_any(c(x, y), ~ !is.na(.)))

  # start building the plot
  p <- ggplot(data, aes_string(x = x, y = y, color = color, size = size))
  
  # add plot type
  if (plot_type == "scatterplot") {p <- p + geom_point(...)}
  if (plot_type == "boxplot") {p <- p + geom_boxplot(...)}
  
  # apply log scaling if chosen
  if (x_log) {p <- p + scale_x_log10()}
  if (y_log) {p <- p + scale_y_log10()}
  
  # add faceting if chosen
  if (!is.null(facet)) {
    p <- p + facet_wrap(as.formula(paste("~", facet)), labeller = if (!is.null(facet_label)) as_labeller(facet_label) else label_value)
  }
  
  # add custom labels
  if (!is.null(title)) {p <- p + ggtitle(title)}
  if (!is.null(x_label)) {p <- p + xlab(x_label)}
  if (!is.null(y_label)) {p <- p + ylab(y_label)}
  if (!is.null(legend_label_color)) {p <- p + labs(color = legend_label_color)}
  if (!is.null(legend_label_size)) {p <- p + labs(size = legend_label_size)}
  
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


# example Usage
dynamic_plot(
  data = all_data,
  plot_type = "scatterplot",
  x = "LRT_ExpressionShift",
  y = "N_dup_1",
  facet = NULL,
  facet_label = NULL, 
  title = "Custom Boxplot with Log Scaling",
  x_label = NULL,
  y_label = NULL,
  legend_label_color = NULL,
  custom_theme = NULL, # options are c('bw', 'minimal', 'linedraw', 'classic')
  y_log = F,
  x_log = F
)




cols <- colnames(all_data)

categor_cols <- intersect(cols, all_categor_cols)
numeric_cols <- intersect(cols, all_numeric_cols)


# allow log scaling only when numeric
# ensure categorical/numeric types align with plot type 











##############





