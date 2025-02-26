



#raw_counts_file <- 'C:/Users/17735/Downloads/AAAAA___EXAMPLE_Expression.tsv'
#raw_counts <- read.delim(raw_counts_file, sep = ' ') 
#gene_lengths <- data.frame(id = raw_counts$YOgnID, length = 2.1)
#rm(raw_counts)
#n_replicates <- 7

normalization_function <- function(raw_counts_file, gene_lengths_file, n_replicates, normalization_type, exp_cutoff) {
  
  #### UNIT TESTTTTTTTTTTTTTTTSSS
  #####
  # file format
  # numeric counts
  
  ###
  
  # import raw files 
  gene_lengths <- read.delim(gene_lengths_file, sep = ' ')
  colnames(gene_lengths) <- c('id', 'length')  
  
  raw_counts <- read.delim(raw_counts_file, sep = ' ') # ALLOW FOR DIFFERENT FILE FORMATSSSSSSS
  colnames(raw_counts)[1] <- c('id')
  
  # combine the counts and gene lengths datasets 
  raw_counts <- raw_counts %>%
    merge(., gene_lengths, by = 'id') %>%
    select(id, length, everything())
  
  # take the avg counts of all replicates
  n_groups <- (ncol(raw_counts) - 2 ) / n_replicates
  
  # normalize every column of raw reads
  norm_counts <- raw_counts 
  for (column in colnames(raw_counts)[3:ncol(raw_counts)]) {
    
    if (normalization_type == 'RPKM') {
      total_reads <- sum(raw_counts[[column]])
      total_reads <- total_reads / 1e6 
      
      norm_counts <- norm_counts %>%
        mutate(length_kb = length / 1000,
               !!paste0('RPKM_', column) := .data[[column]] / (length_kb * total_reads))
      
    }
    
    if (normalization_type == 'CPM') {
      total_reads <- sum(counts[[column]])
      
      norm_counts <- norm_counts %>%
        mutate(!!paste0('CPM_', column) := 1e6 * .data[[column]] / total_reads)
    }
    
    if (normalization_type == 'TPM') {
      norm_counts <- norm_counts %>% mutate(n_over_l = .data[[column]] / length)
      denominator <- sum(norm_counts$n_over_l) / 1e6
      
      norm_counts <- norm_counts %>%
        mutate(!!paste0('TPM_', column) := n_over_l / denominator)
    }
    
  }
  
  # keep only normalized columns
  norm_counts <- norm_counts %>%
    select(id, starts_with(paste(normalization_type)))

  # get the average expression for each group
  avg_counts <- norm_counts
  for (group in 1:n_groups) {
    
    starting_col <- 2 + n_replicates*(group-1)
    ending_col <- 1 + n_replicates*group
    
    first_group <- colnames(avg_counts)[starting_col]
    last_group <- colnames(avg_counts)[ending_col]
    
    avg_counts <- avg_counts %>%
      mutate(!!paste0(first_group, '_to_', last_group) := rowMeans(select(avg_counts, starting_col:ending_col), na.rm = TRUE))
  }
  
  # keep only the columns with the averages
  exp <- avg_counts %>%
    select(id, tail(names(.), n_groups))
  
  # set expression values lower than exp_cutoff to 0 
  exp <- exp %>%
    mutate(across(2:ncol(.), ~ if_else(. <= exp_cutoff, 0, .)))
  
  # write averaged expression to file
  write.table(exp, file = paste0(here_results, '/normalized_expression.tsv'))
  
}






