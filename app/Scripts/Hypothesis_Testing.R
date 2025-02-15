
library(car)

source(paste0(here_duplica, '/app/Scripts/Hypothesis_Testing_functions.R'))


# PER GENE 
main_hypothesis_test <- function(data, column_1, is, column_2, one_tailed, subset_column, subset_factor, sig_threshold, paired_data) {
  
  # get subset of data 
  data <- data %>% filter(.data[[subset_column]] == subset_factor)
  
  # perform hypothesis test 
  test_to_use <- get_test_to_use(data, column_1, column_2, sig_threshold, paired_data)
  p_value <- use_test(test_to_use, data, column_1, column_2, sig_threshold, one_tailed, is)
  summary <- get_significance_summary(p_value, test_to_use, data, column_1, is, column_2, sig_threshold, subset_column, subset_factor)
  
  cat(summary)
}



# example usage 
main_hypothesis_test(
  data = all_data,
  subset_column = 'Functional_Category',
  subset_factor = 'Pseudo(Dup1)',
  
  column_1 = 'prot_length_dup_1',
  is = 'lower',
  column_2 = 'prot_length_dup_2',
  
  one_tailed = F,
  sig_threshold = 0.01,
  paired_data = T
)


