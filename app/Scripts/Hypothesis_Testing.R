

#library(stats)
library(car)



all_data


# PER GENE 

column_1 <- 'exp_dup_1'
is <- 'higher' # can be any of c('higher', 'lower')
column_2 <- 'exp_dup_2'

subset_column <- 'func'
subset_factor <- 'Neofunctionalization(Dup1)'

sig_threshold = 0.05


paired_data <- T


data <- all_data %>%
  filter(.data[[subset_column]] == subset_factor)



get_test_to_use <- function(data, column_1, column_2, sig_threshold){
  # test for normality 
  norm_pval_col1 <- shapiro.test(data[[column_1]])$p.value
  norm_pval_col2 <- shapiro.test(data[[column_2]])$p.value
  
  # if normal
  if(norm_pval_col1 < sig_threshold & norm_pval_col2 < sig_threshold) {
    if (paired_data) {test_to_use <- 'wilcox_signed_rank'}
    if (!paired_data) {test_to_use <- 'wilcox_ranked_sum'}
  }
  
  # if not normal
  if(norm_pval_col1 >= sig_threshold & norm_pval_col2 >= sig_threshold) {
    if (paired_data) {test_to_use <- 'paired_students_test'}
    if (!paired_data) {
      # test for equal variance 
      data_long <- data %>% select(all_of(c(column_1, column_2))) %>% pivot_longer(cols = everything(), names_to = "Group", values_to = "Value")
      equalvar_pval <- leveneTest(Value ~ Group, data = data_long)[["Pr(>F)"]][1]
      
      # if nonequal variances 
      if(equalvar_pval < sig_threshold) {test_to_use <- 'welchs_test'}
      # if equal variances 
      if(equalvar_pval >= sig_threshold) {test_to_use <- 'unpaired_students_test'} 
    }
  }
  
  return(test_to_use)
}



test_to_use <- get_test_to_use(data, column_1, column_2, sig_threshold)


use_test <- function(test_to_use, data, column_1, column_2, sig_threshold) {
  
  # run the indicated test 
  if (test_to_use == "wilcox_signed_rank") {
    test_result <- wilcox.test(data[[column_1]], data[[column_2]], paired = T)
  }
  
  if (test_to_use == "wilcox_ranked_sum") {
    test_result <- wilcox.test(data[[column_1]], data[[column_2]], paired = F)
  }
  
  if (test_to_use == "paired_students_test") {
    test_result <- t.test(data[[column_1]], data[[column_2]], paired = T)
  } 
  
  if (test_to_use == "welchs_test") {
    t.test(data[[column_1]], data[[column_2]], paired = F, var.equal = F)
  }
  
  if (test_to_use == "unpaired_students_test") {
    t.test(data[[column_1]], data[[column_2]], paired = F, var.equal = T)
  }
  
  # extract the p-value
  p_value <- test_result$p.value
  
  return(p_value)
}


p_value <- use_test(test_to_use, data, column_1, column_2, sig_threshold)



get_significance_summary <- function(p_value, data, column_1, column_2, sig_threshold) {
  
  signif <- p_value < sig_threshold
  higher <- mean(data[[column_1]]) > mean(data[[column_2]])
  lower <- mean(data[[column_1]]) < mean(data[[column_2]])
  
  
  if (signif) {
    if (is == 'higher') {
      if (higher) {stat_summary <- 'signif in chosen direction'}
      if (lower) {stat_summary <- 'signif in other direction'}
    }
    
    if (is == 'lower') {
      if (higher) {stat_summary <- 'signif in other direction'}
      if (lower) {stat_summary <- 'signif in chosen direction'}
    }
  }
  
  if (!signif) {
    stat_summary <- 'not signif'
  }
  
  
  pval_larger_or_smaller_than_threshold <- ''
  null_hy_is <- 'rejected, and the alternative hypothesis is accepted.'
  conclusion <- ''
  
  stat_summary <- paste0(
    
    '
    Hypothesis tested:
    P-value: ', p_value, '
    
    The p_value is ', pval_larger_or_smaller_than_threshold, ' than the significance threshold (', sig_threshold,'),
    therefore, the null hypothesis is ', null_hy_is,'
    
    Conclusion:
    ', conclusion
    
    
    
  
  
    
    
  )
  
  
  return(stat_summary)
}




get_significance_summary(p_value, data, column_1, column_2, sig_threshold)




