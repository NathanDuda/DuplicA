


# PER GENE

get_test_to_use <- function(data, column_1, column_2, sig_threshold, paired_data){
  # test for normality 
  norm_pval_col1 <- shapiro.test(data[[column_1]])$p.value
  norm_pval_col2 <- shapiro.test(data[[column_2]])$p.value
  
  # if normal
  if(norm_pval_col1 < sig_threshold & norm_pval_col2 < sig_threshold) {
    if (paired_data) {test_to_use <- 'Wilcox Signed-Rank test'}
    if (!paired_data) {test_to_use <- 'Wilcox Ranked-Sum test'}
  }
  
  # if not normal
  if(norm_pval_col1 >= sig_threshold & norm_pval_col2 >= sig_threshold) {
    if (paired_data) {test_to_use <- 'Paired Students t-test'}
    if (!paired_data) {
      # test for equal variance 
      data_long <- data %>% select(all_of(c(column_1, column_2))) %>% pivot_longer(cols = everything(), names_to = "Group", values_to = "Value")
      equalvar_pval <- leveneTest(Value ~ Group, data = data_long)[["Pr(>F)"]][1]
      
      # if nonequal variances 
      if(equalvar_pval < sig_threshold) {test_to_use <- 'Wilcox Ranked-Sum test'}
      # if equal variances 
      if(equalvar_pval >= sig_threshold) {test_to_use <- 'Unpaired Students t-test'} 
    }
  }
  
  return(test_to_use)
}

use_test <- function(test_to_use, data, column_1, column_2, sig_threshold, one_tailed, is) {
  
  if(!one_tailed) {alternative <- 'two.sided'}
  if(one_tailed){
    if(is == 'higher') {alternative <- 'greater'}
    if(is == 'lower') {alternative <- 'less'}
  }
  
  
  # run the indicated test 
  if (test_to_use == "Wilcox Signed-Rank test") {
    test_result <- wilcox.test(data[[column_1]], data[[column_2]], paired = T, alternative = alternative)
  }
  
  if (test_to_use == "Wilcox Ranked-Sum test") {
    test_result <- wilcox.test(data[[column_1]], data[[column_2]], paired = F, alternative = alternative)
  }
  
  if (test_to_use == "Paired Students t-test") {
    test_result <- t.test(data[[column_1]], data[[column_2]], paired = T, alternative = alternative)
  } 
  
  if (test_to_use == "Welchs test") {
    t.test(data[[column_1]], data[[column_2]], paired = F, var.equal = F, alternative = alternative)
  }
  
  if (test_to_use == "Unpaired Students t-test") {
    t.test(data[[column_1]], data[[column_2]], paired = F, var.equal = T, alternative = alternative)
  }
  
  # extract the p-value
  p_value <- test_result$p.value
  
  return(p_value)
}

get_significance_summary <- function(p_value, test_used, data, column_1, is, column_2, sig_threshold, subset_column, subset_factor) {
  
  hypothesis_tested <- paste0(column_1, ' is significantly ', is, ' than ', column_2, ' when ', subset_column, ' is ', subset_factor, '.')
  null_hypothesis <- paste0(column_1, ' is equal to ', column_2, ' when ', subset_column, ' is ', subset_factor, '. Any differences are not significant.')
  
  signif <- p_value < sig_threshold
  higher <- mean(data[[column_1]]) > mean(data[[column_2]])
  lower <- mean(data[[column_1]]) < mean(data[[column_2]])
  
  
  if (signif) {
    
    pval_larger_or_smaller_than_threshold <- 'smaller'
    null_hy_is <- 'rejected, and the alternative hypothesis is accepted.'
    conclusion <- hypothesis_tested
    
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
    pval_larger_or_smaller_than_threshold <- 'larger'
    null_hy_is <- 'accepted.'
    conclusion <- null_hypothesis
    
    stat_summary <- 'not signif'
  }
  
  
  stat_summary <- paste0(
    
    '
    Null hypothesis: 
      ', null_hypothesis, '
    Hypothesis tested: 
      ', hypothesis_tested, '
    Test used:
      ', test_used, '
      
    p-value: ', p_value, '
    
    The p-value is ', pval_larger_or_smaller_than_threshold, ' than the significance threshold (', sig_threshold,'). Therefore, the null hypothesis is ', null_hy_is,'
    
    Conclusion:
      ', conclusion
    
    
    
    
    
    
    
  )
  
  
  return(stat_summary)
}



# PER DUPLICATION 





