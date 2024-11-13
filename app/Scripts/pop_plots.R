
library(tidyverse)


n_individuals <- 47

### segregating duplicates
# Frequency vs. Ds scatterplot
#cnvselectr_output <- read.csv("C:/Users/17735/Downloads/Dmel_Duplicate_Genes/CNVSelectR_Output.tsv", sep="")


output22 <- output %>%
  na.omit() %>%
  group_by(group) %>%
  mutate(n_flies_in = freq * n_individuals) %>%
  mutate(n_flies_in2 = n()) %>%
  mutate(signif = case_when(p_val > 0.05 ~ 'No',
                            p_val <= 0.05 ~ 'Yes'))


ggplot(cnvselectr_output, aes(x=dS, y=n_flies_in, color=signif)) +
  geom_jitter(size = 0.6) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), 
                     labels = c(0, 10, 20, 30, 40, 50)) +
  xlim(0,1) +
  theme_bw() +
  labs(x='Ds', y='Frequency', color='Significant')



# make frequency distributions 
equiv_counts <- equiv_counts_matrix %>%
  mutate_all(., ~replace(., is.na(.), 0)) %>%
  pivot_longer(cols=c(2:ncol(equiv_counts_matrix)))

n_copies_freq_dist <- data.frame()
for (n_copies in c(1:3)){
  n_df <- equiv_counts %>%
    group_by(community) %>%
    summarise(flies_with_n_copies = sum(value == n_copies)) %>%
    mutate(n_copies = n_copies) %>%
    filter(flies_with_n_copies != 0)
  
  n_copies_freq_dist <- rbind(animate_df, n_df)
  print(n_copies)
}

ggplot(n_copies_freq_dist, aes(x = flies_with_n_copies)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(.~n_copies)



