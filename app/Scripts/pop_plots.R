
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




