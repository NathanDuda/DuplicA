

#OF_dir_path <- 'C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/'
#exp_path <- 'C:/Users/17735/Downloads/Eight_Species/All_Expression_Data.tsv'
#exp_cutoff <- 1
#PC <- T

### simulate a random gn_exons dataset 
#gn_exons <- dups_anc %>% 
#  select(dup_1, dup_2, ancestral_copy) %>%
#  pivot_longer(everything(), values_to = 'id') %>% 
#  select(id) %>%
#  mutate(exon_count = sample(1:20, n(), replace = TRUE))


get_dup_mechanism <- function(dups_anc, exons_path, mech_type) {
  
  # unit test the gn_exons input
  # unit test the gene ids match between exon file and dups file 
  # allow different types of exon path file formats
  #########
  
  # import gn_exons file 
  gn_exons <- read.delim(exons_path, sep = ' ')
  colnames(gn_exons) <- c('id', 'exon_count')
  
  # connect each copy with their exon count 
  dups_anc_exons <- dups_anc %>%
    select(Orthogroup, dup_1, dup_2, ancestral_copy) %>%
    pivot_longer(cols = c(2:4), values_to = 'id', names_to = 'copy') %>%
    merge(gn_exons, by = 'id') %>%
    select(-id) %>%
    pivot_wider(names_from = 'copy', values_from = 'exon_count') %>%
    select(Orthogroup, dup_1_n_exons = dup_1, dup_2_n_exons = dup_2, ancestral_copy_n_exons = ancestral_copy)
  
  
  # classify as DNA-mediated or RNA-mediated based on exon counts
  if (mech_type == 'standard') {
    dup_mech <- dups_anc_exons %>%
      mutate(mech = case_when(dup_1_n_exons > 1 & dup_2_n_exons == 1 ~ 'RNA_dup_2',
                              dup_2_n_exons > 1 & dup_1_n_exons == 1 ~ 'RNA_dup_1',
                              dup_1_n_exons > 1 & dup_2_n_exons > 1 ~ 'DNA',
                              dup_2_n_exons > 1 & dup_1_n_exons > 1 ~ 'DNA',
                              dup_2_n_exons == 1 & dup_1_n_exons == 1 ~ 'unknown'))
    }
  
  if (mech_type == 'conservative') {
    dup_mech <- dups_anc_exons %>%
      mutate(mech = 
               case_when(dup_1_n_exons > 1 & dup_2_n_exons == 1 & ancestral_copy_n_exons > 1 ~ 'RNA_dup_2',
                         dup_2_n_exons > 1 & dup_1_n_exons == 1 & ancestral_copy_n_exons > 1 ~ 'RNA_dup_1',
                         dup_1_n_exons > 1 & dup_2_n_exons > 1 & ancestral_copy_n_exons > 1 ~ 'DNA',
                         dup_2_n_exons > 1 & dup_1_n_exons > 1 & ancestral_copy_n_exons > 1 ~ 'DNA',
                         dup_2_n_exons == 1 & dup_1_n_exons == 1 ~ 'unknown',
                         ancestral_copy_n_exons == 1 ~ 'unknown')) 
    }
  
  if (mech_type == 'ultra-conservative') {
    dup_mech <- dups_anc_exons %>%
    mutate(mech = 
             case_when(dup_1_n_exons > 1 & dup_2_n_exons == 1 & ancestral_copy_n_exons == dup_1_n_exons ~ 'RNA_dup_2',
                       dup_2_n_exons > 1 & dup_1_n_exons == 1 & ancestral_copy_n_exons == dup_2_n_exons ~ 'RNA_dup_1',
                       dup_1_n_exons == dup_2_n_exons & dup_2_n_exons == ancestral_copy_n_exons & ancestral_copy_n_exons > 1 ~ 'DNA',
                       T ~ 'unknown'))
    }
  
  
  dup_mech <- dup_mech %>% select(Orthogroup, mech)
  
  return(dup_mech)
}

main_dup_mechanism <- function(exons_path, dups_anc, mech_type) {
  
  mech <- get_dup_mechanism(dups_anc, exons_path, mech_type)
  
  return(mech)
}







