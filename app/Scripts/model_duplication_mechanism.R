

get_dup_mechanism <- function(dups_anc, gn_exons, mech_type) {
  
  # unit test the gn_exons input
  # unit test the gene ids match between exon file and dups file 
  # allow different types of exon path file formats
  #########
  
  # import gn_exons file 
  #gn_exons <- read.delim(exons_path, sep = ' ')
  colnames(gn_exons) <- c('id', 'exon_count')
  
  # connect each copy with their exon count 
  dups_anc_exons <- dups_anc %>%
    dplyr::select(Orthogroup, dup_1, dup_2, ancestral_copy) %>%
    pivot_longer(cols = c(2:4), values_to = 'id', names_to = 'copy') %>%
    merge(gn_exons, by = 'id') %>%
    dplyr::select(-id) %>%
    pivot_wider(names_from = 'copy', values_from = 'exon_count') %>%
    dplyr::select(Orthogroup, dup_1_n_exons = dup_1, dup_2_n_exons = dup_2, ancestral_copy_n_exons = ancestral_copy)
  
  
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
  
  
  write.table(dup_mech, file = paste0(here_results, '/raw_dup_mechanism_output.tsv'))
  
  dup_mech <- dup_mech %>% select(Orthogroup, mech)
  
  return(dup_mech)
}


main_dup_mechanism <- function(gn_exons_dir, dups_anc, mech_type, selected_organisms) {
  
  gn_exons <- cat_all_in_dir(gn_exons_dir)
  
  mech <- get_dup_mechanism(dups_anc, gn_exons, mech_type)
  
  mech_output_path <- paste0(here_results, '/main_dup_mechanism_output.tsv')
  write.table(mech, file = mech_output_path, row.names = F, sep = '/t')
  
  return(mech)
}


