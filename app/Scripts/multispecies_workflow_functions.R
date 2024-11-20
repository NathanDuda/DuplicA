




format_dups_for_alphafold_db <- function(dups) {
  
  dups <- dups %>%
    mutate(func = 'NA') %>%
    select(-Orthogroup) %>%
    group_by(duplicate_pair_species) %>%
    group_split()
  
  return(dups)
}




