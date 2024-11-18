
source('./app/scripts/setup.R')
source('./app/scripts/NCBI_connection.R')
source('./app/scripts/model_OrthoFinder.R')
source('./app/scripts/multispecies_functions.R')
source('./app/scripts/model_alphafold_db.R')

selected_database_protein <- 'ensembl'
selected_database_cds <- 'ensembl'
selected_database_genome <- NA

selected_organisms <- c('Homo sapiens', 'Pan troglodytes')
data_types <- c('Proteomes', 'CDS')
keep_which_transcript <- 'longest'
must_be_reference <- F

# get protein and cds data from ncbi 
main_ncbi(selected_organisms, data_types, selected_database_protein, selected_database_cds, selected_database_genome, keep_which_transcript, must_be_reference)
prot_output_dir <- paste0(here_results, '/Fastas/Protein_Fastas/')

# run orthofinder on the human + chimp proteins to find duplicate genes
main_OrthoFinder(prot_output_dir, result_name = 'Results', result_dir = here_results)
of_output_dir <- paste0(here_results, '/OrthoFinder/Results/')


# format duplicates from orthofinder results
out <- get_dups_from_OF(of_output_dir)

dups <- out$dups
dup_pair_orthologs <- out$dup_pair_orthologs


#all_dups <- format_dups_for_alphafold_db(dups)

use_ancestral_copy <- T


if (use_ancestral_copy == F) {
  dups$anc <- NA
}


# get ancestral copy
if (use_ancestral_copy == T) {
  
  dups <- get_anc_copy(OF_dir_path = of_output_dir, 
                       dups = dups,
                       dup_pair_orthologs = dup_pair_orthologs, 
                       clean_expression = NA)
  
}

all_dups <- format_dups_for_alphafold_db(dups)


for (i in length(all_dups)) {
  
  dups <- all_dups[[i]]

  chosen_organism <- gsub('_prot', '', dups$duplicate_pair_species[1])
  
  dups <- dups %>%
    select(dup_1, dup_2, anc = ancestral_copy, func)
  

  main_alphafold(dups, chosen_organism)
  
}





# allow no ancestral copy 
# fix proper finding of ancestral copy data since different species 
# deal with error when data does not exist


format_dups_for_alphafold_db <- function(dups) {
  
  dups <- dups %>%
    mutate(func = 'NA') %>%
    select(-Orthogroup) %>%
    group_by(duplicate_pair_species) %>%
    group_split()
  
  return(dups)
}

