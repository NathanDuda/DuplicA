library(tidyverse)
library(biomartr)
library(r3dmol)
library(httr)
library(bio3d)



get_alphafold_for_pair <- function(dup_1, dup_2, func, output_directory, output_file, output_file2) {
  
  # query alphafold database for the alphafold dataset
  alphafold_dataset_1 <- biomart(mart = mart, dataset = 'dmelanogaster_eg_gene', attributes = 'alphafold', filters = 'ensembl_gene_id', genes = dup_1)
  alphafold_dataset_2 <- biomart(mart = mart, dataset = 'dmelanogaster_eg_gene', attributes = 'alphafold', filters = 'ensembl_gene_id', genes = dup_2)
  
  
  if(nrow(alphafold_dataset_1) > 0 & nrow(alphafold_dataset_2) > 0) {
    
    alphafold_id <- alphafold_dataset_1$alphafold[1]
    alphafold_id2 <- alphafold_dataset_2$alphafold[1]
    
    uniprot_id <- sub("AF-([A-Za-z0-9]+)-F[0-9]+", "\\1", alphafold_id)
    uniprot_id2 <- sub("AF-([A-Za-z0-9]+)-F[0-9]+", "\\1", alphafold_id2)
    url <- paste0("https://alphafold.ebi.ac.uk/files/", alphafold_id, "-model_v3.pdb")
    url2 <- paste0("https://alphafold.ebi.ac.uk/files/", alphafold_id2, "-model_v3.pdb")
    
    output_file <- paste0(output_directory, 'pair', row, '_copy1_func', func, '.pdb')
    output_file2 <- paste0(output_directory, 'pair', row, '_copy2_func', func, '.pdb')
    
    response <- GET(url, write_disk(output_file, overwrite = TRUE))
    response2 <- GET(url2, write_disk(output_file2, overwrite = TRUE))
    
    
    #pdb1 <- read.pdb(output_file)
    #pdb2 <- read.pdb(output_file2)
    return('pdbs exist')
  }
  return('no pdb exists for either dup')
  
}


visualize_pdb <- function(pdb_file) {
  
  r3dmol() %>%
    m_add_model(data = pdb_file, format = "pdb") %>%
    m_set_style(style = m_style_cartoon(color = "spectrum")) %>%
    m_zoom_to() %>%
    m_spin(axis = "y", speed = 0.5)  
  
  
}


get_avail_data_for_organism <- function(chosen_organism) {
  chosen_organism <- gsub('_', ' ', chosen_organism)
  avail_data <- organismAttributes(organism = chosen_organism, topic = 'alphafold')
  
  avail_data <- avail_data %>%
    filter(name == 'alphafold' 
        #   & !str_detect(dataset, '_eg_gene')
           ) %>%
    distinct()
  
  return(avail_data)
}


format_dups_for_alphafold_db <- function(dups_anc) {
  
  #dups <- dups %>%
  #  mutate(func = 'NA') %>%
  #  select(-Orthogroup) %>%
  #  group_by(duplicate_pair_species) %>%
  #  group_split()
  
  all_copies <- dups_anc %>%
    select(Orthogroup, dup_1, dup_2, ancestral_copy, duplicate_pair_species, ancestral_species) %>%
    pivot_longer(cols = c('duplicate_pair_species', 'ancestral_species'), 
                 names_to = 'dup_or_anc', 
                 values_to = 'protein_file_name') %>%
    pivot_longer(cols = c('dup_1', 'dup_2', 'ancestral_copy'), 
                 names_to = 'copy', 
                 values_to = 'gene') %>%
    filter((dup_or_anc == 'duplicate_pair_species' & copy %in% c('dup_1', 'dup_2')) | 
             (dup_or_anc == 'ancestral_species' & copy == 'ancestral_copy'))
  
  return(all_copies)
}


get_best_pdb_for_gene <- function(avail_data, output_directory, gene) {
  
  for (data_row in 1:nrow(avail_data)) {
    # Retrieve mart and dataset
    mart <- avail_data$mart[data_row]
    dataset <- avail_data$dataset[data_row]
    
    # Fetch AlphaFold dataset
    alphafold_dataset_1 <- biomart(
      mart = mart, 
      dataset = dataset, 
      attributes = 'alphafold', 
      filters = 'ensembl_gene_id', 
      genes = gene
    ) %>%
      filter(str_detect(alphafold, 'AF'))
    
    # Initialize variables to track the best PDB file
    highest_plddt <- 0
    best_pdb_path <- NULL
    all_temp_files <- c()
    
    # Process each AlphaFold ID
    for (alphafold_id in alphafold_dataset_1$alphafold) {
      # Extract UniProt ID and construct PDB URL
      uniprot_id <- sub("AF-([A-Za-z0-9]+)-F[0-9]+", "\\1", alphafold_id)
      url <- paste0("https://alphafold.ebi.ac.uk/files/", alphafold_id, "-model_v3.pdb")
      
      # Define output file path
      output_file <- paste0(output_directory, "temp_", uniprot_id, ".pdb")
      all_temp_files <- c(all_temp_files, output_file)
      
      # Download the PDB file
      response <- GET(url, write_disk(output_file, overwrite = TRUE))
      
      if (status_code(response) == 200) {
        # Parse the PDB file
        pdb <- read.pdb(output_file)
        
        # Calculate the mean or max pLDDT score
        plddt_score <- mean(pdb$atom$b, na.rm = TRUE) # Use max(pdb$atom$b, na.rm = TRUE) for max
        
        # Check if this file has the highest pLDDT score
        if (plddt_score > highest_plddt) {
          highest_plddt <- plddt_score
          
          # Update the best PDB path
          best_pdb_path <- output_file
        } else {
          # Delete the less optimal PDB file
          file.remove(output_file)
        }
      } else {
        message("Failed to download: ", url)
      }
    }
    
    # Delete all temporary files except the best one
    for (file in all_temp_files) {
      if (file != best_pdb_path && file.exists(file)) {
        file.remove(file)
      }
    }
    
    # Copy the best PDB file to a final destination
    if (!is.null(best_pdb_path)) {
      final_output <- paste0(output_directory, gene, ".pdb")
      file.copy(best_pdb_path, final_output, overwrite = TRUE)
      message("Best PDB file saved as: ", final_output)
      
      # Optionally, remove the temporary best file
      file.remove(best_pdb_path)
    }
  }
}



get_plddt <- function(accession) {
  url <- paste0("https://alphafold.ebi.ac.uk/files/", accession, "-model_v3.pdb")
  response <- GET(url)
  
  if (status_code(response) == 200) {
    pdb <- read.pdb(response$url)
    plddt_score <- mean(pdb$atom$b, na.rm = TRUE) 
    
    return(plddt_score)
  }
}


main_alphafold <- function(dups_anc, file_organism_table) {
  output_directory <- './app/Results/AlphaFold/'
  
  ####
  dups_anc <- dups_anc %>%
    filter(Orthogroup == 'OG0002996' | Orthogroup == 'OG0003101' | Orthogroup == 'OG0002998')
  
  ####
  
  
  all_copies <- format_dups_for_alphafold_db(dups_anc)
  
  all_copies$gene <- sub("\\.[^.]*$", "", all_copies$gene)
  
  

  # iterate over all organisms with data 
  all_gene_alphafold_data <- data.frame()
  for (chosen_organism in file_organism_table$organism_scientific_name) {
    
    avail_data <- get_avail_data_for_organism(chosen_organism)
    if (nrow(avail_data) == 0) {return(paste0('No alphafold data is available for '), chosen_organism)}
    
    # get file name for the chosen organism
    chosen_protein_file_name <- file_organism_table %>%
      filter(organism_scientific_name == chosen_organism) %>%
      select(protein_file_name) %>% as.character()
    
    # get all genes for the organism (any duplicate copies and any ancestral copies)
    species_name <- gsub('.fasta', '', basename(chosen_protein_file_name))
    genes <- all_copies %>%
      filter(protein_file_name == species_name)
    
    
    mart <- avail_data$mart[1]
    dataset <- avail_data$dataset[1]
    
    
    gene_accessions <- biomart(mart = mart,
                               dataset = dataset, 
                               attributes = 'alphafold', 
                               filters = 'ensembl_gene_id', 
                               genes = genes$gene) %>%
      filter(str_detect(alphafold, 'AF'))
    
    
    
    gene_alphafold_data <- gene_accessions %>%
      rowwise() %>%
      mutate(plddt = get_plddt(alphafold)) %>%
      group_by(ensembl_gene_id) %>%
      filter(plddt == max(plddt)) %>%
      mutate(protein_file_name = species_name)
    
    all_gene_alphafold_data <- rbind(all_gene_alphafold_data, gene_alphafold_data)
  }
  
  final_output <- left_join(all_copies, all_gene_alphafold_data, by = c('gene' = 'ensembl_gene_id', 'protein_file_name'))
  
  
}


#pdb1 <- read.pdb(paste0(output_directory, dup_1, '.pdb'))
#pdb2 <- read.pdb(paste0(output_directory, dup_2, '.pdb'))




#pdb$atom      # atom-level information (coordinates, element types)
#pdb$xyz       # atomic coordinates
#pdb$seqres    # sequence of residues
#pdb$calpha    # alpha carbons







accession <- 'AF-Q5MK24-F1'
url <- paste0("https://alphafold.ebi.ac.uk/files/", accession, "-model_v3.pdb")
response1 <- GET(url)
dup1_pdb <- read.pdb(response1$url)

accession <- 'AF-Q5MK23-F1'
url <- paste0("https://alphafold.ebi.ac.uk/files/", accession, "-model_v3.pdb")
response2 <- GET(url)
dup2_pdb <- read.pdb(response2$url)

accession <- 'AF-Q8SYH1-F1'
url <- paste0("https://alphafold.ebi.ac.uk/files/", accession, "-model_v3.pdb")
responsea <- GET(url)
anc_pdb <- read.pdb(responsea$url)



pdbaln(files = list(response1$url, response2$url), 
       exefile = 'C:/Users/17735/Downloads/muscle-win64.v5.3.exe', 
       outfile = 'C:/Users/17735/Downloads/alignment.pir')

options(bio3d.muscle = "wsl muscle")



# does it make enough sense to use your own expression data on public database sequences?




