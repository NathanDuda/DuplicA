
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


get_plddt <- function(accession) {
  url <- paste0("https://alphafold.ebi.ac.uk/files/", accession, "-model_v3.pdb")
  response <- GET(url)
  
  if (status_code(response) == 200) {
    pdb <- read.pdb(response$url)
    plddt_score <- mean(pdb$atom$b, na.rm = TRUE) 
    
    return(plddt_score)
  }
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


main_alphafold <- function(all_copies, file_organism_table) {
  colnames(file_organism_table)[2] <- 'organism_scientific_name'
  file_organism_table$organism_scientific_name <- gsub('_', ' ', file_organism_table$organism_scientific_name)
  
  
  output_directory <- './app/Results/AlphaFold/'
  

  
  

  # iterate over all organisms with data 
  all_gene_alphafold_data <- data.frame()
  for (chosen_organism in file_organism_table$organism_scientific_name) {
    
    avail_data <- get_avail_data_for_organism(chosen_organism, topic = 'alphafold')
    if (nrow(avail_data) == 0) {return(paste0('No alphafold data is available for '), chosen_organism)}
    
    # get file name for the chosen organism
    chosen_protein_file_name <- get_protein_file_name(chosen_organism, file_organism_table)
    
    # get all genes for the organism (any duplicate copies and any ancestral copies)
    genes <- get_genes_for_organism(chosen_protein_file_name)
    
    # get the mart and dataset for the organism
    mart <- avail_data$mart[1] # CHANGEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEee
    dataset <- avail_data$dataset[1]
    
    # get the alphafold accessions for each gene
    gene_accessions <- biomart(mart = mart, dataset = dataset, attributes = 'alphafold', 
                               filters = 'ensembl_gene_id', genes = genes$gene) %>%
      filter(str_detect(alphafold, 'AF'))
    
    
    # get the best plddt score for each gene
    gene_alphafold_data <- gene_accessions %>%
      rowwise() %>%
      mutate(plddt = get_plddt(alphafold)) %>%
      group_by(ensembl_gene_id) %>%
      filter(plddt == max(plddt)) %>%
      mutate(protein_file_name = species_name)
    
    all_gene_alphafold_data <- rbind(all_gene_alphafold_data, gene_alphafold_data)
  }
  
  final_output <- left_join(all_copies, all_gene_alphafold_data, by = c('gene' = 'ensembl_gene_id', 'protein_file_name'))
  
  write.table(final_output, file = paste0(here_results, '/main_AlphaFold_output.tsv'))
  #return(final_output)
  
}

