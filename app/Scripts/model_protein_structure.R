library(tidyverse)
library(biomartr)
library(r3dmol)
library(httr)
library(bio3d)

chosen_organism <- 'Drosophila melanogaster'



# get duplicates
dups <- read.csv("C:/Users/17735/Downloads/Eight_Species/CDROM_CLOUD_pseudo_funcs.tsv", sep="")

dups <- dups %>%
  filter(str_detect(dup_1, 'ME')) %>%
  select(dup_1, dup_2, anc, func = CDROM_pseudo)

# get fbgn ids
fbgns <- read.delim("C:/Users/17735/Downloads/Eight_Species/Raw_Data/YO_Expression/GSE99574_HiSAT2_dmel.nrc.YO.txt")
d1 <- fbgns %>% select(dup_1 = YOgnID, fbgn_1 = FBgnID) %>% left_join(., dups, by = 'dup_1')
d2 <- fbgns %>% select(dup_2 = YOgnID, fbgn_2 = FBgnID) %>% left_join(., dups, by = 'dup_2')
#anc <- fbgns %>% select(anc = YOgnID, fbgn_anc = FBgnID) %>% left_join(., dups, by = 'anc') %>% select(anc, fbgn_anc)

dups <- merge(d1, d2, by = c('dup_1', 'dup_2', 'anc', 'func'))

dups <- dups %>%
  filter(!str_detect('-', fbgn_1) & !str_detect('-', fbgn_2)) %>%
  select(fbgn_1, fbgn_2, anc, func)



format_dups <- function(dups) {
  
  return(dups)
}


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
  avail_data <- organismAttributes(organism = chosen_organism, topic = 'alphafold')
  
  avail_data <- avail_data %>%
    filter(name == 'alphafold' &
             !str_detect(dataset, '_eg_gene')) %>%
    distinct()
  
  return(avail_data)
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


main_alphafold <- function(dups, chosen_organism) {
  output_directory <- './app/Results/AlphaFold/'
  
  dups <- format_dups(dups)
  
  avail_data <- get_avail_data_for_organism(chosen_organism)
  
  if (nrow(avail_data) == 0) {return('no data available for chosen organism')}
  
  for (row in 1:nrow(dups)) {
  
    pair <- dups[row,]
    
    dup_1 <- pair$fbgn_1
    dup_2 <- pair$fbgn_2
    #anc <- pair$anc
    func <- pair$func
    
    #get_alphafold_for_pair(dup_1, dup_2, func, output_directory, output_file, output_file2)
    
    get_best_pdb_for_gene(avail_data, './app/Results/AlphaFold/', dup_1)
    get_best_pdb_for_gene(avail_data, './app/Results/AlphaFold/', dup_2)
    
    if(!file.exists(paste0(output_directory, dup_1, '.pdb')) |
       !file.exists(paste0(output_directory, dup_2, '.pdb'))){break}

    pdb1 <- read.pdb(paste0(output_directory, dup_1, '.pdb'))
    pdb2 <- read.pdb(paste0(output_directory, dup_2, '.pdb'))
    
    visualize_pdb(pdb1)
    
  
  }

}


pdb1 <- read.pdb(paste0(output_directory, dup_1, '.pdb'))
pdb2 <- read.pdb(paste0(output_directory, dup_2, '.pdb'))




pdb$atom      # atom-level information (coordinates, element types)
pdb$xyz       # atomic coordinates
pdb$seqres    # sequence of residues
pdb$calpha    # alpha carbons


# FBgn0010434 FBgn0264975

