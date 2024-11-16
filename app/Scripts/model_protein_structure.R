



# protein structure


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
  alphafold_dataset_1 <- biomart(dataset = 'dmelanogaster_eg_gene', attributes = 'alphafold', filters = 'ensembl_gene_id', genes = dup_1)
  alphafold_dataset_2 <- biomart(dataset = 'dmelanogaster_eg_gene', attributes = 'alphafold', filters = 'ensembl_gene_id', genes = dup_2)
  
  
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


library(r3dmol)
visualize_pdb <- function(pdb_file) {
  
  r3dmol() %>%
    m_add_model(data = pdb_file, format = "pdb") %>%
    m_set_style(style = m_style_cartoon(color = "spectrum")) %>%
    m_zoom_to() %>%
    m_spin(axis = "y", speed = 0.5)  
  
  
}


library(httr)
library(bio3d)
main_alphafold <- function(dups) {
  output_directory <- './app/Results/AlphaFold/'
  
  dups <- format_dups(dups)
  
  for (row in 1:nrow(dups)) {
  
    pair <- dups[row,]
    
    dup_1 <- pair$fbgn_1[row]
    dup_2 <- pair$fbgn_2[row]
    anc <- pair$anc[row]
    func <- pair$func[row]
    
    get_alphafold_for_pair(dup_1, dup_2, func, output_directory, output_file, output_file2)
    
    
    
    pdb1 <- read.pdb(output_file)
    pdb2 <- read.pdb(output_file2)
    
    visualize_pdb(pdb1)
    
  
  }

}





pdb$atom      # atom-level information (coordinates, element types)
pdb$xyz       # atomic coordinates
pdb$seqres    # sequence of residues
pdb$calpha    # alpha carbons


# FBgn0010434 FBgn0264975

















organismBM(organism = 'Drosophila melanogaster')
getDatasets(mart = 'metazoa_mart')

organismAttributes(organism = 'Drosophila melanogaster',topic = 'alphafold')





