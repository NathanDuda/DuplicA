


get_pairs <- function(cnvs_path) {
  
  cnvs <- read.csv(cnvs_path, sep="")
  
  colnames(cnvs) <- c('individual', 'gene', 'group')
  
  pairs <- cnvs %>%
    group_by(individual, group) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 2)
  # underscores in individual name and group name must be changed 
  
  return(pairs)
}


make_temp_dirs <- function(replace_dirs, temp_dir_list) {
  
  create_or_replace_dir <- function(dir_path) {
    if (replace_dirs == T) {system(paste0('wsl ', 'rm -rf ', dir_path), intern = TRUE)}
    system(paste0('wsl ', 'mkdir -p ', dir_path), intern = TRUE)
  }
  
  # create each directory
  for (temp_dir in temp_dir_list) {
    create_or_replace_dir(paste0(here_linux_temp, temp_dir))
  }
}



combine_raw_fastas <- function(file_path, type) {
  
  file_extension <- file_ext(file_path)
  dir_path <- dirname(file_path)
  temp_output_file_path <- paste0(dir_path, '/temp_', type, '_fasta.fa')
  output_file_path <- paste0(dir_path, '/combined_', type, '_fasta.fa')
  
  
  fasta_files <- list.files(paste0(dir_path, "*.", file_extension), full.names = TRUE)
  
  
  # windows to linux conversion
  dir_path <- gsub('C:', '/mnt/c', dir_path)
  output_file_path <- gsub('C:', '/mnt/c', output_file_path)
  
  system(paste0("
  wsl for file in ", dir_path, "/*.", file_extension, "; do
    individual_name=$(basename \"$file\" | sed 's/\\..*//') # Extract the individual name by removing the file extension
    individual_name=$(echo \"${individual_name}\" | sed 's/-/_/g') # Replace '-' with '_'
    individual_name=$(echo \"${individual_name}\" | sed 's/_/./g') # Replace '_' with '.'
    
    # Only prepend the individual's name if it is not already in the header
    awk -v name=\"${individual_name}\" '/^>/ {if ($0 !~ name) print \">\"name\"_\"substr($0, 2); else print $0; next} {print}' \"$file\" > \"$file.temp\"
    
    mv \"$file.temp\" \"$file\" # Replace the original file with the modified one
  done
  
  cat ", dir_path, "/*.", file_extension, " > ", output_file_path))
  
  # Replace illegal characters ('-' with '_') that were in the gene naem in the concatenated file
  #sed 's/-/_/g' #, temp_output_file_path, " > ", output_file_path, 
  
  
  # Remove the temporary file
  #rm , temp_output_file_path, 
  #))
  
  
  # get individual name into each gene name 
  ##individual_name <- sub(paste0("\\.", file_extension, "$"), "", basename(file_path)) # get file name (individual name) without extension
  ##individual_name <- gsub("-", "_", individual_name) # replace any - with _
  ##individual_name <- gsub("_", ".", individual_name) # replace any _ with .
  # unit test: gene names cannot have '>' in their names 
  ##system(paste0("wsl sed -i 's/>/>", individual_name, "_/g' ", dir_path, '/', basename(file_path))) # add individual name to the start of each gene name in the fasta files >gn_name to >indiv_name_gn_name
  
  
  
  
  
  ##system(paste0('wsl cat ', dir_path, '/*.', file_extension, " > ", temp_output_file_path)) # concatenate all fasta files 
  
  ##system(paste0('wsl sed \'s/-/_/g\' ', temp_output_file_path, ' > ', output_file_path)) # replace '-' with '_' (illegal gene name character)
  ##system(paste0('wsl rm ', temp_output_file_path)) 
  
  return(output_file_path)
}


translate_nucs_to_prots <- function(nuc_file_path) {
  nuc_seqs <- readDNAStringSet(nuc_file_path, format = "fasta")
  prot_seqs <- Biostrings::translate(nuc_seqs)
  prot_file_path <- paste0(dirname(nuc_file_path), '/combined_prot_fasta.fa')
  
  writeXStringSet(prot_seqs, prot_file_path, format = "fasta")
  
  return(prot_file_path)
}


