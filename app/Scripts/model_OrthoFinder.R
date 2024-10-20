



main_OrthoFinder <- function(protein_folder, is_dna = FALSE, method = "dendroblast", sequence_search = "diamond", 
                                             msa_program = "mafft", tree_method = "fasttree", species_tree = NULL, 
                                             mcl_inflation = 1.5, orthoxml = NULL, split_hogs = FALSE, 
                                             no_msa_trim = FALSE, result_name = NULL, result_dir = NULL, stop_stage = NULL) {
  
  # Helper function to convert Windows path to WSL path
  #convert_to_wsl_path <- function(path) {
   # return(gsub("^([A-Za-z]):", "/mnt/\\L\\1", gsub("\\\\", "/", path)))
  #}
  
  # Convert directory paths to WSL format
  #dir <- convert_to_wsl_path(dir)
  #if (!is.null(dir2)) dir2 <- convert_to_wsl_path(dir2)
  #if (!is.null(species_tree)) species_tree <- convert_to_wsl_path(species_tree)
  #if (!is.null(orthoxml)) orthoxml <- convert_to_wsl_path(orthoxml)
  #if (!is.null(pickle_dir)) pickle_dir <- convert_to_wsl_path(pickle_dir)
  #if (!is.null(result_dir)) result_dir <- convert_to_wsl_path(result_dir)
  
  #protein_folder <- gsub('C:/Users/17735/Downloads', '/mnt/c/Users/17735/Downloads', protein_folder)
  protein_folder <- gsub(here, here_linux, protein_folder)

  path_to_orthofinder <- paste0(here_linux, '/DuplicA/app/Dependencies/OrthoFinder/orthofinder')
  results_dir <- paste0(here_linux, '/DuplicA/app/Results')
  
  # base command
  command <- paste(path_to_orthofinder, "-f", shQuote(protein_folder))
  
  if (is_dna) {command <- paste(command, "-d")}
  
  command <- paste(command, "-M", method)
  command <- paste(command, "-S", sequence_search)
  
  if (method == "msa") {
    command <- paste(command, "-A", msa_program)
    command <- paste(command, "-T", tree_method)
  }
  
  if (!is.null(species_tree)) {command <- paste(command, "-s", shQuote(species_tree))}
  
  command <- paste(command, "-I", mcl_inflation)

  if (split_hogs) {command <- paste(command, "-y")}
  if (no_msa_trim) {command <- paste(command, "-z")}
  if (!is.null(result_name)) {command <- paste(command, "-n", shQuote(result_name))}
  if (!is.null(result_dir)) {command <- paste(command, "-o", shQuote(result_dir))}
  if (!is.null(stop_stage)) {command <- paste(command, "-", stop_stage)}
  
  # Run the command in WSL
  wsl_command <- paste("wsl", command)

  
  system(wsl_command)
  
}



#command <- generate_orthofinder_command_wsl(protein_folder = "/mnt/c/Users/17735/Downloads/Eight_Species/Protein_Fastas", is_dna = F)





