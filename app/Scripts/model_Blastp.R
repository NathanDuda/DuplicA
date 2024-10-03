

source('C:/Users/17735/Downloads/DuplicA/app/Scripts/pop_functions.R') ######## CHANGE



prot_files_path <- "C:/Users/17735/Downloads/AAAAA_Pop_DupFind_Example_Input/Prot_Fastas/"
input_options <- list(type = 'prot', min_score = 30, min_perc_identity = 90)

build_command <- function(program, input_options) {
  
  # define program
  if (program %in% c('blastp', 'blastx', 'blastn')) {
    
    command_base <- paste0('wsl ', here_linux, '/DuplicA/app/Dependencies/BLAST/', program)
    
    
    command_options <- paste0('-outfmt 6 qseqid sseqid qlen slen length pident')
    
    
    make_db_command <- paste0('wsl ', here_linux, '/DuplicA/app/Dependencies/BLAST/makeblastdb/', inputfile)
    
  }
  
  if (program == 'blat') {
    
    # build base command
    command_base <- paste0('wsl ', here_linux, '/DuplicA/app/Dependencies/BLAT/blat')

    # define default options (NOTE: these are not the same as the default BLAT options that BLAT defines)
    if (input_options$type == 'prot'){default_options = list(allow_one_mismatch = 1, n_tile_matches = 1, min_score = 30, min_perc_identity = 25, out_format = 'blast8')}
    if (input_options$type == 'nuc'){default_options = list(allow_one_mismatch = 1, n_tile_matches = 1, min_score = 30, min_perc_identity = 90, out_format = 'blast8')}
    
    # change default options into input_options where provided
    options <- modifyList(default_options, input_options)

    # make final blat command
    command_options <- paste0(' -t=', options$type, ' -q=', options$type) # add type (prot or dna)
    command_options <- paste0(command_options, ' -oneOff=1 -minMatch=1 -out=blast8 -minScore=', options$min_score, ' -minIdentity=', options$min_perc_identity)
  }
  
  
  
  return(list(command_base = command_base, command_options = command_options))
  
}


run_dup_finder <- function(command_base, prot_files_path, command_options) {
  for (indiv_prot_file in list.files(prot_files_path, full.names = T)) {
    
    # windows to linux path
    indiv_prot_file <- gsub(here, here_linux, indiv_prot_file)
    
    # make output file path
    out_file <-  basename(paste0(file_path_sans_ext(indiv_prot_file), '_', program, '_out.tsv'))
    output_file_path <- paste0(here_linux_temp, '/', program, '_output/', out_file)
    
    # formulate final command and run 
    command <- paste0(command_base, ' ', indiv_prot_file, ' ', indiv_prot_file, command_options, ' ', output_file_path)
    
    command <- "wsl /mnt/c/Users/17735/Downloads/DuplicA/app/Dependencies/BLAT/blat /mnt/c/Users/17735/Downloads/AAAAA_Pop_DupFind_Example_Input/Prot_Fastas/ZH26_annotations.fasta /mnt/c/Users/17735/Downloads/AAAAA_Pop_DupFind_Example_Input/Prot_Fastas/ZH26_annotations.fasta -t=prot -q=prot -oneOff=0 -repMatch=2 -minMatch=1 -out=psl -minScore=30 -minIdentity=90 /mnt/c/Users/17735/Downloads/DuplicA/app/Temp/blat_output/ZH26_annotations_blat_out.tsv"
    system(command)
    
  }
  
}



get_cnvs_from_blastp <- function() {
  
  return(cnvs_path)
}


main_pop_dup_finder <- function(program, input_options) {
  replace_dirs <- F ############## remove 
  temp_dir_list <- c('/blat_output', '/BLAST')
  make_temp_dirs(replace_dirs, temp_dir_list)
  
  command <- build_command(program, input_options)
  
  run_dup_finder(command$command_base, prot_files_path, command$command_options)
  
  cnvs_path <- get_cnvs_from_blastp()
  
  
  # make sure resulting cnvs would be able to be read by get_pairs()
  trash <- get_pairs(cnvs_path)
  
}







