

source('C:/Users/17735/Downloads/DuplicA/app/Scripts/pop_functions.R') ######## CHANGE
library(igraph)


prot_files_path <- "C:/Users/17735/Downloads/AAAAA_Pop_DupFind_Example_Input/Prot_Fastas/"
input_options <- list(
  type = 'prot', min_score = 30, min_perc_identity = 90, # blat
  e_value = 0.001 # blast
  )
program = 'blastp'

min_percent_identity <- 90
min_score <- 100
min_bitscore <- 100
min_gn_length <- 30
min_match_length <- 30
min_match_length_percent <- 90

copy_number <- 3


# allow limiting of n_copies (add 1 to rm the gns with more if they have more) 
# allow parallel processing with n_threads
# to speed up blast 


build_command <- function(program, input_options) {
  # define program
  if (program %in% c('blastp', 'blastx', 'blastn')) {
    
    # build base command
    command_base <- paste0(here_linux, '/DuplicA/app/Dependencies/BLAST/', program) # no 'wsl' needed bc runs immediately with makedb command
    
    # define default options (NOTE: these are not the same as the default BLAST options that BLAST defines)
    default_options = list(e_value = 1)
    
    # change default options into input_options where provided
    options <- modifyList(default_options, input_options)
    
    # make second half of the blat command
    command_options <- paste0('-max_hsps 1 -evalue ', options$e_value, ' -outfmt "6 qseqid sseqid qlen slen length pident score bitscore"')
    
    if (options$type == 'nuc') {options$type <- 'nucl'}
  }
  
  if (program == 'blat') {
    
    # build base command
    command_base <- paste0('wsl ', here_linux, '/DuplicA/app/Dependencies/BLAT/blat')

    # define default options (NOTE: these are not the same as the default BLAT options that BLAT defines)
    if (input_options$type == 'prot'){default_options = list(allow_one_mismatch = 1, n_tile_matches = 1, min_score = 30, min_perc_identity = 25, out_format = 'blast8')}
    if (input_options$type == 'nuc'){default_options = list(allow_one_mismatch = 1, n_tile_matches = 1, min_score = 30, min_perc_identity = 90, out_format = 'blast8')
                                     input_options$type <- 'dna'}
    
    # change default options into input_options where provided
    options <- modifyList(default_options, input_options)

    # make second half of the blat command
    command_options <- paste0(' -t=', options$type, ' -q=', options$type) # add type (prot or dna)
    command_options <- paste0(command_options, ' -oneOff=1 -minMatch=1 -out=blast8 -minScore=', options$min_score, ' -minIdentity=', options$min_perc_identity)
  }
  
  
  
  return(list(command_base = command_base, command_options = command_options, type = options$type))
  
}


run_dup_finder <- function(command_base, prot_files_path, command_options, program, type) {
  for (indiv_file in list.files(prot_files_path, full.names = T)) {
    
    # windows to linux path
    indiv_file <- gsub(here, here_linux, indiv_file)
    
    # make output file path
    out_file <-  basename(paste0(file_path_sans_ext(indiv_file), '_', program, '_out.tsv'))
    output_file_path <- paste0(here_linux_temp, '/', program, '_output/', out_file)
    
    # formulate final command and run 
    if (program == 'blat') {
      command <- paste0(command_base, ' ', indiv_file, ' ', indiv_file, command_options, ' ', output_file_path)
    }
    
    if (program %in% c('blastp', 'blastx', 'blastn')) {
      
      # get blastdb name for individual (db will just be in the blastp_dbs/blastx_dbs/blastn_dbs folder)
      indiv_db_path <- paste0(here_linux_temp, '/', program, '_dbs/', basename(file_path_sans_ext(indiv_file)), '_db')
      
      # command to make blast db
      command_makeblastdb <- paste0('wsl ', here_linux, '/DuplicA/app/Dependencies/BLAST/makeblastdb -in ', indiv_file, ' -dbtype ', type, ' -out ', indiv_db_path)
      
      # command to run blast
      run_blast_command <- paste0(command_base, ' -query ', indiv_file, ' -db ', indiv_db_path, ' ', command_options, ' -out ', output_file_path)
      
      # command to remove blastdb for indicidual
      rm_db_command <- paste0('rm ', dirname(indiv_db_path), '/*')
      
      # combine all commands
      command <- paste(command_makeblastdb, run_blast_command, rm_db_command, sep = " && ")
    }
    
    # try to make it run faster: command <- "wsl /mnt/c/Users/17735/Downloads/DuplicA/app/Dependencies/BLAT/blat /mnt/c/Users/17735/Downloads/AAAAA_Pop_DupFind_Example_Input/Prot_Fastas/ZH26_annotations.fasta /mnt/c/Users/17735/Downloads/AAAAA_Pop_DupFind_Example_Input/Prot_Fastas/ZH26_annotations.fasta -t=prot -q=prot -oneOff=0 -repMatch=2 -minMatch=1 -out=psl -minScore=30 -minIdentity=90 /mnt/c/Users/17735/Downloads/DuplicA/app/Temp/blat_output/ZH26_annotations_blat_out.tsv"
    
    system(command)
    
  }
  
}



get_cnvs_from_blastp <- function(program) {
  
  # filter for length and score and bitscore and pident 
  
  all_dups <- data.frame()
  for (file in list.files(paste0(here_temp, '/', program, '_output'), full.names = T)) {
    
    # windows to linux path
    #file <- gsub(here, here_linux, file)
    
    
    
    
    if (program == 'blat') {
      
      # import blat output
      skip <- 0
      tmp <- readLines(x, 10)
      if( any(grepl("-{2,}", tmp)) ) {
        skip <- grep("-{2,}", tmp)
        for(i in skip:length(tmp)) {
          if( nchar(tmp[i]) == 0 ) {skip <- skip + 1} else break}
      }
      
      
      colnames <- c("match", "mis-match", "rep-match", "N's", "Q gapcount", "Q gapbases", "T gapcount", "T gapbases", "strand", "Q name", "Q size", "Q start", "Q end", "T name", "T size", "T start", "T end", "blockcount", "blockSizes", "qStarts", "tStarts", "score")
      
      

      
      
      
      ####
      
      
      hits
      
    }
    
    if (program %in% c('blastp', 'blastx', 'blastn')) {
      
      hits <- read.delim(file, header=FALSE)
      
      colnames(hits) <- c('qseqid', 'sseqid', 'qlen', 'slen', 'length', 'pident', 'score', 'bitscore')
      
      hits <- hits %>%
        filter(qseqid != sseqid,
               pident > min_percent_identity,
               score > min_score,
               bitscore > min_bitscore,
               qlen > min_gn_length,
               slen > min_gn_length,
               length > min_match_length,
               length > if_else(qlen >= slen, (min_match_length_percent/100) * qlen,
                                              (min_match_length_percent/100) * slen)) %>%
        # remove reciprocal same hits
        mutate(pair_ids = if_else(qseqid > sseqid, paste0(qseqid,sseqid), paste0(sseqid,qseqid))) %>%
        distinct(pair_ids, .keep_all = T) %>%
        select(qseqid, sseqid)
    }
    
    
    
    
    # find each network of connected duplicate families
    graph <- graph_from_data_frame(hits, directed = FALSE)
    components <- components(graph)
    dup_fams <- data.frame(gn = V(graph)$name, group_number = components$membership)
    
    # keep only the duplicate families with the desired number of copies and format the dups df 
    indiv_name <- gsub(paste0('_', program, '_out.tsv'), '', basename(file))
    indiv_dups <- dup_fams %>%
      group_by(group_number) %>%
      filter(n() == copy_number) %>%
      mutate(indiv = indiv_name) %>%
      select(indiv, gn, group_number)
  
    
    all_dups <- rbind(all_dups, indiv_dups)  
  }
  
  
  all_dups
  return(cnvs_path)
}


main_pop_dup_finder <- function(program, input_options) {
  replace_dirs <- F ############## remove 
  temp_dir_list <- c('/blat_output', '/blastp_output', '/blastx_output', '/blastn_output',
                                     '/blastp_dbs', '/blastx_dbs', '/blastn_dbs')
  make_temp_dirs(replace_dirs, temp_dir_list)
  
  command <- build_command(program, input_options)
  
  run_dup_finder(command$command_base, prot_files_path, command$command_options, program, command$type)
  
  cnvs_path <- get_cnvs_from_blastp()
  
  
  # make sure resulting cnvs would be able to be read by get_pairs()
  trash <- get_pairs(cnvs_path)
  
}







