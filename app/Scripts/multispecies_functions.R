
get_dups_from_OF <- function(OF_dir_path) {
  
  ################################
  # unit tests
  ## OrthoFinder files exist 
  test_that('ensure OrthoFinder files exist', {
    expect_true(file.exists(paste0(OF_dir_path, "Orthogroups/Orthogroups.GeneCount.tsv")), 
                label = paste0('The file "', OF_dir_path, 'Orthogroups/Orthogroups.GeneCount.tsv" does not exist. Make sure all files are kept in the OrthoFinder output folder. File exists'))
    expect_true(file.exists(paste0(OF_dir_path, "Orthogroups/Orthogroups.tsv")),
                label = paste0('The file "', OF_dir_path, 'Orthogroups/Orthogroups.tsv" does not exist. Make sure all files are kept in the OrthoFinder output folder. File exists'))
    expect_true(file.exists(paste0(OF_dir_path, "Orthogroups/Orthogroups_SingleCopyOrthologues.txt")),
                label = paste0('The file "', OF_dir_path, 'Orthogroups/Orthogroups_SingleCopyOrthologues.txt" does not exist. Make sure all files are kept in the OrthoFinder output folder. File exists'))
    expect_true(file.exists(paste0(OF_dir_path, "Species_Tree/SpeciesTree_rooted.txt")),
                label = paste0('The file "', OF_dir_path, 'Species_Tree/SpeciesTree_rooted.txt" does not exist. Make sure all files are kept in the OrthoFinder output folder. File exists'))
  })
  ################################
  
  
  orthogroup_gene_count <- read.delim(paste0(OF_dir_path, "Orthogroups/Orthogroups.GeneCount.tsv"))
  n_species <- ncol(orthogroup_gene_count) - 2 
  
  # get the two to one to ones to zeros 
  two_to_ones <- orthogroup_gene_count %>%
    filter(if_all(2:(n_species + 1), ~. <= 2)) %>%          # keep only pairs 
    filter(rowSums(dplyr::select(., 2:(n_species + 1)) == 2) == 1) # make sure there is only one species with 2 copies 
  
  ################################
  # unit tests
  test_that('ensure there is at least one two to one found', 
            expect_true(nrow(two_to_ones) > 0, 
                        label = paste0('No duplicate gene pairs were found in any species. More than 0 duplicate pair orthogroups found')))
  ## orthogroups dont repeat 
  test_that('ensure orthogroups do not repeat',
            expect_false(any(duplicated(two_to_ones$Orthogroup)), 
                         label = paste0('There are repeating Orthogroups in "', OF_dir_path, 'Orthogroups/Orthogroups.GeneCount.tsv". Orthogroups repeat')))
  ################################
  
  # add a column with the name of the species with the duplication
  two_to_ones$duplicate_pair_species <- 
    apply(two_to_ones[, 2:(n_species + 1)], 1, function(tto) {
      col_index <- which(tto == 2)
      return(colnames(two_to_ones[, 2:(n_species + 1)])[col_index])})
  
  two_to_ones <- two_to_ones %>%
    dplyr::select(Orthogroup, duplicate_pair_species)
  
  # merge back with the gene names 
  orthogroups <- read.delim(paste0(OF_dir_path, "/Orthogroups/Orthogroups.tsv"))
  two_to_ones <- merge(orthogroups, two_to_ones, by = 'Orthogroup')
  
  
  # extract the duplicate pair genes
  dups <- two_to_ones %>%
    rowwise() %>%
    mutate(duplicate_pair = toString(c_across(2:(n_species + 1))[grep(",", c_across(2:(n_species + 1)))])) %>%
    dplyr::select(Orthogroup, duplicate_pair, duplicate_pair_species) %>%
    separate(duplicate_pair, into = c("dup_1", "dup_2"), sep = ", ")
  
  ################################
  # unit tests
  test_that('ensure genes are not part of more than one duplicate pair',
            expect_false(any(duplicated(c(dups$dup_1, dups$dup_2))), 
                         label = paste0('There are gene ids that participate in multiple duplicate pairs. Duplicate gene ids repeat'))
  )
  ## duplicate pairs do not repeat 
  test_that('ensure duplicate pairs do not repeat',
            expect_false(any(duplicated(dups %>% dplyr::select(dup_1, dup_2))), 
                         label = paste0('There are repeating duplicate gene ids. Duplicate gene pairs repeat'))
  )
  ################################
  
  return(list(dups = dups, dup_pair_orthologs = two_to_ones))
}


clean_exp_and_pseudo <- function(exp_path, dups, normalization_type, add_pseudofunc, missing_expr_is_zero, exp_cutoff, PC) {
  
  ################################
  # unit tests
  test_that('expression file exists',
            expect_true(file.exists(paste0(exp_path)),
                        label = paste0('The file "', exp_path, '" does not exist. Expression file exists')))
  ################################
  
  # read in expression file depending on extension
  file_ext <- file_ext(exp_path)
  
  if (file_ext == "csv") {all_expression <- read.csv(exp_path)}
  if (file_ext == "xlsx") {all_expression <- read_excel(exp_path)}  # reads the first sheet by default
  if (file_ext == "tsv" | 
      file_ext == 'txt') {all_expression <- read.delim(exp_path, sep = ' ')}
  
  ################################
  # unit tests
  test_that('expression file is in accepted format (csv, tsv, txt, or xlsx)',
            expect_true(file_ext %in% c('csv', 'tsv', 'txt', 'xlsx'),
                        label = paste0('The file "', exp_path, '" is in a non-supported format. Expression file type is csv, tsv, txt, or xlsx')))
  test_that('expression file has proper table format',
            expect_true(ncol(all_expression) > 1,
                        label = paste0('The file "', exp_path, '" seems to have only one column when imported. Expression file has more than 1 column')))
  test_that('expression file has gene IDs in first column',
            expect_false(any(is.numeric(all_expression$YOgnID)),
                         label = paste0('The file "', exp_path, '" has numbers in the first column. The first column should only contain gene IDs. There are numeric values in the first column')))
  test_that('expression file has numeric values in the second column',
            expect_true(is.numeric(all_expression[,2]),
                        label = paste0('The file "', exp_path, '" does not have numbers in the second column. The second column should contain numeric gene expression values. There are numeric values in the second column')))
  test_that('expression file has no repeating gene IDs',
            expect_false(any(duplicated(all_expression$YOgnID)),
                         label = paste0('The file "', exp_path, '" has repeating genes. There are multiple rows with the same gene ID in the first column. There are no repeating gene IDs')))
  test_that('expression file gene IDs match the gene IDs in OrthoFinder results',
            expect_true(any(dups$dup_1 %in% all_expression[,1]),
                        label = 'No genes given to OrthoFinder were found in the expression data file. Any duplicate genes in expression file'))
  ################################
  
  
  # format expression file 
  all_expression <- all_expression %>%
    mutate(across(-1, ~ as.numeric(.))) %>%
    mutate(across(-1, ~ ifelse(. < as.numeric(exp_cutoff), 0, .))) %>%
    rename(id = 1)
  
  clean_expression <- all_expression
  pseudo <- NA
  
  if (add_pseudofunc == TRUE) {
    
    if (missing_expr_is_zero == TRUE) {
      colnames(all_expression)[1] <- 'dup_1'
      dups <- left_join(dups, all_expression, by = 'dup_1')
      
      colnames(all_expression)[1] <- 'dup'
      colnames(all_expression) <- paste0(colnames(all_expression), "_2")
      dups <- left_join(dups, all_expression, by = 'dup_2')
    }
    if (missing_expr_is_zero == FALSE) { # duplicates with missing expression are removed 
      colnames(all_expression)[1] <- 'dup_1'
      dups <- merge(dups, all_expression, by = 'dup_1')
      
      colnames(all_expression)[1] <- 'dup'
      colnames(all_expression) <- paste0(colnames(all_expression), "_2")
      dups <- merge(dups, all_expression, by = 'dup_2')
      
      all_expression[is.na(all_expression)] <- 0 
    }
    
    n_tissues <- (ncol(dups) - 4) / 2
    
    pseudo <- dups %>%
      rowwise() %>%
      mutate(pseudo = case_when(sum(across(5:(n_tissues + 4))) == 0 & sum(across((n_tissues + 5):((n_tissues*2) + 4))) == 0 ~ 'pseudo_both',
                                sum(across(5:(n_tissues + 4))) == 0 ~ 'pseudo_dup_1',
                                sum(across((n_tissues + 5):((n_tissues*2) + 4))) == 0 ~ 'pseudo_dup_2')) %>%
      dplyr::select(dup_1, dup_2, pseudo) # add Orthogroup column if genes not unique
    
    if (PC == T) {colnames(pseudo) <- c('parent', 'child', 'pseudo')}
    
    return(list(pseudo = pseudo, clean_expression = clean_expression))
  }
  
  if (!is.na(normalization_type)) {
    
    counts <- clean_expression
    
    
    norm_counts <- counts 
    if (normalization_type == 'RPKM') {
      total_reads <- sum(counts)
      total_reads <- total_reads / 1e6 
      
      norm_counts <- norm_counts %>%
        mutate(length_kb = Length / 1000,
               !!paste0('norm_', colnames(.)) := .data / (length_kb * total_reads))
    }
    if (normalization_type == 'TPM') {
      norm_counts <- norm_counts %>% mutate(n_over_l = .data[[group]] / Length)
      denominator <- sum(norm_counts$n_over_l) / 1e6
      
      norm_counts <- norm_counts %>%
        mutate(!!paste0('norm_', group) := n_over_l / denominator)
    }
    
    
    # keep only normalized columns
    norm_counts <- norm_counts %>%
      dplyr::select(ID, starts_with(paste('norm_')))
    
    
    
    
    
    
  }
  
  return(clean_expression)
}


get_anc_copy <- function(OF_dir_path, dups, dup_pair_orthologs, clean_expression){
  
  expression <- clean_expression
  orthologs <- dup_pair_orthologs
  
  # create function to find the closest expressed ortholog to each duplicate pair
  newick_tree <- ape::read.tree(paste0(OF_dir_path, 'Species_Tree/SpeciesTree_rooted.txt'))
  
  ################################
  # unit tests
  test_that('species in species tree from OrthoFinder contains same species as duplicate genes file',
            expect_true(all(orthologs$duplicate_pair_species %in% newick_tree[["tip.label"]]),
                        label = 'The species in the newick tree do not match the duplicate gene species. Ensure there are no unaccepted special characters in the file names input into OrthoFinder. Species match'))
  ################################
  
  find_closest_ortholog <- function(row, species, newick_tree) {
    
    # calculate phylogenetic distances between the given species and each tip  
    species_node <- which(newick_tree$tip.label == species)
    distances <- cophenetic(newick_tree)[species_node, ]
    distances <- distances[distances > 0] # remove itself from distance calculation 
    
    # set non-expressed genes to NA 
    if(all(!is.na(expression))) {row[!row %in% expression$id] <- NA}
    
    # remove the duplicate pair species and missing species from the possible top choices 
    exclude_species <- colnames(row)[apply(row, 2, function(x) all(is.na(x)))]
    exclude_species <- c(exclude_species, species)
    distances <- distances[setdiff(names(distances), exclude_species)]
    
    # pick the closest available tip to the species
    closest_species <- names(which.min(distances)) # find the closest species by minimum distance
    
    
    if (is.null(closest_species)) {return(list(closest_gene = NA, closest_species = NA))}
    
    # get the ortholog from that species 
    closest_gene <- row[[closest_species]]
    
    if (exists("closest_gene")) {return(list(closest_gene = closest_gene, 
                                             closest_species = as.character(closest_species)))}
    return(list(closest_gene = NA, closest_species = NA))
    
  }
  
  # apply the function to each row of the ortholog table 
  orthologs$ancestral_copy <- NA
  orthologs$ancestral_species <- NA
  for (row_num in 1:nrow(orthologs)) {
    row <- orthologs[row_num,] 
    out <- find_closest_ortholog(row, species = row$duplicate_pair_species, newick_tree)
    orthologs[row_num, 'ancestral_copy'] <- out$closest_gene
    orthologs[row_num, 'ancestral_species'] <- out$closest_species
    
    #if (exists("closest_gene")) {rm(closest_gene)}
  }
  
  dups <- orthologs %>%
    dplyr::select(Orthogroup, ancestral_copy, ancestral_species) %>%
    merge(., dups, by = 'Orthogroup') %>%
    filter(!is.na(ancestral_copy) & !ancestral_copy == "") # remove duplicates without ancestral copies 
  
  ################################
  # unit tests
  test_that('ancestral copy exists for at least one duplicate gene pair',
            expect_true(nrow(dups) > 0,
                        label = 'None of the duplicate genes have any single-copy orthologs. At least one duplicate pair has single-copy ortholog'))
  test_that('duplicate pair species and ortholog species are not the same',
            expect_false(any(dups$ancestral_species == dups$duplicate_pair_species),
                         label = 'The duplicate pair species and ancestral species (single-copy ortholog) are the same. The species of the duplicate gene and the species of the ancestral gene are the same'))
  test_that('different duplicate pairs do not have the same ancestral copy',
            expect_false(any(duplicated(dups$ancestral_copy)),
                         label = 'There are multiple duplicate pairs that have the same ancestral copy ortholog. Ancestral copy gene is unique per duplicate gene pair'))
  ################################
  
  return(dups)
  
}


get_exp_df_for_copy <- function(copy, dups_anc, clean_expression) {
  
  dups <- dups_anc %>% dplyr::select(Orthogroup, all_of(copy))
  
  colnames(clean_expression)[1] <- copy
  exp_df <- merge(dups, clean_expression, by = copy)
  
  return(exp_df)
}


main_get_dups_anc_exp_from_OF <- function(OF_dir_path, exp_path = NA, normalization_type = NA, add_pseudofunc = NA, missing_expr_is_zero = NA, rm_exp_lower_than = NA) {
  
  OF_dir_path <- paste0(OF_dir_path, '/')
  
  out1 <- get_dups_from_OF(OF_dir_path)
  dups <- out1$dups
  dup_pair_orthologs <- out1$dup_pair_orthologs
  
  
  if (!is.na(exp_path)) {
    clean_expression <- clean_exp_and_pseudo(exp_path = exp_path, dups = dups, normalization_type = normalization_type, add_pseudofunc = add_pseudofunc, missing_expr_is_zero = missing_expr_is_zero, exp_cutoff = rm_exp_lower_than, PC = F)
    
    }
  if (is.na(exp_path)) {
    clean_expression <- NA
  }
  
  pseudo = NA
  if(add_pseudofunc == T) {
    pseudo <- clean_expression$pseudo
    clean_expression <- clean_expression$clean_expression}
  
  dups_anc <- get_anc_copy(OF_dir_path, dups, dup_pair_orthologs, clean_expression)
  
  if(length(clean_expression) > 1) {write.table(clean_expression, paste0(here_results, '/Expression.tsv'))}
  
  return(list(dups = dups, dups_anc = dups_anc, clean_expression = clean_expression, pseudo = pseudo))
  
}


format_dups_for_db_search <- function(dups_anc) {
  
  #dups <- dups %>%
  #  mutate(func = 'NA') %>%
  #  dplyr::select(-Orthogroup) %>%
  #  group_by(duplicate_pair_species) %>%
  #  group_split()
  
  all_copies <- dups_anc %>%
    dplyr::select(Orthogroup, dup_1, dup_2, ancestral_copy, duplicate_pair_species, ancestral_species) %>%
    pivot_longer(cols = c('duplicate_pair_species', 'ancestral_species'), 
                 names_to = 'dup_or_anc', 
                 values_to = 'protein_file_name') %>%
    pivot_longer(cols = c('dup_1', 'dup_2', 'ancestral_copy'), 
                 names_to = 'copy', 
                 values_to = 'gene') %>%
    filter((dup_or_anc == 'duplicate_pair_species' & copy %in% c('dup_1', 'dup_2')) | 
             (dup_or_anc == 'ancestral_species' & copy == 'ancestral_copy'))
  
  # remove the characters after the first period in the gene name (causes issues with human genes)
  all_copies$gene <- sub("\\.[^.]*$", "", all_copies$gene) # remove the version indicator in human gene ids 
  
  return(all_copies)
}


get_protein_file_name <- function(chosen_organism, file_organism_table) {
  
  chosen_protein_file_name <- file_organism_table %>%
    filter(organism_scientific_name == chosen_organism) %>%
    dplyr::select(protein_file_name) %>% as.character()
  
  return(chosen_protein_file_name)
}


get_genes_for_organism <- function(chosen_protein_file_name) {
  species_name <- gsub('.fasta', '', basename(chosen_protein_file_name))
  genes <- all_copies %>% filter(protein_file_name == species_name)
  return(genes)
}


get_avail_data_for_organism <- function(chosen_organism, topic) {
  chosen_organism <- gsub('_', ' ', chosen_organism)
  avail_data <- organismAttributes(organism = chosen_organism, topic = topic)
  
  avail_data <- avail_data %>%
    filter(name == topic 
           #   & !str_detect(dataset, '_eg_gene')
    ) %>%
    distinct()
  
  return(avail_data)
}


get_kept_transcript_ids_list <- function(selected_organism, kept_transcript_dir) {
  gn_tr <- read.delim(paste0(kept_transcript_dir, '/', selected_organism, "_transcript_kept_per_gene.tsv"))
  colnames(gn_tr) <- c('gene_id', 'transcript_id')
  
  gn_tr$gene_id <- sub("\\.[^.]*$", "", gn_tr$gene_id) # remove the version indicator in human gene ids 
  gn_tr$transcript_id <- sub("\\.[^.]*$", "", gn_tr$transcript_id) # remove the version indicator in human gene ids 
  
  kept_transcript_ids_list <- gn_tr$transcript_id
  
  return(kept_transcript_ids_list)
}


get_exon_counts_per_copy <- function(dups_anc, selected_organisms, exon_output_dir, kept_transcript_dir) {
  
  exon_file <- paste0(exon_output_dir, selected_organism, '_exon.tsv')
  exon_data <- read.delim(exon_file)
  
  gene_column_number <- 1
  
  kept_transcript_ids_list <- get_kept_transcript_ids_list(selected_organism, kept_transcript_dir)
  
  # convert gene symbols to ensembl gene ids
  exon_data <- main_id_convert(df = exon_data, 
                               gene_column_number = gene_column_number, 
                               chosen_organism = selected_organism, 
                               kept_transcript_ids_list = kept_transcript_ids_list)
  
}


# FOR generate_file_organism_table()
# function to check if both genus and species are in the protein file name
match_protein_file <- function(organism_name, protein_files) {
  # Split the organism name into genus and species
  genus_species <- unlist(strsplit(organism_name, " "))
  # Check if both genus and species are in any protein file name
  matched_file <- protein_files[sapply(protein_files, function(file) {
    all(sapply(genus_species, function(word) grepl(word, file)))
  })]
  # Return the first match (or NA if no match)
  if (length(matched_file) > 0) matched_file[1] else NA
}


generate_file_organism_table <- function(prot_output_dir, selected_organisms) {
  
  # make all names lowercase
  organism_scientific_names <- tolower(selected_organisms)
  protein_files <- tolower(list.files(prot_output_dir))
  
  # match each organism with a protein file
  matched_files <- sapply(organism_scientific_names, match_protein_file, protein_files = protein_files)
  
  # create file_organism_table with the matched results
  file_organism_table <- data.frame(
    protein_file_name = matched_files,
    organism_scientific_name = organism_scientific_names,
    stringsAsFactors = FALSE
  )
  
  return(file_organism_table)
  
}


cat_all_in_dir <- function(dir, file_type = 'delim') {
  
  output_file <- data.frame()
  for (file in list.files(dir, full.names = T)) {
    
    if(file_type == 'delim'){output_file_one <- read.delim(file)}
    if(file_type == 'aa_fasta'){
      output_file_one <- readAAStringSet(file)
      output_file_one <- as.data.frame(output_file_one)
      if (colnames(output_file_one) == 'x') {
        output_file_one  <- output_file_one %>% rownames_to_column('id')
        colnames(output_file_one) <- c('id', 'aa_seq')
      }
    }
    
    if(file_type == 'nuc_fasta'){
      output_file_one <- readDNAStringSet(file)
      output_file_one <- as.data.frame(output_file_one)
    }
    

    output_file <- rbind(output_file, output_file_one)
  }
  
  return(output_file)
  
}


split_into_list <- function(string) {
  string <- strsplit(string, ',')[[1]]
  list <- trimws(string) # remove extra space from each element
  
  return(list)
}


format_list <- function(words) {
  if (length(words) == 1) {
    return(words)  # Single word, no need for "and"
  } else if (length(words) == 2) {
    return(paste(words, collapse = " and "))  # Just join with "and"
  } else {
    return(paste(paste(words[-length(words)], collapse = ", "), "and", words[length(words)]))
  }
}

