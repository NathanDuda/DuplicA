



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
    filter(rowSums(select(., 2:(n_species + 1)) == 2) == 1) # make sure there is only one species with 2 copies 
  
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
    apply(two_to_ones[, 2:(n_species + 1)], 1, function(x) {
      col_index <- which(x == 2)
      return(colnames(two_to_ones[, 2:(n_species + 1)])[col_index])})
  
  two_to_ones <- two_to_ones %>%
    select(Orthogroup, duplicate_pair_species)
  
  # merge back with the gene names 
  orthogroups <- read.delim(paste0(OF_dir_path, "/Orthogroups/Orthogroups.tsv"))
  two_to_ones <- merge(orthogroups, two_to_ones, by = 'Orthogroup')
  
  
  # extract the duplicate pair genes
  dups <- two_to_ones %>%
    rowwise() %>%
    mutate(duplicate_pair = toString(c_across(2:(n_species + 1))[grep(",", c_across(2:(n_species + 1)))])) %>%
    select(Orthogroup, duplicate_pair, duplicate_pair_species) %>%
    separate(duplicate_pair, into = c("dup_1", "dup_2"), sep = ", ")
  
  ################################
  # unit tests
  test_that('ensure genes are not part of more than one duplicate pair',
            expect_false(any(duplicated(c(dups$dup_1, dups$dup_2))), 
                         label = paste0('There are gene ids that participate in multiple duplicate pairs. Duplicate gene ids repeat'))
  )
  ## duplicate pairs do not repeat 
  test_that('ensure duplicate pairs do not repeat',
            expect_false(any(duplicated(dups %>% select(dup_1, dup_2))), 
                         label = paste0('There are repeating duplicate gene ids. Duplicate gene pairs repeat'))
  )
  ################################
  
  return(list(dups = dups, dup_pair_orthologs = two_to_ones))
}


clean_exp_and_pseudo <- function(exp_path, dups, add_pseudofunc, missing_expr_is_pseudo, exp_cutoff, PC) {
  
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
  all_expression[all_expression < exp_cutoff] <- 0 
  colnames(all_expression)[1] <- 'id'
  
  clean_expression <- all_expression
  pseudo <- NA
  
  if (add_pseudofunc == TRUE) {
    
    if (missing_expr_is_pseudo == TRUE) {
      colnames(all_expression)[1] <- 'dup_1'
      dups <- left_join(dups, all_expression, by = 'dup_1')
      
      colnames(all_expression)[1] <- 'dup'
      colnames(all_expression) <- paste0(colnames(all_expression), "_2")
      dups <- left_join(dups, all_expression, by = 'dup_2')
    }
    if (missing_expr_is_pseudo == FALSE) { # duplicates with missing expression are removed 
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
      select(dup_1, dup_2, pseudo) # add Orthogroup column if genes not unique
    
    if (PC == T) {colnames(pseudo) <- c('parent', 'child', 'pseudo')}
    
    return(list(pseudo = pseudo, clean_expression = clean_expression))
  }
  return(list(clean_expression = clean_expression))
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
    if(!is.na(expression)) {row[!row %in% expression$id] <- NA}
    
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
    select(Orthogroup, ancestral_copy, ancestral_species) %>%
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
  
  dups <- dups_anc %>% select(Orthogroup, all_of(copy))
  
  colnames(clean_expression)[1] <- copy
  exp_df <- merge(dups, clean_expression, by = copy)
  
  return(exp_df)
}

