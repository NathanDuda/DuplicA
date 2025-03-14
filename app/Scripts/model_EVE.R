
get_orthogroups <- function(OF_dir_path, dup_species_list, copy_amount, nondup_species_need_onecopy) {
  
  orthogroup_gene_count <- read.delim(paste0(OF_dir_path, "Orthogroups/Orthogroups.GeneCount.tsv"))
  n_species <- ncol(orthogroup_gene_count) - 2 
  
  # get list of non-dup species 
  species_list <- colnames(orthogroup_gene_count)
  species_list <- species_list[!species_list %in% c("Orthogroup", "Total")]
  nondup_species_list <- species_list[!species_list %in% dup_species_list]
  
  # define the number of copies the non-dup species can have
  if(nondup_species_need_onecopy == T) {copies_in_nondup_species <- c(1)}
  if(nondup_species_need_onecopy == F) {copies_in_nondup_species <- c(0,1)}
  
  # get the two to one to ones to zeros 
  orthogroups <- orthogroup_gene_count %>%
    #filter(if_all(2:(n_species + 1), ~. <= copy_amount)) # keep only pairs
    filter(if_all(all_of(dup_species_list), ~ . == copy_amount)) %>%
    filter(if_all(all_of(nondup_species_list), ~ . == copies_in_nondup_species)) 
  # CHANGE: add unit test for nrows existing for the chosen dup species 
  
  # merge back with the gene names 
  orthogroup_gns <- read.delim(paste0(OF_dir_path, "/Orthogroups/Orthogroups.tsv"))
  orthogroups <- orthogroups %>%
    dplyr::select(Orthogroup) %>%
    merge(orthogroup_gns, ., by = 'Orthogroup')
  
  # split each column with duplicates
  for (col in dup_species_list) {
    orthogroups <- orthogroups %>% separate(col, into = paste0(col, "_", 1:copy_amount), sep = ", ", remove = TRUE, fill = "right")
  }
  
  return(orthogroups)
  
}


get_tissueexp <- function(all_exp, tissue) {
  
  all_tissue_exp <- all_exp %>%
    dplyr::select(id, all_of(tissue))
  
  return(all_tissue_exp)
}


add_tissueexp_to_orthogroups <- function(tissue, all_orthogroups, all_tissueexp, missing_exp_is_zero, copy_amount, dup_species_list) {
  
  # merge expression data with all_orthogroups
  orthogroups_cols <- colnames(all_orthogroups)
  orthogroups_cols <- orthogroups_cols[!orthogroups_cols %in% c("Orthogroup")]
  
  # get expression value of the tissue for each gene
  all_orthogroups_exp <- all_orthogroups %>%
    pivot_longer(cols = all_of(orthogroups_cols), values_to = 'id', names_to = 'colnames') %>%
    merge(all_tissueexp, by = 'id') %>%
    pivot_wider(names_from = 'colnames', values_from = c(id, all_of(tissue))) %>%
    dplyr::select(-starts_with("id_")) %>%
    column_to_rownames('Orthogroup')
  
  # remove problematic rows
  all_orthogroups_exp <- all_orthogroups_exp %>%
    filter(rowSums(., na.rm = TRUE) != 0) %>%  # rm rows summing to 0 
    filter(apply(., 1, function(row) length(unique(na.omit(row))) > 1))  # rm rows with all same expression
  
  all_orthogroups_exp <- all_orthogroups_exp %>% na.omit()
  
  
  # format column names 
  colnames(all_orthogroups_exp) <- gsub(paste0("^", tissue, '_'), "", colnames(all_orthogroups_exp))
  
  # generate the colnames of the duplicate species 
  old_dup_species_colnames <- unlist(lapply(dup_species_list, function(x) paste0(x, "_", 1:copy_amount)))
  new_dup_species_colnames <- gsub("_[0-9]+$", "", old_dup_species_colnames)
  

  
  ######### remove orthogroups where all dups are non-expressed 
  all_orthogroups_exp <- all_orthogroups_exp %>%
    rownames_to_column(var = 'orthogroup') %>%
    pivot_longer(cols = 2:ncol(.), names_to = 'group')
  
  all_orthogroups_exp$new_group <- all_orthogroups_exp$group
  all_orthogroups_exp$new_group[all_orthogroups_exp$new_group %in% old_dup_species_colnames] <- new_dup_species_colnames
  
  all_orthogroups_exp <- all_orthogroups_exp %>%
    group_by(orthogroup, new_group) %>%
    filter(if (n() > 1) !all(value == 0) else TRUE) %>% 
    ungroup() %>%
    dplyr::select(-new_group) %>%
    pivot_wider(names_from = 'group', values_from = 'value') %>%
    na.omit()%>%
    column_to_rownames(var = 'orthogroup')
  #########
  
  # replace the old column names with the new column names (same column name for same species)
  colnames(all_orthogroups_exp)[colnames(all_orthogroups_exp) %in% old_dup_species_colnames] <- new_dup_species_colnames
  
  
  return(all_orthogroups_exp)
}


get_orthogroup_tree <- function(orthogroup, OF_dir_path, species_gn_key) {
  orthogroup_tree <- read.tree(file = paste0(OF_dir_path, "Gene_Trees/", orthogroup, "_tree.txt"))
  
  # add arbitrary node labels to the tree
  orthogroup_tree$node.label <- paste0("N", 0:(orthogroup_tree$Nnode -1)) 
  
  # remove species names from tip labels when exist
  pattern <- paste0(paste0(unique(species_gn_key), "_"), collapse = "|")
  orthogroup_tree$tip.label <- gsub(pattern, '', orthogroup_tree$tip.label)
  
  # replace current tip labels (which are gene ids) with the species names (so they can match dup_species_list)
  current_labels <- orthogroup_tree$tip.label
  orthogroup_tree$tip.label <- species_gn_key[current_labels]
  
  return(orthogroup_tree)
}


get_is_theta2_edge <- function(tree, dup_species_list) {
  
  # remove root from node labels
  node_labels <- tree$node.label
  node_labels <- node_labels[!node_labels %in% c("N0")]
  # combine tip labels with node labels
  tree_labels <- c(tree$tip.label, node_labels)
  
  # indicate nodes of duplicates species 
  is_theta2_edge <- tree_labels %in% dup_species_list
  
  
  
  return(is_theta2_edge)
}


get_species_gn_key <- function(all_orthogroups, all_orthogroups_tissueexp, dup_species_list) {
  
  # format the colnames of all_orthogroups
  species_gn_key <- all_orthogroups %>% column_to_rownames('Orthogroup')
  colnames(species_gn_key) <- colnames(all_orthogroups_tissueexp)
  
  # make into key
  species_gn_key <- pivot_longer(species_gn_key, cols = (1:(ncol(species_gn_key) - length(dup_species_list))),
                                 values_to = 'gene_id',
                                 names_to = 'species')
  
  # make the key into a named vector
  species_gn_key <- setNames(species_gn_key$species, species_gn_key$gene_id)
  
  return(species_gn_key)
}


# TwoTheta
main_Expression_Shift <- function(OF_dir_path, clean_expression, dup_species_list, tissue_list, copy_amount, nondup_species_need_onecopy, use_gene_trees) {
  
  allspecies_all_tissue_tt_results <- data.frame()
  for (dup_species in dup_species_list) {
    OF_dir_path <- paste0(OF_dir_path, '/')
    
    all_exp <- clean_expression #get_exp(exp_path)
    if ('All Tissues' %in% tissue_list) {tissue_list <- colnames(all_exp)[2:ncol(all_exp)]}
    
    
    all_orthogroups <- get_orthogroups(OF_dir_path, dup_species, copy_amount, nondup_species_need_onecopy)
    if(nrow(all_orthogroups) == 0) {return(paste0('No orthogroups exist with ', copy_amount, 'copies in ', dup_species))}
    
    
    
    alltissue_tt_results <- data.frame()
    for (tissue in tissue_list) {
      all_tissueexp <- get_tissueexp(all_exp, tissue)
      all_orthogroups_tissueexp <- add_tissueexp_to_orthogroups(tissue, all_orthogroups, all_tissueexp, missing_exp_is_zero, copy_amount, dup_species)
      
      
      nreps <- nrow(all_orthogroups_tissueexp)
      onetissue_res <- data.frame(orthogroup = rep(NA, nreps), tissue = rep(NA, nreps), LRT = rep(NA, nreps))
      
      if(use_gene_trees == F) {
        species_tree <- read.tree(file = paste0(OF_dir_path, "Species_Tree/SpeciesTree_rooted_node_labels.txt"))
        
        is_theta2_edge <- get_is_theta2_edge(species_tree, dup_species)
        
        tt_result <- twoThetaTest(tree = species_tree,
                                  gene.data = all_orthogroups_tissueexp, 
                                  isTheta2edge = is_theta2_edge,
                                  colSpecies = colnames(all_orthogroups_tissueexp))
        
        onetissue_res$orthogroup <- rownames(all_orthogroups_tissueexp)
        onetissue_res$LRT <- tt_result$LRT
        
      }
      
      
      # REQUIRE ENOUGH SPECIES and copies CHOSEN SO GENE TREE WAS MADE 
      # if true, might require nondup_species_need_onecopy
      if (use_gene_trees == T) {
        species_gn_key <- get_species_gn_key(all_orthogroups, all_orthogroups_tissueexp, dup_species)
        for (row in 1:nrow(all_orthogroups_tissueexp)){
          
          orthogroup <- rownames(all_orthogroups_tissueexp)[row]
          orthogroup_tree <- get_orthogroup_tree(orthogroup, OF_dir_path, species_gn_key)
          
          is_theta2_edge <- get_is_theta2_edge(orthogroup_tree, dup_species) 
          
          tt_result <- twoThetaTest_gene_tree(tree = orthogroup_tree,
                                              gene.data = all_orthogroups_tissueexp[row,], 
                                              isTheta2edge = is_theta2_edge,
                                              colSpecies = colnames(all_orthogroups_tissueexp))
          
          
          onetissue_res[row, 'orthogroup'] <- orthogroup
          onetissue_res[row, 'LRT'] <- tt_result$LRT
          
        }
        
      }
      
      onetissue_res$tissue <- tissue
      alltissue_tt_results <- rbind(alltissue_tt_results, onetissue_res)
    }
    allspecies_all_tissue_tt_results <- rbind(allspecies_all_tissue_tt_results, alltissue_tt_results)
    
  }
  write.table(allspecies_all_tissue_tt_results, file = paste0(here_results, '/main_ExpressionShift_output.tsv'))
  return(allspecies_all_tissue_tt_results)
}


# BetaShared
main_DiversityDivergence <- function(OF_dir_path, clean_expression, dup_species_list, tissue_list, copy_amount, nondup_species_need_onecopy, use_gene_trees, lower_beta_lim, upper_beta_lim) {
  
  allspecies_all_tissue_bt_results <- data.frame()
  for (dup_species in dup_species_list) {
    OF_dir_path <- paste0(OF_dir_path, '/')
    
    all_exp <- clean_expression #get_exp(exp_path)
    if ('All Tissues' %in% tissue_list) {tissue_list <- colnames(all_exp)[2:ncol(all_exp)]}
    
    all_orthogroups <- get_orthogroups(OF_dir_path, dup_species, copy_amount, nondup_species_need_onecopy)
    if(nrow(all_orthogroups) == 0) {return(paste0('No orthogroups exist with ', copy_amount, 'copies in ', dup_species))}
    
    
    alltissue_bt_results <- data.frame()
    for (tissue in tissue_list) {
      all_tissueexp <- get_tissueexp(all_exp, tissue)
      all_orthogroups_tissueexp <- add_tissueexp_to_orthogroups(tissue, all_orthogroups, all_tissueexp, missing_exp_is_zero, copy_amount, dup_species)
      
      
      nreps <- nrow(all_orthogroups_tissueexp)
      onetissue_res <- data.frame(orthogroup = rep(NA, nreps), tissue = rep(NA, nreps), LRT = rep(NA, nreps))
      
      if(use_gene_trees == F) {
        species_tree <- read.tree(file = paste0(OF_dir_path, "Species_Tree/SpeciesTree_rooted_node_labels.txt"))
        
  
        bt_result <- betaSharedTest(tree = species_tree,
                                    gene.data = all_orthogroups_tissueexp, 
                                    colSpecies = colnames(all_orthogroups_tissueexp),
                                    sharedBetaInterval = c(lower_beta_lim, upper_beta_lim))
        
        onetissue_res$orthogroup <- rownames(all_orthogroups_tissueexp)
        onetissue_res$LRT <- bt_result$LRT
        
      }
      
      
      # REQUIRE ENOUGH SPECIES and copies CHOSEN SO GENE TREE WAS MADE 
      # if true, might require nondup_species_need_onecopy
      if (use_gene_trees == T) {
        species_gn_key <- get_species_gn_key(all_orthogroups, all_orthogroups_tissueexp, dup_species)
        for (row in 1:nrow(all_orthogroups_tissueexp)){
          
          orthogroup <- rownames(all_orthogroups_tissueexp)[row]
          orthogroup_tree <- get_orthogroup_tree(orthogroup, OF_dir_path, species_gn_key)
          
          
          bt_result <- betaSharedTest_gene_tree(tree = orthogroup_tree, 
                                                gene.data = all_orthogroups_tissueexp[row,], 
                                                colSpecies = colnames(all_orthogroups_tissueexp),
                                                sharedBetaInterval = c(lower_beta_lim, upper_beta_lim))
          
          onetissue_res[row, 'orthogroup'] <- orthogroup
          onetissue_res[row, 'LRT'] <- bt_result$LRT
          
        }
        
      }
      
      onetissue_res$tissue <- tissue
      alltissue_bt_results <- rbind(alltissue_bt_results, onetissue_res)
    }
    allspecies_all_tissue_bt_results <- rbind(allspecies_all_tissue_bt_results, alltissue_bt_results)
    
  }
  write.table(allspecies_all_tissue_bt_results, file = paste0(here_results, '/main_DiversityDivergence_output.tsv'))
  return(allspecies_all_tissue_bt_results)
}



