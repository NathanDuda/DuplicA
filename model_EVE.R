
library(evemodel)
library(tidyverse)
library(ape)

exp_row_1 <- rnorm(5, mean = 40, sd = 1)   
exp_row_2 <- rnorm(5, mean = 43, sd = 1.5)  
exp_row_3 <- rnorm(5, mean = 12, sd = 3)       
exp_row_4 <- rnorm(5, mean = 11, sd = 2)       
exp_row_5 <- rnorm(5, mean = 12, sd = 5)       
exp_row_6 <- rnorm(5, mean = 15, sd = 2)       

exp <- cbind(exp_row_1, exp_row_2, exp_row_3, exp_row_4, exp_row_5, exp_row_6)
rownames(exp) <- c("gn_a", "gn_b", "gn_c", "gn_d", "gn_e")
colnames(exp) <- c('s1', 's1', 's2', 's2', 's3', 's3')

newick <- ape::read.tree(text = "(s1:0.2,(s2:0.3,s3:0.4):0.5);")

###
# add genes with drastically increased and decreased ratios
increased_ratio_gene <- c(1000,40,1,1002,43,900)
decreased_ratio_gene <- c(1,2,40,43,1000,1002)
 
exp <- rbind(increased_ratio_gene, exp)
exp <- rbind(decreased_ratio_gene, exp)
###

exp <- cbind(exp_row_1, exp_row_3, exp_row_5)
colnames(exp) <- c('s1', 's3', 's3')


tt <- betaSharedTest(tree = newick,
                     gene.data = exp,
                     colSpecies = colnames(exp))

tt$LRT



################################


t <- twoThetaTest(tree = newick,
                  gene.data = exp, 
                  isTheta2edge = c(T,F,F,F),
                  colSpecies = colnames(exp))

t$LRT



########################



results <- exp


inc_dec_ratio_test_LRT <- data.frame(inc_dec_ratio_test_LRT = tt$LRT)
species_1_test_LRT <- data.frame(species_1_test_LRT = t$LRT)

results <- cbind(results, inc_dec_ratio_test_LRT)
results <- cbind(results, species_1_test_LRT)


############################################################################
# implement for my duplicate gene data





exp_path <- 'C:/Users/17735/Downloads/AAAAA___EXAMPLE_Expression.tsv'
OF_dir_path <- 'C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/'
#OF_dir_path <- paste0(OF_dir_path, '/')
dup_species_list <- c('dmel_prot', 'dyak_prot')
rm_exp_lower_than <- 1
nondup_species_need_onecopy = F
copy_amount = 2
use_gene_trees = T
missing_exp_is_zero = T # F default

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
    select(Orthogroup) %>%
    merge(orthogroup_gns, ., by = 'Orthogroup')
  
  # split each column with duplicates
  for (col in dup_species_list) {
    orthogroups <- orthogroups %>% separate(col, into = paste0(col, "_", 1:copy_amount), sep = ", ", remove = TRUE, fill = "right")
  }
  
  return(orthogroups)
  
}

get_exp <- function(exp_path) {
  
  # import expression data 
  all_exp <- read.delim(exp_path, sep = ' ')
  all_exp[all_exp < rm_exp_lower_than] <- 0 
  
  colnames(all_exp)[1] <- 'id'
  
  return(all_exp)
}

get_tissueexp <- function(all_exp, tissue) {
  
  all_tissue_exp <- all_exp %>%
    select(id, all_of(tissue))
  
  return(all_tissue_exp)
}

add_tissueexp_to_orthogroups <- function(tissue, all_orthogroups, all_tissueexp, rm_exp_lower_than, missing_exp_is_zero, copy_amount, dup_species_list) {
  
  # merge expression data with all_orthogroups
  orthogroups_cols <- colnames(all_orthogroups)
  orthogroups_cols <- orthogroups_cols[!orthogroups_cols %in% c("Orthogroup")]
  
  # get expression value of the tissue for each gene
  all_orthogroups_exp <- all_orthogroups %>%
    pivot_longer(cols = all_of(orthogroups_cols), values_to = 'id', names_to = 'colnames') %>%
    merge(all_tissueexp, by = 'id') %>%
    pivot_wider(names_from = 'colnames', values_from = c(id, all_of(tissue))) %>%
    select(-starts_with("id_")) %>%
    column_to_rownames('Orthogroup')
  
  # remove problematic rows
  all_orthogroups_exp <- all_orthogroups_exp %>%
    filter(rowSums(., na.rm = TRUE) != 0) %>%  # rm rows summing to 0 
    filter(apply(., 1, function(row) length(unique(na.omit(row))) > 1))  # rm rows with all same expression
  
  if (missing_exp_is_zero == T) {
    all_orthogroups_exp <- all_orthogroups_exp %>% 
      mutate(across(everything(), ~ replace(., is.na(.), 0)))
  }
  
  all_orthogroups_exp <- all_orthogroups_exp %>% na.omit()
  
  
  # format column names 
  colnames(all_orthogroups_exp) <- gsub(paste0("^", tissue, '_'), "", colnames(all_orthogroups_exp))
  
  # generate the colnames of the duplicate species 
  old_dup_species_colnames <- unlist(lapply(dup_species_list, function(x) paste0(x, "_", 1:copy_amount)))
  new_dup_species_colnames <- gsub("_[0-9]+$", "", old_dup_species_colnames)
  
  # replace the old column names with the new column names (same column name for same species)
  colnames(all_orthogroups_exp)[colnames(all_orthogroups_exp) %in% old_dup_species_colnames] <- new_dup_species_colnames
  
  res <- all_orthogroups_exp %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column(var = 'group')
  
  original_groups <- res$group
  res$group <- colnames(all_orthogroups_exp)
  
  
  ################ fix
  t <- res %>%
    group_by(group) %>%
    filter(if (n() > 1) !all(across(everything(), ~ . == 0)) else TRUE) %>%  # remove non expressed dup species
    ungroup() 
  
  t$group <- original_groups
  
  t <- t %>%
    column_to_rownames(var = 'group') %>%
    t() %>%
    as.data.frame()
  
  
  return(res)
}

get_orthogroup_tree <- function(orthogroup, OF_dir_path, species_gn_key) {
  orthogroup_tree <- read.tree(file = paste0(OF_dir_path, "Gene_Trees/", orthogroup, "_tree.txt"))
  
  # add arbitrary node labels to the tree
  orthogroup_tree$node.label <- paste0("N", 0:(orthogroup_tree$Nnode -1)) 
  
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


main_Expression_Shift <- function(OF_dir_path, exp_path, dup_species_list, copy_amount, nondup_species_need_onecopy, rm_exp_lower_than, use_gene_trees, missing_exp_is_zero) {
  
  all_orthogroups <- get_orthogroups(OF_dir_path, dup_species_list, copy_amount, nondup_species_need_onecopy)
  all_exp <- get_exp(exp_path)
  
  tissue <- 'f_wb'
  all_tissueexp <- get_tissueexp(all_exp, tissue)
  
  all_orthogroups_tissueexp <- add_tissueexp_to_orthogroups(tissue, all_orthogroups, all_tissueexp, rm_exp_lower_than, missing_exp_is_zero, copy_amount, dup_species_list)
  
  
  
  
  
  
  if(use_gene_trees == F) {
    species_tree <- read.tree(file = paste0(OF_dir_path, "Species_Tree/SpeciesTree_rooted_node_labels.txt"))
    
    is_theta2_edge <- get_is_theta2_edge(species_tree, dup_species_list, type = 'species')
    
    t <- twoThetaTest(tree = species_tree,
                      gene.data = all_orthogroups_tissueexp, 
                      isTheta2edge = is_theta2_edge,
                      colSpecies = colnames(all_orthogroups_tissueexp))
  }
  
  
  # REQUIRE ENOUGH SPECIES and copies CHOSEN SO GENE TREE WAS MADE 
  if (use_gene_trees == T) {
    species_gn_key <- get_species_gn_key(all_orthogroups, all_orthogroups_tissueexp, dup_species_list)
    for (row in 1:nrow(all_orthogroups_tissueexp)){
      orthogroup <- rownames(all_orthogroups_tissueexp)[row]
      
      orthogroup_tree <- get_orthogroup_tree(orthogroup, OF_dir_path, species_gn_key)
      
      is_theta2_edge <- get_is_theta2_edge(orthogroup_tree, dup_species_list) 
      
      t <- twoThetaTest(tree = orthogroup_tree,
                        gene.data = all_orthogroups_tissueexp, 
                        isTheta2edge = is_theta2_edge,
                        colSpecies = colnames(all_orthogroups_tissueexp))
      
      print(t$LRT)
      
    }
    
    
  }
  
  
  
  
  
}


# per tissue


