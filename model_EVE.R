
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



exp_orig <- read.csv("C:/Users/17735/Downloads/Eight_Species/Expressed_Expression_Data.tsv", sep="")
dups_orig <- read.csv("C:/Users/17735/Downloads/Eight_Species/Dup_Pairs_Ancestral.tsv", sep="")
newick <- read.tree(text = '((dmoj:0.169203,dvir:0.151256)0.890019:0.076246,(dwil:0.266137,((dana:0.183164,(dyak:0.0732827,dmel:0.0732855)0.894349:0.121418)0.684997:0.0691683,(dpse:0.0284468,dper:0.0570781)0.94934:0.208308)0.61247:0.0587724)0.890019:0.076246);')
funcs_orig <- read.csv("C:/Users/17735/Downloads/Eight_Species/Dup_Functionalizations.tsv", sep="")



dups_orig <- dups_orig %>%
  mutate(dup_species = case_when(str_detect(dup_1, "AN") ~ 'dana',
                                 str_detect(dup_1, "ME") ~ 'dmel',
                                 str_detect(dup_1, "MO") ~ 'dmoj',
                                 str_detect(dup_1, "PE") ~ 'dper',
                                 str_detect(dup_1, "PS") ~ 'dpse',
                                 str_detect(dup_1, "VI") ~ 'dvir',
                                 str_detect(dup_1, "WI") ~ 'dwil',
                                 str_detect(dup_1, "YA")~ 'dyak'),
         ancestral_species = case_when(str_detect(ancestral_copy, "AN") ~ 'dana',
                                       str_detect(ancestral_copy, "ME") ~ 'dmel',
                                       str_detect(ancestral_copy, "MO") ~ 'dmoj',
                                       str_detect(ancestral_copy, "PE") ~ 'dper',
                                       str_detect(ancestral_copy, "PS") ~ 'dpse',
                                       str_detect(ancestral_copy, "VI") ~ 'dvir',
                                       str_detect(ancestral_copy, "WI") ~ 'dwil',
                                       str_detect(ancestral_copy, "YA")~ 'dyak')) %>%
  select(dup_1, dup_2, anc = ancestral_copy, dup_species, ancestral_species) %>%
  mutate(species_pair = paste0(dup_species, ancestral_species)) %>%
  group_by(species_pair) %>%
  mutate(n = n()) %>%
  filter(n >= 294)




tissue_list <- colnames(exp_orig)[-1]

tissue_list <- 'f_wb'

all_results <- data.frame()
for (tissue in tissue_list) {
  
  exp <- exp_orig %>% select(YOgnID, all_of(tissue))
  
  
  
  
  # merge with exp
  colnames(exp) <- c('dup_1', 'dup_1_exp')
  dups <- dups_orig %>% merge(exp, by = 'dup_1')
  
  colnames(exp) <- c('dup_2', 'dup_2_exp')
  dups <- dups %>% merge(exp, by = 'dup_2')
  
  colnames(exp) <- c('anc', 'anc_exp')
  dups <- dups %>% merge(exp, by = 'anc')
  
  
  # add funcs
  funcs <- funcs_orig %>%
    select(dup_1 = Dup1, dup_2 = Dup2, anc = Ancestor, func = func_actual) %>%
    merge(dups, ., by = c('dup_1','dup_2','anc')) %>%
    select(func, dup_1_exp, dup_2_exp, anc_exp)
  
  
  exp <- dups %>%
    select(dup_1_exp, dup_2_exp, anc_exp) %>%
    filter(rowSums(.) != 0)
  colnames(exp) <- c('dyak', 'dyak', 'dmel')
  
  
  
  # run
  
  tt <- betaSharedTest(tree = newick,
                       gene.data = exp,
                       colSpecies = colnames(exp))
  
  tt$LRT
  
  
  
  ################################
  
  isTheta2edge = c(F,F,F,F,F,
                   F,F,F,F,T,
                   F,F,F,F)
  shiftSpecies = newick$tip.label[newick$edge[isTheta2edge & newick$edge[,2] <= Ntip(newick),2]]
  shiftSpecies
  
  
  t <- twoThetaTest(tree = newick,
                    gene.data = exp, 
                    isTheta2edge = c(F,F,F,F,F,
                                     F,F,F,F,T,
                                     F,F,F,F), ###########################?
                    colSpecies = colnames(exp))
  
  t$LRT
  
  
  #############
  
  
  results <- exp
  
  
  # format LRTs
  inc_dec_ratio_test_LRT <- data.frame(inc_dec_ratio_test_LRT = tt$LRT)
  species_1_test_LRT <- data.frame(species_1_test_LRT = t$LRT)
  
  # format beta values
  x <- as.data.frame(tt$indivBetaRes$par)
  beta_values <- data.frame(beta_values = x$beta)
  
  # add to results 
  results <- cbind(results, inc_dec_ratio_test_LRT)
  results <- cbind(results, species_1_test_LRT)
  results <- cbind(results, beta_values)
  
  results$tissue <- tissue
  
  all_results <- rbind(all_results, results)
  
  
  print('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')
  print(tissue)
}




#write.table(all_results, 'C:/Users/17735/Downloads/DuplicA/eve_model_all_tissues_result.tsv')

all_results_orig <- all_results
results <- all_results

# merge with ids
colnames(results)[1:3] <- c('dup_1_exp', 'dup_2_exp', 'anc_exp')

# filter results with non expressed dups or anc
results <- results %>%
  filter(anc_exp != 0) %>%
  filter(dup_1_exp != 0 & dup_2_exp != 0)


#
#exp <- exp_orig %>%
 # pivot_longer(cols = c(2:ncol(.))) %>%
  #filter(value != 0)

results <- exp %>%
  #select(dup_1 = YOgnID, tissue = name, dup_1_exp = value) %>%
  select(dup_1 = YOgnID, tissue, dup_1_exp = value) %>%
  filter(dup_1 %in% dups$dup_1) %>%
  merge(results, by = c('tissue', 'dup_1_exp'))

results <- exp %>%
  select(dup_2 = YOgnID, tissue = name, dup_2_exp = value) %>%
  filter(dup_2 %in% dups$dup_2) %>%
  merge(results, by = c('tissue', 'dup_2_exp'))

results <- exp %>%
  select(anc = YOgnID, tissue = name, anc_exp = value) %>%
  filter(anc %in% dups$anc) %>%
  merge(results, by = c('tissue', 'anc_exp'))




# merge with funcs
orig_results <- results %>%
  merge(funcs, by = c('dup_1_exp', 'dup_2_exp', 'anc_exp'))



##### plot results

results <- orig_results %>%
  #select(inc_dec_ratio_test_LRT, species_1_test_LRT, beta_values, func) %>%
  mutate(beta_log = log(beta_values)) %>%
  na.omit() %>%
  filter(func != 'sub')
  

results %>%
  ggplot(aes(x = beta_log, y = inc_dec_ratio_test_LRT, color = func)) + 
    geom_jitter(width = 1, height = 2)

results %>%
  ggplot(aes(x = species_1_test_LRT, y = inc_dec_ratio_test_LRT, color = func)) + 
  geom_jitter(width = 10, height = 3)

results %>%
  ggplot(aes(x = func, y = beta_log)) +
    geom_boxplot() +
    geom_point()


results %>%
  ggplot(aes(x = func, y = inc_dec_ratio_test_LRT)) +
  geom_boxplot() +
  geom_point()


results %>%
  ggplot(aes(x = func, y = species_1_test_LRT)) +
  geom_boxplot() +
  geom_point()


###############



results %>%
  ggplot(aes(x = species_1_test_LRT, y = beta_values)) + 
    geom_jitter(width = 10, height = 3) +
    scale_y_log10()


results %>%
  mutate(dup1_anc_diff = abs(dup_1_exp - anc_exp),
         dup2_anc_diff = abs(dup_2_exp - anc_exp)) %>%
  pivot_longer(cols = c('dup1_anc_diff', 'dup2_anc_diff'),
               names_to = 'type',
               values_to = 'diff') %>%
  ggplot(aes(x = species_1_test_LRT, y = diff)) + 
    geom_point() +
    scale_y_log10() +
    facet_wrap(.~type)

results %>%
  mutate(diff = anc_exp - abs(dup_2_exp - dup_1_exp)) %>%
  ggplot(aes(x = species_1_test_LRT, y = diff)) + 
  geom_point() 


#  pchisq(LRT, df = 1, lower.tail = F). However, in practice, with relatively small phylogenies, we recommend obtaining empirical significance thresholds using simulations.

###########################################################################################
###########################################################################################






exp_path <- 'C:/Users/17735/Downloads/AAAAA___EXAMPLE_Expression.tsv'
OF_dir_path <- 'C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/'
#OF_dir_path <- paste0(OF_dir_path, '/')
dup_species_list <- c('dmel_prot', 'dyak_prot')
rm_exp_lower_than <- 1
nondup_species_need_onecopy = F
copy_amount = 2
use_gene_trees = T
missing_exp_is_zero = T

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

add_tissueexp_to_orthogroups <- function(tissue, all_orthogroups, all_tissueexp, rm_exp_lower_than, missing_exp_is_zero) {
  
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
  
  
  return(all_orthogroups_exp)
}

get_orthogroup_tree <- function(orthogroup, OF_dir_path) {
  orthogroup_tree <- read.tree(file = paste0(OF_dir_path, "Gene_Trees/", orthogroup, "_tree.txt"))
  return(orthogroup_tree)
}

get_is_theta2_edge <- function(tree, dup_species_list) {
  
  is_theta2_edge <- tree$tip.label %in% dup_species_list
  
  # make it all nodes 
  ###
  
  
  
  return(is_theta2_edge)
}


main_Expression_Shift <- function(OF_dir_path, exp_path, dup_species_list, copy_amount, nondup_species_need_onecopy, rm_exp_lower_than, use_gene_trees, missing_exp_is_zero) {
  
  all_orthogroups <- get_orthogroups(OF_dir_path, dup_species_list, copy_amount, nondup_species_need_onecopy)
  all_exp <- get_exp(exp_path)
  
  tissue <- 'f_wb'
  all_tissueexp <- get_tissueexp(all_exp, tissue)
  
  all_orthogroups_tissueexp <- add_tissueexp_to_orthogroups(tissue, all_orthogroups, all_tissueexp, rm_exp_lower_than, missing_exp_is_zero)
  
  
  
  
  
  
  if(use_gene_trees == F) {
    species_tree <- read.tree(file = paste0(OF_dir_path, "Species_Tree/SpeciesTree_rooted_node_labels.txt"))
    
    is_theta2_edge <- get_is_theta2_edge(species_tree, dup_species_list)
    
    t <- twoThetaTest(tree = species_tree,
                      gene.data = all_orthogroups_tissueexp, 
                      isTheta2edge = is_theta2_edge,
                      colSpecies = colnames(all_orthogroups_tissueexp))
  }
  
  
  # REQUIRE ENOUGH SPECIES and copies CHOSEN SO GENE TREE WAS MADE 
  if (use_gene_trees == T) {
    for (row in 1:nrow(all_orthogroups_tissueexp)){
      orthogroup <- rownames(all_orthogroups_exp)[row]
      
      orthogroup_tree <- get_orthogroup_tree(orthogroup, OF_dir_path)
      
      is_theta2_edge <- get_is_theta2_edge(orthogroup_tree, dup_species_list) 
      
      
      
      t <- twoThetaTest(tree = orthogroup_tree,
                        gene.data = orthogroup_expression, 
                        isTheta2edge = is_theta2_edge,
                        colSpecies = colnames(orthogroup_expression))
    }
    
    
  }
  
  
  
  
  
}





