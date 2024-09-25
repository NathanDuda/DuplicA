
library(evemodel)

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



exp <- read.csv("C:/Users/17735/Downloads/Eight_Species/Expressed_Expression_Data.tsv", sep="")
dups <- read.csv("C:/Users/17735/Downloads/Eight_Species/Dup_Pairs_Ancestral.tsv", sep="")
newick <- read.tree(text = '((dmoj:0.169203,dvir:0.151256)0.890019:0.076246,(dwil:0.266137,((dana:0.183164,(dyak:0.0732827,dmel:0.0732855)0.894349:0.121418)0.684997:0.0691683,(dpse:0.0284468,dper:0.0570781)0.94934:0.208308)0.61247:0.0587724)0.890019:0.076246);')
funcs <- read.csv("C:/Users/17735/Downloads/Eight_Species/Dup_Functionalizations.tsv", sep="")

exp <- exp %>% select(YOgnID, f_wb)



dups <- dups %>%
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



# merge with exp
colnames(exp) <- c('dup_1', 'dup_1_f_wb')
dups <- dups %>% merge(exp, by = 'dup_1')

colnames(exp) <- c('dup_2', 'dup_2_f_wb')
dups <- dups %>% merge(exp, by = 'dup_2')

colnames(exp) <- c('anc', 'anc_f_wb')
dups <- dups %>% merge(exp, by = 'anc')


# add funcs
funcs <- funcs %>%
  select(dup_1 = Dup1, dup_2 = Dup2, anc = Ancestor, func = func_actual) %>%
  merge(dups, ., by = c('dup_1','dup_2','anc')) %>%
  select(func, dup_1_f_wb, dup_2_f_wb, anc_f_wb)


exp <- dups %>%
  select(dup_1_f_wb, dup_2_f_wb, anc_f_wb) %>%
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


##### plot results

results %>%
  select(inc_dec_ratio_test_LRT, species_1_test_LRT, beta_values) %>%
  mutate(beta_log = log(beta_values)) %>%
  na.omit() %>%
  ggplot(aes(x = beta_log, y = inc_dec_ratio_test_LRT)) + geom_point()
















