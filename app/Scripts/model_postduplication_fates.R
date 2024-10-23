
source('./app/Scripts/multispecies_functions.R')



# general functions
contrib <- function(g) {
  return(sum(g))
}

contrib_intersection <- function(g, h) {
  return(sum(pmin(g, h)))
}

i <- function(g, h) {
  if (contrib(g) == 0) {return(1)}
  return(contrib_intersection(g, h) / contrib(g))
}

delta <- function(x, t = v) {
  first_term <- x - t
  second_term <- 1 - t
  
  return(max(0, (first_term / second_term) ))
}


# pseudofunctionalization term function
P_x <- function(a, x) {
  return(i(a, x) * (1 - (i(x, a) / p)))
}

# neofunctionalization term function
N <- function(a, b, g) {
  first_term <- 1 - i(a, g)
  second_term <- delta(i(b, g))
  
  return(first_term * second_term * i(g, b))
}

# all classification functions
pseudo_prob <- function(a, b, g) {
  P_a = P_x(a, g)
  P_b = P_x(b, g) 
  return(max(0, P_a, P_b))
}

neo_prob <- function(a, b, g) {
  N_a <- N(a, b, g)
  N_b <- N(b, a, g)
  
  first_term <- max(N_a, N_b)
  second_term <- 1 - pseudo_prob(a, b, g)
  
  return(first_term * second_term)
}

double_neo_prob <- function(a, b, g) {
  a_plus_b <- a + b
  
  first_term <- 1 - i(a, g)
  second_term <- 1 - i(b, g)
  third_term <- 1 - i(g, a_plus_b)
  fourth_term <- 1 - pseudo_prob(a, b, g)
  
  return(first_term * second_term * third_term * fourth_term)
}

conserv_prob <- function(a, b, g) {
  a_plus_b <- a + b
  b_plus_g <- b + g
  
  first_term <- delta(i(a, g))
  second_term <- delta(i(b, g))
  third_term <- i(g, a_plus_b)
  fourth_term <- delta(i(a, b_plus_g), t = 0.5)
  fifth_term <- 1 - pseudo_prob(a, b, g)
  
  return(first_term * second_term * third_term * fourth_term * fifth_term)
}

subfun_prob <- function(a, b, g) {
  a_plus_b <- a + b
  
  first_term <- delta(i(a, g))
  second_term <- delta(i(b, g))
  third_term <- i(g, a_plus_b)
  fourth_term <- delta(i(a_plus_b, g), t = 0.5)
  fifth_term <- 1 - pseudo_prob(a, b, g)
  
  return(first_term * second_term * third_term * fourth_term * fifth_term)
}

specializ_prob <- function(a, b, g) {
  a_plus_b <- a + b
  
  first_term <- i(g, a_plus_b)
  second_term <- 1 - delta(i(a, g))
  third_term <- 1 - delta(i(b, g))
  fourth_term <- 1 - pseudo_prob(a, b, g)
  
  return(first_term * second_term * third_term * fourth_term)
}



get_exp_df_for_all_copies <- function(dups_anc, clean_expression) {
  
  # get expression dataframe for each copy 
  dup_1 <- get_exp_df_for_copy(copy = 'dup_1', dups_anc, clean_expression)
  dup_2 <- get_exp_df_for_copy(copy = 'dup_2', dups_anc, clean_expression) 
  anc <- get_exp_df_for_copy(copy = 'ancestral_copy', dups_anc, clean_expression) 
  
  # add copy name to start of each column name 
  dup_1 <- dup_1 %>% select(-dup_1) %>% rename_with(~ paste0("dup_1_", .), -1)
  dup_2 <- dup_2 %>% select(-dup_2) %>% rename_with(~ paste0("dup_2_", .), -1)
  anc <- anc %>% select(-ancestral_copy) %>% rename_with(~ paste0("anc_", .), -1)
  
  # combine all expression dataframes into one dataframe
  dups_anc_exp <- merge(dup_1, dup_2, by = 'Orthogroup')
  dups_anc_exp <- merge(dups_anc_exp, anc, by = 'Orthogroup')
  
  return(dups_anc_exp)
}

# calculate probabilities function
get_func_probs <- function(dups_anc_exp) {
  
  func_probs <- dups_anc_exp %>%
    rowwise() %>%
    mutate(
      a = list(c_across(starts_with('dup_1_'))),
      b = list(c_across(starts_with('dup_2_'))),
      g = list(c_across(starts_with('anc'))),
      
      PS = pseudo_prob(a, b, g),
      N = neo_prob(a, b, g),
      DN = double_neo_prob(a, b, g),
      C = conserv_prob(a, b, g),
      SB = subfun_prob(a, b, g),
      SP = specializ_prob(a, b, g)
    ) %>%
    ungroup() %>%
    select(Orthogroup, PS, N, DN, C, SB, SP)
  
  return(func_probs)
}



main_postduplication_fates <- function(OF_dir_path, exp_path, exp_cutoff, PC) {
  
  out1 <- get_dups_from_OF(OF_dir_path)
  dups <- out1$dups
  dup_pair_orthologs <- out1$dup_pair_orthologs
  
  
  out2 <- clean_exp_and_pseudo(exp_path, dups, add_pseudofunc = F, missing_expr_is_pseudo = F, exp_cutoff, PC)
  clean_expression <- out2$clean_expression
  
  dups_anc <- get_anc_copy(OF_dir_path, dups, dup_pair_orthologs, clean_expression)
  dups_anc_exp <- get_exp_df_for_all_copies(dups_anc, clean_expression)
  
  func_probs <- get_func_probs(dups_anc_exp)
  
  
  
  
}





cdrom_func <- read.csv("C:/Users/17735/Downloads/Eight_Species/Dup_Functionalizations.tsv", sep="")

t <- cdrom_func %>%
  select(Orthogroup, func_pseudo)




#OF_dir_path <- 'C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/'
#exp_path <- 'C:/Users/17735/Downloads/Eight_Species/All_Expression_Data.tsv'
#exp_cutoff <- 1
#PC <- T





# g is a vector of expression values (one per tissue) for the gene g
g <- c(12, 436, 2, 24, 3, 34, 1, 7, 3)
h <- c(57, 46, 4, 12, 7, 91, 23, 5, 3)
a <- c(47, 16, 2, 162, 5, 1, 2, 21, 8)

p = 0.25
v = 0.25



