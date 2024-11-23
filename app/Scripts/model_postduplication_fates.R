
source('./app/Scripts/multispecies_functions.R')


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

delta <- function(x, v) {
  first_term <- x - v
  second_term <- 1 - v
  
  return(max(0, (first_term / second_term) ))
}


# pseudofunctionalization term function
P_x <- function(a, x, p) {
  return(i(a, x) * (1 - (i(x, a) / p)))
}

# neofunctionalization term function
N <- function(a, b, g, v) {
  first_term <- 1 - i(a, g)
  second_term <- delta(i(b, g), v)
  
  return(first_term * second_term * i(g, b))
}

# all classification functions
pseudo_prob <- function(a, b, g, p) {
  P_a = P_x(a, g, p)
  #P_b = P_x(b, g, p) 
  
  term <- max(0, P_a)
  #term <- max(0, P_a, P_b)
  return(term)
}

neo_prob <- function(a, b, g, v, p) {
  N_a <- N(a, b, g, v)
  #N_b <- N(b, a, g, v)
  
  first_term <- N_a
  #first_term <- max(N_a, N_b)
  second_term <- 1 - pseudo_prob(a, b, g, p)
  
  return(first_term * second_term)
}

double_neo_prob <- function(a, b, g, p) {
  a_plus_b <- a + b
  
  first_term <- 1 - i(a, g)
  second_term <- 1 - i(b, g)
  third_term <- 1 - i(g, a_plus_b)
  fourth_term <- 1 - pseudo_prob(a, b, g, p)
  
  return(first_term * second_term * third_term * fourth_term)
}

conserv_prob <- function(a, b, g, v, p) {
  a_plus_b <- a + b
  b_plus_g <- b + g
  
  first_term <- delta(i(a, g), v)
  second_term <- delta(i(b, g), v)
  third_term <- i(g, a_plus_b)
  fourth_term <- delta(i(a, b_plus_g), v = 0.5)
  fifth_term <- 1 - pseudo_prob(a, b, g, p)
  
  return(first_term * second_term * third_term * fourth_term * fifth_term)
}

subfun_prob <- function(a, b, g, v, p) {
  a_plus_b <- a + b
  
  first_term <- delta(i(a, g), v)
  second_term <- delta(i(b, g), v)
  third_term <- i(g, a_plus_b)
  fourth_term <- delta(i(a_plus_b, g), v = 0.5)
  fifth_term <- 1 - pseudo_prob(a, b, g, p)
  
  return(first_term * second_term * third_term * fourth_term * fifth_term)
}

specializ_prob <- function(a, b, g, v, p) {
  a_plus_b <- a + b
  
  first_term <- i(g, a_plus_b)
  second_term <- 1 - delta(i(a, g), v)
  third_term <- 1 - delta(i(b, g), v)
  fourth_term <- 1 - pseudo_prob(a, b, g, p)
  
  return(first_term * second_term * third_term * fourth_term)
}


# calculate probabilities function
get_func_probs <- function(dups_anc_exp, v, p) {
  
  func_probs <- dups_anc_exp %>%
    rowwise() %>%
    mutate(
      a = list(c_across(starts_with('dup_1_'))),
      b = list(c_across(starts_with('dup_2_'))),
      g = list(c_across(starts_with('anc'))),
      
      PS_dup_1 = pseudo_prob(a, b, g, p),
      PS_dup_2 = pseudo_prob(b, a, g, p),
      N_dup_1 = neo_prob(a, b, g, v, p),
      N_dup_2 = neo_prob(b, a, g, v, p),
      DN = double_neo_prob(a, b, g, p),
      C = conserv_prob(a, b, g, v, p),
      SB = subfun_prob(a, b, g, v, p),
      SP = specializ_prob(a, b, g, v, p)
    ) %>%
    ungroup() %>%
    select(Orthogroup, PS_dup_1, PS_dup_2, N_dup_1, N_dup_2, DN, C, SB, SP)
  
  return(func_probs)
}



main_postduplication_fates <- function(dups_anc, clean_expression, v, p) {
  
  dups_anc_exp <- get_exp_df_for_all_copies(dups_anc, clean_expression)
  
  func_probs <- get_func_probs(dups_anc_exp, v, p)
  
  return(func_probs)
}


#OF_dir_path <- 'C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/'
#exp_path <- 'C:/Users/17735/Downloads/Eight_Species/All_Expression_Data.tsv'
#exp_cutoff <- 1
#PC <- F
#v <- 0.2
#p <- 0.05
#main_postduplication_fates('C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/',
 #                          'C:/Users/17735/Downloads/Eight_Species/All_Expression_Data.tsv',
  #                         1, F, 0.2, 0.05)

