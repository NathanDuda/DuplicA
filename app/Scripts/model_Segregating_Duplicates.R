
library(tidyverse)
library(Biostrings)
library(seqinr)
#install_github("peterbchi/CNVSelectR")
library(CNVSelectR)

#######################

# format data

# simulate data
simulated_pop_cnvs <- data.frame(Individual = c("ABC123", "DEF456", "GHI789",
                                                "ABC123", "DEF456", "GHI789",
                                                "ABC123", "XXXXXX"), 
                                 gene = c(paste0('ID', sprintf("%05d", 1:8))),
                                 group = c('A', 'B', 2, 'A', 'B', 2, 3, 4))
cnvs <- simulated_pop_cnvs

nuc_file <- 'C:/Users/17735/Downloads/Simulated_Data/nuc_sim.fasta'
prot_file <- 'C:/Users/17735/Downloads/Simulated_Data/prot_sim.fasta'


####




make_temp_dirs <- function(replace_dirs) {
  
  create_or_replace_dir <- function(dir_path) {
    if (replace_dirs == T) {system(paste0('wsl ', 'rm -rf ', dir_path), intern = TRUE)}
    system(paste0('wsl ', 'mkdir -p ', dir_path), intern = TRUE)
  }
  
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Protein_Sequences'))
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Protein_Alignments'))
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Nucleotide_Sequences'))
  create_or_replace_dir(paste0(here_linux_temp, '/Connected_Eq_Codon_Alignments'))
  
  
}


get_pairs <- function(cnvs) {
  
  colnames(cnvs) <- c('individual', 'gene', 'group')
  
  pairs <- cnvs %>%
    group_by(individual, group) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 2)
  # underscores in individual name and group name must be changed 

  return(pairs)
}


get_freqs <- function(pairs) {
  freqs <- pairs %>%
    group_by(group) %>%
    summarise(freq = n_distinct(individual))
  
  return(freqs)
}


get_paired_fastas <- function(cnvs, nuc_file, prot_file) {
  nucs <- readDNAStringSet(nuc_file)
  nucs <- data.frame(gene = names(nucs), nuc = as.character(nucs))  
  
  cnvs <- left_join(cnvs, nucs, by = 'gene')
  rm(nucs)


  prots <- readAAStringSet(prot_file)
  prots <- data.frame(gene = names(prots), prot = as.character(prots))
  
  cnvs <- left_join(cnvs, prots, by = 'gene')
  rm(prots)

  
  # write pairs to fasta files 
  for (group_id in unique(cnvs$group)) {
    group_rows <- cnvs %>% filter(group == group_id)
    
    for (row_num in 1:nrow(group_rows)) {
      row <- group_rows[row_num,]
      group_indiv_id <- paste0(group_id, '_', row$individual)
      
      output_file <- paste0(here_temp, '/Connected_Eq_Nucleotide_Sequences/group_', group_indiv_id, '.fa')  # NOT LINUX 
      cat(">", row$gene, '\n', row$nuc, "\n", file = output_file, append = T, sep = '')
      
      output_file <- paste0(here_temp, '/Connected_Eq_Protein_Sequences/group_', group_indiv_id, '.fa')  
      cat(">", row$gene, '\n', row$prot, "\n", file = output_file, append = T, sep = '')
    }
  }
}


get_prot_alignments <- function(aligner) {
  if (aligner == 'muscle') {
    
    muscle_path <- paste0(here_linux_dep, '/MUSCLE/muscle-linux-x86.v5.2')
    
    # mkdirs if don't exist 
    # align proteins of groups of pairs 
    muscle_command <- paste0(
      'for file in ', here_linux_temp, '/Connected_Eq_Protein_Sequences/*; do ',
      'filename=$(basename "$file"); ',
      muscle_path, ' -quiet -align "$file" -output "', here_linux_temp, '/Connected_Eq_Protein_Alignments/${filename}"; ',
      'done'
    )
    
    system(paste('wsl', muscle_command), intern = T)

  }
}


get_codon_alignments <- function() { # pal2nal
  pal2nal_path <- paste0(here_linux_dep, '/pal2nal.v14/pal2nal.pl')
  
  
  pal2nal_command <- paste0(
    'for file in ', here_linux_temp, '/Connected_Eq_Protein_Sequences/*; do ',
    'filename=$(basename "$file" .fa); ',  # Assumes .fa as the extension for protein and nucleotide files
    pal2nal_path, ' ', here_linux_temp, '/Connected_Eq_Protein_Alignments/${filename}.fa ', 
    here_linux_temp, '/Connected_Eq_Nucleotide_Sequences/${filename}.fa ', 
    '-output fasta > ', here_linux_temp, '/Connected_Eq_Codon_Alignments/${filename}.fa; ',
    'done'
  )
  system(paste('wsl', pal2nal_command), intern = T)
}


# change this to paml or something in the cl 
get_dnds <- function() {
  dnds_results <- data.frame()

  for (file in list.files(paste0(here_temp, '/Connected_Eq_Codon_Alignments'), full.names = T)) {
    group <- str_extract(basename(file), "(?<=_)[^_]+(?=_)")
    
    codon_alignment <- read.alignment(file = file, format = 'fasta')
    dnds <- kaks(codon_alignment)
  
    row <- data.frame(group = group, dn = dnds['ka'], ds = dnds['ks'])
    dnds_results <- rbind(dnds_results, row)
  }
  return(dnds_results)
}

##################################################

# CNVSelectR functions





mexpv_dipneut_CUSTOM <- function(t, A, v,N,Pos, tol=1e-7, m=NULL){
  #TLS Have to modify number of arguments compared to standard expokit implementation to
  #account for extra variables N,Pos.
  n <- dim(A)[1]
  if(is.null(m)){
    m <- min(n,30)
  }
  
  anorm <- norm(A,'I')
  mxrej <- 10;  btol  <- 1.0e-7
  gamma <- 0.9; delta <- 1.2
  mb    <- m; t_out   <- abs(t)
  istep <- 0; t_new   <- 0
  t_now <- 0; s_error <- 0
  rndoff <-  anorm*2.2204e-16
  
  k1 <- 2; xm <- 1/m; normv <- sqrt(sum(v)^2); beta <- normv
  fact <- (((m+1)/exp(1))^(m+1))*sqrt(2*pi*(m+1))
  t_new <- (1/anorm)*((fact*tol)/(4*beta*anorm))^xm
  s <- 10^(floor(log10(t_new))-1); t_new <- ceiling(t_new/s)*s
  sgn <- sign(t); istep <- 0
  tlist <- NULL
  w <- v
  hump <- normv
  count1 <- 0
  
  # I think I need to initialize these since they are vectors
  dupgenomesavgMat <- NA
  PIlowerMat <- NA
  PIupperMat <- NA
  
  while (t_now < t_out){
    ####################################################################
    #TLS Main modifications come in here.
    tlist <- c(tlist, t_now)
    count1 <- count1+1
    # calculate proportion duplicated conditional on wt not fixing
    pn <- rep(0,2*N)#pn(i) stores conditional probably of haveing i haploid genomes with a duplicate at time t_now (each individual has two haploid genomes)
    if (w[1] > 0){
      w <- w/(1-w[1])
      w[1] <- 0
    }
    pnsummer <- 0
    pnflag <- 0
    for (i in 1:(N+1)){
      for (j in 1:(N+2-i)){
        if (i!=1 || j != 1){
          pnsummer <- pnsummer+w[Pos[i,j]]
          pn[2*(i-1)+(j-1)] <- pn[2*(i-1)+(j-1)] + w[Pos[i,j]]
          if (pnsummer == 1){
            pnflag <- 1
            break
          }
        }
        if (pnflag ==1){
          break
        }
      }
      if (pnflag == 1){
        break
      }
    }
    dupgenomesavg <- pn%*%(1:(2*N))/(2*N) #Stores the expected number of haploid genomes with a duplicate at time t_now
    
    #Now calculate the 95# prediction interval on the proportion of
    #duplicate haplotypes
    
    cumsumpn <- cumsum(pn)
    PIlower <- min(which(cumsumpn>=0.025,1))/(2*N)  # something is wrong here with larger N
    PIupper <- min(which(cumsumpn>=0.975,1))/(2*N)
    dupgenomesavgMat[count1] <- dupgenomesavg
    PIlowerMat[count1] <- PIlower
    PIupperMat[count1] <- PIupper
    
    
    #################################################
    #TLS - Now back to standard mexpv as implemented in expokit.
    
    istep <- istep + 1
    t_step <- min( t_out-t_now,t_new )
    V <- matrix(0, nrow=n,ncol=(m+1))
    H <- matrix(0, nrow=(m+2),ncol=(m+2))
    
    V[,1] <- as.double((1/beta)*as.matrix(w)) # this line seems to cause problems with bigger N
    for (j in 1:m){
      p <- A%*%V[,j]
      for (i in 1:j){
        H[i,j] <- as.double((V[,i]) %*% p)
        p <- p-H[i,j]*V[,i]
      }
      s <- norm(p, "f")   # Matlab defaults to Frobenius norm, make sure this is ok though since p is a vector
      if (s < btol){
        k1 <- 0
        mb <- j
        t_step <- t_out-t_now
        break
      }
      H[j+1,j] <- s
      V[,j+1] <- as.double((1/s)*p)
    }
    if (k1 != 0){
      H[m+2,m+1] <- 1
      avnorm <- norm(A%*%V[,m+1], "f")  # 4/29/21 I think this is right but check closer
    }
    ireject <- 0
    while (ireject <= mxrej){
      mx <- mb + k1
      F <- expm(sgn*t_step*H[1:mx,1:mx])
      if (k1 == 0){
        err_loc <- btol
        break
      } else {
        phi1 <- abs( beta*F[m+1,1] )
        phi2 <- abs( beta*F[m+2,1] * avnorm )
        if (phi1 > 10*phi2){
          err_loc <- phi2
          xm <- 1/m
        } else if (phi1 > phi2){
          err_loc <- (phi1*phi2)/(phi1-phi2)
          xm <- 1/m
        } else {
          err_loc <- phi1
          xm <- 1/(m-1)
        }
      }
      if (err_loc <= delta * t_step*tol){
        break
      } else {
        t_step <- gamma * t_step * (t_step*tol/err_loc)^xm
        s <- 10^(floor(log10(t_step))-1)
        t_step <- ceiling(t_step/s) * s
        if (ireject == mxrej){
          #error('The requested tolerance is too high.')
          return(NA)
        }
        ireject <- ireject + 1
      }
    }
    mx <- mb + max( c(0,(k1-1) ))
    w <- V[,(1:mx)]%*%(beta*F[1:mx,1])   # check this 4/29/21
    beta <- norm( w , "f")
    hump <- max(hump,beta)
    
    ineg <- 0
    for (i in 1:n){
      if (w[i] < 0){
        w[i] <- 0
        ineg <- ineg + 1
      }
    }
    wnorm <- norm(w)
    if (ineg > 0){
      w <- (1/wnorm)*w
    }
    roundoff <- abs(1-wnorm)/n   # this was 1.0d0, no idea what that means
    
    t_now <- t_now + t_step
    t_new <- gamma * t_step * (t_step*tol/err_loc)^xm
    s <- 10^(floor(log10(t_new))-1)
    t_new <- ceiling(t_new/s) * s
    
    err_loc <- max(err_loc,roundoff)
    err_loc <- max(err_loc,rndoff)
    s_error <- s_error + err_loc
  }
  err <- s_error
  hump <- hump / normv
  
  return(list(dupgenomesavgMat,PIlowerMat,PIupperMat,tlist, cumsumpn ,w))
}


CNVSelect_test_custom <- function(neut_matrix, input_file_table, dnds_results){
  output <- list()
  
  ####
  ### inputs <- read_DAF(input_file)
  DAF <- input_file_table
  first.col <- dim(DAF)[1]
  inputs <- list()
  inputs$N <- as.numeric(DAF[1,2])   # N
  inputs$ploidy <- as.numeric(DAF[2,2])   # ploidy
  inputs$freqs <- as.numeric(DAF[5:first.col,1][1])  # frequencies

  
  dS <- dnds_results$ks[dnds_results$group == group]

  
  
  
  output$freqs <- rep(inputs$freqs, length(dS))
  output$dS <- dS
  
  
  #if(any(dS == 0)){return(NA)} 
  
  for(i in 1:length(dS)){ # will be 1:1 when only dup pairs 
    up <- 1e-3
    t <- (dS[i]*35)/up

    out2 <- mexpv_dipneut_CUSTOM(t=t, A=t(neut_matrix[[1]]), v=neut_matrix[[2]], N=inputs$N, Pos=neut_matrix[[3]])
    
    #if (any(is.na(out2))){return(NA)}
    
    output$crit_lower[i] <- out2[[2]][length(out2[[2]])]
    output$crit_upper[i] <- out2[[3]][length(out2[[3]])]
    
    lower.tail <- out2[[5]][inputs$freqs[1]*2*inputs$N]
    if(lower.tail < 0.5){
      output$p_val[i] <- lower.tail * 2}
    else{
      output$p_val[i] <- (1-lower.tail) * 2}
  }
  return(output)
  
}


run_CNVSelectR <- function(freqs, dnds_results, neut_matrix, n_individuals, ploidy, full_or_approx) {
  
  dnds_results <- dnds_results %>%
    mutate(group = as.numeric(group)) %>%
    left_join(freqs, ., by = 'group') %>%
    mutate(freq = freq / n_individuals)
  
  
  output_df <- data.frame()
  
  for (fasta_name in list.files('C:/Users/17735/Downloads/Dmel_Duplicate_Genes/CNVSelectR/Connected_Eq_Combined_Codon_Alignments/', full.names = T)) {
    group <- str_extract(basename(fasta_name), "(?<=group_)[^\\.]+(?=\\.fa)")
    
    freq <- dnds_results$freq[dnds_results$group == group][1]
    add_frequencies <- rep(freq, nrep)
    add_NAs <- rep(NA, nrep + 1)
    
    input_file_table <- data.frame(
      A = c("Ne", "ploidy", "full/approximate", "frequency", add_frequencies),
      B = c(n_individuals, ploidy, full_or_approx, add_NAs))
    
    
    tryCatch({# error whenever internal stop codons 
      out <- CNVSelect_test_custom(neut_matrix, input_file_table, dnds_results)
      
      
      for (i in 1:length(out[['dS']])) {
        ds <- out[['dS']][i]
        name <- group
        pval <- out[['p_val']][i]
        
        output_df <- rbind(output_df, data.frame(name, ds, pval, group))
      }
      
    }, error = function(e) {
      # If an error occurs, print a message and continue with the loop
      print(paste("Error occurred for file:", file_name))
    })
    
    print(group)
  }
  
  
  
  
  
  
  
}







##########
main_pop_dnds <- function(nuc_file, prot_file, aligner = 'muscle', replace_dirs = T) {
  replace_dirs <- F ############## remove 
  make_temp_dirs(replace_dirs)
  
  #get_paired_fastas(pairs, nuc_file, prot_file)
  get_prot_alignments(aligner)
  get_codon_alignments()
  dnds_results <- get_dnds()
  return(dnds_results)
}


cnvs <- read.csv("C:/Users/17735/Downloads/Dmel_Duplicate_Genes/connected_dups_sep.tsv", sep="")
full_or_approx = 'full'
n_individuals = 47
ploidy = 2

main_Segregating_Duplications <- function(cnvs, dnds_results, popgen_dnds_exists = F, n_individuals, ploidy, full_or_approx) {
  
  pairs <- get_pairs(cnvs)
  freqs <- get_freqs(pairs)
  
  if (popgen_dnds_exists == F){dnds_results <- main_pop_dnds()}
  
  
  neut_matrix <- CNVSelectR::Genedupdip_neutralgenerator(n_individuals, up = 1e-3)
  output <- run_CNVSelectR(freqs, dnds_results, neut_matrix, n_individuals, ploidy, full_or_approx)
  

}

###########


