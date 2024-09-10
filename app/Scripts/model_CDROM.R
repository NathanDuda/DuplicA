

library(tidyverse)




get_dups_from_OF <- function(OF_dir_path) {
  
  orthogroup_gene_count <- read.delim(paste0(OF_dir_path, "Orthogroups/Orthogroups.GeneCount.tsv"))
  n_species <- ncol(orthogroup_gene_count) - 2 
  
  # get the two to one to ones to zeros 
  two_to_ones <- orthogroup_gene_count %>%
    filter(if_all(2:(n_species + 1), ~. <= 2)) %>%          # keep only pairs 
    filter(rowSums(select(., 2:(n_species + 1)) == 2) == 1) # make sure there is only one species with 2 copies 
  
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
  
  return(list(dups = dups, dup_pair_orthologs = two_to_ones))
}
# ensure no genes repeat in either column 
# no repeating gene pairs 

clean_exp_and_pseudo <- function(exp_path, dups, add_pseudofunc, missing_expr_is_pseudo, rm_exp_lower_than, PC) {
  all_expression <- read.delim(exp_path, sep = ' ')
  all_expression[all_expression < rm_exp_lower_than] <- 0 
  colnames(all_expression)[1] <- 'id'
  
  clean_expression <- all_expression
  
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

library(ape)
get_anc_copy <- function(OF_dir_path, dups, dup_pair_orthologs, clean_expression){
  
  expression <- clean_expression
  orthologs <- dup_pair_orthologs
  
  # create function to find the closest expressed ortholog to each duplicate pair
  newick_tree <- ape::read.tree(paste0(OF_dir_path, 'Species_Tree/SpeciesTree_rooted.txt'))
  find_closest_ortholog <- function(row, species, newick_tree) {
    
    # calculate phylogenetic distances between the given species and each tip  
    species_node <- which(newick_tree$tip.label == species)
    distances <- cophenetic(newick_tree)[species_node, ]
    distances <- distances[distances > 0] # remove itself from distance calculation 
    
    # set non-expressed genes to NA 
    row[!row %in% expression$id] <- NA
    
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
    filter(!is.na(ancestral_copy)) # remove duplicates without ancestral copies 
    
  return(dups)
  
}


list_species_pairs <- function(dups_anc, min_dups_per_species_pair) {
  
  dups_anc <- dups_anc %>%
    mutate(species_pair = paste0(duplicate_pair_species, ancestral_species)) %>%
    group_by(species_pair) %>%
    mutate(n = n()) %>%
    filter(n >= min_dups_per_species_pair)
  
  
  species_pair_list <- unique(dups_anc$species_pair)
  
  return(species_pair_list)
  
}


get_all_sc_genes <- function(OF_dir_path) {
  orthogroups <- read.delim(paste0(OF_dir_path, "Orthogroups/Orthogroups.tsv"))
  sc_orthogroups <- read.table("C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/Orthogroups/Orthogroups_SingleCopyOrthologues.txt", quote="\"", comment.char="")
  colnames(sc_orthogroups)[1] <- 'sc_og'
  
  sc_orthogroups <- orthogroups %>% 
    filter(Orthogroup %in% sc_orthogroups$sc_og) %>%
    select(-Orthogroup)
  
  return(sc_orthogroups)
}

get_CDROM_inputs <- function(spec_pair, dups_anc, all_sc_genes, clean_expression, PC) {
  
  
  # get duplicate gene input (CPA columns) and sc gene input (two ortholog columns)
  dups_for_spec_pair <- dups_anc %>%
    mutate(species_pair = paste0(duplicate_pair_species, ancestral_species)) %>% 
    filter(species_pair == spec_pair) 
  
  duplicate_pair_species <- dups_for_spec_pair$duplicate_pair_species[1]
  ancestral_species <- dups_for_spec_pair$ancestral_species[1]
  
  sc_for_spec_pair <- all_sc_genes %>%
    select(all_of(c(duplicate_pair_species, ancestral_species)))
  
  dups_for_spec_pair <- dups_for_spec_pair %>%
    select(dup_1, dup_2, ancestral_copy)
  
  # get expression file for the ancestral species 
  rownames(clean_expression) <- NULL
  exp_anc_species <- clean_expression %>%
    filter((id %in% dups_for_spec_pair$ancestral_copy) | 
           (id %in% sc_for_spec_pair[[2]])) %>%
    column_to_rownames(var = 'id')
  
  # get expression file for the dup species 
  exp_dup_species <- clean_expression %>%
    filter((id %in% dups_for_spec_pair$dup_1) | 
           (id %in% dups_for_spec_pair$dup_2) | 
           (id %in% sc_for_spec_pair[[1]])) %>%
    column_to_rownames(var = 'id')
  
  if (PC == T) {
    colnames(dups_for_spec_pair) <- c('parent', 'child', 'ancestral_copy')
  }
  
  return(list(dup_input = dups_for_spec_pair,
              sc_input = sc_for_spec_pair,
              exp_1_input = exp_dup_species,
              exp_2_input = exp_anc_species))
  

  
}




# CDROM functions:
CreatePlot_EuclideanDistanceDensities_NOTPC <- function(densS1S2, densBoth, densPCA, 
                                                        lowerX, upperX, upperY, Ediv, legend) {
  
  # make plot 
  par(mar = c(4, 5, 1, 1), cex.axis = 1.4, cex.lab = 2)
  plot(densS1S2, col = "black", main = "", xlab = "Euclidean Distance", ylab = "", 
       lwd = 6, mgp = c(3, 1, 0), yaxt = "n", ylim = c(0, upperY), 
       xlim = c(lowerX, upperX), xaxs = "i", yaxs = "i")
  lines(densBoth, col = "green3", lwd = 6)
  lines(densPCA, col = "purple3", lwd = 6)
  abline(v = Ediv, col = "grey50", lty = 2, lwd = 4)
  
  if (legend == "topright") {
    legend("topright", c(expression(italic("E")["S1,S2"]), expression(italic("E")["D1,A"]+italic("E")["D2,A"]),
                         expression(italic("E")["D1+D2,A"]), paste("Ediv = ",round(Ediv, 4))), 
           fill = c("black", "green3", "purple3", "grey50"), cex = 2, pt.cex = 1.1)
  } else {
    legend("topleft", c(expression(italic("E")["S1,S2"]), expression(italic("E")["D1,A"]+italic("E")["D2,A"]),
                        expression(italic("E")["D1+D2,A"]), paste("Ediv = ",round(Ediv, 4))), 
           fill = c("black", "green3", "purple3", "grey50"), cex = 2, pt.cex = 1.1)
  }
  
  title(ylab = "Density", mgp = c(1.6, 1, 0), cex.lab = 2.2)
}
CreatePlot_EuclideanDistanceDensities_PC <- function(densS1S2, densCA, densPA, densPCA, 
                                                     lowerX, upperX, upperY, Ediv, legend) {
  # make plot 
  par(mar = c(4, 5, 1, 1), cex.axis = 1.4, cex.lab = 2)
  plot(densS1S2, col = "black", main = "", xlab = "Euclidean Distance", ylab = "", 
       lwd = 6, mgp = c(3, 1, 0), yaxt = "n", ylim = c(0, upperY), 
       xlim = c(lowerX, upperX), xaxs = "i", yaxs = "i")
  lines(densCA, col = "red", lwd = 6)
  lines(densPA, col = "blue3", lwd = 6)
  lines(densPCA, col = "purple3", lwd = 6)
  abline(v = Ediv, col = "grey50", lty = 2, lwd = 4)
  
  if (legend == "topright") {
    legend("topright", c(expression(italic("E")["S1,S2"]), expression(italic("E")["C,A"]), 
                         expression(italic("E")["P,A"]), expression(italic("E")["P+C,A"]), 
                         paste("Ediv = ",round(Ediv, 4))), 
           fill = c("black", "red", "blue3", "purple3", "grey50"), cex = 2, pt.cex = 1.1)
  } else {
    legend("topleft", c(expression(italic("E")["S1,S2"]), expression(italic("E")["C,A"]), 
                        expression(italic("E")["P,A"]), expression(italic("E")["P+C,A"]), 
                        paste("Ediv = ",round(Ediv, 4))), 
           fill = c("black", "red", "blue3", "purple3", "grey50"), cex = 2, pt.cex = 1.1)
  }
  
  title(ylab = "Density", mgp = c(1.6, 1, 0), cex.lab = 2.2)
}
# change so the plot does not appear when generating 
CreatePlot_ClassificationCross <- function(classes, PC, Ediv) {
  
  if(PC == FALSE){
    ClassificationCross <- classes %>%
      filter(Classification != 'NA') %>% # remove pseudo if exists because unplottable 
      mutate(`E_D1,A` = as.numeric(`E_D1,A`),
             `E_D2,A` = as.numeric(`E_D2,A`)) %>%
    
      ggplot(aes(x = `E_D2,A`, color = Classification, y = `E_D1,A`)) +
        geom_point() +
        theme_bw() +
        geom_hline(yintercept = Ediv, linewidth = 1) +
        geom_vline(xintercept = Ediv, linewidth = 1) +
        geom_point(aes(x = Ediv, y = Ediv), color = "gray", fill = 'gray', size = 3, shape = 21)
    }
  
  if(PC == TRUE){
    ClassificationCross <- classes %>%
      filter(Classification != 'NA') %>% # remove pseudo if exists because unplottable 
      mutate(`E_C,A` = as.numeric(`E_C,A`),
             `E_P,A` = as.numeric(`E_P,A`)) %>%
      
      ggplot(aes(x = `E_P,A`, color = Classification, y = `E_C,A`)) +
        geom_point() +
        theme_bw() +
        geom_hline(yintercept = Ediv, linewidth = 1) +
        geom_vline(xintercept = Ediv, linewidth = 1) +
        geom_point(aes(x = Ediv, y = Ediv), color = "gray", fill = 'gray', size = 3, shape = 21)
    
    
  }
  return(ClassificationCross)
}
CDROM <- function(dupFile, singleFile, exprFile1, exprFile2, out = "out", Ediv,  
                  PC = FALSE, useAbsExpr = FALSE, legend = "topleft", 
                  PlotEuclideanDistanceDensities = TRUE, PlotClassificationCross = TRUE) {
  
  out <- list()
  
  ## General checks are made
  
  #if (missing(exprFile1)) 
  #  stop("'exprFile1' is missing")
  #if (missing(exprFile2)) 
  #  stop("'exprFile2' is missing")
  #if (missing(dupFile)) 
  #  stop("'dupFile' is missing")
  #if (missing(singleFile)) 
  #  stop("'singleFile' is missing")
  #if (missing(Ediv)) 
  #  cat("Note: 'SIQR' will be used as Ediv\n")
  #if(! any(legend == c("topleft", "topright"))) {
  #  legend <- "topleft"
  #  cat("Legend will be set to default position (topleft)\n")
  #}
  
  
  ## Input files are read
  expr1 <- exprFile1
  expr2 <- exprFile2
  dups <- dupFile
  singles <- singleFile
  
  
  ## Expression data are obtained from expression files
  
  colnames(expr2) <- colnames(expr1)
  expr1$zeros <- 0
  expr2$zeros <- 0
  expr <- rbind(expr1[! (row.names(expr1) %in% row.names(expr2)), ], expr2)
  
  P <- dups[[1]]
  C <- dups[[2]]
  A <- dups[[3]]
  S1 <- singles[[1]]
  S2 <- singles[[2]]
  
  getP <- expr[row.names(expr) %in% P, ]
  getC <- expr[row.names(expr) %in% C, ]
  getA <- expr[row.names(expr) %in% A, ]
  getS1 <- expr[row.names(expr) %in% S1, ]
  getS2 <- expr[row.names(expr) %in% S2, ]
  
  exprP <- data.matrix(getP[match(P, rownames(getP)), ])
  exprC <- data.matrix(getC[match(C, rownames(getC)), ])
  exprA <- data.matrix(getA[match(A, rownames(getA)), ])
  exprS1 <- data.matrix(getS1[match(S1, rownames(getS1)), ])
  exprS2 <- data.matrix(getS2[match(S2, rownames(getS2)), ])
  exprPC <- exprP + exprC
  
  if (useAbsExpr == FALSE) {
    ## Relative expression values are calculated
    exprP <- exprP / rowSums(exprP)
    exprC <- exprC / rowSums(exprC)
    exprA <- exprA / rowSums(exprA)
    exprPC <- exprPC / rowSums(exprPC)
    exprS1 <- exprS1 / rowSums(exprS1)
    exprS2 <- exprS2 / rowSums(exprS2)
  }
  
  ## Euclidean distances are calculated
    eucPA <- (rowSums((exprP - exprA) ^ 2)) ^ (1/2)
    eucCA <- (rowSums((exprC - exprA) ^ 2)) ^ (1/2)
    eucPCA <- (rowSums((exprPC - exprA) ^ 2)) ^ (1/2)
    eucS1S2 <- (rowSums((exprS1 - exprS2) ^ 2)) ^ (1/2)
  
  ## Ediv is calculated 
  if (missing(Ediv)) {
    SIQR <- (IQR(eucS1S2, na.rm = TRUE) / 2)
    Ediv <- median(eucS1S2, na.rm = TRUE) + SIQR
    } 
    else {
      Ediv
      if (Ediv < 0) stop("'Ediv' must be greater than or equal to zero\n")
  }
  
  ## Classifications are made
  eucDists <- data.frame((eucPA), (eucCA), (eucPCA))
  eucDists <- replace(eucDists, is.na(eucDists), "NA")
  
  if (PC == FALSE) {
    eucDists <- within(eucDists, Classification <- "NA")
    eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. <= 
                Ediv), "Classification"] <- "Conservation"
    eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. <= 
                Ediv), "Classification"] <- "Neofunctionalization(Dup1)"
    eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. > 
                Ediv), "Classification"] <- "Neofunctionalization(Dup2)"
    eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                Ediv & eucDists$X.eucPCA. <= Ediv), "Classification"] <- "Subfunctionalization"
    eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                Ediv & eucDists$X.eucPCA. > Ediv), "Classification"] <- "Specialization"
    eucDists[(eucDists$X.eucPA. == "NA" | eucDists$X.eucCA. == "NA" | 
                eucDists$X.eucPCA. == "NA"), "Classification"] <- "NA"
    
    
    ## Densities of Euclidian distances are plotted 
    
    if (PlotEuclideanDistanceDensities == TRUE) {
      densS1S2 <- density(eucS1S2, na.rm = TRUE)
      Ymax_S1S2 <- max(densS1S2$y)
      Xmax_S1S2 <- max(densS1S2$x)
      Xmin_S1S2 <- min(densS1S2$x)
      
      densBoth <- density(c(eucCA, eucPA), na.rm = TRUE)
      Ymax_Both <- max(densBoth$y)
      Xmax_Both <- max(densBoth$x)
      Xmin_Both <- min(densBoth$x)
      
      densPCA <- density(eucPCA, na.rm = TRUE)
      Ymax_PCA <- max(densPCA$y)
      Xmax_PCA <- max(densPCA$x)
      Xmin_PCA <- min(densPCA$x)
      
      Ymax <- max(Ymax_S1S2, Ymax_Both, Ymax_PCA)
      Xmax <- max(Xmax_S1S2, Xmax_Both, Xmax_PCA)
      Xmin <- min(Xmin_S1S2, Xmin_Both, Xmin_PCA)
      
      Yrange <- (Ymax - 0)
      Xrange <- (Xmax - Xmin)
      
      if (useAbsExpr == FALSE) {
        upperY <- (Ymax + (0.05 * Yrange))
        lowerX <- (Xmin - (0.02 * Xrange))
        upperX <- (Xmax + (0.02 * Xrange))
      }
      
      if (useAbsExpr == TRUE) {
        upperY <- (Ymax + (0.05 * Yrange))			
        upperX <- (20 * IQR(eucS1S2, na.rm = TRUE))
        lowerX <- -IQR(eucS1S2, na.rm = TRUE)
      }
      
      
      
      
      CreatePlot_EuclideanDistanceDensities_NOTPC(densS1S2, densBoth, densPCA, 
                                                  lowerX, upperX, upperY, Ediv, legend)
      
      out[['EuclideanDistanceDensities']] <- recordPlot()

    }
    
    
    ## Output file1 is written
    
    classes <- data.frame(P, C, A, eucDists)
    rownames(classes) <- NULL
    colnames(classes) <- c("Dup1", "Dup2", "Ancestor", "E_D1,A", "E_D2,A", 
                           "E_D1+D2,A", "Classification")
    
    out[['classes']] <- classes
    
    
    ## Five E_div values are calculated
    
    meanSD <- mean(eucS1S2, na.rm = TRUE) + sd(eucS1S2, na.rm = TRUE)
    mean2SD <- mean(eucS1S2, na.rm = TRUE) + 2*sd(eucS1S2, na.rm = TRUE)
    medSIQR <- median(eucS1S2, na.rm = TRUE) + (IQR(eucS1S2, na.rm = TRUE) / 2)
    medIQR <- median(eucS1S2, na.rm = TRUE) + IQR(eucS1S2, na.rm = TRUE)
    quant75 <- quantile(eucS1S2, 0.75, na.rm = TRUE)
    
    
    ## Output2 file with five E_div values is generated
    
    Edivs <- (c(meanSD,mean2SD,medSIQR,medIQR,quant75))
    classify <- matrix(nrow = 5, ncol = 5)
    eucDists <- data.frame((eucPA), (eucCA), (eucPCA))
    
    for ( i in 1:5){
      
      Ediv <- Edivs[i]
      eucDists <- replace(eucDists, is.na(eucDists), "NA")
      
      eucDists <- within(eucDists, Classification <- "NA")
      eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. <= 
                  Ediv), "Classification"] <- "Conservation"
      eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. <= 
                  Ediv), "Classification"] <- "Neofunctionalization(Dup1)"
      eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. > 
                  Ediv), "Classification"] <- "Neofunctionalization(Dup2)"
      eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                  Ediv & eucDists$X.eucPCA. <= Ediv), "Classification"] <- "Subfunctionalization"
      eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                  Ediv & eucDists$X.eucPCA. > Ediv), "Classification"] <- "Specialization"
      eucDists[(eucDists$X.eucPA. == "NA" | eucDists$X.eucCA. == "NA" | 
                  eucDists$X.eucPCA. == "NA"), "Classification"] <- "NA"
      
      counts <- as.data.frame(table(eucDists[[4]]))
      countsOrdered <- counts[match(c("Conservation","Neofunctionalization(Dup1)",
                                      "Neofunctionalization(Dup2)","Subfunctionalization","Specialization"), counts[[1]]),]
      classify[i,] <- countsOrdered[[2]] 
    }
    
    classDF <- as.data.frame(classify)
    classDF[6] <- c("meanSD","mean2SD","medSIQR","medIQR","quant75")
    names(classDF)[6] <- "Ediv"
    
    idDF <- data.frame(Edivs)
    idDF[1] <- c("meanSD","mean2SD","medSIQR","medIQR","quant75")
    idDF[2] <- c(meanSD,mean2SD,medSIQR,medIQR,quant75)
    
    names(idDF)[1] <- "Ediv"
    
    total <- merge(idDF,classDF,by="Ediv")
    colnames(total) <- c("E_div","Value","Conservation","Neofunctionalization(Dup1)",
                         "Neofunctionalization(Dup2)","Subfunctionalization","Specialization")
    total[is.na(total)] <- 0
    totalOrdered <- total[match(c("meanSD","mean2SD","medSIQR","medIQR","quant75"), total[[1]]),]
    
    out[['EDiv_values']] <- totalOrdered
    
    
  }
  
  if (PC == TRUE) {
    eucDists <- within(eucDists, Classification <- "NA")
    eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. <= 
                Ediv), "Classification"] <- "Conservation"
    eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. <= 
                Ediv), "Classification"] <- "Neofunctionalization(Parent)"
    eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. > 
                Ediv), "Classification"] <- "Neofunctionalization(Child)"
    eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                Ediv & eucDists$X.eucPCA. <= Ediv), "Classification"] <- "Subfunctionalization"
    eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                Ediv & eucDists$X.eucPCA. > Ediv), "Classification"] <- "Specialization"
    eucDists[(eucDists$X.eucPA. == "NA" | eucDists$X.eucCA. == "NA" | 
                eucDists$X.eucPCA. == "NA"), "Classification"] <- "NA"
    
    
    ## Densities of Euclidian distances are plotted 
    
    
    if (PlotEuclideanDistanceDensities == TRUE) {
      
      densS1S2 <- density(eucS1S2, na.rm = TRUE)
      Ymax_S1S2 <- max(densS1S2$y)
      Xmax_S1S2 <- max(densS1S2$x)
      Xmin_S1S2 <- min(densS1S2$x)
      
      densCA <- density(eucCA, na.rm = TRUE)
      Ymax_CA <- max(densCA$y)
      Xmax_CA <- max(densCA$x)
      Xmin_CA <- min(densCA$x)
      
      densPA <- density(eucPA, na.rm = TRUE)
      Ymax_PA <- max(densPA$y)
      Xmax_PA <- max(densPA$x)
      Xmin_PA <- min(densPA$x)
      
      densPCA <- density(eucPCA, na.rm = TRUE)
      Ymax_PCA <- max(densPCA$y)
      Xmax_PCA <- max(densPCA$x)
      Xmin_PCA <- min(densPCA$x)
      
      Ymax <- max(Ymax_S1S2, Ymax_CA, Ymax_PA, Ymax_PCA)
      Xmax <- max(Xmax_S1S2, Xmax_CA, Xmax_PA, Xmax_PCA)
      Xmin <- min(Xmin_S1S2, Xmin_CA, Xmin_PA, Xmin_PCA)
      
      Yrange <- (Ymax - 0)
      Xrange <- (Xmax - Xmin)
      
      if (useAbsExpr == FALSE) {
        upperY <- (Ymax + (0.05 * Yrange))
        lowerX <- (Xmin - (0.02 * Xrange))
        upperX <- (Xmax + (0.02 * Xrange))
      }
      
      if (useAbsExpr == TRUE) {
        upperY <- (Ymax + (0.05 * Yrange))			
        upperX <- (20 * IQR(eucS1S2, na.rm = TRUE))
        lowerX <- -IQR(eucS1S2, na.rm = TRUE)
      }
      
      CreatePlot_EuclideanDistanceDensities_PC(densS1S2, densCA, densPA, densPCA,
                                               lowerX, upperX, upperY, Ediv, legend)
      
      out[['EuclideanDistanceDensities']] <- recordPlot()
      
    }
    
    
    ## Output file1 is written
    
    classes <- data.frame(P, C, A, eucDists)
    rownames(classes) <- NULL
    colnames(classes) <- c("Parent", "Child", "Ancestor", "E_P,A", "E_C,A", 
                           "E_P+C,A", "Classification")
    out[['classes']] <- classes
    
    
    
    ## Five E_div values are calculated
    
    meanSD <- mean(eucS1S2, na.rm = TRUE) + sd(eucS1S2, na.rm = TRUE)
    mean2SD <- mean(eucS1S2, na.rm = TRUE) + 2*sd(eucS1S2, na.rm = TRUE)
    medSIQR <- median(eucS1S2, na.rm = TRUE) + (IQR(eucS1S2, na.rm = TRUE) / 2)
    medIQR <- median(eucS1S2, na.rm = TRUE) + IQR(eucS1S2, na.rm = TRUE)
    quant75 <- quantile(eucS1S2, 0.75, na.rm = TRUE)
    
    
    ## Output2 file with five E_div values is generated
    
    Edivs <- (c(meanSD,mean2SD,medSIQR,medIQR,quant75))
    classify <- matrix(nrow = 5, ncol = 5)
    eucDists <- data.frame((eucPA), (eucCA), (eucPCA))
    
    for ( i in 1:5){
      
      Ediv <- Edivs[i]
      eucDists <- replace(eucDists, is.na(eucDists), "NA")
      
      eucDists <- within(eucDists, Classification <- "NA")
      eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. <= 
                  Ediv), "Classification"] <- "Conservation"
      eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. <= 
                  Ediv), "Classification"] <- "Neofunctionalization(Parent)"
      eucDists[(eucDists$X.eucPA. <= Ediv & eucDists$X.eucCA. > 
                  Ediv), "Classification"] <- "Neofunctionalization(Child)"
      eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                  Ediv & eucDists$X.eucPCA. <= Ediv), "Classification"] <- "Subfunctionalization"
      eucDists[(eucDists$X.eucPA. > Ediv & eucDists$X.eucCA. > 
                  Ediv & eucDists$X.eucPCA. > Ediv), "Classification"] <- "Specialization"
      eucDists[(eucDists$X.eucPA. == "NA" | eucDists$X.eucCA. == "NA" | 
                  eucDists$X.eucPCA. == "NA"), "Classification"] <- "NA"
      
      counts <- as.data.frame(table(eucDists[[4]]))
      countsOrdered <- counts[match(c("Conservation","Neofunctionalization(Parent)",
                                      "Neofunctionalization(Child)","Subfunctionalization","Specialization"), counts[[1]]),]
      classify[i,] <- countsOrdered[[2]] 
    }
    
    classDF <- as.data.frame(classify)
    classDF[6] <- c("meanSD","mean2SD","medSIQR","medIQR","quant75")
    names(classDF)[6] <- "Ediv"
    
    idDF <- data.frame(Edivs)
    idDF[1] <- c("meanSD","mean2SD","medSIQR","medIQR","quant75")
    idDF[2] <- c(meanSD,mean2SD,medSIQR,medIQR,quant75)
    
    names(idDF)[1] <- "Ediv"
    
    total <- merge(idDF, classDF, by = "Ediv")
    colnames(total) <- c("E_div","Value","Conservation","Neofunctionalization(Parent)",
                         "Neofunctionalization(Child)","Subfunctionalization","Specialization")
    total[is.na(total)] <- 0
    totalOrdered <- total[match(c("meanSD","mean2SD","medSIQR","medIQR","quant75"), total[[1]]),]
    
    out[['EDiv_values']] <- totalOrdered

  }
  
  
  if (PlotClassificationCross == TRUE){
    out[['ClassificationCross']] <- CreatePlot_ClassificationCross(out$classes, PC, Ediv)
  }
  
  return(out)
} 

####


add_pseudo_to_func <- function(all_func, pseudo, PC) {
  
  if (PC == F){
    all_func <- all_func %>%
    left_join(., pseudo, by = c('dup_1', 'dup_2')) %>%
    mutate(func = gsub('NA', NA, func),
           func = coalesce(func, pseudo)) %>%
    select(-pseudo)
  }
  
  if (PC == T){
    pseudo <- pseudo %>%
      mutate(pseudo = gsub('_dup_1', '_parent', pseudo),
             pseudo = gsub('_dup_2', '_child', pseudo))
    
    all_func <- all_func %>%
      left_join(., pseudo, by = c('parent', 'child')) %>%
      mutate(func = gsub('NA', NA, func),
             func = coalesce(func, pseudo)) %>%
      select(-pseudo)
  }
  
    
  return(all_func)
  
}

CreatePlot_FuncPie <- function(all_func){
  
  FuncPie <- all_func %>%
    group_by(func) %>%
    count() %>%
    ggplot(aes(x = '', y = n, group = func, fill = func)) +
      geom_bar(width = 1, stat = "identity", position = position_fill()) +
      geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour = 'black', size = 3) +
      theme_void() +
      theme(legend.title = element_blank(), panel.background = element_rect(fill = "white", color = "white"), legend.background = element_rect(fill = "white", color = 'white')) +
      coord_polar("y")# +
      #scale_fill_manual(values=c("#3B7BBD", "#E23F51", 'gray', 'darkgray', "#6CBC4D", '#F18244'))
    
  return(FuncPie)
}


########


#OF_dir_path <- 'C:/Users/17735/Downloads/Eight_Species/OrthoFinder_Output/Results_Jan01/'
#exp_path <- 'C:/Users/17735/Downloads/Eight_Species/All_Expression_Data.tsv'
#add_pseudofunc <- TRUE
#missing_expr_is_pseudo <- FALSE # only when add_pseudofunc is TRUE 
#rm_exp_lower_than <- 1 # set default to 1 - ensure numeric 
#PC <- T 
# MAKE SURE first column is p (dup_1) then c then a 
# ensure OGs dont repeat
# rm unecessary objects throughout code 
# make sure all necessary orthofinder files exist 

main_CDROM <- function(exp_path, OF_dir_path, add_pseudofunc, missing_expr_is_pseudo, rm_exp_lower_than, PC, min_dups_per_species_pair, useAbsExpr){
  
  OF_dir_path <- paste0(OF_dir_path, '/')
  
  out1 <- get_dups_from_OF(OF_dir_path)
  dups <- out1$dups
  dup_pair_orthologs <- out1$dup_pair_orthologs

  
  out2 <- clean_exp_and_pseudo(exp_path, dups, add_pseudofunc, missing_expr_is_pseudo, rm_exp_lower_than, PC)
  clean_expression <- out2$clean_expression
  
  dups_anc <- get_anc_copy(OF_dir_path, dups, dup_pair_orthologs, clean_expression)
  
  

  all_sc_genes <- get_all_sc_genes(OF_dir_path)
  

  species_pairs <- list_species_pairs(dups_anc, min_dups_per_species_pair)
  
  
  all_func <- data.frame()
  for (spec_pair in species_pairs){
  

    out3 <- get_CDROM_inputs(spec_pair, dups_anc, all_sc_genes, clean_expression, PC)
    dups_spec_pair <- out3$dup_input
    
    out4 <- CDROM(dupFile = out3$dup_input,
                  singleFile = out3$sc_input,
                  exprFile1 = out3$exp_1_input,
                  exprFile2 = out3$exp_2_input,
                  PC = PC,
                  useAbsExpr = useAbsExpr)
    
    
    out4$EDiv_values
    out4$EuclideanDistanceDensities
    out4$ClassificationCross
    
    if (PC == F) {
      func <- out4$classes %>%
      select(dup_1 = Dup1, dup_2 = Dup2, ancestral_copy = Ancestor, func = Classification) %>%
      left_join(dups_spec_pair, ., by = c('dup_1', 'dup_2', 'ancestral_copy'))
    }
    if (PC == T) {
      func <- out4$classes %>%
        select(parent = Parent, child = Child, ancestral_copy = Ancestor, func = Classification) %>%
        left_join(dups_spec_pair, ., by = c('parent', 'child', 'ancestral_copy'))
    }
    
    
    all_func <- rbind(all_func, func)
  }
  
  if (add_pseudofunc == TRUE) {
    pseudo <- out2$pseudo
    all_func <- add_pseudo_to_func(all_func, pseudo, PC)
  }
  
  
  FuncPie <- CreatePlot_FuncPie(all_func)
  
  
  #print('Finished successfully!')
  
  
}




#args <- c('C:/Users/17735/Downloads/EXAMPLE_Expression.tsv', 'C:/Users/17735/Downloads/EXAMPLE_OF_dir', 'True', 'False', '2', 'False', '10')

#exp_path <- args[1]
#OF_dir_path <- args[2]
#add_pseudofunc <- as.logical(args[3])
#missing_expr_is_pseudo <- as.logical(args[4])
#rm_exp_lower_than <- as.numeric(args[5])
#PC <- as.logical(args[6])
#min_dups_per_species_pair <- as.numeric(args[7])
#useAbsExpr <- as.logical(args[8])

args <- commandArgs(trailingOnly = TRUE)
main_CDROM(args[1], args[2], as.logical(args[3]), as.logical(args[4]), as.numeric(args[5]), as.logical(args[6]), as.numeric(args[7]), as.logical(args[8]))







# min_dups_per_species_pair must be above 2 (maybe 3) 


# allow folder of expression for each species (just combine) (unit test if ncol same)
# allow only keep duplicates in one species and not the other (or in a couple species) 

# allow custom colors for each func 
# change euc densities to ggplot

# when custom input (not orthofinder) force separate files for each species 

