
get_all_sc_genes <- function(OF_dir_path) {
  orthogroups <- read.delim(paste0(OF_dir_path, "Orthogroups/Orthogroups.tsv"))
  sc_orthogroups <- read.table(paste0(OF_dir_path, "Orthogroups/Orthogroups_SingleCopyOrthologues.txt"), quote="\"", comment.char="")
  colnames(sc_orthogroups)[1] <- 'sc_og'
  
  ################################
  # unit tests
  test_that('the orthogroups in "Orthogroups/Orthogroups_SingleCopyOrthologues.txt" match the orthogroups in Orthogroups/Orthogroups.tsv',
            expect_true(any(orthogroups$Orthogroup %in% sc_orthogroups$sc_og),
                        label = paste0('The orthogroups in the file "', OF_dir_path, 'Orthogroups/Orthogroups_SingleCopyOrthologues.txt" and ',
                                       OF_dir_path, 'Orthogroups/Orthogroups.tsv do not match. Orthogroups overlap in the two files')))
  ################################
  
  sc_orthogroups <- orthogroups %>% 
    filter(Orthogroup %in% sc_orthogroups$sc_og) %>%
    dplyr::select(-Orthogroup)
  
  return(sc_orthogroups)
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


get_CDROM_inputs <- function(spec_pair, dups_anc, all_sc_genes, clean_expression, PC) {
  
  
  # get duplicate gene input (CPA columns) and sc gene input (two ortholog columns)
  dups_for_spec_pair <- dups_anc %>%
    mutate(species_pair = paste0(duplicate_pair_species, ancestral_species)) %>% 
    filter(species_pair == spec_pair)
  
  ################################
  # unit tests
  test_that('duplicate genes exist for the species pair',
            expect_true(nrow(dups_for_spec_pair) > 0,
                        label = paste0('No duplicate genes exist with the species pair: ', spec_pair, ' as the duplicate pair species and the ancestral species, respectively. Duplicate genes exist for the species pair')))

  # HERE##
  ################################
  
  duplicate_pair_species <- dups_for_spec_pair$duplicate_pair_species[1]
  ancestral_species <- dups_for_spec_pair$ancestral_species[1]
  
  sc_for_spec_pair <- all_sc_genes %>%
    dplyr::select(all_of(c(duplicate_pair_species, ancestral_species)))
  
  dups_for_spec_pair <- dups_for_spec_pair %>%
    dplyr::select(dup_1, dup_2, ancestral_copy)
  
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


CDROM <- function(dupFile, singleFile, exprFile1, exprFile2, out = "out", Ediv = NULL,  
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
  if (is.null(Ediv)) {
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


add_pseudo_to_func <- function(all_func, pseudo, PC) {
  
  if (PC == F){
    all_func <- all_func %>%
      left_join(., pseudo, by = c('dup_1', 'dup_2')) %>%
      mutate(func = gsub('NA', NA, func),
             func = ifelse(is.na(pseudo), func, pseudo)) %>%
      dplyr::select(-pseudo) %>%
      na.omit()
  }
  
  if (PC == T){
    pseudo <- pseudo %>%
      mutate(pseudo = gsub('_dup_1', '_parent', pseudo),
             pseudo = gsub('_dup_2', '_child', pseudo))
    
    all_func <- all_func %>%
      left_join(., pseudo, by = c('parent', 'child')) %>%
      mutate(func = gsub('NA', NA, func),
             func = ifelse(is.na(pseudo), func, pseudo)) %>%
      dplyr::select(-pseudo) 
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


main_CDROM <- function(dups, dups_anc, clean_expression, OF_dir_path, add_pseudofunc, rm_exp_lower_than, PC, min_dups_per_species_pair, useAbsExpr, pseudo = NA){
  
  OF_dir_path <- paste0(OF_dir_path, '/')
  
  all_sc_genes <- get_all_sc_genes(OF_dir_path)
  

  
  if (!exists("min_dups_per_species_pair") || is.null(min_dups_per_species_pair) || 
      length(min_dups_per_species_pair) == 0 || min_dups_per_species_pair == "") {min_dups_per_species_pair <- 1}
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

    
    if (PC == F) {
      func <- out4$classes %>%
      dplyr::select(dup_1 = Dup1, dup_2 = Dup2, ancestral_copy = Ancestor, func = Classification) %>%
      left_join(dups_spec_pair, ., by = c('dup_1', 'dup_2', 'ancestral_copy'))
    }
    if (PC == T) {
      func <- out4$classes %>%
        dplyr::select(parent = Parent, child = Child, ancestral_copy = Ancestor, func = Classification) %>%
        left_join(dups_spec_pair, ., by = c('parent', 'child', 'ancestral_copy'))
    }
    
    
    all_func <- rbind(all_func, func)
  }
  
  if (add_pseudofunc == TRUE) {
    #pseudo <- out2$pseudo
    all_func <- add_pseudo_to_func(all_func, pseudo, PC)
  }
  
  write.table(all_func, paste0(here_results, '/main_CDROM_output.tsv'))
  return(all_func)
}




# min_dups_per_species_pair must be above 2 (maybe 3) 


# allow folder of expression for each species (just combine) (unit test if ncol same)
# allow only keep duplicates in one species and not the other (or in a couple species) 

# allow custom colors for each func 
# change euc densities to ggplot

# when custom input (not orthofinder) force separate files for each species 


# MAKE SURE first column is p (dup_1) then c then a 
# ensure OGs dont repeat
# make sure all necessary orthofinder files exist 



