install.packages("tidyverse")
#install.packages("shiny")
#install.packages("shinyFiles")
install.packages("bslib")
install.packages("htmltools")
#install.packages("shinyalert")
install.packages("fs") # for file paths
install.packages("tools") # file_ext() in combine_raw_fastas() in model_OrthoFinder.R and file_path_sans_ext() in blat
install.packages("testthat") # for unit tests
install.packages("tidyverse")
install.packages("readxl")
#install.packages("shinyjs")
install.packages("ape")
install.packages("R.utils")
install.packages("seqinr")
install.packages("biomartr")
#install.packages("r3dmol")
install.packages("httr")
#install.packages("bio3d")
install.packages("igraph")
install.packages("future")
install.packages("furrr")

install.packages("plumber") # for the API 

install.packages("BiocManager")
BiocManager::install("Biostrings", ask = F)# translate() in translate_nucs_to_prots() in model_OrthoFinder.R
BiocManager::install("biomaRt", ask = F)
BiocManager::install("biomartr", ask = F)
BiocManager::install("clusterProfiler", ask = F)

install.packages("devtools")
remotes::install_gitlab("sandve-lab/evemodel")
remotes::install_github("peterbchi/CNVSelectR")


