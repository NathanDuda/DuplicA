
############################################################
# Change this to your path:
#path_to_duplica <- 'C:/Users/NathanD/Downloads/DuplicA'
path_to_duplica <- '/app'
############################################################


#library(shiny)
#library(shinyFiles)
library(bslib)
library(htmltools)
#library(shinyalert)
library(fs)
library(Biostrings) 
library(tools)
library(testthat) 
library(tidyverse)
library(readxl)
#library(shinyjs)
library(ape)
library(R.utils)
library(seqinr)
library(evemodel)
library(igraph)
library(biomartr)
library(data.table)
library(biomaRt)
library(RColorBrewer)
library(jsonlite)
library(future)
library(furrr)

#library(r3dmol)
#library(httr)
#library(bio3d)




here_duplica <- path_to_duplica

source(paste0(here_duplica, '/Scripts/multispecies_functions.R'))
source(paste0(here_duplica, '/Scripts/model_OrthoFinder.R'))
source(paste0(here_duplica, '/Scripts/model_DnDs.R'))
source(paste0(here_duplica, '/Scripts/model_EVE.R'))


here_temp <- paste0(here_duplica, '/Temp')
here_results <- paste0(here_duplica, '/Results')
here_static_images <- paste0(here_duplica, '/Static/Images')


#here_duplica_linux <- '/mnt/c/Users/17735/Downloads/DuplicA'
#here_linux_temp <- paste0(here_duplica_linux, '/app/Temp')
#here_linux_dep <- paste0(here_duplica_linux, '/app/Dependencies')
#here_linux_results <- paste0(here_duplica_linux, '/app/Results')

# wsl is required 


