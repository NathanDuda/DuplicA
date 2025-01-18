library(shiny)
library(shinyFiles)
library(bslib)
library(htmltools)
library(shinyalert)
library(fs) # for file paths 
library(Biostrings) # translate() in translate_nucs_to_prots() in model_OrthoFinder.R
library(tools) # file_ext() in combine_raw_fastas() in model_OrthoFinder.R and file_path_sans_ext() in blat
library(testthat) # for unit tests
library(tidyverse)
library(readxl)
library(shinyjs)
library(ape)
library(R.utils)
library(seqinr)
library(evemodel)
library(igraph)

here_duplica <- paste0(fs::path_home(), '/Downloads/DuplicA')

#source('./Scripts/multispecies_workflow.R')
source(paste0(here_duplica, '/app/Scripts/multispecies_functions.R'))
#source('./app/workflow.R')
#source('./app/app_functions.R')
#source('./app/app_page_layouts.R')
source(paste0(here_duplica, '/app/Scripts/model_OrthoFinder.R'))
source(paste0(here_duplica, '/app/Scripts/model_DnDs.R'))
source(paste0(here_duplica, '/app/Scripts/model_Segregating_Duplicates.R'))
source(paste0(here_duplica, '/app/Scripts/model_EVE.R'))
source(paste0(here_duplica, '/app/Scripts/model_Blat_Blast.R'))



here <- paste0(fs::path_home(), '/Downloads')
here_temp <- paste0(fs::path_home(), '/Downloads/DuplicA/app/Temp')
here_results <- paste0(fs::path_home(), '/Downloads/DuplicA/app/Results')
here_static_images <- paste0(fs::path_home(), '/Downloads/DuplicA/app/Static/Images')

here_linux <- '/mnt/c/Users/17735/Downloads'
here_linux_temp <- paste0(here_linux, '/DuplicA/app/Temp')
here_linux_dep <- paste0(here_linux, '/DuplicA/app/Dependencies')
here_linux_results <- paste0(here_linux, '/DuplicA/app/Results')

# wsl is required 
# chmod +x /mnt/c/Users/17735/Downloads/DuplicA/app/Dependencies/MUSCLE/muscle-linux-x86.v5.2


