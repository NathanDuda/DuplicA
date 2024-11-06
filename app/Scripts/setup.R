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


here <- paste0(fs::path_home(), '/Downloads')
here_temp <- paste0(fs::path_home(), '/Downloads/DuplicA/app/Temp')
here_results <- paste0(fs::path_home(), '/Downloads/DuplicA/app/Results')
here_static_images <- paste0(fs::path_home(), '/Downloads/DuplicA/app/Static/Images')

here_linux <- '/mnt/c/Users/17735/Downloads'
here_linux_temp <- paste0(here_linux, '/DuplicA/app/Temp')
here_linux_dep <- paste0(here_linux, '/DuplicA/app/Dependencies')

# wsl is required 
# chmod +x /mnt/c/Users/17735/Downloads/DuplicA/app/Dependencies/MUSCLE/muscle-linux-x86.v5.2


