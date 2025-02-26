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



here_duplica <- getwd()
here_duplica <- dirname(here_duplica)
here_duplica <- dirname(here_duplica)
#print(here_duplica)
source(paste0(here_duplica, '/app/Scripts/multispecies_functions.R'))
source(paste0(here_duplica, '/app/Scripts/model_OrthoFinder.R'))
source(paste0(here_duplica, '/app/Scripts/model_DnDs.R'))
source(paste0(here_duplica, '/app/Scripts/model_Segregating_Duplicates.R'))
source(paste0(here_duplica, '/app/Scripts/model_EVE.R'))
source(paste0(here_duplica, '/app/Scripts/model_Blat_Blast.R'))


here <- here_duplica

here <- paste0(here)
here_temp <- paste0(here,'/app/temp')
here_results <- paste0(here, '/app/Results')
here_static_images <- paste0(here, '/app/Static/Images')
