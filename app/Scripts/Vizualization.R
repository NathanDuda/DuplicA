

# example expression file generation 
files <- c('C:/Users/17735/Downloads/Eight_Species/Raw_Data/YO_Expression/GSE99574_HiSAT2_dana.nrc.YO.txt',
           'C:/Users/17735/Downloads/Eight_Species/Raw_Data/YO_Expression/GSE99574_HiSAT2_dmel.nrc.YO.txt',
           'C:/Users/17735/Downloads/Eight_Species/Raw_Data/YO_Expression/GSE99574_HiSAT2_dmoj.nrc.YO.txt')
all_exp <- data.frame()
for (file in files) {
  
  exp <- read.delim(file)
  exp <- exp %>%
    dplyr::select(id = FBgnID, exp = 4) %>%
    filter(grepl("\\d", id))
  
  all_exp <- rbind(all_exp, exp)
  
  
}
write.table(all_exp, file = 'C:/Users/17735/Downloads/AAAAA_Expression_Input_Example/dana_dmel_dmoj_exp.tsv')

#######################################################################################











main_run_workflow(selected_models, input) 




