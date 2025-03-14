
############################################################
# Change this to your path:
path_to_duplica <- 'C:/Users/17735/Downloads/DuplicA'
############################################################

library(plumber)

r <- plumb(paste0(path_to_duplica, "/app/api.R"))
r$run(host = "0.0.0.0", port = 8000)

