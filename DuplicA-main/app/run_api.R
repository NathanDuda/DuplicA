library(plumber)


prefix <- '/mnt/c'
prefix <- 'C:'

r <- plumb(paste0(prefix, "/Users/17735/Downloads/DuplicA/app/api.R"))
r$run(host = "0.0.0.0", port = 8000)

