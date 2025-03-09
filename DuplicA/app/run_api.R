library(plumber)


r <- plumb(paste0("/Users/stellawang/Documents/new_dup/Untitled/DuplicA/app/api.R"))
r$run(host = "0.0.0.0", port = 8000)
