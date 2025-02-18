install.packages("plumber", repos = "http://cran.us.r-project.org")
library(plumber)


prefix <- ""


r <- plumb(paste0(prefix, "/Users/stellawang/Documents/dupA/DuplicA/app/api.R"))
r$run(host = "0.0.0.0", port = 8000)
