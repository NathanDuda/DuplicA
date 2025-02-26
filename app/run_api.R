library(plumber)

curDir <- getwd()
curDir <- dirname(curDir)

r <- plumb(paste0(curDir, "/app/api.R"))
r$run(host = "0.0.0.0", port = 8000)
