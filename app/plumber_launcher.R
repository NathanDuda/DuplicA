
library(plumber)

r <- plumb(paste0(path_to_duplica, "/api.R"))
r$run(host = "0.0.0.0", port = 8001)


