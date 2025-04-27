
############################################################
# Change this to your path:
path_to_duplica <- '/app'
#path_to_duplica <- "C:/Users/NathanD/Downloads/DuplicA/app"

############################################################

library(plumber)

r <- plumb(paste0(path_to_duplica, "/api.R")) 
r$run(host = "0.0.0.0", port = 8001)



# system(paste0(
#   "wsl cd ", wsl_path_to_duplica, 
#   "/app/status && python3 ", 
#   wsl_path_to_duplica, "/app/status_file_hosting.py"
# ))


