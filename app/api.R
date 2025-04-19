
source(paste0(path_to_duplica, '/Scripts/setup.R'))
source(paste0(path_to_duplica, '/Scripts/multispecies_workflow.R'))
source(paste0(path_to_duplica, '/Scripts/Visualization.R'))


#* Enable Cross-origin Resource Sharing
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

# get input button list from selected_models 
#* @post /getInputs
getInputs <- function(req, selected_models, parameters) {

  selected_models <- as.character(unlist(selected_models))
    
  # get buttons
  input <- main_get_relevant_parameter_list(selected_models, parameters)
  
  # Return the result as JSON
  return(input)
}




# generate figure image 
#* @post /makeImage
makeImage <- function(req) {
  # Parse incoming JSON request body
  parameters <- fromJSON(req$postBody)
  
  # Debugging: Print received parameters
  print("Received request:")
  print(parameters)
  
  # Call function with extracted parameters
  main_generate_figure(parameters)
  
}



# function to run workflow  
#* @post /runWorkflow
runWorkflow <- function(req, selected_models, parameters) {
  
  #body <- fromJSON(req$postBody)
  #selected_models <- body$selected_models
  #parameters <- body$parameters
  
  
  selected_models <- as.character(unlist(selected_models))

  main_run_workflow(selected_models, parameters)
  
}



#* Delete the generated image file
#* @post /deleteImage
deleteImage <- function() {
  file_path <- paste0(here_duplica, '/status/Figure.png')
  
  # delete the file
  if (file.exists(file_path)) {file.remove(file_path)}
  
}




