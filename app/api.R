
source(paste0(path_to_duplica, '/app/Scripts/setup.R'))
source(paste0(path_to_duplica, '/app/Scripts/multispecies_workflow.R'))
source(paste0(path_to_duplica, '/app/Scripts/Visualization.R'))


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
function(req, selected_models, parameters) {

  selected_models <- as.character(unlist(selected_models))
    
  # get buttons
  input <- main_get_relevant_parameter_list(selected_models, parameters)
  
  # Return the result as JSON
  return(input)
}




# generate figure image 
#* @post /makeImage
function(req) {
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
function(req, selected_models, parameters) {
  selected_models <- req$body$selected_models
  parameters <- req$body$parameters
  
  main_run_workflow(selected_models, parameters)
  
}





