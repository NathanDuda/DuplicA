

# Function to run R script
run_r_script <- function(run_type, script_name, expression_file, ortho_dir, dups_file, exp_dir, add_pseudofunc, missing_expr_is_pseudo, exp_cutoff, PC, min_dups_per_species_pair, use_absolute_exp) {
  source(paste0("C:/Users/17735/Downloads/DuplicA/app/Scripts/", script_name))
  
  print(paste('is:', expression_file, ortho_dir, as.logical(add_pseudofunc), as.logical(missing_expr_is_pseudo), as.numeric(exp_cutoff), as.logical(PC), as.numeric(min_dups_per_species_pair), as.logical(use_absolute_exp)))
  print(paste('should be:', 'C:/Users/17735/Downloads/AAAAA___EXAMPLE_Expression.tsv', 'C:/Users/17735/Downloads/AAAAA_Results_Jan01', 'True', 'False', '1', 'False', '10', 'False'))
  
  tryCatch({
    if (run_type == 'OF') {
      main_CDROM(expression_file, ortho_dir, as.logical(add_pseudofunc), as.logical(missing_expr_is_pseudo), as.numeric(exp_cutoff), as.logical(PC), as.numeric(min_dups_per_species_pair), as.logical(use_absolute_exp))
    } else if (run_type == 'custom') {
      # does not work with custom input 
      #main_CDROM(dups_file, exp_dir, as.logical(add_pseudofunc), as.logical(missing_expr_is_pseudo), as.numeric(exp_cutoff), as.logical(PC), as.numeric(min_dups_per_species_pair), as.logical(use_absolute_exp))
    }
  }, error = function(e) {
    return(paste("An error occurred:\n", e$message))
  })
}


# allow input of directories 

dirInput <- function(inputId, label, width = NULL, buttonLabel = "Browse...", 
                     placeholder = "No directory selected", directory = TRUE) {
  
  restoredValue <- restoreInput(id = inputId, default = NULL)
  
  # Ensure the restored value is either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  # Input element to allow directory selection (directory=TRUE is the key)
  inputTag <- tags$input(
    id = inputId,
    class = "shiny-input-file",
    name = inputId,
    type = "file",
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )
  
  # Allow directory selection if the `directory` argument is TRUE
  if (directory) {
    inputTag$attribs$webkitdirectory <- "webkitdirectory"
  }
  
  # Return the UI for file input that is actually selecting a directory
  div(class = "form-group shiny-input-container",
      style = css(width = validateCssUnit(width)),
      
      div(class = "input-group",
          tags$label(class = "input-group-btn input-group-prepend",
                     span(class = "btn btn-default btn-file",
                          buttonLabel,
                          inputTag
                     )
          ),
          tags$input(type = "text", class = "form-control",
                     placeholder = placeholder, readonly = "readonly"
          )
      ),
      
      tags$div(
        id=paste(inputId, "_progress", sep=""),
        class="progress active shiny-file-input-progress",
        tags$div(class="progress-bar")
      )
  )
}

