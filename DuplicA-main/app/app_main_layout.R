




server_rendering <- function(output) {
  
  # render the logo image 
  output$DuplicA_logo <- renderImage({list(src = paste0(here_static_images, '/DuplicA_logo.png'))}, deleteFile = FALSE)
  
  
  return(output)
}

