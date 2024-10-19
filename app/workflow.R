


source('./app_page_options.R')




workflow_page <- function() {
  useShinyjs()  # Initialize shinyjs
  
  titlePanel("Workflow Selection")
  
  tabsetPanel(
    tabPanel("Multi-Species", 
             mainPanel(
               fluidRow(
                 column(4,
                        h4("Step 1: First Model"),
                        actionButton("btn_orthofinder", "OrthoFinder", 
                                     class = "model-btn", 
                                     style = "background-color: #007bff;"),
                        tags$script(HTML("
              $('#btn_orthofinder').click(function() {
                var currentColor = $(this).css('background-color');
                if (currentColor === 'rgb(0, 123, 255)') { // Blue
                  $(this).css('background-color', 'green');
                  $('#multi_species_second_models').show();  // Show second models
                } else {
                  $(this).css('background-color', '#007bff');  // Back to blue
                  $('#multi_species_second_models').hide();  // Hide second models
                  $('#multi_species_plot_options').hide();   // Hide plot options
                }
              });
            "))
                 ),
                 column(4,
                        h4("Step 2: Second Model"),
                        div(id = "multi_species_second_models", style = "display: none;",
                            actionButton("btn_dnDs", "DnDs", class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_cdrom", "CDROM", class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_eve_expr", "EVE Expression Shift", 
                                         class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_eve_div", "EVE Diversity/Divergence", 
                                         class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            tags$script(HTML("
              $('.model-btn').click(function() {
                var currentColor = $(this).css('background-color');
                if (currentColor === 'rgb(0, 123, 255)') { // Blue
                  $(this).css('background-color', 'green');
                  $('#multi_species_plot_options').show();  // Show plot options
                } else {
                  $(this).css('background-color', '#007bff');  // Back to blue
                  $('#multi_species_plot_options').hide();  // Hide plot options
                }
              });
            "))
                        )
                 ),
                 column(4,
                        h4("Step 3: Plot Options"),
                        div(id = "multi_species_plot_options", style = "display: none;",
                            checkboxGroupInput("plot_opts", "Select Plot Options:",
                                               choices = c("Option A", "Option B", "Option C"))
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        h4("Model Customization and Input"),
                        uiOutput("multi_species_customization"),
                        style = "margin-top: 20px; border-top: 2px solid #ddd; padding-top: 10px;"
                 )
               )
             )
    ),
    tabPanel("One-Species", 
             mainPanel(
               fluidRow(
                 column(3,
                        h4("Step 1: First Model"),
                        actionButton("btn_blat", "BLAT", class = "model-btn", 
                                     style = "background-color: #007bff;"),
                        actionButton("btn_blast", "Blastp/Blastn/Blastx", 
                                     class = "model-btn", 
                                     style = "background-color: #007bff;"),
                        tags$script(HTML("
              $('#btn_blat, #btn_blast').click(function() {
                var currentColor = $(this).css('background-color');
                if (currentColor === 'rgb(0, 123, 255)') { // Blue
                  $(this).css('background-color', 'green');
                  $('#one_species_second_models').show();  // Show second models
                } else {
                  $(this).css('background-color', '#007bff');  // Back to blue
                  $('#one_species_second_models').hide();  // Hide second models
                  $('#one_species_third_model').hide();    // Hide third model
                  $('#one_species_plot_options').hide();   // Hide plot options
                }
              });
            "))
                 ),
                 column(3,
                        h4("Step 2: Second Model"),
                        div(id = "one_species_second_models", style = "display: none;",
                            actionButton("btn_dnDs_one", "Dn/Ds", class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_seg_dup", "Segregating Duplicates", 
                                         class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            tags$script(HTML("
              $('#btn_dnDs_one').click(function() {
                var currentColor = $(this).css('background-color');
                if (currentColor === 'rgb(0, 123, 255)') { // Blue
                  $(this).css('background-color', 'green');
                  $('#one_species_third_model').show();  // Show third model
                } else {
                  $(this).css('background-color', '#007bff');  // Back to blue
                  $('#one_species_third_model').hide();  // Hide third model
                  $('#one_species_plot_options').hide();  // Hide plot options
                }
              });
            "))
                        )
                 ),
                 column(3,
                        h4("Step 3: Third Model"),
                        div(id = "one_species_third_model", style = "display: none;",
                            actionButton("btn_seg_dup_third", "Segregating Duplicates", 
                                         class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            tags$script(HTML("
              $('#btn_seg_dup_third').click(function() {
                var currentColor = $(this).css('background-color');
                if (currentColor === 'rgb(0, 123, 255)') { // Blue
                  $(this).css('background-color', 'green');
                  $('#one_species_plot_options').show();  // Show plot options
                } else {
                  $(this).css('background-color', '#007bff');  // Back to blue
                  $('#one_species_plot_options').hide();  // Hide plot options
                }
              });
            "))
                        )
                 ),
                 column(3,
                        h4("Step 4: Plot Options"),
                        div(id = "one_species_plot_options", style = "display: none;",
                            checkboxGroupInput("plot_opts_one", "Select Plot Options:",
                                               choices = c("Option X", "Option Y", "Option Z"))
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        h4("Model Customization and Input"),
                        uiOutput("one_species_customization"),
                        style = "margin-top: 20px; border-top: 2px solid #ddd; padding-top: 10px;"
                 )
               )
             )
    )
  )
}


get_multi_species_model_options <- function(input) {
  renderUI({
    ui_elements <- list()  # Initialize an empty list for UI elements
    
    # Check if 'Orthofinder' is selected
    if (input$btn_orthofinder %% 2 == 1) {
      ui_elements <- c(ui_elements,
                       orthofinder_options(),
                       tags$div(style = "margin-bottom: 20px;")  # Space between models
      )
    }
    
    # Check if 'CDROM' is selected
    if (input$btn_cdrom %% 2 == 1) {
      ui_elements <- c(ui_elements,
                       cdrom_options(),
                       tags$div(style = "margin-bottom: 20px;")  # Space between models
      )
    }
    
    # Additional Options Button
    ui_elements <- c(ui_elements,
                     actionButton("toggle_additional_options", "Additional Options", class = "btn-secondary"),
                     conditionalPanel(
                       condition = "input.toggle_additional_options % 2 == 1",
                       tags$div(style = "margin-top: 20px;"),
                       orthofinder_additional_options(),
                       cdrom_additional_options()
                     )
    )
    
    # Return the combined UI elements as a fluid page
    fluidPage(do.call(tagList, ui_elements))
  })
}





get_single_species_model_options <- function(input) {
  
  renderUI({
    # Check if either BLAT or BLAST is selected
    if (!any(c("blat", "blast") %in% input$one_species_models)) return(NULL)
    
    # Show related buttons if any model is selected
    tagList(
      actionButton("btn_DnDs_one", 
                   "Dn/Ds", 
                   class = "model-btn", 
                   style = "background-color: #007bff;"),
      actionButton("btn_segregating_duplicates", 
                   "Segregating Duplicates", 
                   class = "model-btn", 
                   style = "background-color: #007bff;")
    )
  })
}




