


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
                            actionButton("btn_dnds", "DnDs", class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_cdrom", "CDROM", class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_expression_shift", "EVE Expression Shift", 
                                         class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_diversity_divergence", "EVE Diversity/Divergence", 
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
               ),
               conditionalPanel(
                 condition = "input.btn_orthofinder % 2 == 1",
                 actionButton("wf_additional_options", "Additional Options", class = "btn-secondary")
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
                            actionButton("btn_dnds_one", "Dn/Ds", class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            actionButton("btn_segregating_duplicates", "Segregating Duplicates", 
                                         class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            tags$script(HTML("
              $('#btn_dnds_one').click(function() {
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
                            actionButton("btn_segregating_duplicates_third", "Segregating Duplicates", 
                                         class = "model-btn", 
                                         style = "background-color: #007bff;"),
                            tags$script(HTML("
              $('#btn_segregating_duplicates_third').click(function() {
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
    ),
    div(style = "text-align: center; margin-top: 30px;",
        actionButton("run_workflow", "Run Workflow", 
                     class = "btn-success", 
                     style = "width: 200px; height: 50px; font-size: 18px;")
    )
  )
}


multi_species_models <- c('orthofinder', 'cdrom', 'dnds', 'segregating_duplicates',
                          'expression_shift', 'diversity_divergence', 'blat', 'blast')



get_multi_species_model_options <- function(input) {
  renderUI({
    ui_elements <- list()
    
    # Main Options
    for (model in multi_species_models) {
      btn_model <- paste0('btn_', model)
      model_options <- get(paste0(model, '_options'))
      
      # if model is selected, add its options to ui_elements
      if (input[[btn_model]] %% 2 == 1) {ui_elements <- c(ui_elements, model_options())}
    }
    
    # Additional Options
    # If the additional options button is toggled, add additional options
    if (!is.null(input$wf_additional_options) && input$wf_additional_options %% 2 == 1) {
      for (model in multi_species_models) {
        btn_model <- paste0('btn_', model)
        additional_options <- get(paste0(model, '_additional_options'))
        
        if (input[[btn_model]] %% 2 == 1) {
          ui_elements <- c(ui_elements, additional_options())
        }
      }
    }
    
    # Return the combined UI elements as a fluid page
    tagList(ui_elements)
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




