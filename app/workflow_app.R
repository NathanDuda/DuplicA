library(shiny)
library(shinyFiles)
library(bslib)
library(htmltools)
library(shinyalert)
library(fs) # for file paths 
library(Biostrings) # translate() in translate_nucs_to_prots() in model_OrthoFinder.R
library(tools) # file_ext() in combine_raw_fastas() in model_OrthoFinder.R and file_path_sans_ext() in blat
library(testthat) # for unit tests
library(tidyverse)
library(readxl)
library(shinyjs)

source('./app/Scripts/setup.R')
source('./app/app_functions.R')
source('./app/app_pages.R')
source('./app/Scripts/model_OrthoFinder.R')
source('./app/Scripts/model_DnDs.R')
source('./app/Scripts/model_Segregating_Duplicates.R')
source('./app/Scripts/model_EVE.R')
source('./app/Scripts/model_Blat_Blast.R')





workflow_page <- function() {
  useShinyjs()  # Initialize shinyjs
  
  titlePanel("Workflow Selection")
  
  tabsetPanel(
    tabPanel("Multi-Species", 
             mainPanel(
               fluidRow(
                 column(4,
                        h4("Step 1: First Model"),
                        actionButton("btn_orthofinder", "OrthoFinder", class = "model-btn", style = "background-color: #007bff;"),
                        tags$script(HTML("
              $('#btn_orthofinder').click(function() {
                $(this).css('background-color', $(this).css('background-color') === 'rgb(0, 123, 255)' ? 'green' : '#007bff');
              });
            "))
                 ),
                 column(4,
                        h4("Step 2: Second Model"),
                        uiOutput("multi_species_second_models")
                 ),
                 column(4,
                        h4("Step 3: Plot Options"),
                        uiOutput("multi_species_plot_options")
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
                        actionButton("btn_blat", "BLAT", class = "model-btn", style = "background-color: #007bff;"),
                        actionButton("btn_blast", "Blastp/Blastn/Blastx", class = "model-btn", style = "background-color: #007bff;"),
                        tags$script(HTML("
              $('#btn_blat').click(function() {
                $(this).css('background-color', $(this).css('background-color') === 'rgb(0, 123, 255)' ? 'green' : '#007bff');
              });
              $('#btn_blast').click(function() {
                $(this).css('background-color', $(this).css('background-color') === 'rgb(0, 123, 255)' ? 'green' : '#007bff');
              });
            "))
                 ),
                 column(3,
                        h4("Step 2: Second Model"),
                        uiOutput("one_species_second_models")
                 ),
                 column(3,
                        h4("Step 3: Third Model"),
                        uiOutput("one_species_third_model")
                 ),
                 column(3,
                        h4("Step 4: Plot Options"),
                        uiOutput("one_species_plot_options")
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



# UI with dark mode theme and sidebar layout
ui <- fluidPage(
  theme = bs_theme(bg = "#222", fg = "white", primary = '#555'),  # Dark mode
  
  # Add CSS to ensure buttons are styled and the sidebar is narrower
  add_css_style(),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-buttons",  # Apply a class to target buttons in the sidebar
          
          actionButton("select_HOME", "HOME", class = "btn-primary"),
          h3("Workflow"),  # Move Workflow section up
          actionButton("select_workflow", "Workflow", class = "btn-primary"),  # Workflow button
          h5('Detect Duplications'),
          actionButton("select_blat", "BLAT", class = "btn-primary"),
          actionButton("select_blast", "BLAST", class = "btn-primary"),
          actionButton("select_orthofinder", "OrthoFinder", class = "btn-primary"),
          h3("Models"),
          h5('Functional Divergence'),
          actionButton("select_cdrom", "CDROM", class = "btn-primary"),
          actionButton("select_expression_shift", "Expression Shift", class = "btn-primary"), # EVE
          h5('Selective Pressure'),
          actionButton("select_dnds", "Dn/Ds", class = "btn-primary"),
          actionButton("select_diversity_divergence", "Diversity/Divergence", class = "btn-primary"), # EVE
          actionButton("select_segregating_duplicates", "Segregating Duplicates", class = "btn-primary"),
          br()
      )
    ),
    
    mainPanel(
      uiOutput("model_ui")  # Dynamic UI output for the selected model
    )
  )
)

server <- function(input, output, session) {
  
  roots <- c(home = "C:/Users/17735/Downloads")
  
  options(shiny.maxRequestSize = 50 * 1024^2)
  
  # Reactive value to store the current model selection
  current_model <- reactiveVal(NULL)
  
  # Update the UI based on the model selection
  observeEvent(input$select_cdrom, {current_model("cdrom")})
  observeEvent(input$select_cloud, {current_model("cloud")})
  observeEvent(input$select_orthofinder, {current_model("orthofinder")})
  observeEvent(input$select_HOME, {current_model("HOME")})
  observeEvent(input$select_dnds, {current_model("dnds")})
  observeEvent(input$select_segregating_duplicates, {current_model("segregating_duplicates")})
  observeEvent(input$select_expression_shift, {current_model("expression_shift")}) 
  observeEvent(input$select_diversity_divergence, {current_model("diversity_divergence")})
  observeEvent(input$select_blat, {current_model("blat")})
  observeEvent(input$select_blast, {current_model("blast")})
  
  # New observer for the workflow button
  observeEvent(input$select_workflow, {current_model("workflow")})
  
  # Render UI dynamically based on the selected model
  output$model_ui <- renderUI({
    model <- current_model()
    
    if (is.null(model)) {
      return(h3("Select a model from the sidebar."))  # Default message when no model is selected
    }
    
    switch(model,
           "cdrom" = tabsetPanel(
             cdrom_orthofinder_tab(),
             cdrom_custom_tab()
           ),
           "orthofinder" = tagList(
             orthofinder_page()
           ),
           "dnds" = tagList(
             dnds_page()
           ),
           'segregating_duplicates' = tagList(
             segregating_duplicates_page()
           ), 
           'expression_shift' = tagList(
             expression_shift_page()
           ),
           'diversity_divergence' = tagList(
             diversity_divergence_page()
           ),
           'blat' = tagList(
             blat_page()
           ),
           'blast' = tagList(
             blast_page()
           ),
           "workflow" = tagList(   # New workflow page
             workflow_page()
           ),
           "HOME" = tagList(
             titlePanel("DuplicA"),
             tags$div(style = "margin-bottom: 50px;"), # empty space
             
             h4('Select Data Types:'),
             checkboxGroupInput("data_types", "", 
                                choices = c(# multi species
                                  "Expression Data", 
                                  "OrthoFinder Data", 
                                  "Duplicate Gene Data",
                                  'Nucleotide CDS Sequences', 
                                  'Protein Sequences',
                                  # pop gen 
                                  'Population Duplicate Gene Data',
                                  'Population Nucleotide CDS Sequences'),
                                selected = character(0)),
             tags$div(style = "margin-bottom: 40px;"), # empty space
             uiOutput("available_models")
           ),
           h3("Please select a model from the sidebar.")  # Default content when no model is selected
    )
  })
  
  output$available_models <- renderUI({
    data_types <- input$data_types
    models <- c()
    
    # multi species
    if (("Nucleotide Sequences" %in% data_types) | ("Protein Sequences" %in% data_types)) {
      models <- c(models, "OrthoFinder")
      data_types <- c(data_types, 'OrthoFinder Data')
    }
    
    if (("Expression Data" %in% data_types) & ("OrthoFinder Data" %in% data_types)) {
      models <- c(models, "CDROM Model - OrthoFinder Input")
    }
    
    if (("Expression Data" %in% data_types) & ("Duplicate Gene Data" %in% data_types)) {
      models <- c(models, "CDROM Model - Custom Input")  # Assuming "CDROM Model" also works with OrthoFinder Data
    }
    
    # pop gen 
    if (("Population Duplicate Gene Data" %in% data_types) & ("Population Nucleotide CDS Sequences" %in% data_types)) {
      models <- c(models, "Segregating Duplicates")
    }
    
    if (("Nucleotide Sequences" %in% data_types)) {
      models <- c(models, "BLAT")
    }
    
    if (("Protein Sequences" %in% data_types)) {
      models <- c(models, "Blastp/Blastn/Blastx")
    }
    
    if (("Nucleotide Sequences" %in% data_types) & ("Protein Sequences" %in% data_types)) {
      models <- c(models, "Dn/Ds")
    }
    
    if (("Expression Data" %in% data_types)) {
      models <- c(models, "Expression Shift")
      models <- c(models, "Diversity/Divergence")
    }
    
    if (length(models) == 0) {
      return(h4("No models available based on selected data types."))
    }
    
    selectInput("selected_models", "Available Models", choices = models, selected = NULL, multiple = TRUE)
  })
  
  
  
  # Multi-species second model options
  output$multi_species_second_models <- renderUI({
    if (input$btn_orthofinder == 0) return(NULL)  # Check if Orthofinder is selected
    actionButton("btn_dnDs", "DnDs", class = "model-btn", style = "background-color: #007bff;")
    actionButton("btn_cdrom", "CDROM", class = "model-btn", style = "background-color: #007bff;")
    actionButton("btn_eve_expression_shift", "EVE Expression Shift", class = "model-btn", style = "background-color: #007bff;")
    actionButton("btn_eve_diversity", "EVE Diversity/Divergence", class = "model-btn", style = "background-color: #007bff;")
  })
  
  # Define other outputs similarly for one species and the plot options based on selected models
  output$one_species_second_models <- renderUI({
    # Show second model options based on the selected first model
    if (input$btn_blat == 0 && input$btn_blast == 0) return(NULL)
    actionButton("btn_dnDs_one", "Dn/Ds", class = "model-btn", style = "background-color: #007bff;")
    actionButton("btn_segregating_duplicates", "Segregating Duplicates", class = "model-btn", style = "background-color: #007bff;")
  })
}

shinyApp(ui = ui, server = server)
