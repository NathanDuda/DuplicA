library(shiny)
library(shinyFiles)
library(bslib)  # For dark mode
library(htmltools)


source('./app_functions.R')

# UI with dark mode theme and sidebar layout
ui <- fluidPage(
  theme = bs_theme(bg = "#222", fg = "white", primary = '#555'),  # Dark mode
  
  # Add CSS to ensure buttons are styled and the sidebar is narrower
  tags$style(HTML("
    .sidebar-buttons .action-button {
      display: block;
      width: 100%;
      margin-bottom: 15px;
      background-color: #007BFF;  /* Blue buttons */
      border-color: #007BFF;
    }
    .sidebar-buttons .action-button:hover {
      background-color: #0056b3;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-buttons",  # Apply a class to target buttons in the sidebar
          
          h3("Models"),
          h5('Functional Divergence'),
          actionButton("select_cdrom", "CDROM Model", class = "btn-primary"),
          actionButton("select_cloud", "CLOUD Model", class = "btn-primary"),
          h5('Selective Pressure', ),
          actionButton("select_model3", "Model 3", class = "btn-primary"),
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
  observeEvent(input$select_cdrom, {
    current_model("cdrom")
  })
  
  observeEvent(input$select_cloud, {
    current_model("model2")
  })
  
  observeEvent(input$select_model3, {
    current_model("model3")
  })
  
  # Render UI dynamically based on the selected model
  output$model_ui <- renderUI({
    model <- current_model()
    
    if (is.null(model)) {
      return(h3("Select a model from the sidebar."))  # Default message when no model is selected
    }
    
    switch(model,
           "cdrom" = tabsetPanel(
             tabPanel("OrthoFinder Input", 
                      titlePanel("CDROM Model - OrthoFinder Input"),
                      h3("OrthoFinder Input"),
                      p("Run the CDROM model using OrthoFinder data."),
                      
                      shinyFilesButton("expression_file", "Browse: Expression File", '', multiple = F),  # Expression as a file input
                      shinyFilesButton("ortho_dir", "Browse: OrthoFinder Folder", '', multiple = F),
                      
                      numericInput("min_dups_per_species_pair", "Min dups per species pair", value = 10, min = 1),
                      numericInput("exp_cutoff", "Expression cutoff", value = 1),
                      
                      checkboxInput("add_pseudofunc", "Add pseudofunctionalization?", value = FALSE),
                      checkboxInput("missing_expr_is_pseudo", "Should genes with missing expression data be considered pseudofunctionalized?", value = FALSE),
                      checkboxInput("use_absolute_exp", "Use absolute expression?", value = FALSE),
                      
                      actionButton("run_cdrom", "Run Model", class = "btn-success")
             ),
             tabPanel("Custom Input", 
                      titlePanel("CDROM Model - Custom Input"),
                      h3("Custom Input"),
                      p("Run the CDROM model using custom input."),
                      
                      shinyFilesButton("dups_file", "Browse: Duplicate Genes File", '', multiple = F),
                      shinyFilesButton("exp_dir", "Browse: Expression Folder", '', multiple = F),
                      
                      numericInput("min_dups_per_species_pair_custom", "Min dups per species pair", value = 10, min = 1),
                      numericInput("exp_cutoff_custom", "Set expression values lower than _ to 0", value = 1),
                      
                      checkboxInput("PC", "Are parent/child copies differentiated?", value = FALSE),
                      checkboxInput("add_pseudofunc_custom", "Add pseudofunctionalization?", value = FALSE),
                      checkboxInput("missing_expr_is_pseudo_custom", "Should genes with missing expression data be considered pseudofunctionalized?", value = FALSE),
                      checkboxInput("use_absolute_exp_custom", "Use absolute expression?", value = FALSE),
                      
                      actionButton("run_custom_cdrom", "Run Model", class = "btn-success")
             )
           ),
           "model2" = tagList(
             titlePanel("Model 2"),
             h3("Model 2 Page"),
             p("This page will allow you to run Model 2."),
             actionButton("run_model2", "Run Model", class = "btn-success")
           ),
           "model3" = tagList(
             titlePanel("Model 3"),
             h3("Model 3 Page"),
             p("This page will allow you to run Model 3."),
             actionButton("run_model3", "Run Model", class = "btn-success")
           ),
           h3("Please select a model from the sidebar.")  # Default content when no model is selected
    )
  })
  
  # Function to handle directory selection
  shinyFileChoose(input, "expression_file", roots = roots, session = session)
  shinyFileChoose(input, "ortho_dir", roots = roots, session = session)
  shinyFileChoose(input, "dups_file", roots = roots, session = session)
  shinyFileChoose(input, "exp_dir", roots = roots, session = session)
  
  # Reactive expression to get the selected file's absolute path
  expression_file <- reactive({
    req(input$expression_file)
    parseFilePaths(roots, input$expression_file)$datapath
  })
  ortho_dir <- reactive({
    req(input$ortho_dir)
    full_path <- parseFilePaths(roots, input$ortho_dir)$datapath
    
    # Remove the last two directory levels
    dirname(dirname(full_path))
  })
  

  dups_file <- reactive({
    req(input$dups_file)
    parseFilePaths(roots, input$dups_file)$datapath
  })
  exp_dir <- reactive({
    req(input$exp_dir)
    parseFilePaths(roots, input$exp_dir)$datapath
  })
  
  
  # Run model when "Run Model" button is clicked
  observeEvent(input$run_cdrom, {
    
    result <- run_r_script(
      run_type = "OF",
      script_name = "model_CDROM.R",
      expression_file = expression_file(),
      ortho_dir = ortho_dir(),
      dups_file = NULL,
      exp_dir = NULL,
      add_pseudofunc = input$add_pseudofunc,
      missing_expr_is_pseudo = input$missing_expr_is_pseudo,
      exp_cutoff = input$exp_cutoff,
      PC = FALSE,
      min_dups_per_species_pair = input$min_dups_per_species_pair,
      use_absolute_exp = input$use_absolute_exp
    )
    
    shinyalert("Result", result, type = "info")
  })
  
  observeEvent(input$run_custom_cdrom, {
    req(input$dups_file)
    req(input$exp_dir)
    
    dups_file <- input$dups_file$datapath
    exp_dir <- parseDirPath(roots, input$exp_dir)
    
    result <- run_r_script(
      run_type = "custom",
      script_name = "model_CDROM.R",
      expression_file = NULL,
      ortho_dir = NULL,
      dups_file = dups_file,
      exp_dir = exp_dir,
      add_pseudofunc = input$add_pseudofunc_custom,
      missing_expr_is_pseudo = input$missing_expr_is_pseudo_custom,
      exp_cutoff = input$exp_cutoff_custom,
      PC = input$PC,
      min_dups_per_species_pair = input$min_dups_per_species_pair_custom,
      use_absolute_exp = input$use_absolute_exp_custom
    )
    
    shinyalert("Result", result, type = "info")
  })
  
  observeEvent(input$run_model2, {
    shinyalert("Result", "Model 2 execution logic is not implemented yet.", type = "info")
  })
  
  observeEvent(input$run_model3, {
    shinyalert("Result", "Model 3 execution logic is not implemented yet.", type = "info")
  })
}

shinyApp(ui, server)
