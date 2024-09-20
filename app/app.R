library(shiny)
library(shinyFiles)
library(bslib)
library(htmltools)
library(shinyalert)
library(fs) # for file paths 

source('./Scripts/setup.R')
source('./app_functions.R')
source('./app_pages.R')
source('./Scripts/model_OrthoFinder.R')
source('./Scripts/model_DnDs.R')
source('./Scripts/model_Segregating_Duplicates.R')

# UI with dark mode theme and sidebar layout
ui <- fluidPage(
  theme = bs_theme(bg = "#222", fg = "white", primary = '#555'),  # Dark mode
  
  
  # Add CSS to ensure buttons are styled and the sidebar is narrower
  add_css_style(),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-buttons",  # Apply a class to target buttons in the sidebar

          actionButton("select_HOME", "HOME", class = "btn-primary"),
          h5('Detect Duplications'),
          actionButton("select_orthofinder", "OrthoFinder", class = "btn-primary"),
          h3("Models"),
          h5('Functional Divergence'),
          actionButton("select_cdrom", "CDROM", class = "btn-primary"),
          actionButton("select_cloud", "CLOUD", class = "btn-primary"),
          h5('Selective Pressure'),
          actionButton("select_dnds", "DnDs", class = "btn-primary"),
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
  
  # Render UI dynamically based on the selected model
  output$model_ui <- renderUI({
    model <- current_model()
    
    if (is.null(model)) {
      return(h3("Select a model from the sidebar."))  # Default message when no model is selected
    }
    
    switch(model,
           "cdrom" = tabsetPanel(
             cdrom_orthofinder_tab(),
             cdrom_custom_tab()),
           "cloud" = tagList(
             titlePanel("CLOUD"),
             p("This page will allow you to run CLOUD."),
             actionButton("run_cloud", "Run Model", class = "btn-success")
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
           "HOME" = tagList(
             titlePanel("DuplicA"),
             tags$div(style = "margin-bottom: 50px;"), # empty space
             
             h4('Select Data Types:'),
             checkboxGroupInput("data_types", "", 
                                choices = c(# comparative gen
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
    
    # comparative gen
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
      models <- c(models, "DnDs")  
      models <- c(models, "Segregating Duplicates Selection Model") 
    }
    
    
    
    # Output the list of available models
    tagList(
      h4("Available Models:"),
      tags$ul(lapply(models, function(model) {
        tags$li(model)
      }))
    )
  })
      
      
  # Function to handle file selection
  shinyFileChoose(input, "expression_file", roots = roots, session = session)
  shinyFileChoose(input, "ortho_dir", roots = roots, session = session)
  shinyFileChoose(input, "dups_file", roots = roots, session = session)
  shinyFileChoose(input, "exp_dir", roots = roots, session = session)
  shinyFileChoose(input, "protein_folder", roots = roots, session = session)
  
  shinyFileChoose(input, "cnvs_path", roots = roots, session = session)
  shinyFileChoose(input, "nuc_seqs_file", roots = roots, session = session)
  shinyFileChoose(input, "prot_seqs_file", roots = roots, session = session)
  
  
  # Reactive expression to get the selected file's absolute path
  expression_file <- reactive({
    req(input$expression_file)
    parseFilePaths(roots, input$expression_file)$datapath
  })
  
  ortho_dir <- reactive({
    req(input$ortho_dir)
    parseFilePaths(roots, input$ortho_dir)$datapath
  })
  
  dups_file <- reactive({
    req(input$dups_file)
    parseFilePaths(roots, input$dups_file)$datapath
  })
  
  exp_dir <- reactive({
    req(input$exp_dir)
    parseFilePaths(roots, input$exp_dir)$datapath
  })
  
  protein_folder <- reactive({
    req(input$protein_folder)
    parseFilePaths(roots, input$protein_folder)$datapath
  })
  
  
  cnvs_path <- reactive({
    req(input$cnvs_path)
    parseFilePaths(roots, input$cnvs_path)$datapath
  })
  
  nuc_seqs_file <- reactive({
    req(input$nuc_seqs_file)
    parseFilePaths(roots, input$nuc_seqs_file)$datapath
  })
  
  prot_seqs_file <- reactive({
    req(input$prot_seqs_file)
    parseFilePaths(roots, input$prot_seqs_file)$datapath
  })
  
  # show file paths in UI when selected 
  output$expression_file_path <- renderText({expression_file()})
  output$ortho_dir_path <- renderText({ortho_dir()})
  output$dups_file_path <- renderText({dups_file()})
  output$exp_dir_path <- renderText({exp_dir()})
  output$protein_folder_path <- renderText({protein_folder()})
  output$cnvs_path <- renderText({cnvs_path()})
  output$nuc_seqs_file <- renderText({nuc_seqs_file()})
  output$prot_seqs_file <- renderText({prot_seqs_file()})
  
  # Run model when "Run Model" button is clicked
  observeEvent(input$run_cdrom, {
    withProgress(message = 'Running CDROM model...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      result <- run_r_script(
        run_type = "OF",
        script_name = "model_CDROM.R",
        expression_file = expression_file(),
        ortho_dir = dirname(dirname(ortho_dir())),
        dups_file = NULL,
        exp_dir = NULL,
        add_pseudofunc = input$add_pseudofunc,
        missing_expr_is_pseudo = input$missing_expr_is_pseudo,
        exp_cutoff = input$exp_cutoff,
        PC = FALSE,
        min_dups_per_species_pair = input$min_dups_per_species_pair,
        use_absolute_exp = input$use_absolute_exp
      )
      
      incProgress(0.7, detail = "Processing results")
      
      shinyalert("Result", result, type = "info")
    })
  })
  
  observeEvent(input$run_custom_cdrom, {
    withProgress(message = 'Running CDROM model...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
        
      req(input$dups_file)
      req(input$exp_dir)
      
      result <- run_r_script(
        run_type = "custom",
        script_name = "model_CDROM.R",
        dups_file = dups_file(),
        exp_dir = exp_dir(),
        add_pseudofunc = input$add_pseudofunc_custom,
        missing_expr_is_pseudo = input$missing_expr_is_pseudo_custom,
        exp_cutoff = input$exp_cutoff_custom,
        PC = input$PC,
        min_dups_per_species_pair = input$min_dups_per_species_pair_custom,
        use_absolute_exp = input$use_absolute_exp_custom
      )
      
      shinyalert("Success!", result, type = "info")
    })
  })
  
  observeEvent(input$run_cloud, {
    shinyalert("Result", "CLOUD execution logic is not implemented yet.", type = "info")
  })
  
  observeEvent(input$run_orthofinder, {
    withProgress(message = 'Running OrthoFinder', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      main_OrthoFinder(
        protein_folder = dirname(protein_folder()),
        is_dna = input$nuc_not_prot, 
        method = input$gene_tree_inference_method,
        sequence_search = input$sequence_search_method, 
        msa_program = input$msa_method,
        tree_method = input$tree_method,
        species_tree = input$species_tree_path, 
        mcl_inflation = input$mcl_inflation,
        split_hogs = input$split_hogs, 
        no_msa_trim = input$msa_trim) 
        
      
      shinyalert("Success!", '', type = "info")
    })
  })
  
  observeEvent(input$run_dnds_pop, {
    
    main_pop_dnds(cnvs_path = input$cnvs_path,
                  input$nuc_seqs_file, 
                  input$prot_seqs_file, 
                  aligner = input$dnds_aligner, 
                  replace_dirs = F) # CHANGE DEFAULT TO TRUE WHEN can input directory of nuc and prot files 
      
  })
  
  observeEvent(input$run_segregating_duplications, {
    withProgress(message = 'Running Segregating Duplications', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      main_Segregating_Duplications(
        cnvs_path = input$cnvs_path, 
        popgen_dnds_exists = F, # set to true always in model. fix when dnds workflow option works 
        n_individuals = input$n_individuals, 
        ploidy = input$ploidy, 
        ks_oversaturation_cutoff = input$ks_cutoff,
        filter_whole_group = input$filter_whole_group)

      
      shinyalert("Success!", '', type = "info")
    })
  })
  

}

shinyApp(ui, server)
